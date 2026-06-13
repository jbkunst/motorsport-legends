# setup ------------------------------------------------------------------
source(here::here("R/rsc/00-rsc-helpers.R"))
source(here::here("R/00-helpers.R"))

library(dplyr)
library(purrr)
library(tibble)

# data -------------------------------------------------------------------
races_other <- tribble(
  ~track,          ~year, ~event,                            ~reason,                                      ~url,
  "nurburgring",   1960, "Nürburgring 1000 Kilometres",      "Maserati Tipo 61 #5 winner",                 "https://www.racingsportscars.com/photo/Nurburgring-1960-06-22.html?sort=Results",
  "riverside",     1960, "Grand Prix Riverside 200 Miles",   "Maserati Tipo 61 #98 Carroll Shelby",        "https://www.racingsportscars.com/photo/Riverside-1960-10-16.html?sort=Results",
  "laguna_seca",   1981, "Laguna Seca",                      "Porsche 935/78 Moby Dick MOMO",              "https://www.racingsportscars.com/photo/Laguna_Seca-1981-05-03.html?sort=Results",
  "sebring",       1971, "Sebring 12 Hours",                 "Porsche 917 K winner",                       "https://www.racingsportscars.com/photo/Sebring-1971-03-20.html?sort=Results",
  "sebring",       1973, "Sebring 12 Hours",                 "Porsche 911 Carrera RSR winner",             "https://www.racingsportscars.com/photo/Sebring-1973-03-24.html?sort=Results",
  "watkins_glen",  1981, "Watkins Glen 6 Hours",             "Lancia Beta Montecarlo Turbo CM's 1:64",     "https://www.racingsportscars.com/photo/Watkins_Glen-1981-07-12.html?sort=Results",
  "norisring",     1977, "Norisring Trophy",                 "Porsche 935 #52 Jägermeister Kyosho",        "https://www.racingsportscars.com/photo/Norisring-1977-07-03t.html?sort=Results",
  "zolder",        1977, "DRM Zolder Bergischer Löwe",       "Minichamps Porsche 935 Vaillant",            "https://www.racingsportscars.com/photo/Zolder-1977-03-13.html?sort=Results", # Tambien participa el porshce 935 de Jägermeister
  "nurburgring",   1977, "Nürburgring 300 Kilometres",       "BMW 320i Gr.5 #15 Jägermeister Kyosho",      "https://www.racingsportscars.com/photo/Nurburgring-1977-03-27.html?sort=Results",
  "nurburgring",   1976, "Nürburgring 1000 Kilometres",      "Porsche 934 #24 Jägermeister Kyosho",        "https://www.racingsportscars.com/photo/Nurburgring-1976-05-30.html?sort=Results",
  "nurburgring",   1976, "Nürburgring 300 Kilometres",       "Porsche 934 #53 Jägermeister Kyosho",        "https://www.racingsportscars.com/photo/Nurburgring-1976-04-04.html?sort=Results"
)

races_other_extra <- tribble(
  ~track,          ~year, ~event,                         ~reason,                                      ~url,
  "fuji",          1985, "Fuji 1000 Kilometres",           "Group C Japan reference",                    "https://www.racingsportscars.com/photo/Fuji-1985-10-06.html?sort=Results",
  "suzuka",        1995, "Suzuka 1000 Kilometres",         "Japanese GT reference",                      "https://www.racingsportscars.com/photo/Suzuka-1995-08-27.html?sort=Results",
  "spa",           1970, "Spa 1000 Kilometres",            "Porsche 917 vs Ferrari 512 era",             "https://www.racingsportscars.com/photo/Spa-1970-05-17.html?sort=Results",
  "monza",         1971, "Monza 1000 Kilometres",          "Classic Ferrari, Porsche and Alfa prototypes","https://www.racingsportscars.com/photo/Monza-1971-04-25.html?sort=Results",
  "spa",           1982, "Spa 1000 Kilometres",            "Early Group C reference",                    "https://www.racingsportscars.com/photo/Spa-1982-09-05.html?sort=Results",
  "fuji",          1985, "Fuji 1000 Kilometres",           "All Japan local Group C context",            "https://www.racingsportscars.com/photo/Fuji-1985-05-05.html?sort=Results",
  "brands_hatch",  1985, "Brands Hatch 1000 Kilometres",   "Group C reference",                          "https://www.racingsportscars.com/photo/Brands_Hatch-1985-09-22.html?sort=Results",
  "nurburgring",   1971, "Nürburgring 500 Kilometres",     "GT and touring context",                     "https://www.racingsportscars.com/photo/Nurburgring-1971-09-05.html?sort=Results"
)

races_other <- bind_rows(races_other, races_other_extra)
# scrape -----------------------------------------------------------------
walk(races_other$url, scrape_race)


races_other <- races_other |>
  mutate(
    date = str_extract(url, "\\d{4}-\\d{2}-\\d{2}"),
    track = str_match(url, "/photo/([^/-]+)-\\d{4}-\\d{2}-\\d{2}")[, 2] |>
      str_to_lower() |>
      str_replace_all("-", "_"),
    file = here::here("data/races", track, "results", paste0(date, ".csv"))
  ) 

dt <- races_other |>
  pull(file) |>
  map(
    read_csv,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_guess(),
      grid_time = col_character(),
      dnf_reason = col_character()
    )
  ) |>
  map(add_missing_cols, c("chassis", "chassis_url")) |>
  map2(races_other$event, function(x, y) { x |> mutate(track = y, .before = 1) }) |>
  bind_rows() |>
  mutate(grid_time = suppressWarnings(lubridate::ms(grid_time))) |> 
  prep_rsc_dt_data() |> 
  make_race_dt("other")

dt
