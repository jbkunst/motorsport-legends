# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)

source(here::here("R/00-helpers.R"))
source(here::here("R/wrc/00-ewrc-helpers.R"))

# config -----------------------------------------------------------------
ewrc_models_dir <- "data/models/wrc"
scrape_limit <- 5L # set to Inf for a full missing-files run

fs::dir_create(ewrc_models_dir)

# rallies ----------------------------------------------------------------
rallies_min <- tribble(
  ~rally_slug,       ~rally_name,       ~year_from, ~year_to,
  "monte_carlo",     "Monte Carlo",          1980,     2010,
  "sweden",          "Sweden",               1980,     2010,
  "portugal",        "Portugal",             1980,     2010,
  "safari",          "Safari",               1980,     2010,
  "acropolis",       "Acropolis",            1980,     2010,
  "finland",         "1000 Lakes",           1980,     2010,
  "sanremo",         "Sanremo",              1980,     2010,
  "corsica",         "Tour de Corse",        1980,     2010,
  "great_britain",   "Rally GB",             1980,     2010,
  "catalunya",       "Catalunya",            1991,     2010,
  "new_zealand",     "New Zealand",          1993,     2010,
  "australia",       "Australia",            1990,     2010
)

rallies_extra <- tribble(
  ~rally_slug,                  ~rally_name,              ~year,

  # 70s / Lancia Stratos, Fiat 124, Escort, Alpine, Peugeot 504
  "monte_carlo",                "Monte Carlo",            1974,
  "monte_carlo",                "Monte Carlo",            1975,
  "sweden",                     "Sweden",                 1975,
  "safari",                     "Safari",                 1975,
  "finland",                    "1000 Lakes",             1975,
  "sanremo",                    "Sanremo",                1975,
  "corsica",                    "Tour de Corse",          1975,
  "great_britain",              "RAC Rally",              1975,
  "canada_rideau_lakes",        "Rideau Lakes",           1974,
  "morocco",                    "Morocco",                1975,
  "southern_cross",             "Southern Cross",         1975,

  # North American WRC events with collectible/model interest
  "canada_quebec",              "Quebec",                 1977,
  "canada_quebec",              "Quebec",                 1978,
  "canada_quebec",              "Quebec",                 1979,
  "usa_olympus",                "Olympus",                1986,
  "usa_olympus",                "Olympus",                1987,
  "usa_olympus",                "Olympus",                1988,
  "usa_press_on_regardless",    "Press-on-Regardless",    1973,
  "usa_press_on_regardless",    "Press-on-Regardless",    1974,

  # africanos/duros que pueden tener Peugeot, Datsun, Mitsubishi, Lancia, etc.
  "ivory_coast",                "Ivory Coast",            1981,
  "ivory_coast",                "Ivory Coast",            1982,

  # 90s fuera del bloque principal, utiles por Subaru/Mitsubishi/Toyota/Ford
  "argentina",                  "Argentina",              1996,
  "argentina",                  "Argentina",              1997,
  "argentina",                  "Argentina",              1998,
  "argentina",                  "Argentina",              1999,
  "indonesia",                  "Indonesia",              1996,
  "indonesia",                  "Indonesia",              1997,
  "china",                      "China",                  1999
)

rallies <- rallies_min |>
  mutate(year = map2(year_from, year_to, seq)) |>
  unnest(year) |>
  select(
    rally_slug,
    rally_name,
    year
  ) |>
  bind_rows(rallies_extra) |>
  distinct(rally_slug, year, .keep_all = TRUE) |>
  mutate(
    event_slug = str_glue("{rally_slug}_{year}"),
    event_name = str_glue("{rally_name} {year}"),
    fout = str_glue("{ewrc_models_dir}/{event_slug}.csv"),
    already_done = file.exists(fout)
  ) |>
  arrange(year, rally_slug)

rallies |>
  count(already_done) |>
  print(n = Inf)

scrape_plan <- rallies |>
  filter(!already_done) |>
  slice_head(n = scrape_limit)

scrape_plan

# scrape -----------------------------------------------------------------
new_ewrc_session <- function() {
  tryCatch(
    chromote::ChromoteSession$new(),
    error = function(e) {
      cli::cli_abort(c(
        "No se pudo iniciar Chrome via chromote.",
        "i" = "Ejecuta `chromote::chromote_info()` para revisar la configuracion local.",
        "i" = "Si Chrome quedo colgado de una corrida anterior, reinicia la sesion de R e intenta de nuevo.",
        "x" = conditionMessage(e)
      ))
    }
  )
}

close_ewrc_session <- function(ewrc_session) {
  try(ewrc_session$close(), silent = TRUE)
}

scrape_one_ewrc_models_file_local <- function(rally_slug, rally_name, year, models_dir = ewrc_models_dir) {
  event_slug <- str_glue("{rally_slug}_{year}")
  event_name <- str_glue("{rally_name} {year}")
  fout <- str_glue("{models_dir}/{event_slug}.csv")
  
  if (file.exists(fout)) {
    cli::cli_inform("Already exists: {fout}")
    return(invisible(TRUE))
  }
  
  fs::dir_create(dirname(fout))
  
  ewrc_event_url <- resolve_ewrc_event_url(
    rally_slug = rally_slug,
    rally_name = rally_name,
    year = year
  )
  
  if (is.na(ewrc_event_url) || ewrc_event_url == "") {
    empty_ewrc_models_file() |>
      write_csv(fout)
    
    cli::cli_warn("Saved empty file: {fout}")
    return(invisible(FALSE))
  }
  
  ewrc_session <- new_ewrc_session()
  on.exit(close_ewrc_session(ewrc_session), add = TRUE)
  
  models <- scrape_ewrc_models(
    ewrc_session = ewrc_session,
    ewrc_event_url = ewrc_event_url
  ) |>
    filter(
      !is.na(image_url),
      image_url != ""
    ) |>
    mutate(
      event_slug = event_slug,
      rally_slug = rally_slug,
      rally_name = rally_name,
      event_name = event_name,
      year = year,
      ewrc_event_url = ewrc_event_url,
      .before = 1
    )
  
  if (nrow(models) == 0) {
    models <- empty_ewrc_models_file()
  }
  
  models |>
    write_csv(fout)
  
  cli::cli_inform("Saved: {fout} ({nrow(models)} rows)")
  
  invisible(TRUE)
}

scrape_plan |>
  select(rally_slug, rally_name, year) |>
  pwalk(function(rally_slug, rally_name, year) {
    scrape_one_ewrc_models_file_local(
      rally_slug = rally_slug,
      rally_name = rally_name,
      year = year
    )
  })
