# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)

source(here::here("R/00-helpers.R"))
source(here::here("R/wrc/00-ewrc-helpers.R"))

# config -----------------------------------------------------------------
ewrc_models_dir <- "data/models/wrc"

fs::dir_create(ewrc_models_dir)

# rallies ----------------------------------------------------------------
rallies_min <- tribble(
  ~rally_slug,       ~rally_name,       ~year_from, ~year_to,
  "monte_carlo",     "Monte Carlo",          1980,     2010,
  "sweden",          "Sweden",               1980,     2010,
  "portugal",        "Portugal",             1980,     2010,
  "safari",          "Safari",               1980,     2010,
  "acropolis",       "Acropolis",            1980,     2010,
  "finland",         "Finland",              1980,     2010,
  "sanremo",         "Sanremo",              1980,     2010,
  "tour_de_corse",   "Tour de Corse",        1980,     2010,
  "rally_gb",        "Rally GB",             1980,     2010,
  "catalunya",       "Catalunya",            1991,     2010,
  "new_zealand",     "New Zealand",          1993,     2010,
  "australia",       "Australia",            1990,     2010
)

rallies_extra <- tribble(
  ~rally_slug,        ~rally_name,          ~year,
  
  # 70s / Lancia Stratos, Fiat 124, Escort, Alpine, Peugeot 504
  "monte_carlo",      "Monte Carlo",         1974,
  "monte_carlo",      "Monte Carlo",         1975,
  "sweden",           "Sweden",              1975,
  "safari",           "Safari",              1975,
  "finland",          "1000 Lakes",          1975,
  "sanremo",          "Sanremo",             1975,
  "tour_de_corse",    "Tour de Corse",       1975,
  "rally_gb",         "RAC Rally",           1975,
  "rideau_lakes",     "Rideau Lakes",        1974,
  "morocco",          "Morocco",             1975,
  "southern_cross",   "Southern Cross",      1975,
  
  # africanos/duros que pueden tener Peugeot, Datsun, Mitsubishi, Lancia, etc.
  "ivory_coast",      "Ivory Coast",         1981,
  "ivory_coast",      "Ivory Coast",         1982,
  
  # 90s fuera del bloque principal, útiles por Subaru/Mitsubishi/Toyota/Ford
  "argentina",        "Argentina",           1996,
  "argentina",        "Argentina",           1997,
  "argentina",        "Argentina",           1998,
  "argentina",        "Argentina",           1999,
  "indonesia",        "Indonesia",           1996,
  "indonesia",        "Indonesia",           1997,
  "china",            "China",               1999
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

# scrape -----------------------------------------------------------------
ewrc_session <- chromote::ChromoteSession$new()

rallies |>
  # filter(!already_done) |>
  select(rally_slug, rally_name, year) |>
  pwalk(function(rally_slug, rally_name, year) {
    scrape_one_ewrc_models_file(
      ewrc_session = ewrc_session,
      rally_slug = rally_slug,
      rally_name = rally_name,
      year = year
    )
  })
