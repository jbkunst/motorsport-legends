# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)

source(here::here("R/00-helpers.R"))
source(here::here("R/wrc/00-ewrc-helpers.R"))

# config -----------------------------------------------------------------
wrc_results_dir <- "data/wrc"
ewrc_models_dir <- "data/models/wrc"

fs::dir_create(ewrc_models_dir)

# helpers ----------------------------------------------------------------
slug_to_rally_name <- function(rally_slug) {
  rally_slug |>
    str_remove("^(usa|canada)_") |>
    str_replace_all("_", " ") |>
    str_to_title()
}

rally_name_overrides <- tribble(
  ~rally_slug,                 ~rally_name,
  "canada_quebec",             "Quebec",
  "canada_rideau_lakes",       "Rideau Lakes",
  "central_european_rally",    "Central European Rally",
  "corsica",                   "Tour de Corse",
  "finland",                   "1000 Lakes",
  "great_britain",             "Rally GB",
  "ivory_coast",               "Ivory Coast",
  "monte_carlo",               "Monte Carlo",
  "new_zealand",               "New Zealand",
  "saudiarabia",               "Saudi Arabia",
  "tour_de_corse",             "Tour de Corse",
  "usa_olympus",               "Olympus",
  "usa_press_on_regardless",   "Press-on-Regardless"
)

read_wrc_results_index <- function(wrc_results_dir = "data/wrc") {
  fs::dir_ls(wrc_results_dir, recurse = TRUE, glob = "*.csv") |>
    tibble(file = _) |>
    mutate(
      rally_slug = basename(dirname(file)),
      year = fs::path_ext_remove(basename(file)) |> as.integer()
    ) |>
    filter(!is.na(year)) |>
    distinct(rally_slug, year) |>
    left_join(rally_name_overrides, by = join_by(rally_slug)) |>
    mutate(
      rally_name = coalesce(rally_name, slug_to_rally_name(rally_slug))
    ) |>
    select(
      rally_slug,
      rally_name,
      year
    ) |>
    arrange(year, rally_slug)
}

# rallies ----------------------------------------------------------------
rallies <- read_wrc_results_index(wrc_results_dir) |>
  distinct(rally_slug, year, .keep_all = TRUE) |>
  select(
    rally_slug,
    rally_name,
    year
  ) |>
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
