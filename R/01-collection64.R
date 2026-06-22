# setup ------------------------------------------------------------------
library(tidyverse)
source(here::here("R/endurance/00-rsc-helpers.R"))
# source(here::here("R/wrc/00-ewrc-helpers.R"))
# source(here::here("R/00-helpers.R"))

# helpers ----------------------------------------------------------------
wrc_driver_key <- function(driver_name) {
  driver_name |>
    clean_txt() |>
    stringr::str_remove(",.*$") |>
    stringr::str_to_lower()
}

read_wrc_results_files <- function(wrc_dir = "data/wrc") {
  fs::dir_ls(wrc_dir, recurse = TRUE, glob = "*.csv") |>
    purrr::map_dfr(function(file) {
      rally_slug <- basename(dirname(file))
      year <- fs::path_ext_remove(basename(file)) |> as.integer()

      readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) |>
        mutate(
          rally_slug = rally_slug,
          year = year,
          entry_number = readr::parse_integer(entry_number),
          position = readr::parse_integer(position),
          driver_key = wrc_driver_key(driver_name),
          result_status = if_else(is.na(position), "dnf", "finished"),
          .before = 1
        )
    })
}

read_wrc_models_files <- function(models_dir = "data/models/wrc") {
  fs::dir_ls(models_dir, glob = "*.csv") |>
    purrr::map_dfr(
      readr::read_csv,
      col_types = readr::cols(.default = readr::col_character())
    ) |>
    mutate(year = as.integer(year)) |>
    select_preferred_ewrc_models()
}

# data collection ---------------------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQxCLyOJ3I5tnjgwTlxiMVZagQ19DiZpoc_3xBOTdmnoo8gbai5MepFqCY2vAE27guGTAxjKlWti0SD/pub?gid=132013729&single=true&output=csv"

data_collection <- readr::read_csv(url)

data_collection <- data_collection |>
  # avoid extra column due incorrect pasting
  # select(track, year, number, name, make, scale64_maker, scale64_status, note)
  select(everything())

glimpse(data_collection)

# endurance ---------------------------------------------------------------
collection_endurance <- fs::dir_ls("data/endurance/") |>
  purrr::map_df(function(folder = "data/endurance/daytona") {
    cli::cli_inform(folder)

    rr <- load_race_results(str_c(folder, "")) |>
      mutate(year = lubridate::year(date), .after = 1)

    rr <- rr |>
      arrange(date, group, result) |>
      group_by(date, group) |>
      mutate(result_group = if_else(is.na(result), NA_integer_, cumsum(!is.na(result)))) |>
      ungroup()

    dc <- filter(data_collection, event_slug == basename(folder))

    # inner: conserva solo cruces validos; el anti_join posterior falla si falta algo de la coleccion.
    dout <- inner_join(rr, dc, by = join_by(track == event_slug, year, number, make))
    dout <- arrange(dout, year, result, number)

    dout |>
      count(track, year, number, sort = TRUE) |>
      filter(n > 1) |>
      nrow() |>
      {\(x) stopifnot("Hay duplicados por race/year/number" = x == 0)}()

    dout
  })

collection_endurance <- arrange(collection_endurance, year, track, result)

checks_endurance <- anti_join(data_collection, collection_endurance, by = join_by(event_slug == track, year, number, make))
checks_endurance

# wrc ---------------------------------------------------------------------


collection_wrc <- fs::dir_ls("data/wrc/") |>
  purrr::map_df(function(folder = "data/endurance/daytona") {
    cli::cli_inform(folder)

    rr <- load_race_results(str_c(folder, "")) |>
      mutate(year = lubridate::year(date), .after = 1)

    rr <- rr |>
      arrange(date, group, result) |>
      group_by(date, group) |>
      mutate(result_group = if_else(is.na(result), NA_integer_, cumsum(!is.na(result)))) |>
      ungroup()

    dc <- filter(data_collection, event_slug == basename(folder))

    # inner: conserva solo cruces validos; el anti_join posterior falla si falta algo de la coleccion.
    dout <- inner_join(rr, dc, by = join_by(track == event_slug, year, number, make))
    dout <- arrange(dout, year, result, number)

    dout |>
      count(track, year, number, sort = TRUE) |>
      filter(n > 1) |>
      nrow() |>
      {\(x) stopifnot("Hay duplicados por race/year/number" = x == 0)}()

    dout
  })


# Bloque de trabajo para completar. Idea:
# 1. filtrar desde data_collection las filas WRC,
# 2. cruzarlas con data/wrc para obtener resultado historico,
# 3. cruzar ese resultado con data/models/wrc para traer imagen/modelo,
# 4. normalizar columnas para poder hacer bind_rows con endurance.

wrc_results <- read_wrc_results_files()
wrc_models <- read_wrc_models_files()

# TODO: ajustar este filtro a las columnas finales del Sheet.
# Ejemplos posibles:
# collection_wrc_sheet <- filter(data_collection, source == "wrc")
# collection_wrc_sheet <- filter(data_collection, category == "wrc")
# collection_wrc_sheet <- filter(data_collection, !is.na(rally_slug))
collection_wrc_sheet <- data_collection |> filter(FALSE)

# TODO: ajustar las llaves reales del Sheet.
# Recomendacion inicial:
# - Sheet -> data/wrc: rally_slug + year + entry_number
# - data/wrc -> data/models/wrc: rally_slug + year + driver_key
collection_wrc <- collection_wrc_sheet

# collection_wrc <- collection_wrc_sheet |>
#   mutate(
#     year = as.integer(year),
#     entry_number = readr::parse_integer(number)
#   ) |>
#   inner_join(
#     wrc_results,
#     by = join_by(rally_slug, year, entry_number)
#   ) |>
#   left_join(
#     wrc_models,
#     by = join_by(rally_slug, year, driver_key)
#   )

# TODO: activar cuando collection_wrc ya tenga las llaves definitivas.
# checks_wrc <- anti_join(collection_wrc_sheet, collection_wrc, by = join_by(rally_slug, year, entry_number))
# checks_wrc
# stopifnot(nrow(checks_wrc) == 0)

# bind --------------------------------------------------------------------
dt_endurance <- prep_rsc_dt_data(collection_endurance) |>
  bind_cols(collection_endurance |> select(scale64_maker, scale64_status, result_group, note)) |>
  mutate(
    source = "endurance",
    note = dplyr::if_else(is.na(note) | note == "", "", glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(note, attribute = TRUE)}\">{info_icon}</span>")),
    car_title = stringr::str_squish(paste(car_title, note))
  ) |>
  relocate(source, .before = 1) |>
  relocate(track, .after = source) |>
  relocate(result_group, .after = result) |>
  select(-date, -note, -grid)

# TODO: cuando el bloque WRC este listo, construir dt_wrc con las mismas columnas
# principales que dt_endurance. Mientras tanto queda vacio para que el script
# siga reproduciendo la tabla actual de endurance.
dt_wrc <- dt_endurance |> slice(0)

dt_collection <- bind_rows(dt_endurance, dt_wrc)

# datatable html output ---------------------------------------------------
dt <- dt_collection |>
  mutate(across(c(source, track, result_status, scale64_maker, scale64_status), as.factor)) |>
  glimpse() |>
  make_dt("collection", search = "columns") |>
  style_result_status()

dt

# save -------------------------------------------------------------------
htmlwidgets::saveWidget(
  dt,
  file     = here::here("outputs/html/collection164.html"),
  libdir   = "lib",
  selfcontained = FALSE,
  title = "Collection 1:64 - Results and Status"
)
