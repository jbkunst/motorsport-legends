# setup ------------------------------------------------------------------
library(tidyverse)
library(here)

source(here::here("R/00-helpers.R"))

# params -----------------------------------------------------------------
out_csv  <- here::here("data/cars/cars_models_unified.csv")
out_html <- here::here("outputs/html/cars_models_unified.html")

fs::dir_create(fs::path_dir(out_csv))
fs::dir_create(fs::path_dir(out_html))

# files ------------------------------------------------------------------
cars_files <- tibble::tribble(
  ~brand,     ~file,
  "Porsche",  here::here("data/cars/porsche_stuttcars_models_specs_final.csv"),
  "Ferrari",  here::here("data/cars/ferrari_past_models.csv"),
  "Nissan",   here::here("data/cars/nissan_heritage_collection.csv")
)

# read -------------------------------------------------------------------
cars_models_raw <- cars_files |>
  dplyr::mutate(
    data = purrr::map(file, \(x) readr::read_csv(x, col_types = readr::cols(.default = readr::col_character())) |> dplyr::select(-dplyr::contains("_data_uri")))
  ) |>
  dplyr::select(brand, data) |>
  tidyr::unnest(data)

# clean ------------------------------------------------------------------
cars_models <- cars_models_raw |>
  dplyr::transmute(
    brand,
    model,
    model_full = dplyr::coalesce(model_full, model),
    model_family,
    model_extra = dplyr::coalesce(generation, model_code),
    model_code,
    year = readr::parse_number(year),
    category,
    engine,
    engine_position,
    displacement_cc = readr::parse_number(displacement_cc),
    max_power_cv = readr::parse_number(max_power_cv),
    rpm_max_power = readr::parse_number(rpm_max_power),
    torque_nm = readr::parse_number(torque_nm),
    rpm_max_torque = readr::parse_number(rpm_max_torque),
    top_speed_kmh = readr::parse_number(top_speed_kmh),
    weight_kg = readr::parse_number(weight_kg),
    wheelbase_mm = readr::parse_number(wheelbase_mm),
    length_mm = readr::parse_number(length_mm),
    width_mm = readr::parse_number(width_mm),
    height_mm = readr::parse_number(height_mm),
    photo_url = dplyr::coalesce(image_url, thumb_url),
    url,
    description
  ) |>
  dplyr::mutate(
    model_extra = dplyr::na_if(model_extra, ""),
    photo_url = dplyr::na_if(photo_url, ""),
    url = dplyr::na_if(url, ""),
    description = dplyr::na_if(description, ""),
    dplyr::across(where(is.numeric), \(x) round(x))
  ) |>
  dplyr::arrange(brand, year, model)

# checks -----------------------------------------------------------------
cars_models |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_brand = dplyr::n_distinct(brand),
    n_photo_url = sum(!is.na(photo_url) & photo_url != ""),
    pct_photo_url = round(100 * n_photo_url / n_rows, 1),
    n_description = sum(!is.na(description) & description != ""),
    pct_description = round(100 * n_description / n_rows, 1)
  ) |>
  print(n = Inf)

cars_models |>
  dplyr::count(brand, sort = TRUE) |>
  print(n = Inf)

# datatable --------------------------------------------------------------
cars_models_dt <- cars_models |>
  dplyr::mutate(
    model = dplyr::if_else(is.na(url) | url == "", model, glue::glue("<a href='{url}' target='_blank'>{model}</a>")),
    photo = dplyr::if_else(is.na(photo_url) | photo_url == "", "", glue::glue("<img src='{photo_url}' width='150' />"))
  ) |>
  dplyr::select(
    brand,
    model,
    model_extra,
    photo,
    category,
    year,
    engine,
    displacement_cc,
    max_power_cv,
    torque_nm,
    top_speed_kmh
  ) |>
  make_dt(search = "columns")

cars_models_dt

# save -------------------------------------------------------------------
readr::write_csv(cars_models, out_csv)

htmlwidgets::saveWidget(
  cars_models_dt,
  file = out_html,
  libdir = "lib",
  selfcontained = FALSE,
  title = "Cars Models Unified"
)