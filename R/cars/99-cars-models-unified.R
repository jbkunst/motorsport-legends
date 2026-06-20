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
  "Porsche",  here::here("data/cars/porsche_stuttcars_model_research.csv"),
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

cars_models_raw

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
    category_raw = category,
    category = dplyr::case_when(
      brand == "Ferrari" & category_raw == "Gran Turismo" ~ "Road",
      brand == "Ferrari" & category_raw == "Sport Prototype" ~ "Competition",
      brand == "Porsche" & stringr::str_detect(category_raw, stringr::regex("Race", ignore_case = TRUE)) ~ "Competition",
      brand == "Porsche" & stringr::str_detect(category_raw, stringr::regex("Concept", ignore_case = TRUE)) ~ "Concept",
      brand == "Porsche" & stringr::str_detect(category_raw, stringr::regex("Supercar", ignore_case = TRUE)) ~ "Supercar",
      brand == "Porsche" ~ "Road",
      TRUE ~ category_raw
    ),
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
    model = dplyr::case_when(brand == "Porsche" ~ stringr::str_remove(model, stringr::regex("^Porsche\\s+", ignore_case = TRUE)), TRUE ~ model),
    model = stringr::str_squish(model),
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

cars_models |> 
  count(brand, category)

cars_models |>
  dplyr::group_by(brand) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      ~ mean(!is.na(.x) & if (is.character(.x)) .x != "" else TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = -brand,
    names_to = "column",
    values_to = "pct_non_na"
  ) |>
  dplyr::mutate(pct_non_na = round(100 * pct_non_na, 1)) |>
  dplyr::arrange(brand, pct_non_na, column) |>
    tidyr::pivot_wider(
    names_from = brand,
    values_from = pct_non_na
  ) |>
  print(n = Inf)

cars_models_validation <- cars_models |>
  dplyr::mutate(
    issue = dplyr::case_when(
      is.na(model) | model == "" ~ "missing_model",
      is.na(year) | year < 1880 | year > lubridate::year(Sys.Date()) + 2 ~ "bad_year",
      !is.na(displacement_cc) & (displacement_cc < 100 | displacement_cc > 12000) ~ "bad_displacement_cc",
      !is.na(max_power_cv) & (max_power_cv < 3 | max_power_cv > 2000) ~ "bad_max_power_cv",
      !is.na(torque_nm) & (torque_nm < 5 | torque_nm > 2500) ~ "bad_torque_nm",
      !is.na(top_speed_kmh) & (top_speed_kmh < 10 | top_speed_kmh > 500) ~ "bad_top_speed_kmh",
      !is.na(weight_kg) & (weight_kg < 150 | weight_kg > 5000) ~ "bad_weight_kg",
      !is.na(length_mm) & (length_mm < 1000 | length_mm > 7000) ~ "bad_length_mm",
      !is.na(width_mm) & (width_mm < 700 | width_mm > 3000) ~ "bad_width_mm",
      !is.na(height_mm) & (height_mm < 500 | height_mm > 3000) ~ "bad_height_mm",
      !is.na(wheelbase_mm) & (wheelbase_mm < 800 | wheelbase_mm > 5000) ~ "bad_wheelbase_mm",
      is.na(photo_url) | photo_url == "" ~ "missing_photo_url",
      is.na(url) | url == "" ~ "missing_url",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(issue)) |>
  dplyr::select(
    issue,
    brand,
    model,
    year,
    displacement_cc,
    max_power_cv,
    torque_nm,
    top_speed_kmh,
    weight_kg,
    wheelbase_mm,
    length_mm,
    width_mm,
    height_mm,
    url
  ) |>
  dplyr::arrange(issue, brand, model)

cars_models_validation |>
  dplyr::count(issue, brand, sort = TRUE) |>
  print(n = Inf)

cars_models_validation |>
  print(n = Inf)

# datatable --------------------------------------------------------------
cars_models_min <- cars_models |>
  dplyr::mutate(
    info = dplyr::if_else(is.na(description) | description == "", "", glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(description, attribute = TRUE)}\">{info_icon}</span>")),
    model = dplyr::if_else(is.na(url) | url == "", model, glue::glue("<a href='{url}' target='_blank'>{model}</a>")),
    model = stringr::str_squish(paste(model, info)),
    photo = dplyr::if_else(is.na(photo_url) | photo_url == "", "", glue::glue("<img src='{photo_url}' />")),
    brand = as.factor(brand),
    category = as.factor(category),
    engine = as.factor(engine),
    model_extra = as.factor(model_extra)
  ) |>
  dplyr::transmute(
    brand,
    model,
    gen = model_extra,
    photo,
    category,
    year,
    engine,
    cc = displacement_cc,
    cv = max_power_cv,
    vmax = top_speed_kmh
  )

cars_models_dt <- cars_models_min |>
  make_dt(search = "columns", photo_height = 170)

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
