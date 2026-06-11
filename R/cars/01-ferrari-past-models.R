# setup ------------------------------------------------------------------
library(tidyverse)
library(chromote)
library(rvest)
library(here)

source("R/00-helpers.R")

# params -----------------------------------------------------------------
url <- "https://www.ferrari.com/en-EN/auto/past-model"

out_csv        <- here("data/cars/ferrari_past_models.csv")
out_specs_csv  <- here("data/cars/ferrari_past_models_specs.csv")
out_html       <- here("outputs/html/ferrari_past_models.html")

fs::dir_create(fs::path_dir(out_csv))
fs::dir_create(fs::path_dir(out_html))

# helpers ----------------------------------------------------------------
scrape_ferrari_section <- function(section) {
  
  year <- section |>
    html_element("h2") |>
    html_text2() |>
    as.integer()
  
  cli::cli_progress_step("Getting {year} models")

  model_nodes <- section |>
    html_elements("a.garage-thumb")

  # card <- model_nodes |> purrr::pluck(1)
  
  map_dfr(model_nodes, function(card) {
    
    text_spans <- card |>
      html_elements(".PastModels__text__2qL1mq9T span") |>
      html_text2()
    
    img_node <- card |>
      html_element("img")
    
    tibble(
      model = text_spans[1],
      year = year,
      category = text_spans[2],
      image_data_uri = img_node |>
        html_attr("src") |>
        stringr::str_replace("width=\\d+", "width=800") |>
        stringr::str_replace("height=\\d+", "height=540") |>
        image_url_to_data_uri()
      description = img_node |> html_attr("alt"),
      url = url_absolute(html_attr(card, "href"), "https://www.ferrari.com/en-EN/auto/")
    )
  })
}

clean_spec_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

scrape_ferrari_specs <- function(url = "https://www.ferrari.com/en-EN/auto/f8-tributo", session) {
  
  cli::cli_progress_step("Scraping Ferrari specs from {url}")
  
  page <- get_page_html(session, url, n_scroll = 10) |>
    rvest::read_html()
  
  main_nodes <- page |>
    rvest::html_elements("ul[class*='Specs__list'] li")
  
  main_specs_raw <- if (length(main_nodes) == 0) {
      tibble::tibble(url = character(), spec = character(), value = character(), spec_clean = character(),  spec_key   = character())
    } else {
      main_nodes |>
        purrr::map_dfr(function(node) {
          tibble::tibble(
            url   = url,
            spec  = node |> rvest::html_element("div[class*='Specs__label']") |> rvest::html_text2(),
            value = node |> rvest::html_element("div[class*='Specs__value']") |> rvest::html_text2()
          )
        }) |>
        dplyr::filter(!is.na(spec), spec != "") |>
        dplyr::mutate(
          spec_clean = janitor::make_clean_names(spec),
          spec_key   = paste0(spec_clean, "_raw")
        )
    }
    
  main_specs <- main_specs_raw |>
    dplyr::select(url, spec_key, value) |>
    tidyr::pivot_wider(names_from = spec_key, values_from = value)

  accordion_nodes <- page |>
    rvest::html_elements("div[class*='Accordion__accordion']")
  
  specs_long <- if (length(accordion_nodes) == 0) {
    tibble::tibble(
      url           = character(),
      section       = character(),
      spec          = character(),
      value         = character(),
      section_clean = character(),
      spec_clean    = character(),
      spec_key      = character()
    )
  } else {
    accordion_nodes |>
      purrr::map_dfr(function(acc) {
        
        section <- acc |>
          rvest::html_element("div[class*='Accordion__title']") |>
          rvest::html_text2()
        
        spec_nodes <- acc |>
          rvest::html_elements("li[class*='TechSpecs__specification']")
        
        if (length(spec_nodes) == 0) {
          return(tibble::tibble(
            url     = character(),
            section = character(),
            spec    = character(),
            value   = character()
          ))
        }
        
        spec_nodes |>
          purrr::map_dfr(function(item) {
            tibble::tibble(
              url     = url,
              section = section,
              spec    = item |> rvest::html_element("strong") |> rvest::html_text2(),
              value   = item |> rvest::html_element("span[class*='TechSpecs__value']") |> rvest::html_text2()
            )
          })
      }) |>
      dplyr::filter(!is.na(section), !is.na(spec), spec != "") |>
      dplyr::mutate(
        section_clean = janitor::make_clean_names(section),
        spec_clean    = janitor::make_clean_names(spec),
        spec_key      = paste(section_clean, spec_clean, sep = "_")
      )
  }
  
  list(
    main = main_specs,
    long = specs_long
  )
}

clean_maximum_power_rpm <- function(data) {
  # Extrae rpm desde columnas tipo maximum_power_8_500rpm_raw o desde valores
  # tipo 660 CV at 8.000 rpm, corrigiendo 8.000 rpm a 8000 y descartando falsos rpm menores a 1000.
  
  if (nrow(data) == 0) return(data)
  
  rpm_cols <- names(data) |>
    stringr::str_subset("^maximum_power_(at_)?[0-9_]+_?rpm_raw$|^maximum_power_[0-9_]+_raw$")
  
  if (!"maximum_power_raw" %in% names(data)) data$maximum_power_raw <- NA_character_
  if (!"rpm_max_power" %in% names(data)) data$rpm_max_power <- NA_real_
  
  rpm_source <- if (length(rpm_cols) == 0) {
    tibble::tibble(row = integer(), value = character(), rpm = numeric())
  } else {
    rpm_lookup <- tibble::tibble(
      col = rpm_cols,
      rpm = rpm_cols |>
        stringr::str_extract("(?<=maximum_power_)(at_)?[0-9_]+(?=(_?rpm)?_raw$)") |>
        stringr::str_remove("^at_") |>
        stringr::str_remove_all("_") |>
        as.numeric()
    )
    
    data |>
      dplyr::mutate(row = dplyr::row_number()) |>
      dplyr::select(row, dplyr::all_of(rpm_cols)) |>
      tidyr::pivot_longer(-row, names_to = "col", values_to = "value") |>
      dplyr::filter(!is.na(value), value != "") |>
      dplyr::group_by(row) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::left_join(rpm_lookup, by = "col") |>
      dplyr::select(row, value, rpm)
  }
  
  rpm_from_value <- data$maximum_power_raw |>
    stringr::str_extract("\\d+[\\.,]?\\d*\\s*rpm") |>
    stringr::str_remove_all("[\\.,]") |>
    readr::parse_number()
  
  data |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::left_join(rpm_source, by = "row") |>
    dplyr::mutate(
      maximum_power_raw = dplyr::coalesce(maximum_power_raw, value),
      rpm_max_power     = dplyr::coalesce(rpm_max_power, rpm_from_value, rpm),
      rpm_max_power     = dplyr::if_else(rpm_max_power < 1000, NA_real_, rpm_max_power)
    ) |>
    dplyr::select(-row, -value, -rpm, -dplyr::all_of(rpm_cols))
}
# scrape -----------------------------------------------------------------
session <- ChromoteSession$new()

page <- get_page_html(session, url, wait = 5, n_scroll = 20, scroll_wait = 2000, timeout = 60) |>
  read_html()

sections <- page |>
  html_elements("div[class*='pastmodels-section-']")

# section <- sections |> purrr::pluck(66)
# scrape_ferrari_section(section)
ferrari_models <- map_dfr(sections, scrape_ferrari_section)
ferrari_models

ferrari_specs <- ferrari_models |>
  pull(url) |>
  unique() |>
  map(scrape_ferrari_specs, session)

session$close()

# saveRDS(ferrari_specs, "data/cars/ferrari_specs_TEMP.rds")

# cleaning ferrari specs main --------------------------------------------
ferrari_specs_main <- ferrari_specs |> 
  map(pluck, "main") |> 
  map(clean_maximum_power_rpm) |> 
  bind_rows()

ferrari_specs_completeness <- ferrari_specs_main |>
  summarise(
    across(
      everything(),
      list(
        n_non_na = ~ sum(!is.na(.x)),
        pct_non_na = ~ mean(!is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("column", ".value"),
    names_sep = "__"
  ) |>
  mutate(
    pct_non_na = round(100 * pct_non_na, 1),
    group = case_when(
      str_detect(column, "x0_100|0_100|x0_62|0_62") ~ "acceleration_0_100",
      str_detect(column, "maximum_power|max_power") ~ "power",
      str_detect(column, "rpm_max_power") ~ "power",
      str_detect(column, "torque") ~ "torque",
      str_detect(column, "speed") ~ "speed",
      str_detect(column, "displacement") ~ "displacement",
      str_detect(column, "specific|power_per_lit") ~ "specific_output",
      str_detect(column, "weight") ~ "weight",
      str_detect(column, "engine|type|powertrain") ~ "engine",
      TRUE ~ "other"
    )
  ) |>
  arrange(group, desc(n_non_na), column)

ferrari_specs_completeness |> print(n = Inf)
ferrari_specs_completeness |> arrange(pct_non_na) |> print(n = Inf)

ferrari_specs_core <- ferrari_specs_main |>
  mutate(
    engine = coalesce(engine_raw, type_raw),
    displacement_cc = readr::parse_number(total_displacement_raw),
    max_power_value = maximum_power_raw |>
      str_extract("\\d+[\\.,]?\\d*") |>
      str_replace(",", ".") |>
      as.numeric(),
    max_power_unit = maximum_power_raw |>
      str_extract(regex("\\b(kW|CV|hp|bhp)\\b", ignore_case = TRUE)) |>
      str_to_lower(),
    max_power_cv = case_when(
      max_power_unit == "cv" ~ max_power_value,
      max_power_unit == "kw" ~ max_power_value / 0.73549875,
      max_power_unit %in% c("hp", "bhp") ~ max_power_value * 1.01387,
      TRUE ~ NA_real_
    ),
    top_speed_raw = coalesce(top_speed_raw, max_speed_raw, maximum_speed_raw),
    top_speed_value = readr::parse_number(top_speed_raw),
    top_speed_unit = top_speed_raw |>
      str_extract(regex("km/h|mph", ignore_case = TRUE)) |>
      str_to_lower(),
    top_speed_kmh = case_when(
      top_speed_unit == "km/h" ~ top_speed_value,
      top_speed_unit == "mph" ~ top_speed_value * 1.60934,
      TRUE ~ NA_real_
    )
  ) |>
  transmute(
    url,
    engine,
    displacement_cc = round(displacement_cc),
    max_power_cv    = round(max_power_cv),
    rpm_max_power   = round(rpm_max_power),
    top_speed_kmh   = round(top_speed_kmh)
  )

ferrari_specs_core

rm(ferrari_specs_completeness, ferrari_specs_main)

ferrari_models_enriched <- ferrari_models |>
  dplyr::left_join(ferrari_specs_core, by = "url") |>
  dplyr::mutate(
    dplyr::across(
      c(displacement_cc, max_power_cv, rpm_max_power, top_speed_kmh),
      ~ as.integer(round(.x))
    )
  )|>
  relocate(contains("url"), contains("image"), .after = last_col())

ferrari_models_enriched

# datatable --------------------------------------------------------------
ferrari_models_dt <- ferrari_models_enriched |> 
  mutate(
    model = glue::glue("<a href='{url}' target='_blank'>{model}</a>"),
    photo = glue::glue("<img src='{image_data_uri}' width='100' />")
  ) |> 
  select(year, model, everything()) |>
  dplyr::relocate(photo, .after = model) |> 
  dplyr::relocate(description, .after = dplyr::last_col()) |> 
  select(-image_data_uri, -url)
  
ferrari_models_dt

ferrari_models_dt <- make_dt(ferrari_models_dt)

ferrari_models_dt
# export -----------------------------------------------------------------
readr::write_csv(ferrari_models_enriched, out_csv)

htmlwidgets::saveWidget(ferrari_models_dt, file = out_html, libdir = "lib", selfcontained = FALSE)
