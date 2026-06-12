# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(here)

source(here::here("R/00-helpers.R"))

# params -----------------------------------------------------------------
url <- "https://www.stuttcars.com/porsche-model-research/"

out_csv        <- here("data/cars/porsche_stuttcars_model_research.csv")
out_specs_csv  <- here("data/cars/porsche_stuttcars_model_specs.csv")
out_html       <- here("outputs/html/porsche_stuttcars_model_research.html")

fs::dir_create(fs::path_dir(out_csv))
fs::dir_create(fs::path_dir(out_html))

# helpers ----------------------------------------------------------------
scrape_porsche_info_card <- function(card, base_url = url) {
  
  section_node <- card |> html_element(xpath = "preceding::h2[1]")
  title_node   <- card |> html_element(".title a")
  img_nodes    <- card |> html_elements("a.mask-img img")
  
  title     <- title_node |> html_text2() |> str_squish()

  cli::cli_progress_step("Scraping info card from {title}")

  link      <- title_node |> html_attr("href") |> url_absolute(base_url)

  image_url <- img_nodes |>
    html_attr("src") |>
    discard(~ is.na(.x) || .x == "" || str_detect(.x, "\\.svg($|\\?)|data:image/svg")) |>
    first(default = NA_character_)

  image_url
  
  if (is.na(image_url)) image_url <- img_node |> html_attr("data-src")
  
  tibble(
    title          = title,
    image_url      = url_absolute(image_url, base_url),
    image_data_uri = image_url_to_data_uri(image_url),
    url            = link
  )
}

scrape_porsche_specs <- function(url = "https://www.stuttcars.com/porsche-919-hybrid-2014/") {
  
  cli::cli_progress_step("Scraping Porsche specs from {url}")
  
  page <- read_html(url)

  page_titles <- page |>
    rvest::html_elements("h1, h2") |>
    rvest::html_text2() |>
    stringr::str_squish()

  page_title_year <- page_titles |>
    stringr::str_subset("\\b(19|20)\\d{2}\\b") |>
    dplyr::first(default = NA_character_)
  
  subtitle <- page |>
    html_element("p.subtitle.flipboard-subtitle") |>
    html_text2() |>
    str_squish()
  
  info_nodes <- page |>
    html_elements(".lets-info-up-block.lets-info-up-meta-block")
  
  info_raw <- if (length(info_nodes) == 0) {
    tibble(spec = character(), value = character(), spec_clean = character())
  } else {
    info_nodes |>
      map_dfr(function(node) {
        tibble(
          spec       = node |> html_element(".lets-info-up-meta-title") |> html_text2() |> str_squish(),
          value      = node |> html_element(".lets-info-up-meta-content") |> html_text2() |> str_squish(),
          spec_clean = spec |> janitor::make_clean_names()
        )
      }) |>
      filter(!is.na(spec), spec != "", !is.na(value), value != "")
  }
  
  info_wide <- if (nrow(info_raw) == 0) {
    tibble(url = url)
  } else {
    info_raw |>
      select(spec_clean, value) |>
      pivot_wider(names_from = spec_clean, values_from = value) |>
      mutate(url = url, .before = 1)
  }
  
  image_profile_url <- page |>
    html_element(".lets-info-up-fi img") |>
    html_attr("data-lazy-src")
  
  if (is.na(image_profile_url) || image_profile_url == "") {
    image_profile_url <- page |>
      html_element(".lets-info-up-fi img") |>
      html_attr("src")
  }
  
  image_profile_url <- if (is.na(image_profile_url) || image_profile_url == "") {
    NA_character_
  } else {
    url_absolute(image_profile_url, url)
  }
  
  info_up <- info_wide |>
    mutate(
      subtitle               = subtitle,
      image_profile_url      = image_profile_url,
      image_profile_data_uri = if_else(is.na(image_profile_url) | image_profile_url == "", NA_character_, image_url_to_data_uri(image_profile_url)),
      .after                 = url
    )
  
  table_nodes <- page |>
    html_elements("table")
  
  tables_raw <- if (length(table_nodes) == 0) {
    tibble::tibble(url = character(), table_id = integer())
  } else {
    table_nodes |>
      purrr::map2_dfr(seq_along(table_nodes), function(table, table_id) {
        table |>
          rvest::html_table(fill = TRUE) |>
          tibble::as_tibble(.name_repair = "unique") |>
          janitor::clean_names() |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
          dplyr::mutate(url = url, table_id = table_id, .before = 1)
      })
  }
  
  list(
    url = url,
    page_titles = page_titles,
    page_title_year = page_title_year,
    info_up = info_up,
    tables_raw = tables_raw
  )
}
# scrape -----------------------------------------------------------------
session <- chromote::ChromoteSession$new()

page <- get_page_html(session, url, wait = 5, n_scroll = 20, scroll_wait = 800, timeout = 60) |>
  read_html()

session$close()

# Acá obtenemos a que categoría pertenece!
# Seleccionamos tanto titulos como cards y verificamos
# que sea card para luego hacer fill sobre el titulo de la sección.
content_nodes <- page |>
  html_elements("h2, div.preview-mini-wrap")

porsche_models <- tibble(node_id = seq_along(content_nodes)) |>
  mutate(
    node       = map(node_id, ~ content_nodes[[.x]]),
    node_name  = map_chr(node, xml2::xml_name),
    node_class = map_chr(node, ~ replace_na(html_attr(.x, "class"), "")),
    is_section = node_name == "h2",
    is_card    = str_detect(node_class, "preview-mini-wrap"),
    section    = if_else(is_section, map_chr(node, html_text2), NA_character_)
  ) |>
  tidyr::fill(section) |>
  filter(is_card) |>
  mutate(data = map(node, scrape_porsche_info_card, base_url = url)) |>
  select(section, data) |>
  unnest(data) |>
  relocate(section, .before = title) |>
  filter(!is.na(title), title != "") |>
  filter(!str_detect(title, "Sales & Production Numbers")) |> 
  filter_out(str_detect(title, "^FOR SALE|^VIDEO")) |> 
  distinct(url, .keep_all = TRUE)

# "Current Porsches" move to each model "Family"
porsche_models <- porsche_models |>
  mutate(
    section_group = case_when(
      section == "Current Porsche Model Guides" & str_detect(title, "Porsche 911") ~ "Porsche 911 Model Guides",
      section == "Current Porsche Model Guides" & str_detect(title, "Porsche 718|Boxster|Cayman|Spyder") ~ "Porsche Boxster & Cayman Model Guides",
      section == "Current Porsche Model Guides" & str_detect(title, "Taycan|Panamera|Macan|Cayenne") ~ "The Rest of the Porsche Model Guides",
      TRUE ~ section
    ),
    .after = section
  )

porsche_specs <- porsche_models |>
  pull(url) |>
  unique() |>
  map(scrape_porsche_specs)

# saveRDS(porsche_specs, "data/cars/porsche_specs_TEMP.rds")
# porsche_specs <- readRDS("data/cars/porsche_specs_TEMP.rds")

# porsche main specs -----------------------------------------------------
porsche_specs_info_up <- porsche_specs |>
  map_dfr("info_up")

porsche_specs_info_up

porsche_specs_info_up_completeness <- porsche_specs_info_up |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      list(
        n_non_na   = ~ sum(!is.na(.x) & .x != ""),
        pct_non_na = ~ mean(!is.na(.x) & .x != "")
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = c("column", ".value"),
    names_sep = "__"
  ) |>
  dplyr::mutate(
    pct_non_na = round(100 * pct_non_na, 1)
  ) |>
  dplyr::arrange(dplyr::desc(n_non_na), column)

porsche_specs_info_up_completeness |> print(n = Inf)

porsche_specs_info_up |> 
  filter(is.na(model)) |> 
  select(1:5) |>
  print(n = Inf)


# join models and specs --------------------------------------------------
porsche_specs_titles <- porsche_specs |>
  purrr::map_dfr(~ tibble::tibble(url = .x$url, page_title_year = .x$page_title_year))

porsche_911_codes   <- c("901", "911", "912", "930", "934", "935", "959", "961", "964", "965", "969", "993", "996", "997", "991", "992")
porsche_rear_codes  <- c("356", porsche_911_codes)
porsche_mid_codes   <- c("550", "904", "906", "907", "908", "909", "910", "914", "916", "917", "918", "919", "936", "956", "962", "963", "982", "984")
porsche_front_codes <- c("597", "924", "928", "942", "944", "953", "968", "989")

porsche_models_specs <- porsche_models |>
  dplyr::transmute(url, model = title, category = section_group, section, image_data_uri) |>
  dplyr::left_join(
    porsche_specs_info_up |>
      dplyr::select(
        url, engine_raw = engine, power_raw = power, torque_raw = torque,
        top_speed_raw = top_speed, x0_60_mph_raw = x0_60_mph,
        production_raw = production, model_years, years, year_raw = year,
        description = subtitle, image_profile_data_uri
      ),
    by = "url"
  ) |>
  dplyr::left_join(porsche_specs_titles, by = "url") |>
  dplyr::mutate(
    model_card = model,
    model_full = dplyr::coalesce(page_title_year, model),
    
    years_from_model = model |>
      stringr::str_extract("\\(([^\\)]*(19|20)\\d{2}[^\\)]*)\\)") |>
      stringr::str_remove_all("^\\(|\\)$"),
    
    years_from_page_title = page_title_year |>
      stringr::str_extract("\\(([^\\)]*(19|20)\\d{2}[^\\)]*)\\)") |>
      stringr::str_remove_all("^\\(|\\)$"),
    
    year_from_url = url |>
      stringr::str_extract("(?<=/)(19|20)\\d{2}(?=[-/])"),
    
    years_raw = dplyr::coalesce(
      model_years,
      years,
      year_raw,
      years_from_page_title,
      years_from_model,
      year_from_url
    ),
    
    year = years_raw |>
      stringr::str_extract("\\b(19|20)\\d{2}\\b") |>
      as.integer(),
    
    # Año inferido por regla histórica para modelos sin rango declarado en HTML/título/URL.
    year = dplyr::case_when(
      is.na(year) & stringr::str_detect(model, stringr::regex("British Legends Edition", ignore_case = TRUE)) ~ 2017L,
      is.na(year) & stringr::str_detect(model, stringr::regex("WSC-95|LMP1-98", ignore_case = TRUE)) ~ 1996L,
      TRUE ~ year
    ),
    
    year_end = purrr::map_int(years_raw, \(x) {
      yy <- x |> stringr::str_extract_all("\\b(19|20)\\d{2}\\b") |> purrr::pluck(1) |> as.integer()
      yy <- yy[!is.na(yy)]
      if (length(yy) == 0) NA_integer_ else max(yy)
    }),
    year_end = dplyr::if_else(stringr::str_detect(years_raw, stringr::regex("present", ignore_case = TRUE)), NA_integer_, year_end),
    
    generation = model |>
      stringr::str_extract_all("\\([^\\)]*\\)") |>
      purrr::map_chr(\(x) {
        x <- x |> stringr::str_remove_all("^\\(|\\)$") |> stringr::str_squish()
        x <- x[!stringr::str_detect(x, "\\b(19|20)\\d{2}\\b")]
        g <- x |> stringr::str_extract("\\b(930|964|993|996(?:\\.2)?|997(?:\\.2)?|991(?:\\.[12])?|992(?:\\.[12])?|986|987|981|982|718)\\b") |> stats::na.omit()
        if (length(g) == 0) NA_character_ else g[1]
      }),
    
    model_code_main = stringr::str_match(model, "\\b(\\d{3})(?:\\.\\d|[A-Z]|\\b)") |> (\(x) x[, 2])(),
    
    model_family = dplyr::case_when(
      model_code_main %in% porsche_911_codes ~ "911",
      model_code_main == "356" ~ "356",
      model_code_main %in% c("924", "928", "944", "968") ~ model_code_main,
      model_code_main %in% c("550", "904", "906", "907", "908", "909", "910", "914", "916", "917", "918", "919", "936", "956", "962", "963") ~ model_code_main,
      stringr::str_detect(model, stringr::regex("\\bBoxster\\b", ignore_case = TRUE)) ~ "Boxster",
      stringr::str_detect(model, stringr::regex("\\bCayman\\b", ignore_case = TRUE)) ~ "Cayman",
      stringr::str_detect(model, stringr::regex("\\bTaycan\\b", ignore_case = TRUE)) ~ "Taycan",
      stringr::str_detect(model, stringr::regex("\\bMacan\\b", ignore_case = TRUE)) ~ "Macan",
      stringr::str_detect(model, stringr::regex("\\bCayenne\\b", ignore_case = TRUE)) ~ "Cayenne",
      stringr::str_detect(model, stringr::regex("\\bPanamera\\b", ignore_case = TRUE)) ~ "Panamera",
      stringr::str_detect(model, stringr::regex("\\bCarrera GT\\b", ignore_case = TRUE)) ~ "Carrera GT",
      stringr::str_detect(model, stringr::regex("\\bRS Spyder\\b", ignore_case = TRUE)) ~ "RS Spyder",
      stringr::str_detect(model, stringr::regex("\\b99X Electric\\b", ignore_case = TRUE)) ~ "99X Electric",
      stringr::str_detect(model, stringr::regex("\\bMission\\b", ignore_case = TRUE)) ~ "Mission",
      stringr::str_detect(model, stringr::regex("\\bType 64\\b", ignore_case = TRUE)) ~ "Type 64",
      stringr::str_detect(model, stringr::regex("\\b9R3|LMP 2000\\b", ignore_case = TRUE)) ~ "9R3",
      stringr::str_detect(model, stringr::regex("\\bWSC-95|LMP1-98\\b", ignore_case = TRUE)) ~ "WSC-95 / LMP1-98",
      stringr::str_detect(model, stringr::regex("\\bKoenig-Specials C62\\b", ignore_case = TRUE)) ~ "C62",
      stringr::str_detect(model, stringr::regex("\\bVision Gran Turismo\\b", ignore_case = TRUE)) ~ "Vision Gran Turismo",
      stringr::str_detect(model, stringr::regex("\\bLe Mans Living Legend\\b", ignore_case = TRUE)) ~ "Le Mans Living Legend",
      stringr::str_detect(model, stringr::regex("\\bPanamericana\\b", ignore_case = TRUE)) ~ "Panamericana",
      stringr::str_detect(model, stringr::regex("\\bC88\\b", ignore_case = TRUE)) ~ "C88",
      stringr::str_detect(model, stringr::regex("\\bVision E\\b", ignore_case = TRUE)) ~ "Vision E",
      stringr::str_detect(model, stringr::regex("\\b2708 Indy\\b", ignore_case = TRUE)) ~ "2708 Indy",
      stringr::str_detect(model, stringr::regex("\\bSport Tourer Electric\\b", ignore_case = TRUE)) ~ "Sport Tourer Electric",
      stringr::str_detect(model, stringr::regex("\\b1600 Beutler\\b", ignore_case = TRUE)) ~ "1600 Beutler",
      stringr::str_detect(model, stringr::regex("\\bTapiro\\b", ignore_case = TRUE)) ~ "Tapiro",
      stringr::str_detect(model, stringr::regex("\\bBergspyder\\b", ignore_case = TRUE)) ~ "Bergspyder",
      !is.na(model_code_main) ~ model_code_main,
      TRUE ~ NA_character_
    ),
    
    # Posición de motor inferida por regla histórica de familia/modelo Porsche.
    # Se mantiene NA en eléctricos y conceptos donde "engine position" no aplica claramente.
    engine_position = dplyr::case_when(
      model_code_main %in% porsche_rear_codes ~ "rear",
      model_code_main %in% porsche_mid_codes ~ "mid",
      model_code_main %in% porsche_front_codes ~ "front",
      stringr::str_detect(model, stringr::regex("\\b(Boxster|Cayman|718|Bergspyder|Tapiro|RS Spyder|Carrera GT|Mission X)\\b", ignore_case = TRUE)) ~ "mid",
      stringr::str_detect(model, stringr::regex("\\b(Cayenne|Macan|Panamera|C88)\\b", ignore_case = TRUE)) ~ "front",
      stringr::str_detect(model, stringr::regex("\\bPanamericana\\b", ignore_case = TRUE)) ~ "rear",
      TRUE ~ NA_character_
    ),
    
    model_clean = model_full |>
      stringr::str_remove("\\s*\\(([^\\)]*(19|20)\\d{2}[^\\)]*)\\)") |>
      stringr::str_remove("\\s*\\((930|964|993|996(?:\\.2)?|997(?:\\.2)?|991(?:\\.[12])?|992(?:\\.[12])?|986|987|981|982|718)\\)") |>
      stringr::str_squish(),
    
    displacement_raw_l = engine_raw |> stringr::str_extract(stringr::regex("\\d+[\\.,]?\\d*\\s*l\\b", ignore_case = TRUE)),
    displacement_raw_cc = engine_raw |> stringr::str_extract(stringr::regex("\\d+[\\.,]?\\d*\\s*cc\\b", ignore_case = TRUE)),
    displacement_cc = dplyr::case_when(!is.na(displacement_raw_l) ~ readr::parse_number(stringr::str_replace(displacement_raw_l, ",", ".")) * 1000, !is.na(displacement_raw_cc) ~ readr::parse_number(displacement_raw_cc), TRUE ~ NA_real_),
    
    engine = dplyr::case_when(
      stringr::str_detect(engine_raw, stringr::regex("flat[- ]?12|boxer[- ]?12", ignore_case = TRUE)) ~ "flat-12",
      stringr::str_detect(engine_raw, stringr::regex("flat[- ]?8|boxer[- ]?8", ignore_case = TRUE)) ~ "flat-8",
      stringr::str_detect(engine_raw, stringr::regex("flat[- ]?6|boxer[- ]?6", ignore_case = TRUE)) ~ "flat-6",
      stringr::str_detect(engine_raw, stringr::regex("flat[- ]?4|boxer[- ]?4", ignore_case = TRUE)) ~ "flat-4",
      stringr::str_detect(engine_raw, stringr::regex("\\binline[- ]?4\\b", ignore_case = TRUE)) ~ "inline-4",
      stringr::str_detect(engine_raw, stringr::regex("\\bV12\\b", ignore_case = TRUE)) ~ "V12",
      stringr::str_detect(engine_raw, stringr::regex("\\bV10\\b", ignore_case = TRUE)) ~ "V10",
      stringr::str_detect(engine_raw, stringr::regex("\\bV8\\b", ignore_case = TRUE)) ~ "V8",
      stringr::str_detect(engine_raw, stringr::regex("\\bV6\\b", ignore_case = TRUE)) ~ "V6",
      TRUE ~ engine_raw
    ),
    
    max_power_value = power_raw |> stringr::str_extract("\\d{1,3}(,\\d{3})+|\\d+[\\.]?\\d*") |> stringr::str_remove_all(",") |> as.numeric(),
    max_power_unit = power_raw |> stringr::str_extract(stringr::regex("\\b(bhp|hp|ps|cv|kw)\\b", ignore_case = TRUE)) |> stringr::str_to_lower(),
    max_power_cv = dplyr::case_when(max_power_unit %in% c("ps", "cv") ~ max_power_value, max_power_unit %in% c("hp", "bhp") ~ max_power_value * 1.01387, max_power_unit == "kw" ~ max_power_value / 0.73549875, TRUE ~ NA_real_),
    rpm_max_power = power_raw |> stringr::str_extract("\\d+[\\.,]?\\d*\\s*rpm") |> stringr::str_remove_all("[\\.,]") |> readr::parse_number(),
    
    torque_value = torque_raw |> stringr::str_extract("\\d{1,3}(,\\d{3})+|\\d+[\\.]?\\d*") |> stringr::str_remove_all(",") |> as.numeric(),
    torque_nm = dplyr::case_when(stringr::str_detect(torque_raw, stringr::regex("lb-ft|ft lbs|ft-lbs|ftlb|ft lb", ignore_case = TRUE)) ~ torque_value * 1.35582, stringr::str_detect(torque_raw, stringr::regex("\\bnm\\b", ignore_case = TRUE)) ~ torque_value, TRUE ~ NA_real_),
    rpm_max_torque = torque_raw |> stringr::str_extract("\\d+[\\.,]?\\d*\\s*rpm") |> stringr::str_remove_all("[\\.,]") |> readr::parse_number(),
    
    top_speed_value = top_speed_raw |> stringr::str_extract("\\d{1,3}(,\\d{3})+|\\d+[\\.]?\\d*") |> stringr::str_remove_all(",") |> as.numeric(),
    top_speed_kmh = dplyr::case_when(stringr::str_detect(top_speed_raw, stringr::regex("mph", ignore_case = TRUE)) ~ top_speed_value * 1.60934, stringr::str_detect(top_speed_raw, stringr::regex("km/h|kph", ignore_case = TRUE)) ~ top_speed_value, TRUE ~ NA_real_),
    acceleration_0_100_kmh_s = x0_60_mph_raw |> stringr::str_replace(",", ".") |> stringr::str_extract("\\d+[\\.]?\\d*") |> as.numeric() * (100 / 96.56064),
    
    production_is_ongoing = dplyr::if_else(stringr::str_detect(years_raw, stringr::regex("present", ignore_case = TRUE)) | stringr::str_detect(production_raw, stringr::regex("still in production|present", ignore_case = TRUE)), TRUE, FALSE, missing = FALSE),
    production_qty = dplyr::case_when(production_is_ongoing ~ NA_real_, stringr::str_detect(production_raw, stringr::regex("\\d+\\s*(units|cars|examples|built|produced)", ignore_case = TRUE)) ~ production_raw |> stringr::str_extract("\\d{1,3}(,\\d{3})+|\\d+") |> stringr::str_remove_all(",") |> as.numeric(), TRUE ~ NA_real_)
  ) |>
  dplyr::mutate(
    displacement_cc = as.integer(round(displacement_cc)), max_power_cv = as.integer(round(max_power_cv)),
    rpm_max_power = as.integer(round(rpm_max_power)), torque_nm = as.integer(round(torque_nm)),
    rpm_max_torque = as.integer(round(rpm_max_torque)), top_speed_kmh = as.integer(round(top_speed_kmh)),
    acceleration_0_100_kmh_s = round(acceleration_0_100_kmh_s, 1), production_qty = as.integer(round(production_qty))
  ) |>
  dplyr::select(
    model = model_clean,
    model_full,
    model_family,
    model_code = model_code_main,
    year,
    year_end,
    category,
    section,
    generation,
    engine,
    engine_position,
    displacement_cc,
    max_power_cv,
    rpm_max_power,
    torque_nm,
    rpm_max_torque,
    top_speed_kmh,
    acceleration_0_100_kmh_s,
    production_qty,
    production_is_ongoing,
    url,
    image_data_uri,
    image_profile_data_uri,
    description
  )

# checks -----------------------------------------------------------------
porsche_models_specs |>
  dplyr::slice_sample(prop = 1) |>
  dplyr::glimpse()

porsche_models_specs |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      ~ mean(!is.na(.x) & if (is.character(.x)) .x != "" else TRUE)
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "column",
    values_to = "pct_non_na"
  ) |>
  dplyr::mutate(pct_non_na = round(100 * pct_non_na, 1)) |>
  dplyr::arrange(dplyr::desc(pct_non_na)) |>
  print(n = Inf)

porsche_models_specs |>
  dplyr::filter(is.na(year)) |>
  dplyr::select(model, model_full, year, year_end, url)

porsche_models_specs |>
  dplyr::count(model_code, sort = TRUE) |>
  print(n = Inf)

porsche_models_specs |>
  dplyr::count(model_family, sort = TRUE) |>
  print(n = Inf)

porsche_models_specs |>
  dplyr::filter(is.na(model_family)) |>
  dplyr::select(model, model_code, generation, year, category, engine, url) |>
  dplyr::slice_sample(prop = 1)

porsche_models_specs |>
  dplyr::summarise(
    pct_engine_position = round(100 * mean(!is.na(engine_position)), 1),
    n_missing = sum(is.na(engine_position))
  )

porsche_models_specs |>
  dplyr::filter(is.na(engine_position)) |>
  dplyr::select(model, year, category, section, engine, url) |>
  dplyr::slice_sample(prop = 1)


# auditoria diferencias entre titulos ------------------------------------
porsche_model_title_diffs <- porsche_models |>
  dplyr::transmute(
    url,
    model_card = title
  ) |>
  dplyr::left_join(porsche_specs_titles, by = "url") |>
  dplyr::mutate(
    model_full = dplyr::coalesce(page_title_year, model_card),
    model = model_full |>
      stringr::str_remove("\\s*\\(([^\\)]*(19|20)\\d{2}[^\\)]*)\\)") |>
      stringr::str_remove("\\s*\\((930|964|993|996(?:\\.2)?|997(?:\\.2)?|991(?:\\.[12])?|992(?:\\.[12])?|986|987|981|982|718)\\)") |>
      stringr::str_squish()
  ) |>
  dplyr::filter(model_full != model_card) |>
  dplyr::select(model, model_full, model_card, url)

porsche_model_title_diffs

# datatable --------------------------------------------------------------

# export -----------------------------------------------------------------
readr::write_csv( , out_csv)
