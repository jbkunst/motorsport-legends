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
    info_up    = info_up,
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

porsche_specs_info_up |> filter(is.na(model)) |> 
  select(1:5) |>
  print(n = Inf)
