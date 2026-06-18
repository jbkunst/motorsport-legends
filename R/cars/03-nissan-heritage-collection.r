# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(here)

source(here::here("R/00-helpers.R"))

# params -----------------------------------------------------------------
url <- "https://www.nissan-global.com/EN/HERITAGE_COLLECTION/"

out_csv        <- here("data/cars/nissan_heritage_collection.csv")
out_specs_csv  <- here("data/cars/nissan_heritage_collection_specs.csv")
out_html       <- here("outputs/html/nissan_heritage_collection.html")

fs::dir_create(fs::path_dir(out_csv))
fs::dir_create(fs::path_dir(out_html))

# helpers ----------------------------------------------------------------
parse_nissan_dimensions <- function(x) {
  dims <- x |>
    stringr::str_extract_all("\\d+[,.]?\\d*") |>
    purrr::pluck(1) |>
    purrr::map_dbl(parse_num)
  
  if (length(dims) < 3) {
    dims <- c(dims, rep(NA_real_, 3 - length(dims)))
  }
  
  length_mm <- dims[1]
  width_mm <- dims[2]
  height_mm <- dims[3]
  
  if (!is.na(length_mm) && length_mm > 20000 && !is.na(width_mm) && width_mm < 3000) {
    length_mm <- length_mm / 10
  }
  
  tibble::tibble(
    length_mm = length_mm,
    width_mm = width_mm,
    height_mm = height_mm
  )
}

parse_nissan_engine <- function(engine_raw) {
  engine_base <- engine_raw |>
    stringr::str_remove("\\d+[,.]?\\d*\\s*cc.*$") |>
    stringr::str_squish() |>
    dplyr::na_if("")
  
  engine_code <- dplyr::case_when(
    is.na(engine_base) ~ NA_character_,
    stringr::str_detect(engine_base, "\\(") ~ engine_base |> stringr::str_remove("\\s*\\(.*$") |> stringr::str_squish(),
    TRUE ~ NA_character_
  )
  
  engine <- dplyr::case_when(
    is.na(engine_base) ~ NA_character_,
    stringr::str_detect(engine_base, "\\(") ~ engine_base |> stringr::str_remove("^.*?\\(") |> clean_nissan_engine_text(),
    TRUE ~ engine_base |> clean_nissan_engine_text()
  )
  
  engine_code <- engine_code |>
    stringr::str_squish() |>
    dplyr::na_if("")
  
  tibble::tibble(
    engine = engine,
    engine_code = engine_code
  )
}

clean_nissan_engine_text <- function(x) {
  x |>
    stringr::str_replace_all(stringr::regex("\\bin line\\b", ignore_case = TRUE), "inline") |>
    stringr::str_remove("^\\s*\\(+\\s*") |>
    stringr::str_remove("\\s*\\)+\\s*,?\\s*$") |>
    stringr::str_remove("\\s*,\\s*$") |>
    stringr::str_squish() |>
    dplyr::na_if("")
}

clean_nissan_engine_raw <- function(x) {
  x |>
    stringr::str_remove("\\d+[,.]?\\d*\\s*cc.*$") |>
    stringr::str_replace_all(stringr::regex("\\bin line\\b", ignore_case = TRUE), "inline") |>
    stringr::str_remove("^\\s*\\(+\\s*") |>
    stringr::str_remove("\\s*\\)+\\s*,?\\s*$") |>
    stringr::str_remove("\\s*,\\s*$") |>
    stringr::str_squish()
}

clean_nissan_engine_desc <- function(x) {
  x |>
    stringr::str_replace_all(stringr::regex("\\bin line\\b", ignore_case = TRUE), "inline") |>
    stringr::str_remove("^\\s*\\(+\\s*") |>
    stringr::str_remove("\\s*\\)+\\s*,?\\s*$") |>
    stringr::str_remove("\\s*,\\s*$") |>
    stringr::str_squish()
}

read_nissan_detail_html <- function(url) {
  page <- read_html(url)
  
  if (length(page |> html_elements("dt")) > 0) return(page)
  
  cli::cli_inform("Rendering detail with chromote {url}")
  
  session <- chromote::ChromoteSession$new()
  on.exit(session$close(), add = TRUE)
  
  get_page_html(session, url, wait = 5, n_scroll = 2, scroll_wait = 800, timeout = 60) |>
    read_html()
}

parse_nissan_specs_long <- function(page) {
  page |>
    html_elements("dt") |>
    map_dfr(\(dt) {
      tibble(
        spec = dt |> html_text2() |> clean_txt() |> janitor::make_clean_names(),
        value = dt |>
          xml2::xml_parent() |>
          html_element("dd") |>
          html_text2() |>
          clean_txt()
      )
    }) |>
    filter(
      !is.na(spec), spec != "",
      !is.na(value), value != "",
      !str_detect(spec, "shortstory|related|download")
    )
}

parse_nissan_specs <- function(x) {
  specs_long <- if (inherits(x, c("xml_document", "xml_node"))) {
    parse_nissan_specs_long(x)
  } else {
    x
  }
  
  specs <- specs_long |>
    dplyr::select(spec, value) |>
    dplyr::filter(!is.na(spec), spec != "", !is.na(value), value != "") |>
    dplyr::group_by(spec) |>
    dplyr::summarise(value = dplyr::first(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = spec, values_from = value)
  
  get_spec <- function(x) {
    if (x %in% names(specs)) specs[[x]][1] else NA_character_
  }
  
  engine_raw <- get_spec("engine")
  power_raw <- get_spec("engine_max_power")
  torque_raw <- get_spec("engine_max_torque")
  dimensions_raw <- get_spec("overall_length_width_height")
  wheelbase_raw <- get_spec("wheelbase")
  weight_raw <- get_spec("curb_weight")
  top_speed_raw <- get_spec("top_speed")
  
  dimensions <- parse_nissan_dimensions(dimensions_raw)
  engine_parsed <- parse_nissan_engine(engine_raw)
  
  displacement_cc <- engine_raw |> stringr::str_extract("\\d+[,.]?\\d*\\s*cc") |> parse_num()
  max_power_cv <- power_raw |> stringr::str_extract("\\d+[,.]?\\d*\\s*ps") |> parse_num()
  rpm_max_power <- power_raw |> stringr::str_extract("\\d+[,.]?\\d*\\s*rpm") |> parse_num()
  torque_nm <- torque_raw |> stringr::str_extract("\\d+[,.]?\\d*\\s*N\\s*·?\\s*m") |> parse_num()
  rpm_max_torque <- torque_raw |> stringr::str_extract("\\d+[,.]?\\d*\\s*rpm") |> parse_num()
  
  tibble::tibble(
    engine = engine_parsed$engine,
    engine_code = engine_parsed$engine_code,
    displacement_cc = displacement_cc,
    max_power_cv = max_power_cv,
    rpm_max_power = rpm_max_power,
    torque_nm = torque_nm,
    rpm_max_torque = rpm_max_torque,
    top_speed_kmh = parse_num(top_speed_raw),
    weight_kg = parse_num(weight_raw),
    wheelbase_mm = parse_num(wheelbase_raw),
    length_mm = dimensions$length_mm,
    width_mm = dimensions$width_mm,
    height_mm = dimensions$height_mm
  )
}

scrape_nissan_index_page <- function(url = "https://www.nissan-global.com/EN/HERITAGE_COLLECTION/bluebird.html") {
  category <- url |> basename() |> str_remove("\\.html$") |> str_replace_all("_", " ") |> str_to_title()
  
  cli::cli_inform("Scraping category {url}")
  
  page <- read_html(url)
  
  cards <- page |>
    html_elements(".thumbnail_block") |>
    keep(\(x) length(html_elements(x, ".thumbnail_block_list li")) > 0)
  
  map_dfr(cards, \(card) {
    card_lines <- card |> html_elements(".thumbnail_block_list li") |> html_text2() |> clean_txt()
    
    raw_text <- card_lines |> paste(collapse = " | ")
    cli::cli_inform("Scraping card with lines: {raw_text}")
    
    no <- card_lines |> str_subset("^No\\.") |> first(default = NA_character_) |> str_remove("^No\\.\\s*") |> na_if("")
    title <- card |> html_element(".thumbnail_block_list li em") |> html_text2() |> clean_txt() |> na_if("")
    type <- card_lines |> str_subset("^Type:") |> first(default = NA_character_) |> str_remove("^Type:\\s*") |> na_if("")
    year_card <- card_lines |> str_subset("^Year:") |> first(default = NA_character_) |> str_extract("\\b(19|20)\\d{2}\\b") |> as.integer()
    year_title <- str_match(title, "\\([^)]*\\b((?:19|20)\\d{2})\\b\\s*(?::|\\))")[, 2] |> as.integer()
    year <- coalesce(year_card, year_title)
    
    color <- card_lines[!str_detect(card_lines, "^No\\.|^Type:|^Year:") & card_lines != title] |> first(default = NA_character_) |> na_if("")
    
    thumb_src <- card |> html_element(".thumbnail_block_figure img") |> html_attr("src")
    thumb_url <- if (!is.na(thumb_src) && thumb_src != "") url_absolute(thumb_src, url) else NA_character_
    
    detail_a <- card |> html_element(".thumbnail_block_btn a")
    detail_href <- detail_a |> html_attr("href") |> coalesce("")
    detail_class <- detail_a |> html_attr("class") |> coalesce("")
    
    detail_disabled <- str_detect(detail_class, "\\bdisable\\b")
    detail_is_anchor <- detail_href == "" || detail_href == "#" || str_detect(detail_href, "#no-") || str_detect(detail_href, "^#")
    
    detail_url <- if (!detail_disabled && !detail_is_anchor) url_absolute(detail_href, url) else NA_character_
    card_url <- if (is.na(detail_url)) str_c(url, "#no-", no) else detail_url
    
    card_base <- tibble(
      category = category,
      no = no,
      title = title,
      type = type,
      year = year,
      color = color,
      image_url = NA_character_,
      image_data_uri = NA_character_,
      thumb_url = thumb_url,
      thumb_data_uri = tryCatch(if (!is.na(thumb_url) && thumb_url != "") image_url_to_data_uri(thumb_url) else NA_character_, error = \(e) NA_character_),
      description = NA_character_,
      raw_text = raw_text,
      url = card_url,
      has_detail = !is.na(detail_url)
    )
    
    if (is.na(detail_url)) return(card_base)
    
    Sys.sleep(0.3)
    cli::cli_inform("Scraping detail {detail_url}")
    
    detail_page <- read_nissan_detail_html(detail_url)
    
    detail_no <- detail_page |> html_element("#collection_neme_area .note_list_A01 li") |> html_text2() |> clean_txt() |> str_remove("^No\\.\\s*") |> na_if("")
    detail_title <- detail_page |> html_element("#collection_neme_area h1") |> html_text2() |> clean_txt() |> na_if("")
    detail_type <- detail_page |> html_element("#collection_neme_area .lead_text_A01 p") |> html_text2() |> clean_txt() |> na_if("")
    detail_year <- str_match(detail_title, "\\([^)]*\\b((?:19|20)\\d{2})\\b\\s*(?::|\\))")[, 2] |> as.integer()
    
    link_nodes <- detail_page |> html_elements("a")
    
    detail_links <- if (length(link_nodes) == 0) {
      tibble(text = character(), href = character())
    } else {
      link_nodes |>
        map_dfr(\(a) {
          tibble(
            text = a |> html_text2() |> clean_txt(),
            href = a |> html_attr("href")
          )
        })
    }
    
    image_href_thumb <- detail_links |> dplyr::filter(!is.na(href), href != "", str_detect(href, regex("\\.(jpg|jpeg|png)(\\?|$)", ignore_case = TRUE)), str_detect(text, regex("Low resolution image", ignore_case = TRUE))) |> dplyr::pull(href) |> dplyr::first(default = NA_character_)
    image_href_large <- detail_links |> dplyr::filter(!is.na(href), href != "", str_detect(href, regex("\\.(jpg|jpeg|png)(\\?|$)", ignore_case = TRUE)), str_detect(text, regex("High resolution image", ignore_case = TRUE))) |> dplyr::pull(href) |> dplyr::first(default = NA_character_)
    fallback_img <- detail_page |> html_element("#heritage_mainarea img, #heritage_rightbar img, .figure_A01 img") |> html_attr("src")
    
    image_url_thumb <- dplyr::case_when(!is.na(image_href_thumb) && image_href_thumb != "" ~ url_absolute(image_href_thumb, detail_url), !is.na(fallback_img) && fallback_img != "" ~ url_absolute(fallback_img, detail_url), TRUE ~ NA_character_)
    image_url_large <- dplyr::case_when(!is.na(image_href_large) && image_href_large != "" ~ url_absolute(image_href_large, detail_url), !is.na(image_href_thumb) && image_href_thumb != "" ~ url_absolute(image_href_thumb, detail_url), !is.na(fallback_img) && fallback_img != "" ~ url_absolute(fallback_img, detail_url), TRUE ~ NA_character_)
    
    description <- detail_page |>
      html_elements("p, .text_A01, #heritage_mainarea, #heritage_rightbar") |>
      html_text2() |>
      clean_txt() |>
      discard(\(x) is.na(x) || x == "") |>
      str_remove_all(regex("JavaScript is disabled.*?browser\\.?|Some functions.*?disabled.*?browser\\.?|Please enable JavaScript.*?$", ignore_case = TRUE)) |>
      str_remove("\\s*View\\s+(19|20)\\d0's.*$") |>
      str_remove("\\s*Nissan Heritage Collection Home\\s*$") |>
      str_remove("\\s*Heritage Collection\\s*$") |>
      str_squish() |>
      discard(\(x) is.na(x) || x == "") |>
      unique() |>
      keep(\(x) str_length(x) > 120) |>
      discard(\(x) str_detect(x, regex("JavaScript is disabled|Please enable JavaScript|Some functions.*disabled", ignore_case = TRUE))) |>
      first(default = NA_character_) |>
      na_if("")
    
    specs <- parse_nissan_specs(detail_page)
  
    card_base |>
      mutate(
        no = coalesce(detail_no, no),
        title = coalesce(detail_title, title),
        type = coalesce(detail_type, type),
        year = coalesce(detail_year, year),
        image_url = .env$image_url_large,
        image_data_uri = tryCatch(if (!is.na(.env$image_url_thumb) && .env$image_url_thumb != "") image_url_to_data_uri(.env$image_url_thumb) else NA_character_, error = \(e) NA_character_),
        description = .env$description,
        url = detail_url
      ) |>
      bind_cols(specs)
  })
}

extract_nissan_detail_data <- function(page, detail_url) {
  links <- page |>
    html_elements("a") |>
    map_dfr(\(a) {
      tibble(
        text = a |> html_text2() |> clean_txt(),
        href = a |> html_attr("href")
      )
    })
  
  image_url <- links |>
    filter(
      !is.na(href),
      href != "",
      str_detect(href, regex("\\.(jpg|jpeg|png)(\\?|$)", ignore_case = TRUE)),
      str_detect(text, regex("Low resolution image|High resolution image", ignore_case = TRUE)) |
        str_detect(href, regex("modelDetail|uploader|Web", ignore_case = TRUE))
    ) |>
    mutate(priority = case_when(
      str_detect(text, regex("Low resolution image", ignore_case = TRUE)) ~ 1,
      str_detect(text, regex("High resolution image", ignore_case = TRUE)) ~ 2,
      TRUE ~ 3
    )) |>
    arrange(priority) |>
    pull(href) |>
    first(default = NA_character_)
  
  image_url <- if (!is.na(image_url) && image_url != "") {
    url_absolute(image_url, detail_url)
  } else {
    page |>
      html_element("#heritage_mainarea img, #heritage_rightbar img, .figure_A01 img") |>
      html_attr("src") |>
      {\(x) if (!is.na(x) && x != "") url_absolute(x, detail_url) else NA_character_}()
  }
  
  description <- page |>
    html_elements("#heritage_rightbar .text_A01 p, #heritage_rightbar p, #heritage_mainarea .text_A01 p, #heritage_mainarea p") |>
    html_text2() |>
    clean_txt()
  
  description <- description[
    description != "" &
      !str_detect(
        description,
        regex("^(No\\.|Type:|Year:)|Specifications|Download|Related information|High resolution image|Low resolution image", ignore_case = TRUE)
      )
  ] |>
    unique() |>
    paste(collapse = "\n\n") |>
    na_if("")
  
  tibble(
    image_url = image_url,
    image_data_uri = if (!is.na(image_url) && image_url != "") image_url_to_data_uri(image_url) else NA_character_,
    description = description
  )
}

# scrape -----------------------------------------------------------------
session <- chromote::ChromoteSession$new()

page <- get_page_html(session, url, wait = 5, n_scroll = 20, scroll_wait = 800, timeout = 60) |>
  read_html()

session$close()

nissan_category_links <- page |>
  html_elements("a") |>
  map_dfr(\(x) {
    tibble(
      category = x |> html_text2() |> clean_txt(),
      url = x |> html_attr("href") |> url_absolute(url)
    )
  }) |>
  filter(
    category != "",
    str_detect(url, "/EN/HERITAGE_COLLECTION/"),
    str_detect(url, "\\.html$"),
    !str_detect(url, "/(19|20)\\d{2}\\.html$"),
    !str_detect(url, "index|home|evolutionchart|collection-evolutionchart")
  ) |>
  distinct(category, url) |>
  arrange(category)

nissan_year_links <- tibble(
  decade = seq(1920, 2010, by = 10),
  url = paste0(url, decade, ".html")
)

# checks: links
nissan_year_links
nissan_category_links |> slice_sample(prop = 1)

nissan_models_specs <- nissan_category_links |>
  pull(url) |>
  map_dfr(scrape_nissan_index_page) |>
  mutate(
    image_url = if_else(has_detail & !is.na(image_url) & is.na(image_data_uri), NA_character_, image_url),
    engine = case_when(engine_code == "LZ14" ~ "inline-4", TRUE ~ engine)
  )

nissan_models_specs_v2 <- nissan_year_links |>
  pull(url) |>
  map_dfr(scrape_nissan_index_page) |>
  mutate(
    image_url = if_else(has_detail & !is.na(image_url) & is.na(image_data_uri), NA_character_, image_url),
    engine = case_when(engine_code == "LZ14" ~ "inline-4", TRUE ~ engine)
  )

# bind specs all ---------------------------------------------------------
nissan_models_specs_raw <- bind_rows(
  nissan_models_specs |> mutate(source_scrape = "category"),
  nissan_models_specs_v2 |> mutate(source_scrape = "year")
)

nissan_models_specs_all <- nissan_models_specs_raw |>
  mutate(
    image_url = if_else(has_detail & !is.na(image_url) & is.na(image_data_uri), NA_character_, image_url),
    engine = case_when(
      engine_code == "LZ14" ~ "inline-4",
      TRUE ~ engine
    ),
    quality_score =
      as.integer(has_detail) +
      as.integer(!is.na(image_url)) +
      as.integer(!is.na(image_data_uri)) +
      as.integer(!is.na(description)) +
      as.integer(!is.na(length_mm)) +
      as.integer(!is.na(engine)) +
      as.integer(!is.na(displacement_cc)) +
      as.integer(!is.na(max_power_cv)) +
      as.integer(!is.na(torque_nm))
  ) |>
  arrange(no, year, desc(quality_score), source_scrape) |>
  distinct(no, year, .keep_all = TRUE)

nissan_final_cols <- c(
  "no",
  "model", "model_full", "model_family", "model_code",
  "year", "category",
  "heritage_category", "heritage_decade", "vehicle_type",
  "engine", "engine_code", "displacement_cc",
  "max_power_cv", "rpm_max_power",
  "torque_nm", "rpm_max_torque",
  "top_speed_kmh",
  "weight_kg", "wheelbase_mm",
  "length_mm", "width_mm", "height_mm",
  "url", "image_url", "image_data_uri",
  "thumb_url", "thumb_data_uri",
  "description"
)

nissan_models_specs_final <- nissan_models_specs_all |>
  mutate(
    source_category = category,
    heritage_category = if_else(source_scrape == "category", source_category, NA_character_),
    heritage_decade = if_else(source_scrape == "year", source_category, NA_character_),
    vehicle_type = type,
    
    model_full = title,
    model = title |>
      str_remove("\\s*\\([^)]*\\)\\s*$") |>
      str_squish(),
    
    model_family = heritage_category,
    
    model_code = title |>
      str_extract("\\((?:19|20)\\d{2}\\s*:\\s*[^\\)]+\\)") |>
      str_remove("^\\((?:19|20)\\d{2}\\s*:\\s*") |>
      str_remove("\\)$") |>
      str_squish() |>
      na_if("-"),
    
    category = case_when(
      str_detect(
        paste(title, type, source_category, description, raw_text),
        regex("rally|race|racing|silhouette|super silhouette|calsonic|nismo|jgtc|super gt|lemans|le mans|gr\\. c|group c|competition", ignore_case = TRUE)
      ) ~ "Competition",
      TRUE ~ "Road"
    )
  ) |>
  select(all_of(nissan_final_cols))

nissan_models_specs_final

# check specs long -------------------------------------------------------
# This is to see what specs are available and how they look before trying to parse them into structured data.
nissan_specs_long <- nissan_models_specs |>
  filter(has_detail) |>
  distinct(category, no, title, year, url) |>
  mutate(specs = map(url, \(u) {
    cli::cli_inform("Scraping specs long {u}")
    Sys.sleep(0.3)
    
    read_nissan_detail_html(u) |>
      parse_nissan_specs_long()
  })) |>
  unnest(specs)

nissan_specs_long |>
  count(spec, sort = TRUE) |>
  print(n = Inf)

nissan_specs_long |>
  filter(spec == "other") |>
  count(value, sort = TRUE) |>
  print(n = Inf)

nissan_specs_long |>
  filter(str_detect(spec, "power|torque|displacement")) |>
  count(spec, value, sort = TRUE) |>
  print(n = Inf)

nissan_specs_long |>
  filter(spec %in% c(
    "enginemax_power",
    "enginemax_torque",
    "max_power_net",
    "max_torque_net",
    "max_power",
    "engine_max_powerc",
    "engine_max_torquec",
    "engine_displacement_max_power"
  )) |>
  count(spec, sort = TRUE) |>
  print(n = Inf)


# removing intermediate tables -------------------------------------------
rm(nissan_models_specs_raw, nissan_models_specs_all, nissan_final_cols, page, session)

# datatable --------------------------------------------------------------
nissan_models_dt <- nissan_models_specs_final |>
  dplyr::mutate(
    model = dplyr::if_else(
      is.na(url) | url == "",
      model,
      glue::glue("<a href='{url}' target='_blank'>{model}</a>")
    ),
    photo = {
      src <- dplyr::coalesce(
        dplyr::na_if(image_data_uri, ""),
        dplyr::na_if(thumb_data_uri, ""),
        dplyr::na_if(thumb_url, ""),
        ""
      )

      dplyr::if_else(
        src == "",
        "",
        glue::glue("<img src='{src}' width='100' />")
      )
    }
  ) |>
  dplyr::relocate(photo, .after = model) |>
  dplyr::select(-url, -contains("_url"), -contains("_uri")) |>
  glimpse() |>
  make_dt(search = "columns")

nissan_models_dt

# save -------------------------------------------------------------------
readr::write_csv(nissan_models_specs_final, out_csv)

readr::write_csv(nissan_specs_long, out_specs_csv)

htmlwidgets::saveWidget(nissan_models_dt, file = out_html, libdir = "lib", selfcontained = FALSE, title = "Nissan Heritage Collection")