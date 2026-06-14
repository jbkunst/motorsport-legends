# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(here)

source(here::here("R/00-helpers.R"))

# params -----------------------------------------------------------------
url <- "https://www.nissan-global.com/EN/HERITAGE_COLLECTION/"

out_csv  <- here("data/cars/nissan_heritage_collection.csv")
out_html <- here("outputs/html/nissan_heritage_collection.html")

fs::dir_create(fs::path_dir(out_csv))
fs::dir_create(fs::path_dir(out_html))

# helpers ----------------------------------------------------------------
spec_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&", "and") |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove_all("^_|_$")
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
        spec = dt |> html_text2() |> clean_txt() |> spec_name(),
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

parse_nissan_specs <- function(page) {
  specs_long <- parse_nissan_specs_long(page)
  
  if (nrow(specs_long) == 0) return(tibble())
  
  specs_wide <- specs_long |>
    pivot_wider(names_from = spec, values_from = value, values_fn = first)
  
  length_width_height <- get_col(specs_wide, "overall_length_width_height")
  lwh <- str_split_fixed(length_width_height, "/", 3)
  
  engine_raw <- get_col(specs_wide, "engine")
  engine_displacement_raw <- get_col(specs_wide, "engine_displacement")
  power_raw <- get_col(specs_wide, "engine_max_power")
  torque_raw <- get_col(specs_wide, "engine_max_torque")
  
  tibble(
    length_mm = coalesce(parse_num(get_col(specs_wide, "overall_length")), parse_num(lwh[, 1])),
    width_mm = coalesce(parse_num(get_col(specs_wide, "overall_width")), parse_num(lwh[, 2])),
    height_mm = coalesce(parse_num(get_col(specs_wide, "overall_height")), parse_num(lwh[, 3])),
    wheelbase_mm = parse_num(get_col(specs_wide, "wheelbase")),
    tread_front_mm = parse_num(str_split_fixed(get_col(specs_wide, "tread_front_rear"), "/", 2)[, 1]),
    tread_rear_mm = parse_num(str_split_fixed(get_col(specs_wide, "tread_front_rear"), "/", 2)[, 2]),
    curb_weight_kg = parse_num(get_col(specs_wide, "curb_weight")),
    
    engine_code = engine_raw |> str_extract("^[^\\(,]+") |> str_squish(),
    engine = case_when(
      str_detect(engine_raw, regex("6-cyl\\.?\\s*in line|inline[- ]?6", ignore_case = TRUE)) ~ "inline-6",
      str_detect(engine_raw, regex("4-cyl\\.?\\s*in line|inline[- ]?4", ignore_case = TRUE)) ~ "inline-4",
      str_detect(engine_raw, regex("3-cyl\\.?\\s*in line|inline[- ]?3", ignore_case = TRUE)) ~ "inline-3",
      str_detect(engine_raw, regex("\\bV6\\b", ignore_case = TRUE)) ~ "V6",
      str_detect(engine_raw, regex("\\bV8\\b", ignore_case = TRUE)) ~ "V8",
      TRUE ~ NA_character_
    ),
    
    displacement_cc = coalesce(
      parse_num(engine_displacement_raw),
      engine_raw |> str_extract("\\d{1,3}(,\\d{3})+|\\d+\\s*cc") |> parse_num()
    ),
    
    max_power_kw = power_raw |> str_extract("\\d+[\\.,]?\\d*\\s*kW") |> parse_num(),
    max_power_cv = coalesce(
      power_raw |> str_extract("(?<=\\()\\d+[\\.,]?\\d*\\s*PS") |> parse_num(),
      (power_raw |> str_extract("\\d+[\\.,]?\\d*\\s*kW") |> parse_num()) / 0.73549875
    ),
    rpm_max_power = power_raw |> str_extract("\\d{1,2},?\\d{3}\\s*rpm") |> parse_num(),
    
    torque_nm = torque_raw |> str_extract("\\d+[\\.,]?\\d*\\s*N[·\\.]?m") |> parse_num(),
    rpm_max_torque = torque_raw |> str_extract("\\d{1,2},?\\d{3}\\s*rpm") |> parse_num(),
    
    top_speed_kmh = parse_num(get_col(specs_wide, "top_speed")),
    charger = get_col(specs_wide, "charger"),
    transmission = get_col(specs_wide, "transmission"),
    
    suspension = coalesce(
      get_col(specs_wide, "suspension"),
      paste(na.omit(c(get_col(specs_wide, "suspension_front"), get_col(specs_wide, "suspension_rear"))), collapse = " / ") |> na_if("")
    ),
    
    brakes = coalesce(get_col(specs_wide, "brakes"), get_col(specs_wide, "brakes_front_rear")),
    tires = coalesce(get_col(specs_wide, "tires_wheels"), get_col(specs_wide, "tires"), get_col(specs_wide, "tires_falken")),
    seating_capacity = parse_num(get_col(specs_wide, "seating_capacity"))
  )
}

scrape_nissan_index_page <- function(url = "https://www.nissan-global.com/EN/HERITAGE_COLLECTION/bluebird.html") {
  category <- url |>
    basename() |>
    str_remove("\\.html$") |>
    str_replace_all("_", " ") |>
    str_to_title()
  
  cli::cli_inform("Scraping category {url}")
  
  page <- read_html(url)
  
  cards <- page |>
    html_elements(".thumbnail_block") |>
    keep(\(x) length(html_elements(x, ".thumbnail_block_list li")) > 0)
  
  map_dfr(cards, \(card) {
    card_lines <- card |>
      html_elements(".thumbnail_block_list li") |>
      html_text2() |>
      clean_txt()
    
    raw_text <- card_lines |> paste(collapse = " | ")
    cli::cli_inform("Scraping card with lines: {raw_text}")
    
    no <- card_lines |>
      str_subset("^No\\.") |>
      first(default = NA_character_) |>
      str_remove("^No\\.\\s*") |>
      na_if("")
    
    title <- card |>
      html_element(".thumbnail_block_list li em") |>
      html_text2() |>
      clean_txt() |>
      na_if("")
    
    type <- card_lines |>
      str_subset("^Type:") |>
      first(default = NA_character_) |>
      str_remove("^Type:\\s*") |>
      na_if("")
    
    year <- card_lines |>
      str_subset("^Year:") |>
      first(default = NA_character_) |>
      str_remove("^Year:\\s*") |>
      parse_integer()
    
    color <- card_lines[
      !str_detect(card_lines, "^No\\.|^Type:|^Year:") &
        card_lines != title
    ] |>
      first(default = NA_character_) |>
      na_if("")
    
    thumb_src <- card |>
      html_element(".thumbnail_block_figure img") |>
      html_attr("src")
    
    thumb_url <- if (!is.na(thumb_src) && thumb_src != "") url_absolute(thumb_src, url) else NA_character_
    
    detail_a <- card |>
      html_element(".thumbnail_block_btn a")
    
    detail_href <- detail_a |>
      html_attr("href")
    
    detail_class <- detail_a |>
      html_attr("class")
    
    detail_disabled <- !is.na(detail_class) && str_detect(detail_class, "\\bdisable\\b")
    
    detail_url <- if (
      !detail_disabled &&
      !is.na(detail_href) &&
      detail_href != "" &&
      detail_href != "#" &&
      !str_ends(detail_href, "#")
    ) {
      url_absolute(detail_href, url)
    } else {
      NA_character_
    }
    
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
      url = if (!is.na(detail_url)) detail_url else paste0(url, "#no-", no),
      has_detail = !is.na(detail_url)
    )
    
    if (is.na(detail_url)) return(card_base)
    
    Sys.sleep(0.3)
    cli::cli_inform("Scraping detail {detail_url}")
    
    detail_page <- read_nissan_detail_html(detail_url)
    
    detail_no <- detail_page |>
      html_element("#collection_neme_area .note_list_A01 li") |>
      html_text2() |>
      clean_txt() |>
      str_remove("^No\\.\\s*") |>
      na_if("")
    
    detail_title <- detail_page |>
      html_element("#collection_neme_area h1") |>
      html_text2() |>
      clean_txt() |>
      na_if("")
    
    detail_type <- detail_page |>
      html_element("#collection_neme_area .lead_text_A01 p") |>
      html_text2() |>
      clean_txt() |>
      na_if("")
    
    detail_year <- detail_title |>
      str_extract("\\b(19|20)\\d{2}\\b") |>
      as.integer()
    
    link_nodes <- detail_page |>
      html_elements("a")
    
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
    
    image_href <- detail_links |>
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
    
    fallback_img <- detail_page |>
      html_element("#heritage_mainarea img, #heritage_rightbar img, .figure_A01 img") |>
      html_attr("src")
    
    image_url <- case_when(
      !is.na(image_href) && image_href != "" ~ url_absolute(image_href, detail_url),
      !is.na(fallback_img) && fallback_img != "" ~ url_absolute(fallback_img, detail_url),
      TRUE ~ NA_character_
    )
        
    description <- detail_page |>
      html_elements("p, .text_A01, #heritage_mainarea, #heritage_rightbar") |>
      html_text2() |>
      clean_txt() |>
      discard(\(x) is.na(x) || x == "") |>
      str_squish() |>
      unique() |>
      keep(\(x) str_length(x) > 120) |>
      str_remove("\\s*View\\s+(19|20)\\d0's.*$") |>
      str_remove("\\s*Nissan Heritage Collection Home\\s*$") |>
      str_squish() |>
      first(default = NA_character_) |>
      na_if("")
    
    specs <- parse_nissan_specs(detail_page)
    
    card_base |>
      mutate(
        no = coalesce(detail_no, no),
        title = coalesce(detail_title, title),
        type = coalesce(detail_type, type),
        year = coalesce(detail_year, year),
        image_url = .env$image_url,
        image_data_uri = tryCatch(if (!is.na(.env$image_url) && .env$image_url != "") image_url_to_data_uri(.env$image_url) else NA_character_, error = \(e) NA_character_),
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
      url = x |> html_attr("href") |> url_absolute(base_url)
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
  decade = seq(1920, 2020, by = 10),
  url = paste0(base_url, decade, ".html")
)

# checks: links
nissan_year_links
nissan_category_links |> slice_sample(prop = 1)

# scrape by category
nissan_models_specs <- nissan_category_links |>
  pull(url) |> 
  map_dfr(scrape_nissan_index_page)

nissan_models_specs

nissan_models_specs |>
  summarise(
    image_url = sum(!is.na(image_url)),
    image_data_uri = sum(!is.na(image_data_uri)),
    description = sum(!is.na(description))
  )

# fix
nissan_models_specs <- nissan_models_specs |>
  mutate(
    image_url = if_else(has_detail & !is.na(image_url) & is.na(image_data_uri), NA_character_, image_url)
  )

nissan_models_specs |>
  filter(has_detail, is.na(description)) |>
  select(category, no, title, year, url) |>
  print(n = Inf)


# checks: category --------------------------------------------------------
nissan_models_by_category |>
  count(source_name, sort = TRUE)

nissan_models_by_category |>
  filter(str_detect(source_name, regex("racing|race", ignore_case = TRUE))) |>
  slice_sample(prop = 1)

nissan_models_by_category |>
  slice_sample(prop = 1)

# scrape by year ----------------------------------------------------------
nissan_models_by_year <- nissan_year_links |>
  mutate(
    data = map2(url, decade, \(u, dec) {
      Sys.sleep(0.3)
      tryCatch(
        scrape_nissan_index_page(u, source_index = "year", source_name = dec),
        error = \(e) tibble(
          source_index = character(), source_name = character(), no = character(),
          title = character(), type = character(), year = integer(),
          color = character(), raw_text = character(), url = character()
        )
      )
    })
  ) |>
  select(data) |>
  unnest(data)

# checks: year ------------------------------------------------------------
nissan_models_by_year |>
  count(source_name, sort = TRUE)

nissan_models_by_year |>
  slice_sample(prop = 1)

# compare indexes ---------------------------------------------------------
nissan_index_all <- bind_rows(
  nissan_models_by_year,
  nissan_models_by_category
) |>
  distinct(source_index, source_name, url, .keep_all = TRUE)

nissan_index_overlap <- nissan_index_all |>
  summarise(
    sources = paste(sort(unique(source_index)), collapse = " + "),
    n_sources = n_distinct(source_index),
    .by = url
  ) |>
  count(sources, sort = TRUE)

nissan_only_year <- nissan_models_by_year |>
  anti_join(nissan_models_by_category |> distinct(url), by = "url")

nissan_only_category <- nissan_models_by_category |>
  anti_join(nissan_models_by_year |> distinct(url), by = "url")

# checks: overlap ---------------------------------------------------------
nissan_index_overlap

nissan_only_year |>
  slice_sample(prop = 1)

nissan_only_category |>
  slice_sample(prop = 1)

nissan_index_all |>
  filter(str_detect(raw_text, regex("R390|Skyline|GT-R|Silvia|Fairlady|R92|R91|R90|Bluebird|Racing", ignore_case = TRUE))) |>
  slice_sample(prop = 1)

# final index -------------------------------------------------------------
nissan_models_index <- nissan_index_all |>
  group_by(url) |>
  summarise(
    title = first(na.omit(title)),
    type = first(na.omit(type)),
    year = first(na.omit(year)),
    color = first(na.omit(color)),
    no = paste(sort(unique(na.omit(no))), collapse = ", "),
    source_indexes = paste(sort(unique(source_index)), collapse = " + "),
    source_names = paste(sort(unique(source_name)), collapse = " | "),
    raw_text = first(na.omit(raw_text)),
    .groups = "drop"
  ) |>
  mutate(
    no = na_if(no, ""),
    source_indexes = na_if(source_indexes, ""),
    source_names = na_if(source_names, "")
  ) |>
  arrange(year, title)

# checks: final -----------------------------------------------------------
nissan_models_index |>
  glimpse()

nissan_models_index |>
  count(source_indexes, sort = TRUE)

nissan_models_index |>
  filter(str_detect(title, regex("R390|Skyline|GT-R|Silvia|Fairlady|R92|R91|R90|Bluebird|Racing", ignore_case = TRUE))) |>
  slice_sample(prop = 1)

nissan_models_index |>
  filter(is.na(title) | title == "") |>
  slice_sample(prop = 1)

# save -------------------------------------------------------------------
readr::write_csv(nissan_models_index, out_csv)

nissan_models_index |>
  make_dt(search = "columns") |>
  htmlwidgets::saveWidget(out_html, selfcontained = TRUE)