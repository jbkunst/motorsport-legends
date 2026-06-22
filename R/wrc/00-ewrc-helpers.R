# eWRC helpers ------------------------------------------------------------
normalize_ewrc_event_url <- function(url) {
  url |>
    stringr::str_remove("/final-results/?$") |>
    stringr::str_remove("/models/?$") |>
    stringr::str_replace("/?$", "/")
}

absolute_ewrc_models_url <- function(url) {
  case_when(
    is.na(url) ~ NA_character_,
    stringr::str_detect(url, "^https?://") ~ url,
    stringr::str_detect(url, "^/") ~ paste0("https://www.ewrc-models.com", url),
    TRUE ~ paste0("https://www.ewrc-models.com/", url)
  )
}

ewrc_name_key <- function(x) {
  x |>
    clean_txt() |>
    stringr::str_remove_all('"') |>
    stringr::str_extract("^[^\\s]+") |>
    stringr::str_to_lower()
}

empty_ewrc_models_file <- function() {
  tibble(
    event_slug = character(),
    rally_slug = character(),
    rally_name = character(),
    event_name = character(),
    year = integer(),
    ewrc_event_url = character(),
    driver_key = character(),
    codriver_key = character(),
    model_car_name = character(),
    model_brand = character(),
    model_scale = character(),
    model_code = character(),
    ewrc_model_url = character(),
    thumb_url = character(),
    image_url = character()
  )
}

find_ewrc_event_url <- function(rally_name, event_name, year, n = 10, api_key = Sys.getenv("SERPER_API_KEY")) {
  if (api_key == "") {
    stop("Falta SERPER_API_KEY.")
  }
  
  query <- str_glue('site:ewrc-results.com/event "{rally_name}" "{year}"')
  
  cli::cli_inform("Searching eWRC: {event_name}")
  
  res <- httr2::request("https://google.serper.dev/search") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `X-API-KEY` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      list(q = query, num = n),
      auto_unbox = TRUE
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  
  out <- res |>
    httr2::resp_body_json(simplifyVector = TRUE)
  
  if (is.null(out$organic) || length(out$organic) == 0) {
    return(tibble())
  }
  
  out$organic |>
    as_tibble() |>
    mutate(
      title = as.character(title),
      link = as.character(link),
      snippet = as.character(snippet),
      search_text = stringr::str_to_lower(paste(title, snippet, link)),
      ewrc_event_url = stringr::str_extract(link, "https?://(?:www\\.)?ewrc-results\\.com/event/\\d+-[^/?#]+"),
      ewrc_event_url = ewrc_event_url |> stringr::str_replace("/?$", "/"),
      year_match = stringr::str_detect(search_text, as.character(year)),
      rally_match = stringr::str_detect(search_text, stringr::str_to_lower(rally_name)),
      score = case_when(
        !is.na(ewrc_event_url) & year_match & rally_match ~ 100,
        !is.na(ewrc_event_url) & year_match ~ 80,
        !is.na(ewrc_event_url) ~ 50,
        TRUE ~ 0
      )
    ) |>
    filter(!is.na(ewrc_event_url)) |>
    distinct(ewrc_event_url, .keep_all = TRUE) |>
    arrange(desc(score), position) |>
    select(
      score,
      title,
      link,
      ewrc_event_url
    )
}

parse_ewrc_model_card <- function(card, ewrc_models_url) {
  if (inherits(card, "xml_missing")) {
    return(tibble())
  }
  
  model_link <- card |>
    html_element("a[href*='ewrc-models.com/model/'], a[href^='/model/']")
  
  if (inherits(model_link, "xml_missing")) {
    return(tibble())
  }
  
  model_href <- model_link |>
    html_attr("href")
  
  ewrc_model_url <- model_href |>
    absolute_ewrc_models_url()
  
  model_car_name <- model_link |>
    html_text2() |>
    clean_txt()
  
  crew_text <- card |>
    html_elements("div.line-clamp-1.font-bold") |>
    html_text2() |>
    clean_txt()
  
  crew_text <- crew_text[crew_text != ""]
  
  if (length(crew_text) == 0) {
    return(tibble())
  }
  
  crew <- crew_text[[1]] |>
    stringr::str_split("\\s+-\\s+", n = 2, simplify = TRUE)
  
  badges <- card |>
    html_elements("span") |>
    html_text2() |>
    clean_txt()
  
  badges <- badges[badges != ""]
  scale_pos <- which(stringr::str_detect(badges, "^1:\\d+$"))[1]
  
  model_brand <- if (!is.na(scale_pos) && scale_pos > 1) badges[[1]] else NA_character_
  model_scale <- if (!is.na(scale_pos)) badges[[scale_pos]] else NA_character_
  model_code <- if (!is.na(scale_pos) && length(badges) > scale_pos) badges[[scale_pos + 1]] else NA_character_
  
  thumb_url <- card |>
    html_element("img") |>
    html_attr("src") |>
    absolute_ewrc_models_url()
  
  image_url <- thumb_url |>
    stringr::str_replace("/i_", "/m_")
  
  tibble(
    driver_key = ewrc_name_key(crew[, 1]),
    codriver_key = ewrc_name_key(crew[, 2]),
    model_car_name = model_car_name,
    model_brand = model_brand,
    model_scale = model_scale,
    model_code = model_code,
    ewrc_model_url = ewrc_model_url,
    thumb_url = thumb_url,
    image_url = image_url
  )
}

scrape_ewrc_models <- function(ewrc_session, ewrc_event_url, wait = 5, n_scroll = 10, scroll_wait = 800) {
  ewrc_event_url <- normalize_ewrc_event_url(ewrc_event_url)
  ewrc_models_url <- paste0(ewrc_event_url, "models")
  
  cli::cli_inform("Scraping eWRC models: {ewrc_models_url}")
  
  html <- get_page_html(
    session = ewrc_session,
    url = ewrc_models_url,
    wait = wait,
    n_scroll = n_scroll,
    scroll_wait = scroll_wait
  )
  
  page <- html |>
    xml2::read_html()
  
  model_links <- page |>
    html_elements("a[href*='ewrc-models.com/model/'], a[href^='/model/']")
  
  if (length(model_links) == 0) {
    return(empty_ewrc_models_file())
  }
  
  cards <- model_links |>
    map(\(x) html_element(x, xpath = "./ancestor::div[contains(@class, 'relative')][1]"))
  
  out <- cards |>
    map_dfr(parse_ewrc_model_card, ewrc_models_url = ewrc_models_url) |>
    filter(!is.na(ewrc_model_url)) |>
    distinct(ewrc_model_url, .keep_all = TRUE)
  
  if (nrow(out) == 0) {
    return(empty_ewrc_models_file())
  }
  
  out
}

resolve_ewrc_event_url <- function(rally_slug, rally_name, year) {
  event_slug <- str_glue("{rally_slug}_{year}")
  event_name <- str_glue("{rally_name} {year}")
  
  candidates <- find_ewrc_event_url(
    rally_name = rally_name,
    event_name = event_name,
    year = year
  )
  
  if (nrow(candidates) == 0) {
    cli::cli_warn("No eWRC URL found: {event_slug}")
    return(NA_character_)
  }
  
  candidates |>
    arrange(desc(score)) |>
    slice(1) |>
    pull(ewrc_event_url)
}

scrape_one_ewrc_models_file <- function(ewrc_session, rally_slug, rally_name, year, models_dir = "data/models/wrc") {
  event_slug <- str_glue("{rally_slug}_{year}")
  event_name <- str_glue("{rally_name} {year}")
  fout <- str_glue("{models_dir}/{event_slug}.csv")
  
  if (file.exists(fout)) {
    cli::cli_inform("Already exists: {fout}")
    return(invisible(TRUE))
  }
  
  fs::dir_create(dirname(fout))
  
  ewrc_event_url <- resolve_ewrc_event_url(
    rally_slug = rally_slug,
    rally_name = rally_name,
    year = year
  )
  
  if (is.na(ewrc_event_url) || ewrc_event_url == "") {
    empty_ewrc_models_file() |>
      write_csv(fout)
    
    cli::cli_warn("Saved empty file: {fout}")
    return(invisible(FALSE))
  }
  
  models <- scrape_ewrc_models(
    ewrc_session = ewrc_session,
    ewrc_event_url = ewrc_event_url
  ) |>
    filter(
      !is.na(image_url),
      image_url != ""
    ) |>
    mutate(
      event_slug = event_slug,
      rally_slug = rally_slug,
      rally_name = rally_name,
      event_name = event_name,
      year = year,
      ewrc_event_url = ewrc_event_url,
      .before = 1
    )
  
  if (nrow(models) == 0) {
    models <- empty_ewrc_models_file()
  }
  
  models |>
    write_csv(fout)
  
  cli::cli_inform("Saved: {fout} ({nrow(models)} rows)")
  
  invisible(TRUE)
}

select_preferred_ewrc_models <- function(models) {
  models |>
    mutate(
      scale_priority = case_when(
        model_scale == "1:64" ~ 1L,
        model_scale == "1:43" ~ 2L,
        model_scale == "1:24" ~ 3L,
        model_scale == "1:18" ~ 4L,
        TRUE ~ 99L
      )
    ) |>
    arrange(
      event_slug,
      driver_key,
      codriver_key,
      scale_priority,
      model_brand,
      ewrc_model_url
    ) |>
    group_by(event_slug, driver_key, codriver_key) |>
    slice(1) |>
    ungroup() |>
    select(-scale_priority)
}