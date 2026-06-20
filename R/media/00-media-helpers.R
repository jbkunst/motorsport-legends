# Diccionario URLs:
# source_site: sitio fuente scrapeado, por ejemplo "imcdb", "igcd", "fandom".
# source_vehicle_url: URL de la ficha del vehículo en la fuente.
# source_media_url: URL de la obra en la fuente, para ver otros autos de la película/juego/anime.
# reference_url: URL manual externa para contexto, como Wikipedia, YouTube, Fandom o trailer.
# scale64_url: URL de la miniatura 1:64, tienda o ficha del modelo.
# scale64_image_url: URL de imagen de la miniatura 1:64.
# image_url: URL de la imagen del vehículo en la fuente scrapeada.
# image_data_uri: imagen descargada en base64/data URI para mostrar en app/DT.
scrape_media_vehicle <- function(source, url, include_image_data_uri = TRUE) {
  # Ejecuta el scraper correcto usando un registro de funciones por fuente.
  
  scrapers <- list(
    imcdb = scrape_imcdb_vehicle,
    igcd = scrape_igcd_vehicle
  )
  
  source <- stringr::str_to_lower(source)
  scraper <- scrapers[[source]]
  
  if (is.na(source) || is.na(url) || url == "" || is.null(scraper)) {
    return(tibble::tibble(source_site = source, source_vehicle_url = url))
  }
  
  scraper(url, include_image_data_uri = include_image_data_uri)
}

scrape_imcdb_vehicle <- function(url, include_image_data_uri = TRUE) {
  # Scrapea una ficha IMCDb y devuelve metadata esencial estandarizada.
  
  resp <- httr2::request(url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_perform()
  
  final_url <- httr2::resp_url(resp)
  
  page <- resp |>
    httr2::resp_body_string() |>
    xml2::read_html()
  
  box <- rvest::html_element(page, "#VehicleDetails")
  
  if (is.na(box)) {
    return(tibble::tibble(source_site = "imcdb", source_vehicle_url = final_url))
  }
  
  canonical_url <- rvest::html_attr(rvest::html_element(page, "link[rel='canonical']"), "href") |>
    xml2::url_absolute(final_url) |>
    stringr::str_replace_all("&amp;", "&")
  
  source_vehicle_url <- dplyr::coalesce(canonical_url, final_url)
  
  h1_txt <- rvest::html_text2(rvest::html_element(box, "h1.BoxTitle")) |>
    clean_txt()
  
  h2 <- rvest::html_element(box, "h2")
  
  h2_txt <- rvest::html_text2(h2) |>
    clean_txt()
  
  model <- h1_txt |>
    stringr::str_remove("^\\d{4}\\s+") |>
    stringr::str_replace_all("(?<=[a-z])(?=[A-Z0-9])", " ") |>
    stringr::str_replace_all("\\s*\\[", " [") |>
    clean_txt()
  
  make <- rvest::html_elements(h2, "a[href^='vehicles_make-']") |>
    rvest::html_text2() |>
    clean_txt() |>
    dplyr::first(default = NA_character_)
  
  year <- h1_txt |>
    stringr::str_extract("^\\d{4}") |>
    parse_num()
  
  vehicle_model <- if (is.na(make)) {
    model
  } else {
    model |>
      stringr::str_remove(paste0("^", stringr::str_escape(make), "\\s*")) |>
      stringr::str_remove("\\s*\\[[^\\]]+\\]") |>
      clean_txt() |>
      dplyr::na_if("")
  }
  
  movie_node <- rvest::html_element(h2, "a[href^='movie_']")
  
  media_title <- rvest::html_text2(movie_node) |>
    clean_txt()
  
  source_media_url <- rvest::html_attr(movie_node, "href") |>
    xml2::url_absolute(source_vehicle_url) |>
    stringr::str_replace_all("&amp;", "&") |>
    dplyr::na_if("")
  
  media_tail_loc <- stringr::str_locate(h2_txt, stringr::fixed(media_title))
  
  media_tail <- if (any(is.na(media_tail_loc))) {
    NA_character_
  } else {
    stringr::str_sub(h2_txt, media_tail_loc[2] + 1) |>
      clean_txt()
  }
  
  media_match <- stringr::str_match(media_tail, "^,\\s*([^,]+),\\s*(\\d{4})")
  
  media_type <- media_match[, 2] |>
    clean_txt() |>
    stringr::str_to_lower()
  
  media_year <- media_match[, 3] |>
    parse_num()
  
  model_origin <- rvest::html_attr(rvest::html_element(box, ".CarFlag"), "title") |>
    clean_txt()
  
  description <- rvest::html_text2(rvest::html_element(box, ".img-legend p")) |>
    clean_txt() |>
    stringr::str_remove_all("\\[\\*\\]") |>
    stringr::str_remove("Temporarily.*$") |>
    stringr::str_remove("^\\d{2}:\\d{2}:\\d{2}\\s*") |>
    clean_txt() |>
    dplyr::na_if("")
  
  image_url <- rvest::html_attr(rvest::html_element(box, "#MainPicture"), "src") |>
    xml2::url_absolute(source_vehicle_url) |>
    stringr::str_replace_all("&amp;", "&") |>
    dplyr::na_if("")
  
  image_data_uri <- if (include_image_data_uri) {
    tryCatch(image_url_to_data_uri(image_url), error = \(e) NA_character_)
  } else {
    NA_character_
  }
  
  tibble::tibble(
    model = model,
    make = make,
    vehicle_model = vehicle_model,
    year = year,
    model_origin = model_origin,
    media_type = media_type,
    media_year = media_year,
    media_title = media_title,
    source_site = "imcdb",
    source_vehicle_url = source_vehicle_url,
    source_media_url = source_media_url,
    description = description,
    image_url = image_url,
    image_data_uri = image_data_uri
  )
}

scrape_igcd_media <- function(url) {
  # Scrapea una ficha de juego IGCD para obtener título y año de la obra.
  
  if (is.na(url) || url == "") {
    return(tibble::tibble(media_title = NA_character_, media_year = NA_real_))
  }
  
  resp <- httr2::request(url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_headers(
      "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "accept-language" = "en-US,en;q=0.9"
    ) |>
    httr2::req_perform()
  
  html <- resp |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    iconv(from = "", to = "UTF-8", sub = "byte")
  
  if (is.na(html) || html == "") {
    return(tibble::tibble(media_title = NA_character_, media_year = NA_real_))
  }
  
  page <- xml2::read_html(html)
  
  page_title <- rvest::html_text2(rvest::html_element(page, "title")) |>
    clean_txt()
  
  page_txt <- rvest::html_text2(page) |>
    clean_txt()
  
  media_title <- page_title |>
    stringr::str_remove("^IGCD\\.net:\\s*") |>
    stringr::str_remove("^Vehicles/Cars list for\\s*") |>
    stringr::str_remove("\\s*\\([^\\)]*\\)\\s*$") |>
    clean_txt() |>
    dplyr::na_if("")
  
  media_year <- page_txt |>
    stringr::str_match("\\b(19\\d{2}|20\\d{2})\\b") |>
    (\(x) x[, 2])() |>
    parse_num()
  
  tibble::tibble(
    media_title = media_title,
    media_year = media_year
  )
}

scrape_igcd_vehicle <- function(url, include_image_data_uri = TRUE) {
  # Scrapea una ficha IGCD y devuelve metadata esencial estandarizada.
  
  resp <- httr2::request(url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_headers(
      "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "accept-language" = "en-US,en;q=0.9"
    ) |>
    httr2::req_perform()
  
  source_vehicle_url <- httr2::resp_url(resp) |>
    stringr::str_replace("([?&])l=[a-z]{2}(&)?", "\\1") |>
    stringr::str_replace("[?&]$", "")
  
  html <- resp |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    iconv(from = "", to = "UTF-8", sub = "byte")
  
  if (is.na(html) || html == "") {
    return(tibble::tibble(source_site = "igcd", source_vehicle_url = source_vehicle_url))
  }
  
  page <- xml2::read_html(html)
  
  page_txt <- rvest::html_text2(page) |>
    clean_txt()
  
  title_txt <- page_txt |>
    stringr::str_match("\\b(\\d{4}\\s+[A-Z][^\\n]*?)(?:Extra info:|Chassis:|Mk:|Class:|Origin:|Playable|Unplayable|Non-playable)") |>
    (\(x) x[, 2])() |>
    clean_txt()
  
  if (is.na(title_txt)) {
    return(tibble::tibble(source_site = "igcd", source_vehicle_url = source_vehicle_url))
  }
  
  model <- title_txt |>
    stringr::str_remove("^\\d{4}\\s+") |>
    clean_txt()
  
  year <- title_txt |>
    stringr::str_extract("^\\d{4}") |>
    parse_num()
  
  multi_makes <- c("Alfa Romeo", "Aston Martin", "Land Rover", "Mercedes-Benz", "Rolls-Royce")
  
  make <- multi_makes[stringr::str_detect(model, paste0("^", multi_makes, "\\b"))] |>
    dplyr::first(default = NA_character_)
  
  make <- dplyr::coalesce(make, stringr::word(model, 1))
  
  vehicle_model <- model |>
    stringr::str_remove(paste0("^", stringr::str_escape(make), "\\s+")) |>
    clean_txt() |>
    dplyr::na_if("")
  
  game_node <- rvest::html_element(page, "a[href*='game.php?id=']")
  
  media_title_fallback <- rvest::html_text2(game_node) |>
    clean_txt()
  
  source_media_url <- rvest::html_attr(game_node, "href") |>
    xml2::url_absolute(source_vehicle_url) |>
    dplyr::na_if("")
  
  media_info <- scrape_igcd_media(source_media_url)
  
  media_title <- dplyr::coalesce(media_info$media_title, media_title_fallback)
  media_year <- media_info$media_year
  
  extra_info <- page_txt |>
    stringr::str_match("Extra info:\\s*(.*?)(?=Chassis:|Mk:|Class:|Origin:|Playable|Unplayable|Non-playable|$)") |>
    (\(x) x[, 2])() |>
    stringr::str_remove_all("^'|'$") |>
    clean_txt() |>
    dplyr::na_if("")
  
  chassis <- page_txt |>
    stringr::str_match("Chassis:\\s*(.*?)(?=Mk:|Class:|Origin:|Playable|Unplayable|Non-playable|$)") |>
    (\(x) x[, 2])() |>
    clean_txt() |>
    dplyr::na_if("")
  
  vehicle_class <- page_txt |>
    stringr::str_match("Class:\\s*(.*?)(?=Origin:|Playable|Unplayable|Non-playable|$)") |>
    (\(x) x[, 2])() |>
    clean_txt() |>
    dplyr::na_if("")
  
  model_origin <- page_txt |>
    stringr::str_match("Origin:\\s*(.*?)(?=Playable|Unplayable|Non-playable|$)") |>
    (\(x) x[, 2])() |>
    clean_txt() |>
    dplyr::na_if("")
  
  role <- page_txt |>
    stringr::str_match("(Playable and unlockable vehicle|Playable vehicle|Unlockable vehicle|Unplayable vehicle|Non-playable vehicle)") |>
    (\(x) x[, 2])() |>
    clean_txt() |>
    dplyr::na_if("")
  
  unlock_note <- page_txt |>
    stringr::str_match("(?:Playable and unlockable vehicle|Playable vehicle|Unlockable vehicle|Unplayable vehicle|Non-playable vehicle)\\s*(.*?)(?=\\s*Picture|\\s*Pictures|\\s*Contributor|\\s*Comments|$)") |>
    (\(x) x[, 1])() |>
    stringr::str_remove("^(Playable and unlockable vehicle|Playable vehicle|Unlockable vehicle|Unplayable vehicle|Non-playable vehicle)\\s*") |>
    clean_txt() |>
    dplyr::na_if("")
  
  description <- c(
    extra_info,
    if (!is.na(chassis)) paste("Chassis:", chassis) else NA_character_,
    if (!is.na(vehicle_class)) paste("Class:", vehicle_class) else NA_character_,
    role,
    unlock_note
  ) |>
    purrr::discard(is.na) |>
    purrr::discard(\(x) x == "") |>
    paste(collapse = " | ") |>
    dplyr::na_if("")
  
  image_url <- rvest::html_elements(page, "img") |>
    rvest::html_attr("src") |>
    xml2::url_absolute(source_vehicle_url) |>
    stringr::str_replace_all("&amp;", "&") |>
    unique() |>
    purrr::keep(\(x) stringr::str_detect(x, "/images2?/\\d+/\\d+\\.jpg$")) |>
    dplyr::first(default = NA_character_) |>
    dplyr::na_if("")
  
  image_data_uri <- if (include_image_data_uri) {
    tryCatch(image_url_to_data_uri(image_url), error = \(e) NA_character_)
  } else {
    NA_character_
  }
  
  tibble::tibble(
    model = model,
    make = make,
    vehicle_model = vehicle_model,
    year = year,
    model_origin = model_origin,
    media_type = "game",
    media_year = media_year,
    media_title = media_title,
    source_site = "igcd",
    source_vehicle_url = source_vehicle_url,
    source_media_url = source_media_url,
    description = description,
    image_url = image_url,
    image_data_uri = image_data_uri
  )
}