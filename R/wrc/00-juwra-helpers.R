scrape_juwra_editions <- function(rally_slug, rally_name, rally_url, pause = 0.2) {
  # Extrae las ediciones/años de un rally desde Juwra, incluyendo casos con nombres de archivo especiales.
  
  if (pause > 0) {
    Sys.sleep(pause)
  }
  
  cli::cli_progress_step("Scraping Juwra editions: {rally_name}")
  
  page <- httr2::request(rally_url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_timeout(30) |>
    httr2::req_perform() |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    iconv(from = "ISO-8859-1", to = "UTF-8") |>
    xml2::read_html()
  
  edition_nodes <- page |>
    html_elements("table.OK_SOLUVARI_VALKEA a.OK_SIVUNAVI_ALLCAPS")
  
  tibble(
    event_name = edition_nodes |> html_text2() |> clean_txt(),
    href = edition_nodes |> html_attr("href")
  ) |>
    filter(
      !is.na(href),
      str_detect(href, "\\d{4}\\.html$"),
      !str_detect(href, "_results\\.html$")
    ) |>
    mutate(
      rally_slug = rally_slug,
      rally_name = rally_name,
      year = href |> str_extract("\\d{4}(?=\\.html$)") |> as.integer(),
      juwra_event_slug = href |> basename() |> str_remove("\\.html$"),
      wrc_event_url = xml2::url_absolute(href, rally_url),
      wrc_results_url = wrc_event_url |> str_replace("\\.html$", "_results.html")
    ) |>
    distinct(juwra_event_slug, year, .keep_all = TRUE) |>
    select(
      rally_slug,
      rally_name,
      event_name,
      year,
      juwra_event_slug,
      wrc_event_url,
      wrc_results_url
    ) |>
    arrange(year)
}

scrape_wrc_juwra_results <- function(rally_slug, year, wrc_results_url, pause = 0.01) {
  # Descarga resultados Juwra y los guarda usando el rally_slug canónico del proyecto.
  
  if (pause > 0) {
    Sys.sleep(pause)
  }
  
  fout <- str_glue("data/wrc/{rally_slug}/{year}.csv")
  
  fs::dir_create(dirname(fout))
  
  cli::cli_progress_step("{wrc_results_url} -> {fout}")
  
  if (file.exists(fout)) {
    return(invisible(TRUE))
  }
  
  page <- httr2::request(wrc_results_url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_timeout(30) |>
    httr2::req_perform() |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    iconv(from = "ISO-8859-1", to = "UTF-8") |>
    xml2::read_html()
  
  content_tables <- page |>
    html_elements("td.OK_LEIPIS > table")
  
  if (length(content_tables) < 2) {
    cli::cli_warn("No valid result tables found: {wrc_results_url}")
    return(invisible(FALSE))
  }
  
  final_results <- content_tables[[1]] |>
    parse_wrc_juwra_table(status = "classified")
  
  retirements <- content_tables[[2]] |>
    parse_wrc_juwra_table(status = "retired")
  
  wrc_results <- bind_rows(final_results, retirements)
  
  if (nrow(wrc_results) == 0) {
    cli::cli_warn("No result rows found: {wrc_results_url}")
    return(invisible(FALSE))
  }
  
  wrc_results <- wrc_results |>
    clean_wrc_juwra_table()
  
  wrc_results |>
    write_csv(fout)
  
  invisible(TRUE)
}

parse_wrc_juwra_table <- function(table_node, status) {
  rows <- table_node |>
    html_elements("tr")
  
  map_dfr(rows, function(row) {
    cells <- row |>
      html_elements("td.OK_RIVITYS1, td.OK_RIVITYS2")
    
    values <- cells |>
      html_text2() |>
      clean_txt()
    
    if (status == "classified" && length(values) != 7) {
      return(tibble())
    }
    
    if (status == "retired" && length(values) != 6) {
      return(tibble())
    }
    
    driver_cell <- if (status == "classified") cells[[3]] else cells[[2]]
    
    driver_name <- driver_cell |>
      html_element("b") |>
      html_text2() |>
      clean_txt()
    
    if (is.na(driver_name) || driver_name == "") {
      return(tibble())
    }
    
    car_model <- driver_cell |>
      html_text2() |>
      clean_txt() |>
      stringr::str_remove(stringr::fixed(driver_name)) |>
      clean_txt()
    
    if (status == "classified") {
      tibble(
        position = values[[1]],
        entry_number = values[[2]],
        driver_name = driver_name,
        car_model = car_model,
        nationality = values[[4]],
        homologation_group = values[[5]],
        elapsed_time = values[[6]],
        penalty_time = values[[7]],
        retirement_control = NA_character_,
        retirement_reason = NA_character_
      )
    } else {
      retirement_reason <- cells[[6]] |>
        html_element("i") |>
        html_text2() |>
        clean_txt()
      
      tibble(
        position = NA_character_,
        entry_number = values[[1]],
        driver_name = driver_name,
        car_model = car_model,
        nationality = values[[3]],
        homologation_group = values[[4]],
        elapsed_time = NA_character_,
        penalty_time = NA_character_,
        retirement_control = values[[5]],
        retirement_reason = retirement_reason
      )
    }
  })
}

clean_wrc_juwra_table <- function(data) {
  data |>
    mutate(
      across(where(is.character), clean_txt),
      across(where(is.character), \(x) na_if(x, "")),
      across(where(is.character), \(x) na_if(x, "??")),
      elapsed_time = na_if(elapsed_time, "--:--"),
      penalty_time = na_if(penalty_time, "--:--"),
      position = parse_number(position) |> as.integer(),
      entry_number = parse_number(entry_number) |> as.integer()
    ) |>
    select(
      position,
      entry_number,
      driver_name,
      car_model,
      nationality,
      homologation_group,
      elapsed_time,
      penalty_time,
      retirement_control,
      retirement_reason
    )
}

search_serper_real_image <- function(
  query,
  api_key = Sys.getenv("SERPER_API_KEY"),
  n = 10,
  cache_file = "outputs/csv/wrc/serper_wrc_images.csv"
) {
  if (api_key == "") {
    stop("Falta SERPER_API_KEY.")
  }

  fs::dir_create(dirname(cache_file))

  cache <- if (file.exists(cache_file)) {
    readr::read_csv(
      cache_file,
      show_col_types = FALSE,
      col_types = readr::cols(
        .default = readr::col_character(),
        image_score = readr::col_double()
      )
    )
  } else {
    tibble::tibble(
      query = character(),
      image_url = character(),
      page_url = character(),
      title = character(),
      source = character(),
      image_score = double()
    )
  }

  cached <- cache |>
    dplyr::filter(!stringr::str_detect(stringr::str_to_lower(stringr::str_c(image_url, " ", page_url)), "lookaside|modelsshop|ixomodels|1999\\.co|1999\\.co\\.jp")) |>
    dplyr::filter(query == !!query) |>
    dplyr::arrange(dplyr::desc(image_score)) |>
    dplyr::slice_head(n = n)

  if (nrow(cached) > 0) {
    cli::cli_inform("Using cached image: {query}")
    return(cached)
  }

  query_real <- stringr::str_glue("{query} rally photo")

  cli::cli_inform("Searching real image: {query_real}")

  res <- httr2::request("https://google.serper.dev/images") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `X-API-KEY` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      list(q = query_real, num = 20),
      auto_unbox = TRUE
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  out <- httr2::resp_body_json(res, simplifyVector = TRUE)

  if (is.null(out$images) || nrow(out$images) == 0) {
    result <- tibble::tibble(
      query = query,
      image_url = NA_character_,
      page_url = NA_character_,
      title = NA_character_,
      source = NA_character_,
      image_score = NA_real_
    )

    dplyr::bind_rows(cache, result) |>
      dplyr::distinct(query, image_url, .keep_all = TRUE) |>
      readr::write_csv(cache_file)

    return(result)
  }

  image_ext_pattern <- "\\.(jpg|jpeg|png|webp)(\\?|$)"

  direct_image_source_pattern <- paste(
    c(
      "preview.redd.it",
      "i.redd.it",
      "pbs.twimg.com",
      "live.staticflickr.com",
      "c8.alamy.com",
      "snaplap.net/wp-content/uploads",
      "wp-content/uploads"
    ),
    collapse = "|"
  )

  blocked_pattern <- paste(
    c(
      "lookaside.fbsbx", "facebook", "instagram", "lookaside",
      "youtube", "ytimg", "pinterest",
      "hiroboy", "ixomodels", "1999.co.jp", "ebay", "etsy", "amazon",
      "1999.co", "models118", "modelsshop", "diecast", "modelcars",
      "miniatures", "model-car", "car-model"
    ),
    collapse = "|"
  )

  sales_pattern <- paste(
    c(
      "racemarket", "auction", "for-sale", "forsale", "sale",
      "dealer", "showroom", "stock", "classified", "secret-classics"
    ),
    collapse = "|"
  )

  model_word_pattern <- paste(
    c(
      "diecast", "model", "miniature", "ixo", "spark", "trofeu",
      "kyosho", "hpi", "altaya", "1:43", "1:18", "kit", "decals",
      "models118", "modelsshop", "ixo_"
    ),
    collapse = "|"
  )

  result <- out$images |>
    tibble::as_tibble() |>
    dplyr::transmute(
      query = query,
      image_url = imageUrl,
      page_url = link,
      title = dplyr::coalesce(title, ""),
      source = dplyr::coalesce(source, ""),
      text = stringr::str_to_lower(stringr::str_c(title, " ", source, " ", link, " ", imageUrl)),
      image_url_lwr = stringr::str_to_lower(image_url)
    ) |>
    dplyr::mutate(
      is_direct_image =
        stringr::str_detect(image_url_lwr, image_ext_pattern) |
          stringr::str_detect(image_url_lwr, direct_image_source_pattern),
      blocked = stringr::str_detect(text, blocked_pattern),
      looks_sale = stringr::str_detect(text, sales_pattern),
      looks_model = stringr::str_detect(text, model_word_pattern),
      looks_real = stringr::str_detect(text, "rally|wrc|photo|sainz|auriol|toyota|celica|corolla"),
      image_score =
        dplyr::if_else(is_direct_image, 80, 0) +
        dplyr::if_else(looks_real, 50, 0) -
        dplyr::if_else(blocked, 120, 0) -
        dplyr::if_else(looks_model, 80, 0) -
        dplyr::if_else(looks_sale, 40, 0)
    ) |>
    dplyr::filter(!blocked) |>
    dplyr::arrange(dplyr::desc(image_score)) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(query, image_url, page_url, title, source, image_score)

  dplyr::bind_rows(cache, result) |>
    dplyr::distinct(query, image_url, .keep_all = TRUE) |>
    readr::write_csv(cache_file)

  result
}
