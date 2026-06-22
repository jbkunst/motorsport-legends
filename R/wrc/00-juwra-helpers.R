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