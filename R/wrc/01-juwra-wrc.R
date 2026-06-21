# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)

source("R/00-helpers.R")

# parameters -------------------------------------------------------------
wrc_rallies_url <- "https://www.juwra.com/rallies.html"

# helpers ----------------------------------------------------------------
scrape_wrc_editions <- function(rally_slug, rally_name, rally_url, pause = 0) {
  
  cli::cli_inform("Scraping {rally_url}")
  
  if (pause > 0) {
    Sys.sleep(pause)
  }


  
  page <- httr2::request(rally_url) |>
    httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
    httr2::req_timeout(30) |>
    httr2::req_perform() |>
    httr2::resp_body_raw() |>
    rawToChar() |>
    iconv(from = "ISO-8859-1", to = "UTF-8") |>
    xml2::read_html()
  
  edition_nodes <- page |>
    rvest::html_elements("table.OK_SOLUVARI_VALKEA a.OK_SIVUNAVI_ALLCAPS")
  
  tibble::tibble(
    event_name = edition_nodes |> rvest::html_text2() |> clean_txt(),
    href = edition_nodes |> rvest::html_attr("href")
  ) |>
    filter(
      stringr::str_detect(href, glue::glue("^{rally_slug}_\\d{{4}}\\.html$"))
    ) |>
    mutate(
      rally_slug = rally_slug,
      rally_name = rally_name,
      year = href |> stringr::str_extract("\\d{4}") |> as.integer(),
      wrc_event_url = xml2::url_absolute(href, rally_url),
      wrc_results_url = wrc_event_url |> stringr::str_replace("\\.html$", "_results.html")
    ) |>
    select(
      rally_slug,
      rally_name,
      event_name,
      year,
      wrc_event_url,
      wrc_results_url
    ) |>
    arrange(year)
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
        status = "classified",
        position = values[[1]],
        entry_number = values[[2]],
        driver_name = driver_name,
        car_model = car_model,
        nationality = values[[4]],
        homologation_group = values[[5]],
        elapsed_time = values[[6]],
        penalty_time = values[[7]],
        retirement_control = NA_character_,
        retirement_type = NA_character_,
        retirement_reason = NA_character_
      )
    } else {
      retirement_text <- cells[[6]] |>
        html_text2() |>
        clean_txt()
      
      retirement_reason <- cells[[6]] |>
        html_element("i") |>
        html_text2() |>
        clean_txt()
      
      retirement_type <- if (is.na(retirement_reason) || retirement_reason == "") {
        retirement_text
      } else {
        retirement_text |>
          stringr::str_remove(stringr::fixed(retirement_reason)) |>
          clean_txt()
      }
      
      tibble(
        status = "retired",
        position = NA_character_,
        entry_number = values[[1]],
        driver_name = driver_name,
        car_model = car_model,
        nationality = values[[3]],
        homologation_group = values[[4]],
        elapsed_time = NA_character_,
        penalty_time = NA_character_,
        retirement_control = values[[5]],
        retirement_type = retirement_type,
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
      entry_number = parse_number(entry_number) |> as.integer(),
      status = factor(status, levels = c("classified", "retired"))
    )
}

# scrape -----------------------------------------------------------------
wrc_base_url <- "https://www.juwra.com"
wrc_rallies_url <- glue::glue("{wrc_base_url}/rallies.html")

res <- httr2::request(wrc_rallies_url) |>
  httr2::req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
  httr2::req_timeout(30) |>
  httr2::req_perform()

html_txt <- res |>
  httr2::resp_body_raw() |>
  rawToChar() |>
  iconv(from = "ISO-8859-1", to = "UTF-8")

page <- html_txt |>
  xml2::read_html()

rally_nodes <- page |>
  rvest::html_elements("table.OK_SOLUVARI_VALKEA a.OK_SIVUNAVI_ALLCAPS")

wrc_rallies <- tibble::tibble(
  rally_name = rally_nodes |> rvest::html_text2() |> clean_txt(),
  href = rally_nodes |> rvest::html_attr("href")
) |>
  mutate(
    rally_url = xml2::url_absolute(href, wrc_base_url),
    rally_slug = href |> basename() |> stringr::str_remove("\\.html$")
  ) |>
  filter(
    stringr::str_detect(href, "\\.html$"),
    !stringr::str_detect(rally_slug, "_\\d{4}")
  ) |>
  distinct(rally_slug, .keep_all = TRUE) |>
  select(
    rally_slug,
    rally_name,
    rally_url
  )

wrc_rallies

wrc_editions <- wrc_rallies |>
  select(rally_slug, rally_name, rally_url) |>
  purrr::pmap_dfr(scrape_wrc_editions, pause = 0.2)

wrc_editions

function(wrc_results_url = "https://www.juwra.com/monte_carlo_1981_results.html", pause = 0.01) {
  
  if (pause > 0) {
    Sys.sleep(pause)
  }

  year       <- stringr::str_extract(wrc_results_url, "\\d{4}") |> as.integer()
  rally_slug <- wrc_results_url |> basename() |> stringr::str_remove("_\\d{4}_results\\.html$")
  fout       <- str_glue("data/wrc/{rally_slug}/{year}.csv")
  
  fs::dir_create(dirname(fout))

  cli::cli_progress_step("{wrc_results_url} -> {fout}")

  if (file.exists(fout)) { return(invisible(TRUE))}

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

  final_results <- content_tables[[1]] |>
    parse_wrc_juwra_table(status = "classified")

  retirements <- content_tables[[2]] |>
    parse_wrc_juwra_table(status = "retired")

  wrc_results <- bind_rows(final_results, retirements) |>
    clean_wrc_juwra_table()

  write_csv(wrc_results, fout)
  
  invisible(TRUE)

}