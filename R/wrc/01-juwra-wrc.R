# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)

source("R/00-helpers.R")
source("R/wrc/00-juwra-helpers.R")

# parameters -------------------------------------------------------------
wrc_rallies_url <- "https://www.juwra.com/rallies.html"

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
  pmap_dfr(scrape_juwra_editions, pause = 0.2)

wrc_editions |>
  select(rally_slug, year, wrc_results_url) |>
  pwalk(scrape_wrc_juwra_results)


