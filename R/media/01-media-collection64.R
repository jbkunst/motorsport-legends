# setup ------------------------------------------------------------------
library(tidyverse)

source(here::here("R/media/00-media-helpers.R"))
source(here::here("R/00-helpers.R"))

# data collection and join with results ----------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQxCLyOJ3I5tnjgwTlxiMVZagQ19DiZpoc_3xBOTdmnoo8gbai5MepFqCY2vAE27guGTAxjKlWti0SD/pub?gid=944356990&single=true&output=csv"

data_collection <- read_csv(url)

media_scraped <- data_collection |> 
  dplyr::mutate(source = stringr::str_extract(media_source, "imcdb|igcd")) |>
  dplyr::filter(!is.na(source), !is.na(media_source), media_source != "") |>
  dplyr::distinct(source, media_source) |>
  dplyr::mutate(data = purrr::map2(source, media_source, scrape_media_vehicle)) |>
  tidyr::unnest(data) |>
  dplyr::select(-source, -media_source)

media_scraped |>
  dplyr::glimpse()

media_scraped <- media_scraped |> 
  left_join(
    data_collection |> 
      select(collector_note, scale64_maker, scale64_status, media_source, media_link),
    by = join_by(source_vehicle_url == media_source)
  ) |> 
  arrange(media_year)

media_scraped_min <- media_scraped |>
  dplyr::mutate(
    photo_url = dplyr::coalesce(dplyr::na_if(image_data_uri, ""), dplyr::na_if(image_url, "")),
    note_text = dplyr::case_when(!is.na(collector_note) & collector_note != "" & !is.na(description) & description != "" ~ paste(collector_note, description, sep = " | "), !is.na(collector_note) & collector_note != "" ~ collector_note, !is.na(description) & description != "" ~ description, TRUE ~ ""),
    note = dplyr::if_else(note_text == "", "", glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(note_text, attribute = TRUE)}\">{info_icon}</span>")),
    car_txt = htmltools::htmlEscape(model),
    media_txt = htmltools::htmlEscape(media_title),
    car = dplyr::if_else(is.na(source_vehicle_url) | source_vehicle_url == "", car_txt, glue::glue("<a href='{source_vehicle_url}' target='_blank'>{car_txt}</a>")),
    car = stringr::str_squish(paste(car, note)),
    media = dplyr::if_else(is.na(source_media_url) | source_media_url == "", media_txt, glue::glue("<a href='{source_media_url}' target='_blank'>{media_txt}</a>")),
    ref = dplyr::if_else(is.na(media_link) | media_link == "", "", glue::glue("<a href='{media_link}' target='_blank'>ref</a>")),
    photo = dplyr::if_else(is.na(photo_url) | photo_url == "", "", glue::glue("<img src='{photo_url}' />")),
    dplyr::across(c(media_type, scale64_maker, scale64_status), as.factor)
  ) |>
  dplyr::transmute(
    car,
    year,
    photo,
    media,
    media_year,
    type = media_type,
    maker = scale64_maker,
    status = scale64_status,
    ref
  ) |>
  make_dt("media-collection", search = "columns", photo_height = 95)

media_scraped_min
