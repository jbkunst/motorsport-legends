# setup ------------------------------------------------------------------
library(tidyverse)

source(here::here("R/00-helpers.R"))
source(here::here("R/endurance/00-rsc-helpers.R"))

# params -----------------------------------------------------------------
endurance_csv <- here::here("outputs/csv/endurance_collection64.csv")
wrc_csv       <- here::here("outputs/csv/wrc_collection64.csv")

out_csv  <- here::here("outputs/csv/collection64.csv")
out_html <- here::here("outputs/html/collection64.html")

collection_sheet_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQxCLyOJ3I5tnjgwTlxiMVZagQ19DiZpoc_3xBOTdmnoo8gbai5MepFqCY2vAE27guGTAxjKlWti0SD/pub?gid=132013729&single=true&output=csv"

fs::dir_create(fs::path_dir(out_csv))
fs::dir_create(fs::path_dir(out_html))

# rebuild sources --------------------------------------------------------
rebuild_sources <- TRUE

if (rebuild_sources) {
  source(here::here("R/endurance/90-endurance-collection64.R"), local = new.env(parent = globalenv()))
  source(here::here("R/wrc/90-wrc-collection64.R"), local = new.env(parent = globalenv()))
}

# read -------------------------------------------------------------------
endurance_raw <- read_csv(endurance_csv)
wrc_raw       <- read_csv(wrc_csv)

endurance_raw |>
  glimpse()

wrc_raw |>
  glimpse()

# homologate --------------------------------------------------------------
collection64_endurance <- endurance_raw |>
  mutate(model_url = photo_url) |>
  transmute(
    source = "endurance",
    event_slug = track,
    year = as.integer(year),
    number,
    name = coalesce(name, car_title),
    make,
    model,
    result = as.integer(result),
    result_group = as.integer(result_group),
    result_status = case_when(
      result == 1 ~ "winner",
      TRUE ~ result_status
    ),
    homologation_group = group,
    entrant,
    driver = drivers,
    photo_url = map2_chr(thumb_url, contributor, make_wm_url),
    url = model_url,
    scale64_maker,
    scale64_status,
    note
  )

collection64_wrc <- wrc_raw |>
  transmute(
    source = "wrc",
    event_slug,
    year = as.integer(year),
    number = entry_number,
    name = coalesce(name, car_model),
    make,
    model = car_model,
    result = as.integer(position),
    result_group =as.integer(positiont_group),
    result_status = case_when(
      result == 1 ~ "winner",
      !is.na(result) ~ "finished",
      TRUE ~ "not_finished"
    ),
    homologation_group,
    entrant = NA_character_,
    driver = driver_name,
    photo_url = image_url,
    url = page_url,
    scale64_maker,
    scale64_status,
    note
  )

collection64 <- bind_rows(
  collection64_endurance,
  collection64_wrc
) |>
  mutate(
    event_slug = str_squish(event_slug),
    name = str_squish(name),
    make = na_if(str_squish(make), ""),
    model = na_if(str_squish(model), ""),
    homologation_group = na_if(str_squish(homologation_group), ""),
    entrant = na_if(str_squish(entrant), ""),
    driver = na_if(str_squish(driver), ""),
    photo_url = na_if(str_squish(photo_url), ""),
    url = na_if(str_squish(url), ""),
    note = na_if(str_squish(note), "")
  ) |>
  arrange(source, year, event_slug, result, number)

# checks -----------------------------------------------------------------
collection64 |>
  summarise(
    n_rows = n(),
    n_source = n_distinct(source),
    n_photo_url = sum(!is.na(photo_url)),
    pct_photo_url = round(100 * n_photo_url / n_rows, 1),
    n_note = sum(!is.na(note)),
    pct_note = round(100 * n_note / n_rows, 1)
  ) |>
  print(n = Inf)

collection64 |>
  count(source, sort = TRUE) |>
  print(n = Inf)

collection64_checks <- collection64 |>
  mutate(
    issue = case_when(
      is.na(event_slug) | event_slug == "" ~ "missing_event_slug",
      is.na(year) ~ "missing_year",
      is.na(number) | number == "" ~ "missing_number",
      is.na(name) | name == "" ~ "missing_name",
      is.na(make) | make == "" ~ "missing_make",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(issue)) |>
  select(issue, source, event_slug, year, number, name, make)

collection64_checks |>
  count(issue, source, sort = TRUE) |>
  print(n = Inf)

collection64_checks |>
  print(n = Inf)

# sheet validation --------------------------------------------------------
data_collection_sheet <- read_csv(
  collection_sheet_url,
  col_types = cols(.default = col_character())
) |>
  mutate(
    event_slug = str_squish(event_slug),
    year = as.character(as.integer(year)),
    number = str_squish(number),
    make = str_squish(make),
    name = str_squish(name)
  )

endurance_event_slugs <- fs::dir_ls(here::here("data/endurance"), type = "directory") |>
  basename()

wrc_event_slugs <- fs::dir_ls(here::here("data/wrc"), type = "directory") |>
  basename()

collection64_sheet_keys <- data_collection_sheet |>
  mutate(
    source = case_when(
      event_slug %in% endurance_event_slugs ~ "endurance",
      event_slug %in% wrc_event_slugs ~ "wrc",
      TRUE ~ "missing_local_event_data"
    ),
    check_hint = case_when(
      source == "missing_local_event_data" ~ "download_race_or_fix_event_slug",
      TRUE ~ "improve_sheet_key_or_source_data"
    )
  ) |>
  select(source, event_slug, year, number, make, name, scale64_maker, scale64_status, note, check_hint)

collection64_joined_keys <- collection64 |>
  transmute(
    source,
    event_slug = str_squish(event_slug),
    year = as.character(year),
    number = str_squish(as.character(number)),
    make = str_squish(make)
  )

collection64_sheet_checks <- collection64_sheet_keys |>
  anti_join(
    collection64_joined_keys,
    by = join_by(source, event_slug, year, number, make)
  ) |>
  arrange(source, event_slug, year, number, make)

cli::cli_h1("Sheet collection check")
cli::cli_alert_info("{nrow(collection64_sheet_checks)} sheet records did not cross with local collection data.")

collection64_sheet_checks |>
  count(source, check_hint, sort = TRUE) |>
  print(n = Inf)

collection64_sheet_checks |>
  print(n = Inf)

# datatable ---------------------------------------------------------------
collection64_dt_data <- collection64 |>
  mutate(
    info = if_else(
      is.na(note) | note == "",
      "",
      glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(note, attribute = TRUE)}\">{info_icon}</span>")
    ),
    name = if_else(
      is.na(url) | url == "",
      name,
      glue::glue("<a href='{url}' target='_blank'>{name}</a>")
    ),
    name = str_squish(paste(name, info)),
    photo = if_else(
      is.na(photo_url) | photo_url == "",
      NA_character_,
      glue::glue("<img src='{photo_url}' data-full='{photo_url}' height='80' class='lightbox-img' style='cursor:zoom-in;border-radius:4px;'/>")
    ),
    across(c(source, event_slug, result_status, scale64_maker, scale64_status), as.factor)
  ) |>
  transmute(
    source,
    event_slug,
    year = as.character(year),
    number,
    photo,
    name,
    result,
    result_group,
    result_status,
    homologation_group,
    entrant,
    driver,
    make,
    model,
    scale64_maker,
    scale64_status
  )

collection64_dt_data <- collection64_dt_data |> 
  arrange(year, event_slug)

collection64_dt <- collection64_dt_data |>
  glimpse() |>
  make_dt("collection64", search = "columns") |>
  style_result_status()

collection64_dt

# save -------------------------------------------------------------------
readr::write_csv(collection64, out_csv)

htmlwidgets::saveWidget(
  collection64_dt,
  file = out_html,
  libdir = "lib",
  selfcontained = FALSE,
  title = "Collection 1:64 Unified"
)
