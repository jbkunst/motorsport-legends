# setup ------------------------------------------------------------------
library(tidyverse)

source(here::here("R/00-helpers.R"))
source(here::here("R/endurance/00-rsc-helpers.R"))
source(here::here("R/wrc/00-juwra-helpers.R"))

# data collection and join with results ----------------------------------
data_collection <- google_sheet_collection_data()

data_collection <- data_collection |>
  select(everything())

data_collection_join <- fs::dir_ls("data/wrc/") |>
  map_df(function(folder = "data/wrc/usa_") {
    cli::cli_inform(folder)

    rr <- fs::dir_ls(folder) |>
      map_df(function(file = "data/wrc/monte_carlo/1993.csv"){
        read_csv(
          file,
          show_col_types = FALSE,
          col_types = cols(
            # .default = col_character(),
            # entry_number = col_integer(),
            # position = col_integer() 
            penalty_time = col_time(format = "")
          )
        ) |>
          mutate(
            event_slug = basename(dirname(file)),
            year = fs::path_ext_remove(basename(file)) |> as.integer(),
            .before = 1
          )
      })
     
    rr <- rr |>
      arrange(year, homologation_group, position) |>
      group_by(year, homologation_group) |>
      mutate(position_group = if_else(is.na(position), NA_integer_, cumsum(!is.na(position)))) |>
      ungroup() |> 
      arrange(year, position)

    dc <- filter(data_collection, event_slug == basename(folder))

    # inner: conserva solo cruces válidos; el anti_join posterior falla si falta algo de la colección.
    # en este join confiamos en que solo es necesario el numero y no el make
    dout <- inner_join(rr, dc, by = join_by(event_slug, year, entry_number == number))
    dout <- arrange(dout, year, position, entry_number)

    dout |>
      count(event_slug, year, entry_number, sort = TRUE) |>
      filter(n > 1) |>
      nrow() |>
      {\(x) stopifnot("Hay duplicados por race/year/entry_number" = x == 0)}()

    dout
  })

data_collection_join |> glimpse()

# serper image search -----------------------------------------------------
fs::dir_create("outputs/csv/wrc")

serper_cache_file <- "outputs/csv/wrc/serper_wrc_images.csv"

serper_wrc_images <- data_collection_join |>
  mutate(
    image_query = str_glue(
      "{driver_name |> str_remove(',.*$')} {car_model} {event_slug |> str_replace_all('_', ' ')} {year}"
    )
  ) |>
  distinct(event_slug, year, entry_number, image_query) |>
  mutate(image_result = map(image_query, \(query) search_serper_real_image(query, cache_file = serper_cache_file))) |>
  unnest(image_result)

serper_wrc_images |> select(image_url, image_query) |> print(n = Inf)

serper_wrc_images_best <- serper_wrc_images |>
  group_by(event_slug, year, entry_number) |>
  arrange(desc(image_score), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(event_slug, year, entry_number, query, image_url, page_url, title, source, image_score)

# datatable html output ---------------------------------------------------
data_collection_join <- data_collection_join |>
  left_join(
    serper_wrc_images_best,
    by = join_by(event_slug, year, entry_number)
  )

dt_wrc_data <- data_collection_join |>
  transmute(
    track = event_slug,
    year = as.character(year),
    number = as.character(entry_number),
    photo = if_else(
      !is.na(image_url) & image_url != "",
      str_glue("<img src='{image_url}' data-full='{image_url}' height='80' class='lightbox-img' style='cursor:zoom-in;border-radius:4px;'/>"),
      NA_character_
    ),
    car_title = coalesce(name, car_model),
    car_title = if_else(
      !is.na(page_url) & page_url != "",
      str_glue('<a href="{page_url}" target="_blank">{car_title}</a>'),
      car_title
    ),
    result = position,
    result_group = position_group,
    result_status = case_when(
      position == 1 ~ "winner",
      !is.na(position) ~ "finished",
      TRUE ~ "not_finished"
    ),
    group = homologation_group,
    entrant = driver_name,
    make,
    model = car_model,
    chassis = NA_character_,
    scale64_maker,
    scale64_status,
    note
  ) |>
  mutate(
    # source = "wrc",
    note = dplyr::if_else(
      is.na(note) | note == "",
      "",
      glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(note, attribute = TRUE)}\">{info_icon}</span>")
    ),
    car_title = stringr::str_squish(paste(car_title, note))
  ) |>
  mutate(across(c(track, result_status, scale64_maker, scale64_status), as.factor)) |>
  # relocate(source, .before = 1) |>
  # relocate(track, .after = 1) |>
  relocate(result_group, .after = result) |>
  select(-note) |>
  arrange(year, track, result, number)

dt <- dt_wrc_data |>
  glimpse() |>
  make_dt("wrc_collection", search = "columns") |>
  style_result_status()

dt

# save -------------------------------------------------------------------
readr::write_csv(data_collection_join, file = here::here("outputs/csv/wrc_collection64.csv"))

htmlwidgets::saveWidget(
  dt,
  file = here::here("outputs/html/wrc_collection64.html"),
  libdir = "lib",
  selfcontained = FALSE,
  title = "WRC Collection 1:64 - Results and Status"
)
