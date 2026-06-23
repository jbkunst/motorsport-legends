# setup ------------------------------------------------------------------
source(here::here("R/endurance/00-rsc-helpers.R"))
source(here::here("R/00-helpers.R"))

# data collection and join with results ----------------------------------
data_collection <- google_sheet_collection_data()

data_collection <- data_collection |> 
  # avoid extra colun due incorrect pasting
  # select(track, year, number, name, make, maker_164, status, note)
  select(everything())

glimpse(data_collection)

data_collection_join <- fs::dir_ls("data/endurance/") |>
  map_df(function(folder = "data/endurance/brands_hatch") {
    cli::cli_inform(folder)

    rr <- load_race_results(str_c(folder, "")) |>
      mutate(track = basename(folder), year = year(date), .after = 1)

    rr <- rr |>
      arrange(date, group, result) |>
      group_by(date, group) |>
      mutate(result_group = if_else(is.na(result), NA_integer_, cumsum(!is.na(result)))) |>
      ungroup()

    dc <- filter(data_collection, event_slug == basename(folder))

    # inner: conserva solo cruces válidos; el anti_join posterior falla si falta algo de la colección.
    dout <- inner_join(rr, dc, by = join_by(track == event_slug, year, number, make))
    dout <- arrange(dout, year, result, number)

    dout |>
      count(track, year, number, sort = TRUE) |>
      filter(n > 1) |>
      nrow() |>
      {\(x) stopifnot("Hay duplicados por race/year/number" = x == 0)}()

    dout
  })

# ordenar por año, carrea y resultado
data_collection_join <- arrange(data_collection_join, year, track, result)

# check ------------------------------------------------------------------
# anti_join para hacer check de que NO se cruzó o que falta descargar
checks <- anti_join(data_collection, data_collection_join, by = join_by(event_slug == track, year, number, make))
checks

# datatable html output --------------------------------------------------
dt <- prep_rsc_dt_data(data_collection_join)

# prep_rsc_dt_data remove colums
dt <- dt |> 
  bind_cols(data_collection_join |> select(scale64_maker, scale64_status, result_group, note))

dt <- dt |> 
  mutate(
    note = dplyr::if_else(is.na(note) | note == "", "", glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(note, attribute = TRUE)}\">{info_icon}</span>")),
    car_title = stringr::str_squish(paste(car_title , note)),
  ) |>    
  mutate(across(c(track, result_status, scale64_maker, scale64_status), as.factor)) |> 
  relocate(track, .before = 1) |> 
  relocate(result_group, .after = result) |> 
  select(-date, -note, -grid) |>
  glimpse() |> 
  make_dt("collection", search = "columns") |>
  style_result_status()

dt

# save -------------------------------------------------------------------
readr::write_csv(data_collection_join, file = here::here("outputs/csv/endurance_collection64.csv"))

htmlwidgets::saveWidget(
  dt,
  file     = here::here("outputs/html/endurance_collection64.html"),
  libdir   = "lib",
  selfcontained = FALSE,
  title = "RSC Collection 1:64 - Results and Status"
)
