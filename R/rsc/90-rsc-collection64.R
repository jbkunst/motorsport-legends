# setup ------------------------------------------------------------------
source(here::here("R/rsc/00-rsc-helpers.R"))
source(here::here("R/00-helpers.R"))

# data collection and join with results ----------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQxCLyOJ3I5tnjgwTlxiMVZagQ19DiZpoc_3xBOTdmnoo8gbai5MepFqCY2vAE27guGTAxjKlWti0SD/pub?gid=132013729&single=true&output=csv"

data_collection <- read_csv(url)

data_collection <- data_collection |> 
  # avoid extra colun due incorrect pasting
  # select(track, year, number, name, make, maker_164, status, note)
  select(everything())

glimpse(data_collection)

data_collection_join <- fs::dir_ls("data/races/") |>
  map_df(function(folder = "data/races/nurburgring") {
    cli::cli_inform(folder)

    rr <- load_race_results(str_c(folder, "/results")) |>
      mutate(track = basename(folder), year = year(date), .after = 1)

    rr <- rr |>
      arrange(date, group, result) |>
      group_by(date, group) |>
      mutate(result_group = if_else(is.na(result), NA_integer_, cumsum(!is.na(result)))) |>
      ungroup()

    dc <- filter(data_collection, track == basename(folder))

    # right para que queden registros de la colleccion, pero primero columnas de results descargados
    dout <- inner_join(rr, dc, by = join_by(track, year, number, make))
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

# anti_join para hacer check de que NO se cruzó o que falta descargar
anti_join(data_collection, data_collection_join, by = join_by(track, year, number, make))

# datatable html output --------------------------------------------------
dt <- prep_rsc_dt_data(data_collection_join)

dt <- bind_cols(dt, data_collection_join |> select(scale64_maker, scale64_status, result_group)) |> 
  relocate(track, .before = 1) |> 
  relocate(result_group, .after = result) |> 
  select(-date) |>
  glimpse() |> 
  make_dt("collection", center_all = TRUE) |>
  style_result_status()

dt

htmlwidgets::saveWidget(
  dt,
  file     = here::here("outputs/html/collection164.html"),
  libdir   = "lib",
  selfcontained = FALSE
)
