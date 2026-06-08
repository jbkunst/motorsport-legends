source("R/00-helpers.R")

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRcqf4DB1woa-C1O9BE11MrGvItXgMajPozXTcNU_oCvj0B9cmfwXYv2xg2b9snWiw7UehW6bLnXTCT/pub?gid=891576802&single=true&output=csv"

data_collection <- read_csv(url)

data_collection

glimpse(data_collection)

data_collection_join <- fs::dir_ls("data") |> 
  map_df(function(folder = "data/lemans24"){

    cli::cli_inform(folder)

    rr <- load_race_results(str_c(folder, "/results")) |> 
      mutate(race = basename(folder), .after = 1)
    
    dc <- data_collection |> filter(race == basename(folder))
    
    # right para que queden registros de la colleccion, pero primero columnas de results descargados
    dout <- right_join(rr, dc, by = join_by(race, year, number, make))
    dout <- dout |> arrange(year, result, number)

    dout |>
      count(race, year, number, sort = TRUE) |>
      filter(n > 1) |>
      nrow() |>
      {\(x) stopifnot("Hay duplicados por race/year/number" = x == 0)}()

    dout

  })

dt <- prep_dt_data(data_collection_join)

dt <- bind_cols(
  dt,
  data_collection_join |> 
   select(race, maker_164, status)
) |> 
  relocate(race, .before = 1)

dt <- make_race_dt(dt, "collection")

dt

htmlwidgets::saveWidget(
  dt,
  file     = here::here("outputs/html/collection164.html"),
  libdir   = "lib",
  selfcontained = FALSE
)
