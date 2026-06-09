source("R/00-helpers.R")

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRcqf4DB1woa-C1O9BE11MrGvItXgMajPozXTcNU_oCvj0B9cmfwXYv2xg2b9snWiw7UehW6bLnXTCT/pub?gid=891576802&single=true&output=csv"

data_collection <- read_csv(url)

data_collection <- data_collection |> 
  # avoid extra colun due incorrect pasting
  select(track, year, number, name, make, maker_164, status, note)

glimpse(data_collection)

data_collection_join <- fs::dir_ls("data") |>  
  map_df(function(folder = "data/le_mans"){

    cli::cli_inform(folder)

    rr <- load_race_results(str_c(folder, "/results")) |> 
      mutate(track = basename(folder), year = year(date), .after = 1)
    
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

dt <- prep_dt_data(data_collection_join)

dt <- bind_cols(
  dt,
  data_collection_join |> 
   select(maker_164, status)
) |> 
  relocate(track, .before = 1) |> 
  select(-date)

dt <- make_race_dt(dt, "collection")

dt

htmlwidgets::saveWidget(
  dt,
  file     = here::here("outputs/html/collection164.html"),
  libdir   = "lib",
  selfcontained = FALSE
)
