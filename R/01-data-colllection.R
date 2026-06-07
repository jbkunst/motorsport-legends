source("R/00-helpers.R")

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRcqf4DB1woa-C1O9BE11MrGvItXgMajPozXTcNU_oCvj0B9cmfwXYv2xg2b9snWiw7UehW6bLnXTCT/pub?gid=891576802&single=true&output=csv"

data_collection <- read_csv(url)

glimpse(data_collection)

data_collection_join <- fs::dir_ls("data") |> 
  map_df(function(folder = "data/nurburgring24"){

    cli::cli_inform(folder)

    load_race_results(str_c(folder, "/results")) |> 
      mutate(race = basename(folder), .after = 1) |> 
      inner_join(data_collection, by = join_by(race, year, number)) |> 
      order_by(year, result)

  })

dt <- data_collection_join |> 
  prep_dt_data() |> 
  make_race_dt("collection")

dt

# readr::write_csv(data_collection, "outputs/data_collection.csv")

# rm(url, data_collection)