library(tidyverse)

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRcqf4DB1woa-C1O9BE11MrGvItXgMajPozXTcNU_oCvj0B9cmfwXYv2xg2b9snWiw7UehW6bLnXTCT/pub?gid=891576802&single=true&output=csv"

data_collection <- read_csv(url)

glimpse(data_collection)

readr::write_csv(data_collection, "outputs/data_collection.csv")

rm(url, data_collection)