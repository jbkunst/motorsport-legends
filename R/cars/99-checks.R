library(tidyverse)

files <- fs::dir_ls("data/cars")

files |> 
  str_subset("specs|long", negate = TRUE) |>
  map(read_csv) |> 
  map(glimpse) |> 
  walk(slice_sample, n = 10) 


