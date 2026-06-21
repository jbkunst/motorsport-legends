# setup ------------------------------------------------------------------
source(here::here("R/rsc/00-rsc-helpers.R"))
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
nurburgring24_dates <- c(
  # 2020s
  "2025-06-22", "2024-06-02", "2023-05-21", "2022-05-29", "2021-06-06",
  "2020-09-27",
  
  # 2010s
  "2019-06-23", "2018-05-13", "2017-05-28", "2016-05-29", "2015-05-17",
  "2014-06-22", "2013-05-20", "2012-05-20", "2011-06-26", "2010-05-16",
  
  # 2000s
  "2009-05-24", "2008-05-25", "2007-06-10", "2006-06-18", "2005-05-08",
  "2004-06-13", "2003-06-01", "2002-06-02", "2001-05-27", "2000-06-25",
  
  # 1990s
  "1999-06-06", "1998-06-14", "1997-06-08", "1996-06-16", "1995-06-18",
  "1994-06-05", "1993-06-13", "1992-06-21", "1991-06-16", "1990-06-17",
  
  # 1980s
  "1989-06-18", "1988-06-19", "1987-06-21", "1986-06-22", "1985-06-23",
  "1984-08-26", "1982-10-03", "1981-10-04", "1980-10-05",
  
  # 1970s
  "1979-10-07", "1978-10-08", "1977-10-09", "1976-09-26",
  "1973-06-24", "1972-06-25", "1971-06-27", "1970-06-28"
)

nurburgring24_tbl <- tibble(
  date = ymd(nurburgring24_dates),
  # year = as.integer(format(date, "%Y")),
  url = str_glue("https://www.racingsportscars.com/photo/Nurburgring-{date}.html?sort=Results")
)

nurburgring24_tbl |> 
  pull(url) |> 
  walk(scrape_race)

datanbr24 <- load_race_results("data/races/nurburgring/results/")

# Hay results que no son 24h, por eso filtramos por fechas
datanbr24 |> filter_out(date %in% nurburgring24_tbl$date)
datanbr24 |> filter_out(date %in% nurburgring24_tbl$date) |> count(date)

datanbr24 <- datanbr24 |> 
  filter(date %in% nurburgring24_tbl$date)

glimpse(datanbr24)


# extras -----------------------------------------------------------------
datanbr24 |> 
  count(make, model, chassis, sort = TRUE) |> 
  filter(!is.na(chassis))

datanbr24 |> 
  count(make, model, sort = TRUE) 

datanbr24 |> 
  count(car_title, sort = TRUE) 

datanbr24 |>
  filter(!is.na(chassis), chassis != "#") |>
  count(car_title, sort = TRUE)

datanbr24 |>
  mutate(year = year(date)) |> 
  filter(!is.na(chassis), chassis != "#") |>
  group_by(make, model, chassis) |>
  summarise(
    n = n(),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    years = str_c(sort(unique(year)), collapse = ", "),
    entrants = str_c(sort(unique(entrant)), collapse = " | "),
    best_result = if_else(all(is.na(result)), NA_integer_, min(result, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(desc(n), first_year)


# prep_rsc_dt_data -------------------------------------------------------
# _full: todos los registros
datanbr24_full <- prep_rsc_dt_data(datanbr24)

# _min: todos los finished + el mejor DNF por grupo/año, sin "other"
# (los datos vienen ordenados por resultado, entonces slice_head captura el mejor DNF de cada clase)
datanbr24_min <- bind_rows(
  datanbr24 |> filter(result_status == "finished"),
  datanbr24 |>
    filter(result_status == "not_finished") |>
    slice_head(n = 1, by = c(date, group))
) |>
  arrange(desc(date), result) |>
  prep_rsc_dt_data()

nrow(datanbr24_full)
nrow(datanbr24_min)

# datatables -------------------------------------------------------------
dt_n24_full <- datanbr24_full |>
  select(-track, -date) |> 
  make_dt(element_id = "nurburgring24-full", search = "columns") |>
  style_result_status()

dt_n24_min  <- datanbr24_min |> 
  select(-track, -date) |> 
  make_dt(element_id = "nurburgring24-min", search = "columns") |>
  style_result_status()

dt_n24_full  # preview en viewer
dt_n24_min

# save -------------------------------------------------------------------
fs::dir_create("outputs/html")

htmlwidgets::saveWidget(
  dt_n24_full,
  file     = here::here("outputs/html/nurburgring24_results_full.html"),
  libdir   = "lib",
  selfcontained = FALSE,
  title = "Nurburgring 24 Hours - All Results"
)

htmlwidgets::saveWidget(
  dt_n24_min,
  file     = here::here("outputs/html/nurburgring24_results_min.html"),
  libdir   = "lib",
  selfcontained = FALSE,
  title = "Nurburgring 24 Hours - Finished + Best DNF Results"
)

cli::cli_alert_success("Guardado: outputs/html/nurburgring24_results_full.html ({nrow(datanbr24_full)} filas)")
cli::cli_alert_success("Guardado: outputs/html/nurburgring24_results_min.html  ({nrow(datanbr24_min)} filas)")
