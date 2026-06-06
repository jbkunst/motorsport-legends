# setup ------------------------------------------------------------------
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
le_mans_dates <- c(
  # 2020s
  "2025-06-15", "2024-06-16", "2023-06-11", "2022-06-12", "2021-08-22",
  "2020-09-20",
  
  # 2010s
  "2019-06-16", "2018-06-17", "2017-06-18", "2016-06-19", "2015-06-14",
  "2014-06-15", "2013-06-23", "2012-06-17", "2011-06-12", "2010-06-13",
  
  # 2000s
  "2009-06-14", "2008-06-15", "2007-06-17", "2006-06-18", "2005-06-19",
  "2004-06-13", "2003-06-15", "2002-06-16", "2001-06-17", "2000-06-18",
  
  # 1990s
  "1999-06-13", "1998-06-07", "1997-06-15", "1996-06-16", "1995-06-18",
  "1994-06-19", "1993-06-20", "1992-06-21", "1991-06-23", "1990-06-17",
  
  # 1980s
  "1989-06-11", "1988-06-12", "1987-06-14", "1986-06-01", "1985-06-16",
  "1984-06-17", "1983-06-19", "1982-06-20", "1981-06-14", "1980-06-15",
  
  # 1970s
  "1979-06-10", "1978-06-11", "1977-06-12", "1976-06-13", "1975-06-15",
  "1974-06-16", "1973-06-10", "1972-06-11", "1971-06-13", "1970-06-14",
  
  # 1960s
  "1969-06-15", "1968-09-29", "1967-06-11", "1966-06-19", "1965-06-20",
  "1964-06-21", "1963-06-16", "1962-06-24", "1961-06-11", "1960-06-26",
  
  # 1950s
  "1959-06-21", "1958-06-22", "1957-06-23", "1956-07-29", "1955-06-12",
  "1954-06-13", "1953-06-14", "1952-06-15", "1951-06-23", "1950-06-25"
)

le_mans_tbl <- tibble(
  date = ymd(le_mans_dates),
  year = as.integer(format(date, "%Y")),
  url = str_glue("https://www.racingsportscars.com/photo/Le_Mans-{date}.html?sort=Results")
)

scrape_race(le_mans_tbl, "data/lemans/results/", user_agent = "Joshua Kunst jbkunst@gmail.com")

datalm <- load_race_results("data/lemans/results/")

datalm

datalm |> 
  count(make, model, chassis, sort = TRUE) |> 
  filter(!is.na(chassis))

datalm |> 
  count(make, model, sort = TRUE) 

datalm |> 
  count(car_title, sort = TRUE) 

datalm |>
  filter(!is.na(chassis), chassis != "#") |>
  count(car_title, sort = TRUE)

datalm |>
  filter(chassis == "#GTE-003") |>
  select(year, make, model, chassis, entrant, drivers, result, result_status, colour, sponsor) |>
  arrange(year)

datalm |>
  filter(chassis == "#194378S410300") |>
  select(year, make, model, chassis, entrant, drivers, result, result_status, colour, sponsor) |>
  arrange(year)

datalm |>
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


# datatable --------------------------------------------------------------
glimpse(datalm)

# datos ------------------------------------------------------------------

# _full: todos los registros
datalm_full <- prep_dt_data(datalm)

# _min: todos los finished + el mejor DNF por grupo/año, sin "other"
# (los datos vienen ordenados por resultado, entonces slice_head captura el mejor DNF de cada clase)
datalm_min <- bind_rows(
  datalm |> filter(result_status == "finished"),
  datalm |>
    filter(result_status == "not_finished") |>
    slice_head(n = 1, by = c(year, group))
) |>
  arrange(desc(year), result) |>
  prep_dt_data()

nrow(datalm_full)
nrow(datalm_min)

# datatables -------------------------------------------------------------

dt_lm_full <- make_race_dt(datalm_full, "lemans-full")
dt_lm_min  <- make_race_dt(datalm_min,  "lemans-min")

dt_lm_full  # preview en viewer
dt_lm_min

# save -------------------------------------------------------------------
fs::dir_create("outputs/html")

htmlwidgets::saveWidget(
  dt_lm_full,
  file     = here::here("outputs/html/lemans_results_full.html"),
  libdir   = "lib",
  selfcontained = FALSE
)

htmlwidgets::saveWidget(
  dt_lm_min,
  file     = here::here("outputs/html/lemans_results_min.html"),
  libdir   = "lib",
  selfcontained = FALSE
)

cli::cli_alert_success("Guardado: outputs/html/lemans_results_full.html ({nrow(datalm_full)} filas)")
cli::cli_alert_success("Guardado: outputs/html/lemans_results_min.html  ({nrow(datalm_min)} filas)")
