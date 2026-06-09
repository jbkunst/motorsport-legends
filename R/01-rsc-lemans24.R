# setup ------------------------------------------------------------------
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
le_mans_archive_url <- "https://www.racingsportscars.com/photo_lemans.html"

page <- request(le_mans_archive_url) |>
  req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
  req_perform() |>
  resp_body_html()

dlinks <- page |>
  html_elements("a")

dlinks <- tibble(
  text = html_text2(dlinks),
  href = html_attr(dlinks, "href")
)
  
lemans24_tbl <- dlinks |>
  mutate(url = url_absolute(href, le_mans_archive_url)) |> 
  filter(str_detect(href, "/photo/Le_Mans-\\d{4}-\\d{2}-\\d{2}\\.html$")) |>
  filter(text == "Le Mans 24 Hours") |> 
  mutate(date = ymd(str_extract(href, "\\d{4}-\\d{2}-\\d{2}")), url = str_glue("{url}?sort=Results")) |>
  filter(year(date) >= 1950) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(desc(date)) 

lemans24_tbl |>
  count(year(date)) |>
  filter(n > 1)

lemans24_tbl |> 
  pull(url) |> 
  walk(scrape_race)

# scrape_race(lemans24_tbl, "data/lemans24/results/", user_agent = "Joshua Kunst jbkunst@gmail.com")

datalm <- load_race_results("data/le_mans/results/")

datalm

glimpse(datalm)

# extras -----------------------------------------------------------------
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
  select(date, make, model, chassis, entrant, drivers, result, result_status, colour, sponsor) |>
  arrange(date)

datalm |>
  filter(chassis == "#194378S410300") |>
  select(date, make, model, chassis, entrant, drivers, result, result_status, colour, sponsor) |>
  arrange(date)

datalm |>
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


# prep_dt_data -----------------------------------------------------------
# _full: todos los registros
datalm_full <- prep_dt_data(datalm)

# _min: todos los finished + el mejor DNF por grupo/año, sin "other"
# (los datos vienen ordenados por resultado, entonces slice_head captura el mejor DNF de cada clase)
datalm_min <- bind_rows(
  datalm |> filter(result_status == "finished"),
  datalm |>
    filter(result_status == "not_finished") |>
    slice_head(n = 1, by = c(date, group))
) |>
  arrange(desc(year(date)), result) |>
  prep_dt_data()

nrow(datalm_full)
nrow(datalm_min)

# datatables -------------------------------------------------------------
dt_lm_full <- datalm_full |> 
  select(-track, -date) |> 
  make_race_dt("lemans-full")

dt_lm_min  <- datalm_min |> 
  select(-track, -date) |> 
  make_race_dt("lemans-min")

dt_lm_full  # preview en viewer
dt_lm_min

# save -------------------------------------------------------------------
fs::dir_create("outputs/html")

htmlwidgets::saveWidget(
  dt_lm_full,
  file     = here::here("outputs/html/lemans24_results_full.html"),
  libdir   = "lib",
  selfcontained = FALSE
)

htmlwidgets::saveWidget(
  dt_lm_min,
  file     = here::here("outputs/html/lemans24_results_min.html"),
  libdir   = "lib",
  selfcontained = FALSE
)

cli::cli_alert_success("Guardado: outputs/html/lemans24_results_full.html ({nrow(datalm_full)} filas)")
cli::cli_alert_success("Guardado: outputs/html/lemans24_results_min.html  ({nrow(datalm_min)} filas)")
