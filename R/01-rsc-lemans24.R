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
) |>
  mutate(url = url_absolute(href, le_mans_archive_url))

lemans24_tbl <- dlinks |>
  filter(
    str_detect(href, "/photo/Le_Mans-\\d{4}-\\d{2}-\\d{2}\\.html$")
  ) |>
  mutate(
    date = ymd(str_extract(href, "\\d{4}-\\d{2}-\\d{2}")),
    year = lubridate::year(date),
    url = str_glue("{url}?sort=Results")
  ) |>
  filter(
    year >= 1950,
    text == "Le Mans 24 Hours"
  ) |>
  distinct(year, date, .keep_all = TRUE) |>
  arrange(desc(year)) |>
  select(date, year, url)

lemans24_tbl |>
  count(year) |>
  filter(n > 1)

scrape_race(lemans24_tbl, "data/lemans24/results/", user_agent = "Joshua Kunst jbkunst@gmail.com")

datalm <- load_race_results("data/lemans24/results/")

datalm

glimpse(datalm)

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
