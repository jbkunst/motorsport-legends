# setup ------------------------------------------------------------------
source(here::here("R/rsc/00-rsc-helpers.R"))
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
daytona_archive_url <- "https://www.racingsportscars.com/track/archive/daytona.html"

page <- request(daytona_archive_url) |>
  req_user_agent("Joshua Kunst jbkunst@gmail.com") |>
  req_perform() |>
  resp_body_html()

links <- page |>
  html_elements("a") 

links <- tibble(
    text = html_text2(links),
    href = html_attr(links, "href")
  )

daytona24_tbl <- links |>
  filter(text == "Daytona 24 Hours") |>
  mutate(
    url = url_absolute(href, daytona_archive_url),
    date = ymd(str_extract(url, "\\d{4}-\\d{2}-\\d{2}")),
    photo_url = str_replace(url, "/race/", "/photo/"),
    url = str_glue("{photo_url}?sort=Results")
  ) |>
  distinct(date, .keep_all = TRUE) |>
  arrange(desc(date))

daytona24_tbl |> 
  pull(url) |> 
  walk(scrape_race)

datadaytona24 <- load_race_results("data/races/daytona/results/")

datadaytona24

glimpse(datadaytona24)

# datos ------------------------------------------------------------------
datadaytona24_full <- prep_rsc_dt_data(datadaytona24)

datadaytona24_min <- bind_rows(
  datadaytona24 |> filter(result_status == "finished"),
  datadaytona24 |>
    filter(result_status == "not_finished") |>
    slice_head(n = 1, by = c(date, group))
) |>
  arrange(desc(date), result) |>
  prep_rsc_dt_data()

nrow(datadaytona24_full)
nrow(datadaytona24_min)

# datatables -------------------------------------------------------------
dt_daytona24_full <- datadaytona24_full |> 
  select(-track, -date) |> 
  make_dt(element_id = "daytona24-full", search = "columns") |>
  style_result_status()

dt_daytona24_min  <- datadaytona24_min |>
  select(-track, -date) |> 
  make_dt(element_id = "daytona24-min", search = "columns") |>
  style_result_status()

dt_daytona24_full
dt_daytona24_min

# save -------------------------------------------------------------------
fs::dir_create("outputs/html")

htmlwidgets::saveWidget(
  dt_daytona24_full,
  file = here::here("outputs/html/daytona24_results_full.html"),
  libdir = "lib",
  selfcontained = FALSE,
  title = "Daytona 24 Hours - All Results"
)

htmlwidgets::saveWidget(
  dt_daytona24_min,
  file = here::here("outputs/html/daytona24_results_min.html"),
  libdir = "lib",
  selfcontained = FALSE,
  title = 
)

cli::cli_alert_success("Guardado: outputs/html/daytona24_results_full.html ({nrow(datadaytona24_full)} filas)")
cli::cli_alert_success("Guardado: outputs/html/daytona24_results_min.html  ({nrow(datadaytona24_min)} filas)")