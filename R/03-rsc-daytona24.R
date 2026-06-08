# setup ------------------------------------------------------------------
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
  ) |>
  mutate(
    url = url_absolute(href, daytona_archive_url)
  )

daytona24_tbl <- links |>
  filter(text == "Daytona 24 Hours") |>
  mutate(
    date_txt = str_extract(url, "\\d{4}-\\d{2}-\\d{2}"),
    date = ymd(date_txt),
    year = year(date),
    result_url = str_replace(url, "/race/", "/results/"),
    photo_url = str_replace(url, "/race/", "/photo/")
  ) |>
  distinct(year, date, .keep_all = TRUE) |>
  arrange(desc(year)) |>
  transmute(
    date,
    year,
    url = str_glue("{photo_url}?sort=Results")
  )

# daytona24_tbl <- tibble(
#   date = ymd(daytona24_dates),
#   year = as.integer(format(date, "%Y")),
#   url = str_glue("https://www.racingsportscars.com/photo/Daytona-{date}.html?sort=Results")
# )

scrape_race(daytona24_tbl, "data/daytona24/results/", user_agent = "Joshua Kunst jbkunst@gmail.com")

datadaytona24 <- load_race_results("data/daytona24/results/")

datadaytona24

glimpse(datadaytona24)

# datos ------------------------------------------------------------------
datadaytona24_full <- prep_dt_data(datadaytona24)

datadaytona24_min <- bind_rows(
  datadaytona24 |> filter(result_status == "finished"),
  datadaytona24 |>
    filter(result_status == "not_finished") |>
    slice_head(n = 1, by = c(year, group))
) |>
  arrange(desc(year), result) |>
  prep_dt_data()

nrow(datadaytona24_full)
nrow(datadaytona24_min)

# datatables -------------------------------------------------------------
dt_daytona24_full <- make_race_dt(datadaytona24_full, "daytona24-full")
dt_daytona24_min  <- make_race_dt(datadaytona24_min,  "daytona24-min")

dt_daytona24_full
dt_daytona24_min

# save -------------------------------------------------------------------
fs::dir_create("outputs/html")

htmlwidgets::saveWidget(
  dt_daytona24_full,
  file = here::here("outputs/html/daytona24_results_full.html"),
  libdir = "lib",
  selfcontained = FALSE
)

htmlwidgets::saveWidget(
  dt_daytona24_min,
  file = here::here("outputs/html/daytona24_results_min.html"),
  libdir = "lib",
  selfcontained = FALSE
)

cli::cli_alert_success("Guardado: outputs/html/daytona24_results_full.html ({nrow(datadaytona24_full)} filas)")
cli::cli_alert_success("Guardado: outputs/html/daytona24_results_min.html  ({nrow(datadaytona24_min)} filas)")