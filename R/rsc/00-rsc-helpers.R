# packages ---------------------------------------------------------------
library(tidyverse)
library(rvest)
library(httr2)
library(polite)
library(here)

# helpers ----------------------------------------------------------------
html_text_or_na <- function(x, selector) {
  out <- x |>
    html_element(selector) |>
    html_text2()
  
  if (length(out) == 0 || is.na(out)) NA_character_ else out
}

html_attr_or_na <- function(x, selector, attr) {
  out <- x |>
    html_element(selector) |>
    html_attr(attr)
  
  if (length(out) == 0 || is.na(out)) NA_character_ else out
}

first_or_na <- function(x) {
  if (length(x) == 0) NA_character_ else x[[1]]
}

add_missing_cols <- function(data, cols) {
  for (col in cols) {
    if (!col %in% names(data)) data[[col]] <- NA_character_
  }
  data
}

remove_na_cols <- function(data) {
  data |>
    select(-any_of(c("NA", "NA_url")))
}

parse_carview <- function(x) {

  site_url <- "https://www.racingsportscars.com/"

  if(str_detect(html_text_or_na(x, ".result"), "^Result: did not arrive|^Result: did not qualify|^Result: did not start"))  return(tibble())

  car_links <- x |>
    html_elements(".car.header a")

  if(length(car_links) <= 1)  return(tibble())
  
  car_links <- tibble(
    text = html_text2(car_links),
    href = html_attr(car_links, "href") |> url_absolute(base = site_url),
    type = str_extract(href, "(?<=racingsportscars\\.com/)[^/]+(?=/photo/)")
  )
  
  txt <- car_links |> pull(text) |> str_c(collapse = " ")
  cli::cli_progress_step(txt)
  
  car_links <- car_links |>
    summarise(text = str_c(text, collapse = " "), href = str_c(href, collapse = " | "), .by = type) |>
    pivot_wider(names_from = type, values_from = c(text, href), names_glue = "{type}_{.value}") |>
    rename_with(~ str_remove(.x, "_text$"), ends_with("_text")) |>
    rename_with(~ str_replace(.x, "_href$", "_url"), ends_with("_href"))
  
  output <- tibble(
    number      = html_text_or_na(x, ".number") |> as.integer(),
    car_title   = html_text_or_na(x, ".car.header"),
    group       = html_text_or_na(x, ".group.header"),
    entrant     = html_text_or_na(x, ".entrant.subheader"),
    bodywork    = html_text_or_na(x, ".bodywork.subheader"),
    engine      = html_text_or_na(x, ".engine.subheader"),
    drivers     = html_text_or_na(x, ".drivers"),
    result      = html_text_or_na(x, ".result"),
    sponsor     = html_text_or_na(x, ".sponsor"),
    colour      = html_text_or_na(x, ".colour"),
    tyre        = html_text_or_na(x, ".tyre"),
    updated     = html_text_or_na(x, ".updated"),
    contributor = html_text_or_na(x, ".contributor"),
    
    thumb_url   = html_attr_or_na(x, ".photo img", "src") |> url_absolute(base = site_url),
    photo_url   = coalesce(
      html_attr_or_na(x, ".photo input[type='hidden']", "value"),
      html_attr_or_na(x, ".photo a", "href") # cuando no tiene fotos que iterar no existe input, por lo que sacamos el link de la unica foto
    ) |> url_absolute(base = site_url) 
  ) |>
    bind_cols(car_links) |>
    add_missing_cols(c("type", "type_url")) |>
    rename(model = type, model_url = type_url)

  output
}

# limpia y reordena el raw scrape de una carrera RSC
clean_cars <- function(cars) {
  cars |>
    mutate(
      result_raw = result,
      result_txt = str_remove(result_raw, "^Result:\\s*"),

      result_status = case_when(
        str_detect(result_txt, "^(winner|\\d+(st|nd|rd|th)\\b)") ~ "finished",
        str_detect(result_txt, "^did not finish")                ~ "not_finished",
        str_detect(result_txt, "^did not qualify")               ~ "not_qualified",
        str_detect(result_txt, "^did not arrive")                ~ "not_arrived",
        TRUE                                                     ~ "other"
      ),

      result = case_when(
        str_detect(result_txt, "^winner")    ~ 1L,
        result_status == "finished"          ~ str_extract(result_txt, "^\\d+") |> as.integer(),
        TRUE                                 ~ NA_integer_
      ),

      grid      = str_extract(result_txt, "(?<=Grid: )\\d+(?=st|nd|rd|th)") |> as.integer(),
      grid_time = str_match(result_txt, "Grid: \\d+(?:st|nd|rd|th) \\(([^)]+)\\)")[, 2],

      finish_gap = if_else(
        result_status == "finished",
        str_extract(result_txt, "\\([^)]*(behind the winner|kph)[^)]*\\)") |> str_remove_all("^\\(|\\)$"),
        NA_character_
      ),
      dnf_reason = if_else(
        result_status != "finished",
        str_extract(result_txt, "\\([^)]*\\)") |> str_remove_all("^\\(|\\)$"),
        NA_character_
      ),

      across(c(drivers, sponsor, colour, tyre, updated, contributor), ~ str_remove(.x, "^[^:]+:\\s*")),
      across(where(is.character), ~ if_else(str_to_lower(str_squish(.x)) == "unknown", NA_character_, .x))
    ) |>
    select(number, car_title, result, everything()) |>
    relocate(ends_with("_url"), .after = last_col()) |> 
    remove_na_cols()
}

# scraping + carga -------------------------------------------------------
# descarga y guarda CSVs por año para una carrera RSC (skip si ya existe)
scrape_race <- function(url = "https://www.racingsportscars.com/photo/Nurburgring-2003-06-01.html?sort=Results", user_agent = "Joshua Kunst jbkunst@gmail.com") {

  trck <- str_extract(url, "(?<=/photo/).+?(?=-\\d{4}-\\d{2}-\\d{2})") |> str_to_lower()
  date <- str_extract(url, "\\d{4}-\\d{2}-\\d{2}")
  fout <- str_glue("data/races/{trck}/results/{date}.csv")
  
  fs::dir_create(dirname(fout))
  cli::cli_progress_step("{url} -> {fout}")

  if (file.exists(fout)) return(invisible(TRUE))

  session <- bow(url = url, user_agent = user_agent)
  page    <- scrape(session)

  # x <- page |> html_elements(".carview") |> (function(x){x[[2]]})() 
  cars <- page |>
    html_elements(".carview") |>
    map_dfr(parse_carview)

  if (nrow(cars) == 0) {
    cli::cli_warn("Sin autos parseados para {url} — saltando")
    return(invisible(FALSE))
  }

  cars <- clean_cars(cars)
  
  cars <- mutate(cars, track = trck, date = date, .before = 1)
 
  # glimpse(cars)

  write_csv(cars, fout)
  
  invisible(TRUE)

}

# carga todos los CSVs de una carrera y parsea grid_time
load_race_results <- function(dir = "data/races/nurburgring/results/") {
  
  files <- fs::dir_ls(dir, glob = "*.csv") |> rev()

  datas <- files |>
    map(
      read_csv,
      show_col_types = FALSE,
      col_types = cols(.default = col_guess(), grid_time = col_character(), dnf_reason = col_character())
    ) |>
    map(add_missing_cols, c("chassis", "chassis_url"))

  # datas |> map(ncol) |> enframe() |> mutate(value = as.integer(value)) |> filter(value == 34)
  
  datas |>
    bind_rows() |> 
    mutate(grid_time = suppressWarnings(lubridate::ms(grid_time)))
}

# transforma columnas a HTML (links, imágenes, lightbox); tolera columnas opcionales
prep_rsc_dt_data <- function(data) {
  optional_cols <- c("make_url", "model_url", "chassis_url")

  # Cambia thumb_url a watermark
  data <- data |>
    mutate(
      thumb_url = data |>
        select(thumb_url, contributor) |>
        pmap_chr(make_wm_url)
    )

  data |>
    add_missing_cols(optional_cols) |>
    select(any_of(c(
      "track",
      "date",
      "number",
      "car_title",
      "result",
      "result_status",
      "group",
      "entrant",
      "make",
      "model",
      "chassis",
      "grid",
      "thumb_url",
      "photo_url",
      "make_url",
      "model_url",
      "chassis_url"
    ))) |>
    mutate(
      year = as.character(year(date)),
      number = as.character(number),
      result_status = case_when(result == 1 ~ "winner", TRUE ~ result_status), # si bien no viene, es para el dt color dorado
      photo_full = if_else(
        !is.na(photo_url) & !str_ends(photo_url, "/NA"),
        photo_url,
        thumb_url
      ),
      photo = if_else(
        !is.na(thumb_url),
        str_glue(
          "<img src='{thumb_url}' data-full='{photo_full}' height='80' class='lightbox-img' style='cursor:zoom-in;border-radius:4px;'/>"
        ),
        NA_character_
      ),
      car_title = if_else(
        !is.na(photo_url) & !str_ends(photo_url, "/NA"),
        str_glue('<a href="{photo_url}" target="_blank">{car_title}</a>'),
        car_title
      ),
      make = if_else(
        !is.na(make_url),
        str_glue('<a href="{make_url}"    target="_blank">{make}</a>'),
        make
      ),
      model = if_else(
        !is.na(model_url),
        str_glue('<a href="{model_url}"   target="_blank">{model}</a>'),
        model
      ),
      chassis = if_else(
        !is.na(chassis_url),
        str_glue('<a href="{chassis_url}" target="_blank">{chassis}</a>'),
        chassis
      )
    ) |>
    select(any_of(c(
      "track",
      "date",
      "year",
      "number",
      "photo",
      "car_title",
      "result",
      "result_status",
      "group",
      "entrant",
      "make",
      "model",
      "chassis",
      "grid"
    )))
}

style_result_status <- function(dt, status_col = "Result status") {
  # Aplica color de fondo por estado de resultado en tablas de carreras.
  
  dt |>
    DT::formatStyle(
      status_col,
      target = "row",
      backgroundColor = DT::styleEqual(
        c("winner", "finished", "not_finished", "not_qualified", "not_arrived", "other"),
        c("#F6E7B2", "transparent", "#eeeeee", "#f8d7da", "#e2e3e5", "#d1ecf1")
      )
    )
}

make_wm_url <- function(thumb_url, contributor = "") {
  # Convierte una URL thumbnail de RSC (/tn/{section}/{year}/TN_*) 
  # a la versión con watermark (/wm/{section}/{year}/WM_*).
  
  no_photo <- "https://www.racingsportscars.com/images/car_no_photo.png"
  
  if (is.na(thumb_url) || thumb_url == no_photo) {
    return(thumb_url)
  }
  
  m <- stringr::str_match(thumb_url, "/tn/([^/]+)/(\\d{4})/")
  
  if (is.na(m[, 1])) {
    return(thumb_url)
  }
  
  section <- m[, 2]
  year <- m[, 3]
  img <- basename(stringr::str_remove(thumb_url, "\\?.*$")) |> 
    stringr::str_remove("^TN_")
  
  if (is.na(contributor)) {
    contributor <- ""
  }
  
  txt <- URLencode(contributor, reserved = TRUE)
  
  stringr::str_glue(
    "https://www.racingsportscars.com/wm/{section}/{year}/WM_{img}",
    "?dir={section}/{year}&img={img}&txt={txt}&wi=&mode=Null"
  )
}