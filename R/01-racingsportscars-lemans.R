# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(polite)

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

parse_carview <- function(x, base_url) {
  
  site_url <- "https://www.racingsportscars.com/"
  
  car_links <- x |>
    html_elements(".car.header a")

  car_links <- tibble(
    text = html_text2(car_links),
    href = html_attr(car_links, "href") |> url_absolute(base = site_url),
    type = str_extract(href, "(?<=racingsportscars\\.com/)[^/]+(?=/photo/)")
  )
  
  car_links <- car_links |>
    filter(!is.na(type)) |>
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
    photo_url   = html_attr_or_na(x, ".photo input[type='hidden']", "value") |> url_absolute(base = site_url)
  ) |>
    bind_cols(car_links) |>
    rename(model = type, model_url = type_url)
  
  output
}
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

data <- le_mans_tbl |>
  pmap(function(date, year, url) {
    # year <- 1979
    # url <- "https://www.racingsportscars.com/photo/Le_Mans-1979-06-10.html?sort=Results"

    cli::cli_progress_step("{year}: {url}")

    session <- bow(
      url = url,
      user_agent = "Joshua Kunst jbkunst@gmail.com"
    )

    page <- scrape(session)

    cars <- page |>
      html_elements(".carview") |>
      map_dfr(parse_carview, base_url = url)
    
    cars <- cars |>
      mutate(across(c(drivers, sponsor, colour, tyre, updated, contributor), ~ str_remove(.x, "^[^:]+:\\s*")))
    
    cars <- cars |>
      relocate(ends_with("_url"), .after = last_col())
    
   cars_clean <- cars |>
    mutate(
      result_raw = result,
      result_txt = str_remove(result_raw, "^Result:\\s*"),

      result_status = case_when(
        str_detect(result_txt, "^(winner|\\d+(st|nd|rd|th)\\b)") ~ "finished",
        str_detect(result_txt, "^did not finish") ~ "not_finished",
        str_detect(result_txt, "^did not qualify") ~ "not_qualified",
        str_detect(result_txt, "^did not arrive") ~ "not_arrived",
        TRUE ~ "other"
      ),

      result = case_when(
        str_detect(result_txt, "^winner") ~ 1L,
        result_status == "finished" ~ str_extract(result_txt, "^\\d+") |>
          as.integer(),
        TRUE ~ NA_integer_
      ),

      grid = str_extract(result_txt, "(?<=Grid: )\\d+(?=st|nd|rd|th)") |>
        as.integer(),
      grid_time = str_match(
        result_txt,
        "Grid: \\d+(?:st|nd|rd|th) \\(([^)]+)\\)"
      )[, 2],

      finish_gap = if_else(
        result_status == "finished",
        str_extract(result_txt, "\\([^)]*(behind the winner|kph)[^)]*\\)") |>
          str_remove_all("^\\(|\\)$"),
        NA_character_
      ),
      dnf_reason = if_else(
        result_status != "finished",
        str_extract(result_txt, "\\([^)]*\\)") |> str_remove_all("^\\(|\\)$"),
        NA_character_
      ),

      across(
        c(drivers, sponsor, colour, tyre, updated, contributor),
        ~ str_remove(.x, "^[^:]+:\\s*")
      )
    )
    
    cars_clean |> glimpse()
  
  })
