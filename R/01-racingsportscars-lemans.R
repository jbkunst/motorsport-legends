# setup ------------------------------------------------------------------
library(tidyverse)
library(rvest)
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

parse_carview <- function(x, base_url) {
  
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
    photo_url   = html_attr_or_na(x, ".photo input[type='hidden']", "value") |> url_absolute(base = site_url)
  ) |>
    bind_cols(car_links) |>
    add_missing_cols(c("type", "type_url")) |>
    rename(model = type, model_url = type_url)
  
  output
}

# transforma columnas a HTML (links, imágenes, lightbox)
prep_dt_data <- function(data) {
  data |>
    select(year, number, car_title, result, result_status, group, entrant,
           make, model, chassis, grid,
           thumb_url, photo_url, make_url, model_url, chassis_url) |>
    mutate(
      year       = as.character(year),
      number     = as.character(number),
      photo_full = if_else(!is.na(photo_url) & !str_ends(photo_url, "/NA"), photo_url, thumb_url),
      photo = if_else(
        !is.na(thumb_url),
        str_glue("<img src='{thumb_url}' data-full='{photo_full}' height='60' class='lightbox-img' style='cursor:zoom-in;border-radius:4px;'/>"),
        NA_character_
      ),
      car_title = if_else(
        !is.na(photo_url) & !str_ends(photo_url, "/NA"),
        str_glue('<a href="{photo_url}" target="_blank">{car_title}</a>'),
        car_title
      ),
      make    = if_else(!is.na(make_url),    str_glue('<a href="{make_url}"    target="_blank">{make}</a>'),    make),
      model   = if_else(!is.na(model_url),   str_glue('<a href="{model_url}"   target="_blank">{model}</a>'),   model),
      chassis = if_else(!is.na(chassis_url), str_glue('<a href="{chassis_url}" target="_blank">{chassis}</a>'), chassis)
    ) |>
    select(year, number, photo, car_title, result, result_status, group,
           entrant, make, model, chassis, grid)
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

fs::dir_create("data/lemans/results/")

pwalk(le_mans_tbl, function(date, year, url) {
  # year <- 2011
  # url <- "https://www.racingsportscars.com/photo/Le_Mans-1959-06-21.html?sort=Results"

  fout <- str_glue("data/lemans/results/{year}.csv")

  cli::cli_progress_step("{year}: {url} -> {fout}")

  if(file.exists(fout)) return(TRUE)

  session <- bow(
    url = url,
    user_agent = "Joshua Kunst jbkunst@gmail.com"
  )

  page <- scrape(session)

  cars <- page |>
    html_elements(".carview") |>
    map_dfr(parse_carview, base_url = url)
  # x <- page |> html_elements(".carview") |> pluck(25)
  # parse_carview(x, base_url = url)

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
        result_status == "finished" ~ str_extract(result_txt, "^\\d+") |> as.integer(),
        TRUE ~ NA_integer_
      ),
      
      grid = str_extract(result_txt, "(?<=Grid: )\\d+(?=st|nd|rd|th)") |> as.integer(),
      grid_time = str_match(result_txt, "Grid: \\d+(?:st|nd|rd|th) \\(([^)]+)\\)")[, 2],
      
      finish_gap = if_else(result_status == "finished", str_extract(result_txt, "\\([^)]*(behind the winner|kph)[^)]*\\)") |> str_remove_all("^\\(|\\)$"), NA_character_),
      dnf_reason = if_else(result_status != "finished", str_extract(result_txt, "\\([^)]*\\)") |> str_remove_all("^\\(|\\)$"), NA_character_),
      
      across(c(drivers, sponsor, colour, tyre, updated, contributor), ~ str_remove(.x, "^[^:]+:\\s*")),
      across(where(is.character), ~ if_else(str_to_lower(str_squish(.x)) == "unknown", NA_character_, .x))
    )
  
  cars_clean <- cars_clean |> 
    mutate(year = year, .before = 1) |> 
    select(year, number, car_title, result, everything()) |>
    relocate(ends_with("_url"), .after = last_col())

  write_csv(cars_clean, fout)
    
})

datalm <- fs::dir_ls("data/lemans/results/") |>
  rev() |> 
  map_df(read_csv, show_col_types = FALSE, col_types = cols(.default = col_guess(), grid_time = col_character()))

datalm <- datalm |>
  mutate(grid_time = lubridate::ms(grid_time))

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

# construye el DT con estilo, lightbox y filtros
make_lemans_dt <- function(data, element_id) {
  data |>
    rename_with(~ str_replace_all(.x, "_", " ") |> str_to_sentence()) |>
    DT::datatable(
      elementId = element_id,
      extensions = c("FixedHeader", "Buttons"),
      filter = "top",
      class = "hover",
      options = list(
        fixedHeader = TRUE,
        paging = FALSE,
        dom = "Bfrtip",
        buttons = list(list(extend = "colvis", text = "Columnas")),
        scrollY = "calc(100vh - 200px)",
        scrollCollapse = TRUE,
        initComplete = DT::JS(
          "function(settings, json) {",
          "  var link = document.createElement('link');",
          "  link.rel = 'stylesheet';",
          "  link.href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap';",
          "  document.head.appendChild(link);",
          "  var container = $(this.api().table().container());",
          "  container.css({'font-family': 'Inter, system-ui, sans-serif', 'font-size': '13px'});",
          "  container.find('thead th').css({'font-weight': '600', 'font-size': '11px', 'text-transform': 'uppercase', 'letter-spacing': '0.06em', 'color': '#555'});",
          "  if (!document.getElementById('lb-overlay')) {",
          "    var overlay = document.createElement('div');",
          "    overlay.id = 'lb-overlay';",
          "    overlay.style.cssText = 'display:none;position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.85);z-index:9999;justify-content:center;align-items:center;cursor:zoom-out;';",
          "    var lbImg = document.createElement('img');",
          "    lbImg.id = 'lb-img';",
          "    lbImg.style.cssText = 'max-width:90vw;max-height:90vh;border-radius:6px;box-shadow:0 8px 32px rgba(0,0,0,0.6);';",
          "    overlay.appendChild(lbImg);",
          "    document.body.appendChild(overlay);",
          "    overlay.addEventListener('click', function() { overlay.style.display = 'none'; });",
          "    document.addEventListener('keydown', function(e) { if(e.key === 'Escape') overlay.style.display = 'none'; });",
          "  }",
          "  var overlay = document.getElementById('lb-overlay');",
          "  var lbImg   = document.getElementById('lb-img');",
          "  $(document).on('click', 'img.lightbox-img', function() {",
          "    var thumbSrc = this.src;",
          "    var fullSrc  = this.dataset.full || thumbSrc;",
          "    lbImg.style.width  = '';",
          "    lbImg.style.height = '';",
          "    lbImg.onerror = function() { lbImg.onerror = null; lbImg.src = thumbSrc; };",
          "    var tmp = new Image();",
          "    tmp.onload = function() {",
          "      lbImg.style.width  = Math.min(tmp.naturalWidth  * 1.5, window.innerWidth  * 0.9) + 'px';",
          "      lbImg.style.height = Math.min(tmp.naturalHeight * 1.5, window.innerHeight * 0.9) + 'px';",
          "    };",
          "    tmp.src = fullSrc;",
          "    lbImg.src = fullSrc;",
          "    overlay.style.display = 'flex';",
          "  });",
          "}"
        )
      ),
      escape = FALSE,
      rownames = FALSE
    ) |>
    DT::formatStyle(
      "Result status",
      target = "row",
      backgroundColor = DT::styleEqual(
        c("finished", "not_finished", "not_qualified", "not_arrived", "other"),
        c("transparent", "#fff3cd", "#f8d7da", "#e2e3e5", "#d1ecf1")
      )
    )
}

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

dt_lm_full <- make_lemans_dt(datalm_full, "lemans-full")
dt_lm_min  <- make_lemans_dt(datalm_min,  "lemans-min")

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
