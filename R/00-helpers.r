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
clean_cars <- function(cars, year) {
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
    mutate(year = year, .before = 1) |>
    select(year, number, car_title, result, everything()) |>
    relocate(ends_with("_url"), .after = last_col())
}

# scraping + carga -------------------------------------------------------
# descarga y guarda CSVs por año para una carrera RSC (skip si ya existe)
scrape_race <- function(race_tbl, out_dir = "data/daytona24/results/", user_agent = "Joshua Kunst jbkunst@gmail.com") {
  fs::dir_create(out_dir)

  pwalk(race_tbl, function(date, year, url) {

    # year <- 1974 
    # url <- "https://www.racingsportscars.com/photo/Le_Mans-1974-06-16.html?sort=Results"

    fout <- fs::path(out_dir, str_glue("{year}.csv"))
    cli::cli_progress_step("{year}: {url} -> {fout}")

    if (file.exists(fout)) return(invisible(TRUE))

    session <- bow(url = url, user_agent = user_agent)
    page    <- scrape(session)

    cars <- page |>
      html_elements(".carview") |>
      map_dfr(parse_carview)

    if (nrow(cars) == 0) {
      cli::cli_warn("Sin autos parseados para {year} — saltando")
      return(invisible(FALSE))
    }

    clean_cars(cars, year) |> write_csv(fout)
    invisible(TRUE)
  })
}

# carga todos los CSVs de una carrera y parsea grid_time
load_race_results <- function(out_dir = "data/nurburgring24/results/") {
  files <- fs::dir_ls(out_dir, glob = "*.csv") |> rev()

  files |>
    # map(read_csv, show_col_types = FALSE,) |>  map(ncol) |> enframe() |> mutate(value = as.numeric(value)) |>  filter(value == 29)
    map(
      read_csv,
      show_col_types = FALSE,
      col_types = cols(.default = col_guess(), grid_time = col_character(), dnf_reason = col_character())
    ) |>
    map(add_missing_cols, c("chassis", "chassis_url")) |>
    bind_rows() |> 
    mutate(grid_time = lubridate::ms(grid_time))
}

# transforma columnas a HTML (links, imágenes, lightbox); tolera columnas opcionales
prep_dt_data <- function(data) {
  optional_cols <- c("make_url", "model_url", "chassis_url")
  data |>
    add_missing_cols(optional_cols) |>
    select(any_of(c(
      "year", "number", "car_title", "result", "result_status", "group", "entrant",
      "make", "model", "chassis", "grid",
      "thumb_url", "photo_url", "make_url", "model_url", "chassis_url"
    ))) |>
    mutate(
      year           = as.character(year),
      number         = as.character(number),
      result_status  = case_when(result == 1 ~ "winner", TRUE ~ result_status), # si bien no viene, es para el dt color dorado
      photo_full = if_else(!is.na(photo_url) & !str_ends(photo_url, "/NA"), photo_url, thumb_url),
      photo = if_else(
        !is.na(thumb_url),
        str_glue("<img src='{thumb_url}' data-full='{photo_full}' height='80' class='lightbox-img' style='cursor:zoom-in;border-radius:4px;'/>"),
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
    select(any_of(c(
      "year", "number", "photo", "car_title", "result", "result_status", "group",
      "entrant", "make", "model", "chassis", "grid"
    )))
}

# construye un DT con estilo, lightbox y filtros (genérico para cualquier carrera RSC)
make_race_dt <- function(data, element_id) {
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
        c("winner", "finished", "not_finished", "not_qualified", "not_arrived", "other"),
        c("#EAD27A", "transparent", "#eeeeee", "#f8d7da", "#e2e3e5", "#d1ecf1")
      )
    )
}
