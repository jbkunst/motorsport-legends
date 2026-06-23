# setup ------------------------------------------------------------------
library(tidyverse)

source(here::here("R/00-helpers.R"))
source(here::here("R/endurance/00-rsc-helpers.R"))

# data collection and join with results ----------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQxCLyOJ3I5tnjgwTlxiMVZagQ19DiZpoc_3xBOTdmnoo8gbai5MepFqCY2vAE27guGTAxjKlWti0SD/pub?gid=132013729&single=true&output=csv"

data_collection <- read_csv(url)

data_collection <- data_collection |>
  select(everything())

data_collection_join <- fs::dir_ls("data/wrc/") |>
  map_df(function(folder = "data/wrc/usa_") {
    cli::cli_inform(folder)

    rr <- fs::dir_ls(folder) |>
      map_df(function(file = "data/wrc/monte_carlo/1993.csv"){
        read_csv(
          file,
          show_col_types = FALSE,
          col_types = cols(
            # .default = col_character(),
            # entry_number = col_integer(),
            # position = col_integer() 
            penalty_time = col_time(format = "")
          )
        ) |>
          mutate(
            event_slug = basename(dirname(file)),
            year = fs::path_ext_remove(basename(file)) |> as.integer(),
            .before = 1
          )
      })
     
    rr <- rr |>
      arrange(year, homologation_group, position) |>
      group_by(year, homologation_group) |>
      mutate(positiont_group = if_else(is.na(position), NA_integer_, cumsum(!is.na(position)))) |>
      ungroup() |> 
      arrange(year, position)

    dc <- filter(data_collection, event_slug == basename(folder))

    # inner: conserva solo cruces válidos; el anti_join posterior falla si falta algo de la colección.
    # en este join confiamos en que solo es necesario el numero y no el make
    dout <- inner_join(rr, dc, by = join_by(event_slug, year, entry_number == number))
    dout <- arrange(dout, year, position, entry_number)

    dout |>
      count(event_slug, year, entry_number, sort = TRUE) |>
      filter(n > 1) |>
      nrow() |>
      {\(x) stopifnot("Hay duplicados por race/year/entry_number" = x == 0)}()

    dout
  })

data_collection_join |> glimpse()

# serper image search -----------------------------------------------------
fs::dir_create("outputs/csv/wrc")

serper_cache_file <- "outputs/csv/wrc/serper_wrc_images.csv"

search_serper_real_image <- function(
  query,
  api_key = Sys.getenv("SERPER_API_KEY"),
  n = 10,
  cache_file = "outputs/csv/wrc/serper_wrc_images.csv"
) {
  if (api_key == "") {
    stop("Falta SERPER_API_KEY.")
  }

  fs::dir_create(dirname(cache_file))

  cache <- if (file.exists(cache_file)) {
    read_csv(
      cache_file,
      show_col_types = FALSE,
      col_types = cols(
        .default = col_character(),
        image_score = col_double()
      )
    )
  } else {
    tibble(
      query = character(),
      image_url = character(),
      page_url = character(),
      title = character(),
      source = character(),
      image_score = double()
    )
  }

  cached <- cache |>
    filter(!str_detect(str_to_lower(str_c(image_url, " ", page_url)), "lookaside|modelsshop|ixomodels|1999\\.co|1999\\.co\\.jp")) |>
    filter(query == !!query) |>
    arrange(desc(image_score)) |>
    slice_head(n = n)

  if (nrow(cached) > 0) {
    cli::cli_inform("Using cached image: {query}")
    return(cached)
  }

  query_real <- str_glue("{query} rally photo")

  cli::cli_inform("Searching real image: {query_real}")

  res <- httr2::request("https://google.serper.dev/images") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `X-API-KEY` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      list(q = query_real, num = 20),
      auto_unbox = TRUE
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  out <- httr2::resp_body_json(res, simplifyVector = TRUE)

  if (is.null(out$images) || nrow(out$images) == 0) {
    result <- tibble(
      query = query,
      image_url = NA_character_,
      page_url = NA_character_,
      title = NA_character_,
      source = NA_character_,
      image_score = NA_real_
    )

    bind_rows(cache, result) |>
      distinct(query, image_url, .keep_all = TRUE) |>
      write_csv(cache_file)

    return(result)
  }

  image_ext_pattern <- "\\.(jpg|jpeg|png|webp)(\\?|$)"

  direct_image_source_pattern <- paste(
    c(
      "preview.redd.it",
      "i.redd.it",
      "pbs.twimg.com",
      "live.staticflickr.com",
      "c8.alamy.com",
      "snaplap.net/wp-content/uploads",
      "wp-content/uploads"
    ),
    collapse = "|"
  )

  blocked_pattern <- paste(
    c(
      "lookaside.fbsbx", "facebook", "instagram", "lookaside",
      "youtube", "ytimg", "pinterest",
      "hiroboy", "ixomodels", "1999.co.jp", "ebay", "etsy", "amazon",
      "1999.co", "models118", "modelsshop", "diecast", "modelcars",
      "miniatures", "model-car", "car-model"
    ),
    collapse = "|"
  )

  sales_pattern <- paste(
    c(
      "racemarket", "auction", "for-sale", "forsale", "sale",
      "dealer", "showroom", "stock", "classified", "secret-classics"
    ),
    collapse = "|"
  )

  model_word_pattern <- paste(
    c(
      "diecast", "model", "miniature", "ixo", "spark", "trofeu",
      "kyosho", "hpi", "altaya", "1:43", "1:18", "kit", "decals",
      "models118", "modelsshop", "ixo_"
    ),
    collapse = "|"
  )

  result <- out$images |>
    as_tibble() |>
    transmute(
      query = query,
      image_url = imageUrl,
      page_url = link,
      title = coalesce(title, ""),
      source = coalesce(source, ""),
      text = str_to_lower(str_c(title, " ", source, " ", link, " ", imageUrl)),
      image_url_lwr = str_to_lower(image_url)
    ) |>
    mutate(
      is_direct_image =
        str_detect(image_url_lwr, image_ext_pattern) |
        str_detect(image_url_lwr, direct_image_source_pattern),

      blocked = str_detect(text, blocked_pattern),
      looks_sale = str_detect(text, sales_pattern),
      looks_model = str_detect(text, model_word_pattern),
      looks_real = str_detect(text, "rally|wrc|photo|sainz|auriol|toyota|celica|corolla"),

      image_score =
        if_else(is_direct_image, 80, 0) +
        if_else(looks_real, 50, 0) -
        if_else(blocked, 120, 0) -
        if_else(looks_model, 80, 0) -
        if_else(looks_sale, 40, 0)
    ) |>
    filter(!blocked) |>
    arrange(desc(image_score)) |>
    slice_head(n = n) |>
    select(query, image_url, page_url, title, source, image_score)

  bind_rows(cache, result) |>
    distinct(query, image_url, .keep_all = TRUE) |>
    write_csv(cache_file)

  result
}

serper_wrc_images <- data_collection_join |>
  mutate(
    image_query = str_glue(
      "{driver_name |> str_remove(',.*$')} {car_model} {event_slug |> str_replace_all('_', ' ')} {year}"
    )
  ) |>
  distinct(event_slug, year, entry_number, image_query) |>
  mutate(image_result = map(image_query, search_serper_real_image)) |>
  unnest(image_result)

serper_wrc_images |> select(image_url, image_query) |> print(n = Inf)

serper_wrc_images_best <- serper_wrc_images |>
  group_by(event_slug, year, entry_number) |>
  arrange(desc(image_score), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(event_slug, year, entry_number, query, image_url, page_url, title, source, image_score)

# datatable html output ---------------------------------------------------
data_collection_join <- data_collection_join |>
  left_join(
    serper_wrc_images_best,
    by = join_by(event_slug, year, entry_number)
  )

dt_wrc_data <- data_collection_join |>
  transmute(
    track = event_slug,
    year = as.character(year),
    number = as.character(entry_number),
    photo = if_else(
      !is.na(image_url) & image_url != "",
      str_glue("<img src='{image_url}' data-full='{image_url}' height='80' class='lightbox-img' style='cursor:zoom-in;border-radius:4px;'/>"),
      NA_character_
    ),
    car_title = coalesce(name, car_model),
    car_title = if_else(
      !is.na(page_url) & page_url != "",
      str_glue('<a href="{page_url}" target="_blank">{car_title}</a>'),
      car_title
    ),
    result = position,
    result_group = positiont_group,
    result_status = case_when(
      position == 1 ~ "winner",
      !is.na(position) ~ "finished",
      TRUE ~ "not_finished"
    ),
    group = homologation_group,
    entrant = driver_name,
    make,
    model = car_model,
    chassis = NA_character_,
    scale64_maker,
    scale64_status,
    note
  ) |>
  mutate(
    # source = "wrc",
    note = dplyr::if_else(
      is.na(note) | note == "",
      "",
      glue::glue("<span class='dt-tooltip' data-tip=\"{htmltools::htmlEscape(note, attribute = TRUE)}\">{info_icon}</span>")
    ),
    car_title = stringr::str_squish(paste(car_title, note))
  ) |>
  mutate(across(c(track, result_status, scale64_maker, scale64_status), as.factor)) |>
  # relocate(source, .before = 1) |>
  # relocate(track, .after = 1) |>
  relocate(result_group, .after = result) |>
  select(-note) |>
  arrange(year, track, result, number)

dt <- dt_wrc_data |>
  glimpse() |>
  make_dt("wrc_collection", search = "columns") |>
  style_result_status()

dt

# save -------------------------------------------------------------------
readr::write_csv(data_collection_join, file = here::here("outputs/csv/wrc_collection64.csv"))

htmlwidgets::saveWidget(
  dt,
  file = here::here("outputs/html/wrc_collection64.html"),
  libdir = "lib",
  selfcontained = FALSE,
  title = "WRC Collection 1:64 - Results and Status"
)
