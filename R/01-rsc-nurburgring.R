# setup ------------------------------------------------------------------
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
# Nürburgring 24h: desde 1970, sin 1974, 1975 (petróleo) ni 1983 (obras)
# Fechas = domingo (fin de carrera), que es lo que usa RSC en la URL.
# (*) = confirmado desde URL de racingsportscars.com
# (e) = estimado; verificar contra https://www.racingsportscars.com/photo/Nurburgring-{fecha}.html

nurburgring_dates <- c(
  # 2020s
  "2025-05-25",  # (e) 53ª edición
  "2024-06-09",  # (e)
  "2023-05-21",  # (*)
  "2022-06-05",  # (e)
  "2021-06-06",  # (e)
  "2020-09-27",  # (e) COVID: edición de septiembre

  # 2010s
  "2019-06-23",  # (*)
  "2018-05-13",  # (e)
  "2017-05-28",  # (*)
  "2016-05-29",  # (*)
  "2015-05-17",  # (*)
  "2014-06-22",  # (e)
  "2013-05-19",  # (e) Pentecostés 2013
  "2012-05-20",  # (e)
  "2011-06-26",  # (*)
  "2010-05-16",  # (e)

  # 2000s
  "2009-05-24",  # (*)
  "2008-05-25",  # (e)
  "2007-05-27",  # (e)
  "2006-06-18",  # (e) Corpus Christi
  "2005-05-29",  # (e)
  "2004-06-20",  # (e)
  "2003-06-01",  # (*) desde URL de resultados RSC

  # 1990s
  "2002-06-16",  # (e)
  "2001-05-20",  # (e)
  "2000-05-21",  # (e)
  "1999-05-30",  # (e)
  "1998-05-24",  # (e)
  "1997-05-18",  # (e)
  "1996-06-02",  # (e)
  "1995-05-28",  # (e)
  "1994-05-29",  # (e)
  "1993-05-30",  # (e)
  "1992-05-24",  # (e)
  "1991-05-26",  # (e)
  "1990-05-27",  # (e)

  # 1980s — sin 1983
  "1989-05-28",  # (e)
  "1988-05-29",  # (e)
  "1987-05-24",  # (e)
  "1986-05-25",  # (e)
  "1985-05-26",  # (e)
  "1984-05-27",  # (e)
  "1982-05-30",  # (e) sin 1983
  "1981-05-24",  # (e)
  "1980-05-18",  # (e)

  # 1970s — sin 1974, 1975
  "1979-05-27",  # (e)
  "1978-05-28",  # (e)
  "1977-05-29",  # (e)
  "1976-05-30",  # (e)
  "1973-05-27",  # (e)
  "1972-05-28",  # (e)
  "1971-05-30",  # (e)
  "1970-06-28"   # (e) primera edición: 27-28 junio 1970
)

nurburgring_tbl <- tibble(
  date = ymd(nurburgring_dates),
  year = as.integer(format(date, "%Y")),
  url  = str_glue("https://www.racingsportscars.com/photo/Nurburgring-{date}.html?sort=Results")
)

# scrape -----------------------------------------------------------------
scrape_race(nurburgring_tbl, "data/nurburgring/results/", user_agent = "Joshua Kunst jbkunst@gmail.com")

# load -------------------------------------------------------------------
datanur <- load_race_results("data/nurburgring/results/")

datanur

# datatable --------------------------------------------------------------
glimpse(datanur)

# datos ------------------------------------------------------------------

# _full: todos los registros
datanur_full <- prep_dt_data(datanur)

# _min: todos los finished + el mejor DNF por grupo/año, sin "other"
datanur_min <- bind_rows(
  datanur |> filter(result_status == "finished"),
  datanur |>
    filter(result_status == "not_finished") |>
    slice_head(n = 1, by = c(year, group))
) |>
  arrange(desc(year), result) |>
  prep_dt_data()

nrow(datanur_full)
nrow(datanur_min)

# datatables -------------------------------------------------------------
dt_nur_full <- make_race_dt(datanur_full, "nurburgring-full")
dt_nur_min  <- make_race_dt(datanur_min,  "nurburgring-min")

dt_nur_full  # preview en viewer

# save -------------------------------------------------------------------
fs::dir_create("outputs/html")

htmlwidgets::saveWidget(
  dt_nur_full,
  file          = here::here("outputs/html/nurburgring_results_full.html"),
  libdir        = "lib",
  selfcontained = FALSE
)

htmlwidgets::saveWidget(
  dt_nur_min,
  file          = here::here("outputs/html/nurburgring_results_min.html"),
  libdir        = "lib",
  selfcontained = FALSE
)

cli::cli_alert_success("Guardado: outputs/html/nurburgring_results_full.html ({nrow(datanur_full)} filas)")
cli::cli_alert_success("Guardado: outputs/html/nurburgring_results_min.html  ({nrow(datanur_min)} filas)")
