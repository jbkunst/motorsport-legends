# setup ------------------------------------------------------------------
library(tidyverse)
library(here)

# This script does not scrape Ferrari.com. It enriches the existing snapshot
# produced by R/cars/01-ferrari-past-models.R.
#
# Taxonomy principles:
# - main_family is mutually exclusive and stable enough for a timeline Y axis.
# - lineage identifies a narrower generational thread inside a family.
# - base_model describes a derivative; it is not necessarily a predecessor.
# - direct succession, evolution, derivation and spiritual inheritance are distinct.
# - missing links are intentional when the evidence is weak or a line was interrupted.
#
# Historical relationships were curated primarily from Ferrari's model descriptions
# already present in the input and Ferrari's official "Bloodlines" / history articles.
# Review date: 2026-07-27.

# params -----------------------------------------------------------------
in_csv                 <- here::here("data/cars/ferrari_past_models.csv")
production_periods_csv <- here::here("data/cars/ferrari_production_periods.csv")
out_csv                <- here::here("data/cars/ferrari_past_models_enriched.csv")
keep_image_data_uri <- FALSE

fs::dir_create(fs::path_dir(out_csv))

# read and validate -------------------------------------------------------
required_cols <- c(
  "model", "year", "category", "engine", "engine_position", "description", "url"
)

ferrari_source <- readr::read_csv(in_csv, show_col_types = FALSE)

missing_cols <- setdiff(required_cols, names(ferrari_source))
if (length(missing_cols) > 0) {
  cli::cli_abort("Missing required columns: {paste(missing_cols, collapse = ', ')}")
}

# The source snapshot currently ends in 2023. This explicit row closes the
# V12 berlinetta lineage with the 2024 12Cilindri. Technical values come from
# Ferrari's official launch specifications:
# https://www.ferrari.com/en-EN/corporate/articles/ferrari-12cilindri-for-the-few
ferrari_manual <- tibble::tibble(
  model = "12Cilindri",
  year = 2024,
  category = "Gran Turismo",
  engine = "V12",
  displacement_cc = 6496,
  max_power_cv = 830,
  rpm_max_power = 9250,
  top_speed_kmh = 340,
  engine_position = "front",
  engine_orientation = "longitudinal",
  engine_angle_deg = 65,
  compression_ratio = 13.5,
  bore_mm = 94,
  stroke_mm = 78,
  weight_kg = 1560,
  wheelbase_mm = 2700,
  length_mm = 4733,
  width_mm = 2176,
  height_mm = 1292,
  image_url = NA_character_,
  url = "https://www.ferrari.com/en-EN/auto/ferrari-12cilindri",
  image_data_uri = NA_character_,
  description = paste(
    "Front-mid-engined two-seat V12 grand tourer launched in 2024;",
    "official successor to the 812 Superfast."
  ),
  source_origin = "manual_ferrari.com"
)

ferrari_manual_additions <- tibble::tribble(
  ~model, ~year, ~category, ~engine, ~engine_position, ~url, ~description,
  "Monza SP1", 2018, "Gran Turismo", "V12", "front",
  "https://www.ferrari.com/en-EN/auto/monza-sp1",
  "Single-seat front-engined V12 model that inaugurated Ferrari's Icona series.",
  "Monza SP2", 2018, "Gran Turismo", "V12", "front",
  "https://www.ferrari.com/en-EN/auto/monza-sp2",
  "Two-seat sister model to the Monza SP1 in Ferrari's Icona series.",
  "812 Competizione", 2021, "Gran Turismo", "V12", "front",
  "https://www.ferrari.com/en-EN/auto/812-competizione",
  "Limited special-series berlinetta derived from the 812 Superfast.",
  "812 Competizione A", 2021, "Gran Turismo", "V12", "front",
  "https://www.ferrari.com/en-EN/auto/812-competizione-a",
  "Open special-series derivative of the 812 Competizione.",
  "296 GTB", 2021, "Gran Turismo", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/296-gtb",
  "Mid-engined plug-in hybrid V6 sports berlinetta.",
  "Daytona SP3", 2021, "Gran Turismo", "V12", "rear",
  "https://www.ferrari.com/en-EN/auto/daytona-sp3",
  "Mid-engined V12 limited-series model in Ferrari's Icona programme.",
  "296 GTS", 2022, "Gran Turismo", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/296-gts",
  "Open derivative of the 296 GTB hybrid sports car.",
  "296 GT3", 2022, "Sport Prototype", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/296-gt3",
  "Customer GT racing car developed around the 296 concept.",
  "Purosangue", 2022, "Gran Turismo", "V12", "front",
  "https://www.ferrari.com/en-EN/auto/ferrari-purosangue",
  "Four-door, four-seat front-mid-engined V12 performance car.",
  "296 Challenge", 2023, "Sport Prototype", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/296-challenge",
  "One-make racing successor to the 488 Challenge Evo.",
  "499P", 2023, "Sport Prototype", "V6", "rear",
  "https://www.ferrari.com/en-EN/hypercar/499p",
  "Factory hybrid Le Mans Hypercar that returned Ferrari to top-class endurance racing.",
  "499P Modificata", 2023, "Sport Prototype", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/499p-modificata",
  "Non-competitive track-only derivative of the 499P.",
  "SF90 XX Stradale", 2023, "Gran Turismo", "V8", "rear",
  "https://www.ferrari.com/en-EN/auto/sf90-xx-stradale",
  "Road-legal special-series derivative of the SF90 Stradale.",
  "SF90 XX Spider", 2023, "Gran Turismo", "V8", "rear",
  "https://www.ferrari.com/en-EN/auto/sf90-xx-spider",
  "Open special-series derivative of the SF90 XX Stradale.",
  "12Cilindri Spider", 2024, "Gran Turismo", "V12", "front",
  "https://www.ferrari.com/en-EN/auto/ferrari-12cilindri-spider",
  "Open derivative launched alongside the 12Cilindri berlinetta.",
  "F80", 2024, "Gran Turismo", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/f80",
  "Limited-series hybrid halo successor to LaFerrari.",
  "296 Speciale", 2025, "Gran Turismo", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/296-speciale",
  "Special-series evolution of the 296 GTB.",
  "296 Speciale A", 2025, "Gran Turismo", "V6", "rear",
  "https://www.ferrari.com/en-EN/auto/296-speciale-a",
  "Open derivative of the 296 Speciale.",
  "Ferrari Amalfi", 2025, "Gran Turismo", "V8", "front",
  "https://www.ferrari.com/en-EN/auto/ferrari-amalfi",
  "Front-mid-engined V8 2+ grand tourer succeeding the Roma.",
  "849 Testarossa", 2025, "Gran Turismo", "V8", "rear",
  "https://www.ferrari.com/en-EN/auto/849-testarossa",
  "Plug-in hybrid V8 supercar succeeding the SF90 Stradale."
) |>
  dplyr::mutate(source_origin = "manual_ferrari.com")

ferrari_manual <- dplyr::bind_rows(
  ferrari_manual,
  ferrari_manual_additions
)

ferrari_models <- ferrari_source |>
  dplyr::mutate(source_origin = "ferrari_past_models_scraper") |>
  dplyr::bind_rows(ferrari_manual)

if (anyDuplicated(ferrari_models$url)) {
  cli::cli_abort("The source contains duplicate URLs; URL is expected to be unique.")
}

# Production periods are maintained manually and deliberately kept outside
# this script. The first commented line in the CSV documents that policy.
period_required_cols <- c(
  "model", "source_year", "production_start_year", "production_end_year",
  "production_status", "period_source_name", "period_source_url",
  "period_source_type", "period_confidence", "period_review_status",
  "period_notes", "period_reviewed_at"
)

production_periods <- readr::read_csv(
  production_periods_csv,
  comment = "#",
  show_col_types = FALSE
)

missing_period_cols <- setdiff(period_required_cols, names(production_periods))
if (length(missing_period_cols) > 0) {
  cli::cli_abort(
    "Missing production-period columns: {paste(missing_period_cols, collapse = ', ')}"
  )
}

if (anyDuplicated(production_periods[c("model", "source_year")])) {
  cli::cli_abort("Production-period keys must be unique by model and source_year.")
}

unmatched_periods <- production_periods |>
  dplyr::anti_join(
    ferrari_models |> dplyr::select(model, source_year = year),
    by = c("model", "source_year")
  )

if (nrow(unmatched_periods) > 0) {
  cli::cli_abort("Some production-period rows do not match a Ferrari model.")
}

# controlled vocabularies ------------------------------------------------
families <- c(
  "Front-engined V12 two-seat GT",
  "Front-engined V12 2+2 GT",
  "Front-engined V8 GT",
  "Mid-engined sports car",
  "Mid-engined 2+2 GT",
  "Mid-engined 12-cylinder supercar",
  "Limited-series halo",
  "Hybrid supercar",
  "Icona limited series",
  "Sports-racing prototypes",
  "Competition derivatives"
)

relationship_types <- c(
  "direct",
  "evolution",
  "derivative",
  "spiritual_successor"
)

# Model groups ------------------------------------------------------------
v12_2plus2 <- c(
  "250 GT 2+2", "330 GT 2+2", "365 GT 2+2", "365 GTC4",
  "365 GT4 2+2", "400 GT", "400 Automatic", "400 GTi",
  "400 Automatic i", "412", "456 GT", "456 GTA", "456M GT",
  "456M GTA", "612 Scaglietti", "FF", "GTC4Lusso", "GTC4Lusso T",
  "Purosangue"
)

central_2plus2 <- c(
  "Dino 308 GT4", "Dino 208 GT4", "Mondial 8",
  "Mondial Quattrovalvole", "Mondial Cabriolet", "3.2 Mondial",
  "3.2 Mondial Cabriolet", "Mondial T", "Mondial T Cabriolet"
)

flat12_supercars <- c(
  "365 GT4 BB", "512 BB", "512 BBi", "Testarossa", "512 TR", "F512 M"
)

halo_models <- c(
  "GTO", "F40", "F50", "Enzo Ferrari", "LaFerrari", "LaFerrari Aperta",
  "F80"
)

hybrid_supercars <- c(
  "SF90 Stradale", "SF90 Spider", "SF90 XX Stradale", "SF90 XX Spider",
  "849 Testarossa"
)

icona_models <- c("Monza SP1", "Monza SP2", "Daytona SP3")

front_v8_gt <- c(
  "Ferrari California", "Ferrari California 30", "Ferrari California T",
  "Ferrari Portofino", "Ferrari Portofino M", "Ferrari Roma",
  "Ferrari Roma Spider", "Ferrari Amalfi"
)

mid_engine_sports <- c(
  "Dino 206 GT", "Dino 246 GT", "Dino 246 GTS",
  "308 GTB", "308 GTS", "208 GTB", "208 GTS", "308 GTBi", "308 GTSi",
  "208 GTB Turbo", "208 GTS Turbo", "308 GTB Quattrovalvole",
  "308 GTS Quattrovalvole", "208 GTS Turbo", "328 GTB", "328 GTS",
  "GTB Turbo", "GTS Turbo", "348 TB", "348 TS", "348 GTB", "348 GTS",
  "348 Spider", "F355 Berlinetta", "F355 GTS", "F355 Spider",
  "355 F1 Berlinetta", "355 F1 GTS", "355 F1 Spider", "360 Modena",
  "360 spider", "Challenge Stradale", "F430", "F430 Spider",
  "430 Scuderia", "Scuderia Spider 16M", "Ferrari 458 Italia",
  "458 Spider", "458 Speciale", "458 Speciale A", "488 GTB",
  "488 Spider", "Ferrari 488 Pista", "488 Pista Spider",
  "F8 Tributo", "F8 Spider", "296 GTB", "296 GTS", "296 Speciale",
  "296 Speciale A"
)

track_only_models <- c("FXX", "599XX", "FXX K")

competition_derived_pattern <- stringr::regex(
  paste(
    "Competizione|Challenge|\\bGT\\b|\\bGTC\\b|\\bGT3\\b|\\bGTE\\b|",
    "BB LM|F40 Competizione|F50 GT|FXX|599XX"
  ),
  ignore_case = TRUE
)

known_2plus2 <- union(v12_2plus2, central_2plus2)
known_two_seat_front_v12 <- c(
  "365 GTB4", "365 GTS4", "550 Maranello", "550 Barchetta Pininfarina",
  "575M Maranello", "Superamerica", "599 GTB Fiorano", "599 GTO",
  "SA APERTA", "F12berlinetta", "F12tdf", "812 Superfast", "812 GTS",
  "812 Competizione", "812 Competizione A", "12Cilindri",
  "12Cilindri Spider"
)
known_hybrids <- c(
  "LaFerrari", "LaFerrari Aperta", "FXX K", hybrid_supercars,
  "296 GTB", "296 GTS", "296 Challenge", "296 Speciale", "296 Speciale A",
  "F80"
)

# Base taxonomy -----------------------------------------------------------
ferrari_classified <- ferrari_models |>
  dplyr::mutate(
    model_id = paste(
      "ferrari",
      year,
      stringr::str_extract(url, "[^/]+$") |>
        stringr::str_replace_all("-", "_"),
      sep = "_"
    ),
    start_year = as.integer(year),
    end_year = NA_integer_,
    engine_architecture = dplyr::case_when(
      engine %in% c("V12", "V8", "V6", "V4", "I4") ~ engine,
      engine == "4-cyl." ~ "I4",
      engine == "6-cyl." ~ "I6",
      model %in% c(
        "GTC4Lusso T", "F8 Tributo", "Ferrari Roma",
        "Ferrari Roma Spider"
      ) ~ "V8",
      TRUE ~ NA_character_
    ),
    engine_type = dplyr::case_when(
      model %in% known_hybrids ~ "hybrid",
      model %in% c(
        "GTC4Lusso T", "F8 Tributo", "Ferrari Roma",
        "Ferrari Roma Spider"
      ) ~ "combustion",
      !is.na(engine) ~ "combustion",
      TRUE ~ NA_character_
    ),
    engine_location = dplyr::case_when(
      engine_position == "front" ~ "front",
      engine_position == "rear" ~ "mid",
      model %in% c(front_v8_gt, v12_2plus2, "Purosangue") ~ "front",
      model %in% c(
        mid_engine_sports, flat12_supercars, halo_models,
        hybrid_supercars, icona_models
      ) ~ "mid",
      TRUE ~ NA_character_
    ),
    seating_configuration = dplyr::case_when(
      model == "Monza SP1" ~ "1",
      model == "Purosangue" ~ "4",
      model %in% known_2plus2 ~ "2+2",
      category == "Sport Prototype" ~ "race-specific",
      model %in% front_v8_gt ~ "2+",
      model %in% known_two_seat_front_v12 ~ "2",
      model %in% c(
        halo_models, hybrid_supercars, icona_models, mid_engine_sports,
        flat12_supercars, "812 Competizione", "812 Competizione A",
        "12Cilindri", "12Cilindri Spider"
      ) ~ "2",
      TRUE ~ NA_character_
    ),
    main_family = dplyr::case_when(
      category == "Sport Prototype" &
        stringr::str_detect(model, competition_derived_pattern) ~
        "Competition derivatives",
      category == "Sport Prototype" ~ "Sports-racing prototypes",
      model %in% halo_models ~ "Limited-series halo",
      model %in% hybrid_supercars ~ "Hybrid supercar",
      model %in% icona_models ~ "Icona limited series",
      model %in% flat12_supercars ~ "Mid-engined 12-cylinder supercar",
      model %in% central_2plus2 ~ "Mid-engined 2+2 GT",
      model %in% front_v8_gt ~ "Front-engined V8 GT",
      model %in% mid_engine_sports ~ "Mid-engined sports car",
      model %in% c(v12_2plus2, "Purosangue") ~ "Front-engined V12 2+2 GT",
      category == "Gran Turismo" & engine == "V12" ~
        "Front-engined V12 two-seat GT",
      category == "Gran Turismo" & engine %in% c("V8", "V6") ~
        "Mid-engined sports car",
      TRUE ~ "Sports-racing prototypes"
    ),
    subfamily = dplyr::case_when(
      model %in% track_only_models ~ "XX Programme",
      category == "Sport Prototype" &
        stringr::str_detect(model, regex("Challenge", ignore_case = TRUE)) ~
        "Ferrari Challenge",
      category == "Sport Prototype" &
        stringr::str_detect(model, regex("GT3|GTE|GTC|\\bGT\\b|Competizione|BB LM", ignore_case = TRUE)) ~
        "Competition GT",
      category == "Sport Prototype" & year <= 1960 ~ "Front-engined sports prototype",
      category == "Sport Prototype" & year > 1960 ~ "Mid-engined sports prototype",
      model %in% halo_models ~ "Big Five / halo",
      model %in% hybrid_supercars ~ "High-performance AWD hybrid",
      model %in% icona_models ~ "Icona",
      model %in% c("812 Competizione", "812 Competizione A") ~
        "Special series",
      model %in% flat12_supercars ~ "Berlinetta Boxer / Testarossa",
      model %in% central_2plus2 ~ "Dino GT4 / Mondial",
      model %in% front_v8_gt &
        stringr::str_detect(model, regex("California|Portofino", ignore_case = TRUE)) ~
        "Retractable-hardtop GT",
      model %in% front_v8_gt ~ "Roma / Amalfi",
      model == "Purosangue" ~ "Four-door performance car",
      model %in% mid_engine_sports &
        stringr::str_detect(model, regex("Spider|GTS|A$", ignore_case = TRUE)) ~
        "Spider / targa",
      model %in% mid_engine_sports &
        stringr::str_detect(model, regex("Speciale|Scuderia|Pista|Challenge Stradale", ignore_case = TRUE)) ~
        "Special series",
      model %in% mid_engine_sports ~ "Berlinetta",
      model %in% v12_2plus2 ~ "Four-seat grand tourer",
      model %in% c("250 California", "250 GT Cabriolet", "275 GTS", "330 GTS",
                   "365 California", "365 GTS", "365 GTS4",
                   "550 Barchetta Pininfarina", "Superamerica", "SA APERTA",
                   "812 GTS", "12Cilindri Spider") ~ "Spider / cabriolet",
      TRUE ~ "Berlinetta / coupe"
    ),
    orientation = dplyr::case_when(
      model %in% track_only_models ~ "track-only",
      category == "Sport Prototype" ~ "competition",
      model %in% halo_models ~ "halo",
      model %in% hybrid_supercars ~ "supercar",
      model %in% flat12_supercars ~ "supercar",
      model %in% icona_models ~ "limited-series",
      main_family %in% c(
        "Front-engined V12 two-seat GT", "Front-engined V12 2+2 GT",
        "Front-engined V8 GT", "Mid-engined 2+2 GT"
      ) ~ "grand tourer",
      main_family == "Mid-engined sports car" ~ "sports car",
      TRUE ~ NA_character_
    ),
    base_model = NA_character_,
    base_model_year = NA_integer_,
    lineage = NA_character_,
    generation = NA_integer_,
    predecessor = NA_character_,
    predecessor_year = NA_integer_,
    predecessor_relationship = NA_character_,
    successor = NA_character_,
    successor_year = NA_integer_,
    successor_relationship = NA_character_,
    alternative_predecessor = NA_character_,
    alternative_relationship = NA_character_,
    taxonomy_confidence = dplyr::case_when(
      category == "Sport Prototype" & year < 1970 ~ "medium",
      is.na(seating_configuration) | is.na(engine_architecture) ~ "low",
      TRUE ~ "high"
    ),
    taxonomy_notes = NA_character_
  )

# Manually curated production periods ------------------------------------
# Missing rows remain unknown. Low-confidence seed dates stay visible but
# are explicitly marked as pending source review in the output.
ferrari_classified <- ferrari_classified |>
  dplyr::select(-end_year) |>
  dplyr::left_join(
    production_periods,
    by = c("model", "year" = "source_year")
  ) |>
  dplyr::mutate(
    start_year = dplyr::coalesce(production_start_year, start_year),
    end_year = production_end_year,
    production_status = dplyr::coalesce(production_status, "unknown"),
    period_confidence = dplyr::coalesce(period_confidence, "unreviewed"),
    period_review_status = dplyr::coalesce(
      period_review_status,
      "not_in_manual_file"
    )
  ) |>
  dplyr::select(-production_start_year, -production_end_year)

# Core generational lineages ---------------------------------------------
# The relationship describes how each row relates to its predecessor.
lineage_nodes <- tibble::tribble(
  ~model, ~year, ~lineage, ~generation, ~predecessor, ~predecessor_year, ~predecessor_relationship,
  "308 GTB", 1975L, "Mid-engined V8 sports cars", 1L, NA, NA, NA,
  "328 GTB", 1985L, "Mid-engined V8 sports cars", 2L, "308 GTB", 1975L, "evolution",
  "348 TB", 1989L, "Mid-engined V8 sports cars", 3L, "328 GTB", 1985L, "direct",
  "F355 Berlinetta", 1994L, "Mid-engined V8 sports cars", 4L, "348 TB", 1989L, "evolution",
  "360 Modena", 1999L, "Mid-engined V8 sports cars", 5L, "F355 Berlinetta", 1994L, "direct",
  "F430", 2004L, "Mid-engined V8 sports cars", 6L, "360 Modena", 1999L, "evolution",
  "Ferrari 458 Italia", 2009L, "Mid-engined V8 sports cars", 7L, "F430", 2004L, "direct",
  "488 GTB", 2015L, "Mid-engined V8 sports cars", 8L, "Ferrari 458 Italia", 2009L, "evolution",
  "F8 Tributo", 2019L, "Mid-engined V8 sports cars", 9L, "488 GTB", 2015L, "evolution",
  "296 GTB", 2021L, "Mid-engined sports cars", 10L, "F8 Tributo", 2019L, "spiritual_successor",

  "365 GT4 BB", 1971L, "Boxer / Testarossa", 1L, NA, NA, NA,
  "512 BB", 1976L, "Boxer / Testarossa", 2L, "365 GT4 BB", 1971L, "evolution",
  "512 BBi", 1981L, "Boxer / Testarossa", 3L, "512 BB", 1976L, "evolution",
  "Testarossa", 1984L, "Boxer / Testarossa", 4L, "512 BBi", 1981L, "direct",
  "512 TR", 1991L, "Boxer / Testarossa", 5L, "Testarossa", 1984L, "evolution",
  "F512 M", 1994L, "Boxer / Testarossa", 6L, "512 TR", 1991L, "evolution",

  "365 GTB4", 1968L, "Front-engined V12 two-seat GT", 1L, NA, NA, NA,
  "550 Maranello", 1996L, "Front-engined V12 two-seat GT", 2L, "365 GTB4", 1968L, "spiritual_successor",
  "575M Maranello", 2002L, "Front-engined V12 two-seat GT", 3L, "550 Maranello", 1996L, "evolution",
  "599 GTB Fiorano", 2006L, "Front-engined V12 two-seat GT", 4L, "575M Maranello", 2002L, "direct",
  "F12berlinetta", 2012L, "Front-engined V12 two-seat GT", 5L, "599 GTB Fiorano", 2006L, "direct",
  "812 Superfast", 2017L, "Front-engined V12 two-seat GT", 6L, "F12berlinetta", 2012L, "direct",
  "12Cilindri", 2024L, "Front-engined V12 two-seat GT", 7L, "812 Superfast", 2017L, "direct",

  "250 GT 2+2", 1960L, "V12 GT 2+2", 1L, NA, NA, NA,
  "330 GT 2+2", 1964L, "V12 GT 2+2", 2L, "250 GT 2+2", 1960L, "direct",
  "365 GT 2+2", 1967L, "V12 GT 2+2", 3L, "330 GT 2+2", 1964L, "direct",
  "365 GT4 2+2", 1972L, "V12 GT 2+2", 4L, "365 GT 2+2", 1967L, "direct",
  "400 GT", 1976L, "V12 GT 2+2", 5L, "365 GT4 2+2", 1972L, "evolution",
  "400 GTi", 1979L, "V12 GT 2+2", 6L, "400 GT", 1976L, "evolution",
  "412", 1985L, "V12 GT 2+2", 7L, "400 GTi", 1979L, "evolution",
  "456 GT", 1992L, "V12 GT 2+2", 8L, "412", 1985L, "direct",
  "456M GT", 1998L, "V12 GT 2+2", 9L, "456 GT", 1992L, "evolution",
  "612 Scaglietti", 2004L, "V12 GT 2+2", 10L, "456M GT", 1998L, "direct",
  "FF", 2011L, "V12 GT 2+2", 11L, "612 Scaglietti", 2004L, "direct",
  "GTC4Lusso", 2016L, "V12 GT 2+2", 12L, "FF", 2011L, "evolution",
  "Purosangue", 2022L, "Front-engined V12 four-seat", 13L, "GTC4Lusso", 2016L, "spiritual_successor",

  "Dino 308 GT4", 1973L, "Mid-engined V8 2+2", 1L, NA, NA, NA,
  "Mondial 8", 1980L, "Mid-engined V8 2+2", 2L, "Dino 308 GT4", 1973L, "direct",
  "Mondial Quattrovalvole", 1982L, "Mid-engined V8 2+2", 3L, "Mondial 8", 1980L, "evolution",
  "3.2 Mondial", 1985L, "Mid-engined V8 2+2", 4L, "Mondial Quattrovalvole", 1982L, "evolution",
  "Mondial T", 1989L, "Mid-engined V8 2+2", 5L, "3.2 Mondial", 1985L, "evolution",

  "Ferrari California", 2008L, "California / Portofino", 1L, NA, NA, NA,
  "Ferrari California 30", 2012L, "California / Portofino", 2L, "Ferrari California", 2008L, "evolution",
  "Ferrari California T", 2014L, "California / Portofino", 3L, "Ferrari California 30", 2012L, "evolution",
  "Ferrari Portofino", 2017L, "California / Portofino", 4L, "Ferrari California T", 2014L, "direct",
  "Ferrari Portofino M", 2020L, "California / Portofino", 5L, "Ferrari Portofino", 2017L, "evolution",

  "Ferrari Roma", 2019L, "Front-engined V8 coupes", 1L, NA, NA, NA,
  "Ferrari Amalfi", 2025L, "Front-engined V8 coupes", 2L, "Ferrari Roma", 2019L, "direct",

  "GTO", 1984L, "Ferrari halo lineage", 1L, NA, NA, NA,
  "F40", 1987L, "Ferrari halo lineage", 2L, "GTO", 1984L, "spiritual_successor",
  "F50", 1995L, "Ferrari halo lineage", 3L, "F40", 1987L, "spiritual_successor",
  "Enzo Ferrari", 2002L, "Ferrari halo lineage", 4L, "F50", 1995L, "spiritual_successor",
  "LaFerrari", 2013L, "Ferrari halo lineage", 5L, "Enzo Ferrari", 2002L, "spiritual_successor",
  "F80", 2024L, "Ferrari halo lineage", 6L, "LaFerrari", 2013L, "spiritual_successor",

  "348 Challenge", 1993L, "Ferrari Challenge", 1L, NA, NA, NA,
  "F355 Challenge", 1995L, "Ferrari Challenge", 2L, "348 Challenge", 1993L, "direct",
  "360 Challenge", 2000L, "Ferrari Challenge", 3L, "F355 Challenge", 1995L, "direct",
  "F430 Challenge", 2006L, "Ferrari Challenge", 4L, "360 Challenge", 2000L, "direct",
  "458 Challenge", 2010L, "Ferrari Challenge", 5L, "F430 Challenge", 2006L, "direct",
  "458 Challenge EVO", 2014L, "Ferrari Challenge", 6L, "458 Challenge", 2010L, "evolution",
  "488 Challenge", 2016L, "Ferrari Challenge", 7L, "458 Challenge EVO", 2014L, "direct",
  "296 Challenge", 2023L, "Ferrari Challenge", 8L, "488 Challenge", 2016L, "direct",

  "250 P", 1963L, "P-series sports prototypes", 1L, NA, NA, NA,
  "275 P", 1964L, "P-series sports prototypes", 2L, "250 P", 1963L, "evolution",
  "330 P2", 1965L, "P-series sports prototypes", 3L, "275 P", 1964L, "evolution",
  "330 P3", 1966L, "P-series sports prototypes", 4L, "330 P2", 1965L, "evolution",
  "330 P4", 1967L, "P-series sports prototypes", 5L, "330 P3", 1966L, "evolution",

  "333 SP", 1994L, "Top-class endurance prototypes", 1L, NA, NA, NA,
  "499P", 2023L, "Top-class endurance prototypes", 2L, "333 SP", 1994L, "spiritual_successor",

  "SF90 Stradale", 2019L, "Hybrid production supercars", 1L, NA, NA, NA,
  "849 Testarossa", 2025L, "Hybrid production supercars", 2L, "SF90 Stradale", 2019L, "direct",

  "Monza SP1", 2018L, "Icona", 1L, NA, NA, NA,
  "Monza SP2", 2018L, "Icona", 1L, NA, NA, NA,
  "Daytona SP3", 2021L, "Icona", 2L, "Monza SP2", 2018L, "spiritual_successor",

  "FXX", 2005L, "XX Programme", 1L, NA, NA, NA,
  "599XX", 2010L, "XX Programme", 2L, "FXX", 2005L, "spiritual_successor",
  "FXX K", 2014L, "XX Programme", 3L, "599XX", 2010L, "spiritual_successor"
)

# Derivatives and parallel body styles -----------------------------------
derivatives <- tibble::tribble(
  ~model, ~year, ~base_model, ~base_model_year, ~lineage,
  "Dino 246 GTS", 1972L, "Dino 246 GT", 1969L, "Mid-engined Dino V6",
  "308 GTS", 1977L, "308 GTB", 1975L, "Mid-engined V8 sports cars",
  "308 GTBi", 1980L, "308 GTB", 1975L, "Mid-engined V8 sports cars",
  "308 GTSi", 1980L, "308 GTS", 1977L, "Mid-engined V8 sports cars",
  "308 GTB Quattrovalvole", 1982L, "308 GTB", 1975L, "Mid-engined V8 sports cars",
  "308 GTS Quattrovalvole", 1982L, "308 GTS", 1977L, "Mid-engined V8 sports cars",
  "328 GTS", 1985L, "328 GTB", 1985L, "Mid-engined V8 sports cars",
  "348 TS", 1989L, "348 TB", 1989L, "Mid-engined V8 sports cars",
  "348 Spider", 1993L, "348 TB", 1989L, "Mid-engined V8 sports cars",
  "F355 GTS", 1994L, "F355 Berlinetta", 1994L, "Mid-engined V8 sports cars",
  "F355 Spider", 1995L, "F355 Berlinetta", 1994L, "Mid-engined V8 sports cars",
  "355 F1 Berlinetta", 1997L, "F355 Berlinetta", 1994L, "Mid-engined V8 sports cars",
  "360 spider", 2000L, "360 Modena", 1999L, "Mid-engined V8 sports cars",
  "Challenge Stradale", 2003L, "360 Modena", 1999L, "Mid-engined V8 sports cars",
  "F430 Spider", 2005L, "F430", 2004L, "Mid-engined V8 sports cars",
  "430 Scuderia", 2007L, "F430", 2004L, "Mid-engined V8 sports cars",
  "Scuderia Spider 16M", 2008L, "430 Scuderia", 2007L, "Mid-engined V8 sports cars",
  "458 Spider", 2011L, "Ferrari 458 Italia", 2009L, "Mid-engined V8 sports cars",
  "458 Speciale", 2013L, "Ferrari 458 Italia", 2009L, "Mid-engined V8 sports cars",
  "458 Speciale A", 2014L, "458 Speciale", 2013L, "Mid-engined V8 sports cars",
  "488 Spider", 2016L, "488 GTB", 2015L, "Mid-engined V8 sports cars",
  "Ferrari 488 Pista", 2018L, "488 GTB", 2015L, "Mid-engined V8 sports cars",
  "488 Pista Spider", 2018L, "Ferrari 488 Pista", 2018L, "Mid-engined V8 sports cars",
  "F8 Spider", 2019L, "F8 Tributo", 2019L, "Mid-engined V8 sports cars",
  "296 GTS", 2022L, "296 GTB", 2021L, "Mid-engined sports cars",
  "296 Speciale", 2025L, "296 GTB", 2021L, "Mid-engined sports cars",
  "296 Speciale A", 2025L, "296 Speciale", 2025L, "Mid-engined sports cars",
  "550 Barchetta Pininfarina", 2000L, "550 Maranello", 1996L, "Front-engined V12 two-seat GT",
  "Superamerica", 2005L, "575M Maranello", 2002L, "Front-engined V12 two-seat GT",
  "599 GTO", 2010L, "599 GTB Fiorano", 2006L, "Front-engined V12 two-seat GT",
  "SA APERTA", 2010L, "599 GTB Fiorano", 2006L, "Front-engined V12 two-seat GT",
  "F12tdf", 2015L, "F12berlinetta", 2012L, "Front-engined V12 two-seat GT",
  "812 GTS", 2019L, "812 Superfast", 2017L, "Front-engined V12 two-seat GT",
  "812 Competizione", 2021L, "812 Superfast", 2017L, "Front-engined V12 two-seat GT",
  "812 Competizione A", 2021L, "812 Competizione", 2021L, "Front-engined V12 two-seat GT",
  "12Cilindri Spider", 2024L, "12Cilindri", 2024L, "Front-engined V12 two-seat GT",
  "400 Automatic", 1976L, "400 GT", 1976L, "V12 GT 2+2",
  "400 Automatic i", 1979L, "400 GTi", 1979L, "V12 GT 2+2",
  "456 GTA", 1996L, "456 GT", 1992L, "V12 GT 2+2",
  "456M GTA", 1998L, "456M GT", 1998L, "V12 GT 2+2",
  "GTC4Lusso T", 2016L, "GTC4Lusso", 2016L, "V12 GT 2+2",
  "Mondial Cabriolet", 1983L, "Mondial Quattrovalvole", 1982L, "Mid-engined V8 2+2",
  "3.2 Mondial Cabriolet", 1985L, "3.2 Mondial", 1985L, "Mid-engined V8 2+2",
  "Mondial T Cabriolet", 1989L, "Mondial T", 1989L, "Mid-engined V8 2+2",
  "LaFerrari Aperta", 2016L, "LaFerrari", 2013L, "Ferrari halo lineage",
  "SF90 Spider", 2020L, "SF90 Stradale", 2019L, "SF90",
  "SF90 XX Stradale", 2023L, "SF90 Stradale", 2019L, "Hybrid production supercars",
  "SF90 XX Spider", 2023L, "SF90 XX Stradale", 2023L, "Hybrid production supercars",
  "Ferrari Roma Spider", 2023L, "Ferrari Roma", 2019L, "Roma",
  "512 M", 1970L, "512 S", 1970L, "512-series sports prototypes",
  "458 Challenge EVO", 2014L, "458 Challenge", 2010L, "Ferrari Challenge",
  "360 GT", 2002L, "360 Modena", 1999L, "Competition GT",
  "360 GTC", 2004L, "360 Modena", 1999L, "Competition GT",
  "F430 GTC", 2006L, "F430", 2004L, "Competition GT",
  "488 GT3", 2016L, "488 GTB", 2015L, "Competition GT",
  "488 GTE", 2016L, "488 GTB", 2015L, "Competition GT",
  "296 GT3", 2022L, "296 GTB", 2021L, "Competition GT",
  "499P Modificata", 2023L, "499P", 2023L, "Top-class endurance prototypes",
  "F40 Competizione", 1989L, "F40", 1987L, "Competition GT",
  "F50 GT", 1996L, "F50", 1995L, "Competition GT",
  "575 GTC", 2003L, "575M Maranello", 2002L, "Competition GT",
  "512 BB LM", 1978L, "512 BB", 1976L, "Competition GT",
  "365 GTB4 Competizione", 1971L, "365 GTB4", 1968L, "Competition GT"
)

# Add successor to each core node by reversing the predecessor relation.
successors <- lineage_nodes |>
  dplyr::filter(!is.na(predecessor)) |>
  dplyr::mutate(
    child_model = model,
    child_year = year
  ) |>
  dplyr::transmute(
    model = predecessor,
    year = predecessor_year,
    successor = child_model,
    successor_year = child_year,
    successor_relationship = predecessor_relationship
  )

lineage_enrichment <- lineage_nodes |>
  dplyr::left_join(successors, by = c("model", "year"))

ferrari_enriched <- ferrari_classified |>
  dplyr::select(
    -lineage, -generation, -predecessor, -predecessor_year,
    -predecessor_relationship, -successor, -successor_year,
    -successor_relationship
  ) |>
  dplyr::left_join(lineage_enrichment, by = c("model", "year")) |>
  dplyr::select(-base_model, -base_model_year) |>
  dplyr::left_join(derivatives, by = c("model", "year"), suffix = c("", "_derivative")) |>
  dplyr::mutate(
    lineage = dplyr::coalesce(lineage_derivative, lineage),
    predecessor = dplyr::coalesce(base_model, predecessor),
    predecessor_year = dplyr::coalesce(base_model_year, predecessor_year),
    predecessor_relationship = dplyr::if_else(
      !is.na(base_model),
      "derivative",
      predecessor_relationship
    ),
    alternative_predecessor = dplyr::if_else(
      model == "550 Maranello",
      "F512 M",
      alternative_predecessor
    ),
    alternative_relationship = dplyr::if_else(
      model == "550 Maranello",
      "direct",
      alternative_relationship
    ),
    taxonomy_notes = dplyr::case_when(
      model == "550 Maranello" ~ paste(
        "Direct commercial successor to the F512 M, but a spiritual successor",
        "to the 365 GTB4 through its return to a front-engined two-seat V12."
      ),
      model == "365 GTB4" ~ paste(
        "The link to the 550 Maranello is spiritual: the line was interrupted",
        "while Ferrari's flagship role passed through the BB/Testarossa family."
      ),
      model == "Ferrari Roma Spider" ~ paste(
        "Derived from the Roma. Its market relationship with the Portofino M",
        "is not represented as direct technical succession."
      ),
      model == "GTC4Lusso" ~ paste(
        "The Purosangue later occupies part of its role, but changes body style",
        "and concept; the relationship is treated as spiritual succession."
      ),
      model == "F8 Tributo" ~ paste(
        "The 296 GTB continues the mid-engined sports-car spirit, but changes",
        "from a V8 to a hybrid V6; it is not treated as a technical evolution."
      ),
      TRUE ~ taxonomy_notes
    ),
    base_model = base_model,
    base_model_year = base_model_year
  ) |>
  dplyr::select(-lineage_derivative)

# External successors absent from the source snapshot --------------------
external_successors <- tibble::tribble(
  ~model, ~year, ~external_successor, ~external_successor_year, ~external_successor_relationship,
  "F512 M", 1994L, "550 Maranello", 1996L, "direct",
  "F8 Tributo", 2019L, "296 GTB", 2021L, "spiritual_successor",
  "GTC4Lusso", 2016L, "Purosangue", 2022L, "spiritual_successor",
  "LaFerrari", 2013L, "F80", 2024L, "spiritual_successor",
  "488 Challenge", 2016L, "296 Challenge", 2023L, "direct"
)

ferrari_enriched <- ferrari_enriched |>
  dplyr::left_join(external_successors, by = c("model", "year")) |>
  dplyr::mutate(
    successor = dplyr::coalesce(successor, external_successor),
    successor_year = dplyr::coalesce(successor_year, external_successor_year),
    successor_relationship = dplyr::coalesce(
      successor_relationship,
      external_successor_relationship
    ),
    successor_in_dataset = dplyr::case_when(
      is.na(successor) ~ NA,
      TRUE ~ successor %in% ferrari_models$model
    ),
    external_enrichment_required =
      (is.na(end_year) & production_status != "active") |
      period_confidence %in% c("low", "unreviewed") |
      is.na(seating_configuration) |
      taxonomy_confidence != "high",
    start_year_source = dplyr::if_else(
      period_review_status == "not_in_manual_file",
      "ferrari_past_models.csv",
      "ferrari_production_periods.csv"
    ),
    taxonomy_source = dplyr::case_when(
      !is.na(lineage) | !is.na(base_model) ~
        "curated Ferrari.com evidence + repository description",
      TRUE ~ "rules based on repository data"
    ),
    source_category = dplyr::recode(
      category,
      "Gran Turismo" = "Grand Touring",
      "Sport Prototype" = "Sports Prototype"
    )
  ) |>
  dplyr::select(
    -external_successor, -external_successor_year,
    -external_successor_relationship
  ) |>
  dplyr::relocate(
    model_id, model, start_year, end_year,
    production_status, period_source_name, period_source_url,
    period_source_type, period_confidence, period_review_status,
    period_notes, period_reviewed_at,
    main_family, subfamily, lineage, generation,
    engine_architecture, engine_type, engine_location,
    seating_configuration, orientation,
    predecessor, predecessor_year, predecessor_relationship,
    successor, successor_year, successor_relationship, successor_in_dataset,
    alternative_predecessor, alternative_relationship,
    base_model, base_model_year,
    taxonomy_confidence, external_enrichment_required,
    taxonomy_notes, start_year_source, taxonomy_source
  ) |>
  dplyr::rename(source_year = year) |>
  dplyr::select(-category) |>
  dplyr::relocate(source_year, source_category, .after = end_year) |>
  dplyr::arrange(start_year, main_family, model)

if (!keep_image_data_uri) {
  ferrari_enriched <- ferrari_enriched |>
    dplyr::select(-dplyr::any_of("image_data_uri"))
}

# checks -----------------------------------------------------------------
if (nrow(ferrari_enriched) != nrow(ferrari_models)) {
  cli::cli_abort("Enrichment changed row count.")
}

if (anyDuplicated(ferrari_enriched$model_id)) {
  cli::cli_abort("model_id is not unique.")
}

unexpected_families <- setdiff(unique(ferrari_enriched$main_family), families)
if (length(unexpected_families) > 0) {
  cli::cli_abort(
    "Unexpected main families: {paste(unexpected_families, collapse = ', ')}"
  )
}

observed_relationships <- ferrari_enriched |>
  dplyr::select(
    predecessor_relationship,
    successor_relationship,
    alternative_relationship
  ) |>
  unlist(use.names = FALSE) |>
  unique() |>
  stats::na.omit()

unexpected_relationships <- setdiff(observed_relationships, relationship_types)
if (length(unexpected_relationships) > 0) {
  cli::cli_abort(
    "Unexpected relationship types: {paste(unexpected_relationships, collapse = ', ')}"
  )
}

ferrari_enriched |>
  dplyr::count(main_family, sort = TRUE) |>
  print(n = Inf)

ferrari_enriched |>
  dplyr::summarise(
    rows = dplyr::n(),
    pct_end_year = round(mean(!is.na(end_year)) * 100, 1),
    pct_lineage = round(mean(!is.na(lineage)) * 100, 1),
    pct_predecessor = round(mean(!is.na(predecessor)) * 100, 1),
    pct_successor = round(mean(!is.na(successor)) * 100, 1),
    pct_base_model = round(mean(!is.na(base_model)) * 100, 1)
  ) |>
  print()

# save -------------------------------------------------------------------
readr::write_csv(ferrari_enriched, out_csv, na = "")
