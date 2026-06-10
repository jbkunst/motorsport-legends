# setup ------------------------------------------------------------------
source(here::here("R/rsc/00-rsc-helpers.R"))

library(dplyr)
library(purrr)
library(tibble)

# data -------------------------------------------------------------------
races_other <- tribble(
  ~track,          ~year, ~event,                            ~reason,                                      ~url,
  "nurburgring",   1960, "Nürburgring 1000 Kilometres",      "Maserati Tipo 61 #5 winner",                 "https://www.racingsportscars.com/photo/Nurburgring-1960-06-22.html?sort=Results",
  "riverside",     1960, "Grand Prix Riverside 200 Miles",   "Maserati Tipo 61 #98 Carroll Shelby",        "https://www.racingsportscars.com/photo/Riverside-1960-10-16.html?sort=Results",
  "laguna_seca",   1981, "Laguna Seca",                      "Porsche 935/78 Moby Dick MOMO",              "https://www.racingsportscars.com/photo/Laguna_Seca-1981-05-03.html?sort=Results",
  "sebring",       1971, "Sebring 12 Hours",                 "Porsche 917 K winner",                       "https://www.racingsportscars.com/photo/Sebring-1971-03-20.html?sort=Results",
  "sebring",       1973, "Sebring 12 Hours",                 "Porsche 911 Carrera RSR winner",             "https://www.racingsportscars.com/photo/Sebring-1973-03-24.html?sort=Results",
  "watkins_glen",  1981, "Watkins Glen 6 Hours",             "Lancia Beta Montecarlo Turbo CM's 1:64",     "https://www.racingsportscars.com/photo/Watkins_Glen-1981-07-12.html?sort=Results",
  "norisring",     1977, "Norisring Trophy",                 "Porsche 935 #52 Jägermeister Kyosho",        "https://www.racingsportscars.com/photo/Norisring-1977-07-03t.html?sort=Results",
  "nurburgring",   1977, "Nürburgring 300 Kilometres",       "BMW 320i Gr.5 #15 Jägermeister Kyosho",      "https://www.racingsportscars.com/photo/Nurburgring-1977-03-27.html?sort=Results",
  "nurburgring",   1976, "Nürburgring 1000 Kilometres",      "Porsche 934 #24 Jägermeister Kyosho",        "https://www.racingsportscars.com/photo/Nurburgring-1976-05-30.html?sort=Results",
  "nurburgring",   1976, "Nürburgring 300 Kilometres",       "Porsche 934 #53 Jägermeister Kyosho",        "https://www.racingsportscars.com/photo/Nurburgring-1976-04-04.html?sort=Results"
)

# scrape -----------------------------------------------------------------
walk(races_other$url, scrape_race)