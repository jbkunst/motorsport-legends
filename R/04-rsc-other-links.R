# setup ------------------------------------------------------------------
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
urls <- c(
  # Porsche 930/78 Moby Dick Momo
  "https://www.racingsportscars.com/photo/Laguna_Seca-1981-05-03.html?sort=Results"
)

walk(urls, scrape_race)