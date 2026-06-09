# setup ------------------------------------------------------------------
source(here::here("R/00-helpers.R"))

# data -------------------------------------------------------------------
urls <- c(

  # Nürburgring 1000 Kilometres 1960: Maserati tipo 61 #5 winner
  "https://www.racingsportscars.com/photo/Nurburgring-1960-06-22.html?sort=Results",

  # Grand Prix Riverside 200 Miles 1960: Maserati tipo 61 #98 driver Carroll Shelby
  "https://www.racingsportscars.com/photo/Riverside-1960-10-16.html?sort=Results",
  
  # Porsche 930/78 Moby Dick Momo 8th
  "https://www.racingsportscars.com/photo/Laguna_Seca-1981-05-03.html?sort=Results",

  # Porsche 917 K, winner 
  "https://www.racingsportscars.com/photo/Sebring-1971-03-20.html?sort=Results",

  # Porsche 911 Carrera RSR Winner
  "https://www.racingsportscars.com/photo/Sebring-1973-03-24.html?sort=Results",

  # Lancia Beta Montecarlo Turbo (164 CM's)
  "https://www.racingsportscars.com/photo/Watkins_Glen-1981-07-12.html?sort=Results",

  # Norisring Trophy 1977; Kyosho Porsche 935 #52 Jägermeister 
  "https://www.racingsportscars.com/photo/Norisring-1977-07-03t.html?sort=Results",

  # Nurburgring 300 Kilometres 1977: Kyosho BMW 320i Gr.5  #15 Jägermeister. Intenresante que tambien participa el porsche 935 #52
  "https://www.racingsportscars.com/photo/Nurburgring-1977-03-27.html?sort=Results",

  # Nürburgring 1000 Kilometres 1976: Kyosho Porsche 934 #24 Jägermeister
  "https://www.racingsportscars.com/photo/Nurburgring-1976-05-30.html?sort=Results",

  # Nürburgring 300 Kilometres 1976, Kyosho  Porsche 934 #53 Jägermeister
  "https://www.racingsportscars.com/photo/Nurburgring-1976-04-04.html?sort=Results"

)

walk(urls, scrape_race)