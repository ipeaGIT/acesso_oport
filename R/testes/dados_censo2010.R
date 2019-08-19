library(tidyverse)
library(sf)
library(mapview)

# abrir

# escolher os do ceara

ceara <- dir("data-raw/ceara/", full.names = T, pattern = ".shp$")

# testes

vai <- st_read(ceara[2])

data <- ceara %>% 
  map_dfr(st_read, crs = 4326)



data %>%
  slice(1:200) %>%
  mapview()

