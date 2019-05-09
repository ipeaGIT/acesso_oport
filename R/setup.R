
Sys.setenv(TZ='UTC') 

library(sp)
library(ggplot2)
library(dplyr)
library(sf)
library(fasttime)
# library(mapview)
#library(ggmap) #função geocode() pra extrair as coordenadas dos endereços
library(sf) #pra importar os dados espaciais e tal
library(data.table)
library(knitr)
library(readr)
library(tidyr)
# library(hrbrthemes)
library(stringr)
# library(leaflet.minicharts)
library(purrr)
library(lubridate)
library(mapview)
library(RColorBrewer)
library(furrr)
#library(extrafont)
#extrafont::loadfonts(device="win")

options(scipen=10000)

`%nin%` = Negate(`%in%`)


#library(htmltools)
#library(htmlwidgets)

#library(tmap)



to_spatial <- function(df1, coordenada = c("lon", "lat")) {
  x <- st_as_sf(df1, coords = coordenada, crs = 4326)
}
