library(tidyverse)
library(readxl)
library(scales)
library(sf)
library(mapview)

# Esse arquivo elabora uma função para ajeitar as coordenadas do arquivo do censo escolar de 2015

# censo_escolar <- read_delim("data-raw/censo_escolar/CAD_ESC_MAT_DOC_2015.csv", delim = ";")
# 
# fort <- censo_escolar %>%
#   filter(NO_MUNICIPIO == "Fortaleza") %>%
#   select(NO_MUNICIPIO, NU_LATITUDE, NU_LONGITUDE) %>%
#   mutate()
# 
# poa <- censo_escolar %>%
#   filter(NO_MUNICIPIO == "Porto Alegre") %>%
#   select(NO_MUNICIPIO, NU_LATITUDE, NU_LONGITUDE)
#   
# 
# 
# # teste para fortaleza ----------------------------------------------------
# 
# 
# lat <- fort$NU_LATITUDE
# lat <- gsub("\\.", "", lat)
# lat <- stringr::str_sub(lat, 1, -3)
# lat <- as.numeric(lat)
# lat <- scales::comma(lat)
# 
# lon <- fort$NU_LONGITUDE
# lon <- gsub("\\.", "", lon)
# lon <- stringr::str_sub(lon, 1, -3)
# lon <- as.numeric(lon)
# lon <- scales::comma(lon)
# 
# 
# # teste para poa ----------------------------------------------------------
# 
# lat <- poa$NU_LATITUDE
# lat <- gsub("\\.", "", lat)
# lat <- stringr::str_sub(lat, 1, -3)
# lat <- as.numeric(lat)
# lat <- scales::comma(lat)
# 
# lon <- poa$NU_LONGITUDE
# lon <- gsub("\\.", "", lon)
# lon <- stringr::str_sub(lon, 1, -3)
# lon <- as.numeric(lon)
# lon <- scales::comma(lon)
# 
# 
# str_extract("38,333,45,45,4124124", "\\d+\\,")
# gsub("(-?\\d+\\,)(.*)", "\\2", "38,333,455,454,124")
# 
# gsub("(-?\\d+\\.)(.*)", "\\2", "38.333.45.45.4124124")
# 
# 
# 
# # funciona para a lon
# 
# ai <- data.frame(lat = lat) %>%
#   # substituir as virgulas por pontos
#   mutate(lat = gsub("\\,", "\\.", lat)) %>%
#   mutate(lat1 = str_extract(lat, "-?\\d+\\."),
#          lat2 = gsub("(-?\\d+\\.)(.*)", "\\2", lat)) %>%
#   mutate(lat3 = gsub("\\.", "", lat2)) %>%
#   mutate(lat_fim = paste0(lat1, lat3)) %>%
#   mutate(lat_fim = as.numeric(lat_fim))
# 
# ai <- data.frame(lon = lon) %>%
#   # substituir as virgulas por pontos
#   mutate(lon = gsub("\\,", "\\.", lon)) %>%
#   mutate(lon1 = str_extract(lon, "-?\\d+\\."),
#          lon2 = gsub("(-?\\d+\\.)(.*)", "\\2", lon)) %>%
#   mutate(lon3 = gsub("\\.", "", lon2)) %>%
#   mutate(lon_fim = paste0(lon1, lon3)) %>%
#   mutate(lon_fim = as.numeric(lon_fim))


# FUNCAO ------------------------------------------------------------------

convert_coords <- function(coords) {
  
  x <- gsub("\\.", "", coords)
  x <- stringr::str_sub(x, 1, -3)
  x <- as.numeric(x)
  x <- scales::comma(x)
  
  x <- gsub("\\,", "\\.", x)
  x1 <- str_extract(x, "-?\\d+\\.")
  x2 <- gsub("(-?\\d+\\.)(.*)", "\\2", x)
  x3 <- gsub("\\.", "", x2)
  xfim <- paste0(x1, x3)
  xfim <- as.numeric(xfim)
  
}


# # TESTAR ------------------------------------------------------------------
# 
# fort <- censo_escolar %>%
#   filter(NO_MUNICIPIO == "Fortaleza") %>%
#   select(NO_MUNICIPIO, NU_LATITUDE, NU_LONGITUDE) %>%
#   mutate(lat = convert_coords(NU_LATITUDE),
#          lon = convert_coords(NU_LONGITUDE))
# 
# poa <- censo_escolar %>%
#   filter(NO_MUNICIPIO == "Porto Alegre") %>%
#   select(NO_MUNICIPIO, NU_LATITUDE, NU_LONGITUDE) %>%
#   mutate(lat = convert_coords(NU_LATITUDE),
#          lon = convert_coords(NU_LONGITUDE))
# 
# 
# # APLICAR PARA TODOS ------------------------------------------------------
# 
# censo_escolar_novo <- censo_escolar %>%
#   select(NO_MUNICIPIO, NU_LATITUDE, NU_LONGITUDE) %>%
#   mutate(lat = convert_coords(NU_LATITUDE),
#          lon = convert_coords(NU_LONGITUDE))
# 
# # TESTAR PARA CIDADES ALEATORIAS
# 
# teste <- censo_escolar_novo %>%
#   filter(NO_MUNICIPIO == "Sobral") %>%
#   filter(!is.na(lon)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# mapview(teste)
# 
# teste <- censo_escolar_novo %>%
#   filter(NO_MUNICIPIO == "Recife") %>%
#   filter(!is.na(lon)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# mapview(teste)
# 
# teste <- censo_escolar_novo %>%
#   filter(NO_MUNICIPIO == "Curitiba") %>%
#   filter(!is.na(lon)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# mapview(teste)
# 
# teste <- censo_escolar_novo %>%
#   filter(NO_MUNICIPIO == "Manaus") %>%
#   filter(!is.na(lon)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# mapview(teste)
# 
# teste <- censo_escolar_novo %>%
#   filter(NO_MUNICIPIO == "Rio Branco") %>%
#   filter(!is.na(lon)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# mapview(teste)
