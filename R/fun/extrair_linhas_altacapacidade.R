
# CORREDORES DE ALTA E MÉDIA CAPACIDADE DOS ARQUIVOS DE GT --------

# comecar por bel

library(tidytransit)
library(dplyr)
library(sf)
library(mapview)
library(sp)
library(maptools)

gtfs.bel <- read_gtfs("../otp/graphs/bel/gtfs_bhtransit_20181115.zip", local = TRUE)
# gtfs.for <- read_gtfs("../otp/graphs/for/gtfs.zip", local = TRUE)


gtfs.bel[["routes"]] %>%
  filter(grepl("move", route_long_name, ignore.case = T)) %>%
  View()

routes_high <- gtfs.bel[["routes"]] %>%
  filter(route_type %in% c(0, 1, 2))

# arquivo de BEL nao tem shapes!!!!!!!!!!!!!!
# shape_high <- gtfs.bel[["shapes"]]

# entao checar nos trips..
trips_high <- gtfs.bel[["trips"]] %>%
  filter(route_id %in% routes_high$route_id)

# entao checar no stop_times
stop_times_high <- gtfs.bel[["stop_times"]] %>%
  filter(trip_id %in% trips_high$trip_id) %>%
  # pegar uma trip que seja em hora pico (consultar a tabela)
  filter(trip_id == "METRO 011010020700") %>%
  select(trip_id, stop_id, stop_sequence)

# agora, pegar as paradas referentes a trip das linhas de alta capacidade
stops_high <- gtfs.bel[["stops"]] %>%
  select(stop_id, stop_lon, stop_lat) %>%
  # filter(stop_id %in% stop_times_high$stop_id) %>%
  right_join(stop_times_high)



shapes_high <- stops_high %>%
  points_to_line(long = "stop_lon", lat = "stop_lat", sort_field = "stop_sequence") %>%
  mapview()




# PARA O RIO DE JANEIRO ---------------------------------------------------



gtfs.rio <- read_gtfs("../otp/graphs/rio/GTFS Rio feed_20190219.zip", local = TRUE)


# gtfs.bel[["routes"]] %>% View()

routes_high <- gtfs.rio[["routes"]] %>%
  filter(route_type %in% c(0, 1, 2))

# tem BRT?
gtfs.rio[["routes"]] %>%
  filter(grepl("BRT", route_long_name))

# ir atras no trips
trips_high <- gtfs.rio[["trips"]] %>%
  filter(route_id %in% routes_high$route_id) %>%
  distinct(shape_id)


# ir atras no shapes
shapes_high <- gtfs.rio[["shapes"]] %>%
  filter(shape_id %in% trips_high$shape_id)

# aplicar funcao

shapes_high %>%
  points_to_line_sf(long = "shape_pt_lon", lat = "shape_pt_lat", 
                 id_field = "shape_id", sort_field = "shape_pt_sequence") %>%
  st_as_sf(crs = 4326) %>%
  st_set_crs(4326) %>%
  mutate(shape_id = routes_high$shape_id) %>%
  mapview()




# FUNCAO ------------------------------------------------------------------

gtfs_file <- "../otp/graphs/for/gtfs_for_metrofor_2018-11_mod.zip"

extrair_HMcorredores <- function(gtfs_file) {
  
  gtfs.rio <- tidytransit::read_gtfs(gtfs_file, local = TRUE)
  
  
  # gtfs.bel[["routes"]] %>% View()
  
  routes_high <- gtfs.rio[["routes"]] %>%
    dplyr::filter(route_type %in% c(0, 1, 2))
  
  # tem BRT?
  gtfs.rio[["routes"]] %>%
    dplyr::filter(grepl("BRT", route_long_name))
  
  # ir atras no trips
  trips_high <- gtfs.rio[["trips"]] %>%
    dplyr::filter(route_id %in% routes_high$route_id) %>%
    distinct(shape_id)
  
  
  # ir atras no shapes
  shapes_high <- gtfs.rio[["shapes"]] %>%
    dplyr::filter(shape_id %in% trips_high$shape_id)
  
  # aplicar funcao
  
  source("R/points_to_line.R")
  corredores <- shapes_high %>%
    points_to_line_sf(long = "shape_pt_lon", lat = "shape_pt_lat", 
                   id_field = "shape_id", sort_field = "shape_pt_sequence") %>%
    mutate(shape_id = routes_high$shape_id)

} 



vai_for <- extrair_HMcorredores("../otp/graphs/rio/GTFS Rio feed_20190219.zip")


  
  
  
