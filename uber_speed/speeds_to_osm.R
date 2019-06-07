library(data.table)
library(dplyr)
library(sf)
library(mapview)

speeds <- fread("data-raw/movement-speeds-quarterly-by-hod-new-york-2018-Q4.csv")

head(speeds)

segments <- fread("data-raw/movement-segments-to-osm-ways-new-york-2018.csv/movement-segments-to-osm-ways-new-york-2018.csv")

head(segments)

# Criar o id do osm no speeds
speeds_v1 <- speeds %>%
  left_join(segments, by = "segment_id")

head(speeds_v1)

# Abrir osm dados
osm_ny <- st_read("data-raw/osm_ny/ny_planet_osm_line_lines.shp")

head(osm_ny)

# Tentar fazer a juncao entre o osm e o speeds
speeds_osm <- osm_ny %>%
  select(osm_id) %>%
  mutate(osm_id = as.character(osm_id)) %>%
  mutate(osm_id = as.integer(osm_id)) %>%
  left_join(speeds_v1, by = c("osm_id" = "osm_way_id"))

# Selecionar so informacoes de segmentos
speeds_osm_seg <- speeds_osm %>%
  select(-start_junction_id, -end_junction_id)

speeds_osm_seg %>% filter(is.na(osm_id))

head(speeds_osm_seg)

# Test
speeds_osm_seg %>%
  # Selecionar so hora pico
  filter(hour_of_day == 7) %>%
  slice(1:100) %>%
  mapview()
