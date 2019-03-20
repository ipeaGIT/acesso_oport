library(tidyverse)
library(sf)
library(mapview)
library(data.table)
library(RColorBrewer)

# Esse arquivo tem como objetivo agrupar a quantidade de escolas, hospitais e empregos nos hexagonos das cidades

# Primeiro será feito um teste para a cidade de Fortaleza

# ABRIR ARQUIVOS ----------------------------------------------------------

hospitais <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  identity()

hex_fortaleza <- st_read("../data/hex_municipio/fortaleza/hex_fortaleza.shp") %>%
  mutate(hex_id = 1:n()) %>%
  select(hex_id)


# AGRUPAR TODAS OS HOSPITAIS POR HEXAGONO -----------------------------------

hex_fortaleza_v1 <- hospitais %>%
  st_join(hex_fortaleza) %>%
  # filter(hex_id == 278)
  st_set_geometry(NULL) %>%
  count(hex_id)

# hex_fortaleza_v1 %>%
#   filter(!is.na(hex_id)) %>%
#   mapview()

hex_final <- hex_fortaleza %>%
  left_join(hex_fortaleza_v1)


# VISUALIZAR --------------------------------------------------------------

mapview(hex_final, zcol = "n")

hex_final %>%
  ggplot()+
  geom_sf(aes(fill = n))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(8, "BuPu"))
