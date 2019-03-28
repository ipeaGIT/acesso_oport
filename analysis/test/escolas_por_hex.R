library(tidyverse)
library(sf)
library(mapview)
library(data.table)
library(RColorBrewer)

# Esse arquivo tem como objetivo agrupar a quantidade de escolas, hospitais e empregos nos hexagonos das cidades

# Primeiro será feito um teste para a cidade de Fortaleza

# ABRIR ARQUIVOS ----------------------------------------------------------

educacao <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
  # Selecionar somente municipio de Fortaleza
  filter(municipio == "Fortaleza") %>%
  # Deletar as que nao tem localizacao
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  # Selecionar somente o nome da escola
  select(cod_escola)

hex_fortaleza <- st_read("../data/hex_municipio/fortaleza/hex_fortaleza.shp") %>%
  mutate(hex_id = 1:n()) %>%
  select(hex_id)


# AGRUPAR TODAS AS ESCOLAS POR HEXAGONO -----------------------------------

hex_fortaleza_v1 <- educacao %>%
  st_join(hex_fortaleza) %>%
  st_set_geometry(NULL) %>%
  count(hex_id)

hex_final <- hex_fortaleza %>%
  left_join(hex_fortaleza_v1)


# SALVAR ------------------------------------------------------------------

write_rds(hex_final, "../data/escolas_por_hex.rds")


# VISUALIZAR --------------------------------------------------------------

mapview(hex_final, zcol = "n")

hex_final %>%
  ggplot()+
  geom_sf(aes(fill = n))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(8, "BuPu"))
