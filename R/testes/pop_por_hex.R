library(tidyverse)
library(sf)
library(mapview)
library(tools)


# ABRIR ARQUIVOS ----------------------------------------------------------

# GRADE
points <- st_read("../data/grade_municipio/fortaleza/grade_fortaleza.shp") %>%
  st_centroid()

# HEX
hex <- st_read("../data/hex_municipio/fortaleza/hex_fortaleza.shp") %>%
  mutate(id_hex = 1:n())


# viz
mapview(vai) + mapview(hex)



# SIMBORA!!! --------------------------------------------------------------

vai <- st_join(points, hex) %>%
  group_by(id_hex) %>%
  summarise(n = sum(POP)) %>%
  st_set_geometry(NULL)


vai_final <- hex %>%
  left_join(vai)

# testar

mapview(vai_final, zcol = "n")


# salvar

write_rds(vai_final, "../data/pop_por_hex.rds")



