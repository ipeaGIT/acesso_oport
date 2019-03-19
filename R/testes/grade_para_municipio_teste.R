library(tidyverse)
library(sf)
library(mapview)

# esse arquivo buscar extrair os grades referentes aos municipios


# abrir arquivos ----------------------------------------------------------

# id77 eh o grade a qual pertence o municipio de fortaleza
grade_ceara <- read_sf("data-raw/grade_censo/ceara/grade_id77.shp", crs = 4326) %>%
  mutate(id_grade = 1:n()) %>%
  select(id_grade)

grade_ceara_centroids <- grade_ceara %>%
  st_centroid()
  

fortaleza <- read_sf("data-raw/municipios/ceara/23MUE250GC_SIR.shp", crs = 4326) %>%
  filter(NM_MUNICIP == "FORTALEZA") %>%
  select(municipio = NM_MUNICIP)


# geoprocessamento --------------------------------------------------------

vai <- st_join(grade_ceara_centroids, fortaleza) %>%
  filter(!is.na(municipio))
  

grade_fortaleza <- grade_ceara %>%
  filter(id_grade %in% vai$id_grade) %>%
  mutate(municipio = "fortaleza")

# mapview(grade_fortaleza)


# salvar ------------------------------------------------------------------

st_write(grade_fortaleza, "data/grade_municipio/fortaleza/grade_fortaleza.shp")

