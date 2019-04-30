library(sf)
library(dplyr)
library(readr)
library(mapview)

setor_path <- '../data/setores_agregados/setores_agregados_ce.rds'
grade_path <- '../data/grade_municipio/grade_for.rds'

setor <- read_rds(setor_path) %>%
  filter(muni == "FORTALEZA") %>%
  mutate(id_setor = 1:n()) %>%
  dplyr::select(id_setor, renda_total)

grade <- read_rds(grade_path) %>%
  dplyr::select(id_grade, pop_total = POP)

mapview(setor)


vai_setor <- setor %>%
  filter(id_setor %in% c(2656, 2855, 2854))

bbox_setor <- st_bbox(vai_setor)

vai_grade <- st_crop(grade, bbox_setor)

mapview(vai_grade) + mapview(vai_setor)


# exportar

test_setor <- write_rds(vai_setor, "test/test_setor.rds")
test_grade <- write_rds(vai_grade, "test/test_grade.rds")


# AGORA VAI ---------------------------------------------------------------

# Open census tracts
census_tracts <- readRDS()

