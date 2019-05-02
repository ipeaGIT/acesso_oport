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

# mapview(setor)


vai_setor <- setor %>%
  filter(id_setor %in% c(2656, 2855, 2854)) %>%
  rename(id_tract = id_setor, tract_incm = renda_total)

# mapview(vai_setor)

bbox_setor <- st_bbox(vai_setor)

vai_grade <- st_crop(grade, bbox_setor) %>%
  rename(id_square = id_grade, square_pop = pop_total)

# mapview(vai_grade) + mapview(vai_setor)

# # dput
# dput(vai_setor)
# dput(vai_grade)

# mudar nomes
tract <-  vai_setor
square <- vai_grade

# exportar

# test_setor <- st_write(vai_setor, "test/tracts.shp")
# test_grade <- st_write(vai_grade, "test/squares.shp")

save(tract, square, file = "test/shapes.RData")


# AGORA VAI ---------------------------------------------------------------

download.file("https://github.com/ipeaGIT/acesso_oport/raw/master/test/test_setor.rds", "tracts.rds")
download.file("https://github.com/ipeaGIT/acesso_oport/raw/master/test/test_grade.rds", "squares.rds")

download.file("https://github.com/ipeaGIT/acesso_oport/raw/master/test/shapes.RData", "shapes.RData")

load("shapes.RData")


# Open census tracts
census_tracts <- readr::read_rds("tracts.rds")
squares <- readr::read_rds("squares.rds")

# Open census tracts
tempdir()
unzip("test/squares.zip", exdir = "")

tracts <- st_read("test/squares.zip")
squares <- readr::read_rds("squares.rds")


# testar ---------------------------------------------------------------


                                 

