library(tidyverse)
library(readxl)
library(scales)
library(sf)
library(mapview)
library(data.table)
source("R/converter_censo_coords.R")

# Esse arquivo tem como objetivo tratar e salvar o arquivo-raw do censo escolar


# ABRIR ARQUIVO -----------------------------------------------------------


censo_escolar <- 
  # Abrir e selecionar as colunas de interesse
  fread("data-raw/censo_escolar/CAD_ESC_MAT_DOC_2015.csv", sep = ";",
                       select = c(17,3,6,14,128,138,144,150,165,187,196,201,206,27,28)) %>%
  # Renomear as colunas
  rename(cod_escola = CO_ENTIDADE,uf = SIGLA, municipio = NO_MUNICIPIO, rede = REDE, num_funcionarios = NU_FUNCIONARIOS,
         presencial = IN_MEDIACAO_PRESENCIAL, mat_infantil = MAT_INF, mat_fundamental = MAT_FUND,
         mat_medio = MAT_MED, mat_profissional = MAT_PROF, mat_eja = MAT_EJA, mat_especial = MAT_ESP, 
         docentes = DOCTOTAL, lon = NU_LONGITUDE, lat = NU_LATITUDE) %>%
  # Tratar as coordenadas
  mutate(lon = convert_coords(lon),
         lat = convert_coords(lat))


# censo_escolar %>%
#   count(rede)
# 
# censo_escolar %>%
#   count(presencial)


# SALVAR ------------------------------------------------------------------

write_csv(censo_escolar, "data/censo_escolar/censo_escolar_2015.csv")


# TIDYING UP!!!

censo_escolar_long <- censo_escolar %>%
  gather(key = "tipo", value = "total", mat_infantil:docentes)

write_csv(censo_escolar_long, "data/censo_escolar/censo_escolar_2015_long.csv")
