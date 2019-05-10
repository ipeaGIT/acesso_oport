library(sf)
library(readr)
library(dplyr)
library(mapview)


# CRIAR LOOKUP DE CIDADES -------------------------------------------------


cidades_lookup <- tibble::tibble(sigla = c("bel", "for", "rio"), 
                                 cidade = c("1", "2", "3"),
                                 cidade_nome = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro"))


# INDICADOR CUMULATIVO ----------------------------------------------------



acess_bel <- read_rds("../../data/output_access/access_ac_bel.rds") %>%
  as_tibble() %>%
  st_sf(crs = 4326) %>%
  mutate(cidade = "1") %>%
  left_join(cidades_lookup, by = "cidade")

acess_for <- read_rds("../../data/output_access/access_ac_for.rds") %>%
  as_tibble() %>%
  st_sf(crs = 4326) %>%
  mutate(cidade = "2") %>%
  left_join(cidades_lookup, by = "cidade")

acess_rio <- read_rds("../../data/output_access/access_ac_rio.rds") %>%
  as_tibble() %>%
  st_sf(crs = 4326) %>%
  mutate(cidade = "3") %>%
  left_join(cidades_lookup, by = "cidade")


acess_junto <- rbind(acess_bel, acess_for, acess_rio)

write_rds(acess_junto, "acess_cum_junto.rds")


# INDICADOR MINIMO --------------------------------------------------------

acess_min_bel <- read_rds("../../data/output_access/acess_min_bel.rds") %>%
  as_tibble() %>%
  st_sf(crs = 4326) %>%
  mutate(cidade = "1") %>%
  left_join(cidades_lookup, by = "cidade")

acess_min_for <- read_rds("../../data/output_access/acess_min_for.rds") %>%
  as_tibble() %>%
  st_sf(crs = 4326) %>%
  mutate(cidade = "2") %>%
  left_join(cidades_lookup, by = "cidade")

acess_min_rio <- read_rds("../../data/output_access/acess_min_rio.rds") %>%
  as_tibble() %>%
  st_sf(crs = 4326) %>%
  mutate(cidade = "3") %>%
  left_join(cidades_lookup, by = "cidade")

acess_min_junto <- rbind(acess_min_bel, acess_min_for, acess_min_rio)

write_rds(acess_min_junto, "acess_min_junto.rds")


# LIMITE DOS MUNICIPIOS ---------------------------------------------------

muni_for <- read_rds("../../../data/municipios/municipios_ce.rds") %>%
  filter(NM_MUNICIP %in% toupper(cidades_lookup$cidade_nome))

muni_bel <- read_rds("../../../data/municipios/municipios_mg.rds") %>%
  filter(NM_MUNICIP %in% toupper(cidades_lookup$cidade_nome))

muni_rio <- read_rds("../../../data/municipios/municipios_rj.rds") %>%
  filter(NM_MUNICIP %in% toupper(cidades_lookup$cidade_nome))

# juntar

cidades_lookup <- cidades_lookup %>% mutate(cidade_nome = toupper(cidade_nome))

munics <- rbind(muni_for, muni_bel, muni_rio) %>%
  left_join(cidades_lookup, by = c("NM_MUNICIP" = "cidade_nome"))

write_rds(munics, "../data/limites_munis.rds")
