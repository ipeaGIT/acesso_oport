library(sf)
library(readr)
library(dplyr)
library(mapview)

cidades_lookup <- tibble::tibble(sigla = c("bel", "for", "rio"), 
                                 cidade = c("1", "2", "3"),
                                 cidade_nome = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro"))

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

write_rds(acess_junto, "acess_junto.rds")
