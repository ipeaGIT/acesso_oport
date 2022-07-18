


# carregar bibliotecas
source('./R/fun/setup.R')


# ver educacao
# 1.2) Escolas
edu_raw <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", "2019", "2019"))
# remove lat lon missing
edu <- edu_raw[!is.na(lat),] 
# select columns
edu <- edu[, .(co_entidade, code_muni,
               mat_infantil, mat_fundamental, mat_medio,
               lon, lat)]
edu <- st_as_sf(edu, coords = c("lon", "lat"), crs = 4326)

# 1.1) Saude
saude_raw <- readr::read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected2.rds", "2019", "2019")) 
# remove lat lon missing
saude <- saude_raw[!is.na(lat),] 
# select columns
saude <- saude[, .(cnes, code_muni,
                   health_low, health_med, health_high,
                   lon, lat)]
saude <- st_as_sf(saude, coords = c("lon", "lat"), crs = 4326)



# brasilia! ---------------------------------------------------------------
bsb <- read_rds("../../data/acesso_oport/hex_agregados/2019/hex_agregado_bsb_09_2019.rds")
max(bsb$edu_total) # 23
# filtar hexagono com alto numbero de obs
bsb_max <- bsb %>% filter(edu_total == 23)


edu_join <- st_join(edu, bsb_max %>% dplyr::select(id_hex)) %>%
  filter(!is.na(id_hex))

edu_raw_bsb <- edu_raw[co_entidade %in% edu_join$co_entidade]


# chechar esses estabelecimentos
edu_raw_bsb %>%
  select(co_entidade, no_entidade, endereco) %>%
  mutate(latlon = NA) %>%
  googlesheets4::write_sheet(ss = "https://docs.google.com/spreadsheets/d/1qIDL2J3hzXRu0p9u_y2KqaO17l78P0t6e1DhgvyGMD8/edit?usp=sharing",
                             sheet = "bsb")
# read back
edu_raw_bsb_fix <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1qIDL2J3hzXRu0p9u_y2KqaO17l78P0t6e1DhgvyGMD8/edit?usp=sharing",
                                             sheet = "bsb") %>%
  # format latlon
  separate(latlon, into = c("lat", "lon"), sep = ", ") %>%
  mutate(geocode_engine = "gmaps_bsb") %>%
  setDT()



# fix education

edu_2017 <- readr::read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", "2017", "2017"))
edu_2018 <- readr::read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", "2018", "2018"))
edu_2019 <- readr::read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", "2019", "2019"))



edu_2017[edu_raw_bsb_fix, on = "co_entidade",
                         c("lon", "lat", "geocode_engine") :=
                           list(i.lon, i.lat, i.geocode_engine)]

edu_2018[edu_raw_bsb_fix, on = "co_entidade",
                         c("lon", "lat", "geocode_engine") :=
                           list(i.lon, i.lat, i.geocode_engine)]

edu_2019[edu_raw_bsb_fix, on = "co_entidade",
                         c("lon", "lat", "geocode_engine") :=
                           list(i.lon, i.lat, i.geocode_engine)]


readr::write_rds(edu_2017, sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2_bsb.rds", "2017", "2017"))
readr::write_rds(edu_2018, sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2_bsb.rds", "2018", "2018"))
readr::write_rds(edu_2019, sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2_bsb.rds", "2019", "2019"))
