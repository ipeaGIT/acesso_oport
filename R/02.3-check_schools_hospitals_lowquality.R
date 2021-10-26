# carregar bibliotecas
source('./R/fun/setup.R')
library(googlesheets4)


# schools and hospitals with low quality ----------------------------------
sheet <- "https://docs.google.com/spreadsheets/d/19RXGIcjKNGaJAJdmy8HrkuVYss0b99lGD0HNQBeJ2n8/edit#gid=0"

# ano1 <- 2017

correct_low_quality <- function(ano1) {
  
  # 1.1) Saude
  cnes_data <- readr::read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps.rds", ano1, ano1)) 
  # extract low quality
  cnes_data_low <- cnes_data[(Addr_type %nin% c('PointAddress', "StreetAddress", "StreetAddressExt", "StreetName",
                                                'street_number', 'route', 'airport', 'amusement_park', 'intersection','premise','town_square',
                                                # these one are exclusive for educacao and saude
                                                'inep', 'cnes', 'PMAQ'))]
  cnes_data_low[, ano := ano1]
  
  
  # bring previous results
  saude_previous <- lapply(
    list.files("../../data/acesso_oport/geocode_manual_fix/2017-2019", pattern = "hospitals", full.names = TRUE),
    fread
  ) %>%
    rbindlist(fill = TRUE)
  
  table(nchar(saude_previous$id))
  table(nchar(cnes_data_low$cnes))
  
  saude_previous[, cnes := str_pad(id, width = 7, pad = 0)]
  saude_previous[, geocode_engine := "low_quality_corrected"]
  saude_previous[, cnes := as.character(cnes)]
  
  # update
  cnes_data_low[saude_previous, on = c("cnes" = "cnes"),
                c("lon", "lat", "geocode_engine") :=
                  list(i.lon, i.lat, i.geocode_engine)]
  
  table(cnes_data_low$geocode_engine, useNA = 'always')
  cnes_data_low_updated <- cnes_data_low[geocode_engine == "low_quality_corrected"]
  cnes_data_low_updated <- cnes_data_low_updated[!is.na(lon)] 
  cnes_data_low_updated <- cnes_data_low_updated %>%
    rename(code_muni = ibge) %>% setDT()
  
  # corrigir
  cnes_data_good <- readr::read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected.rds", ano1, ano1)) 
  cnes_data_good[, ano := ano1]
  cnes_data_good <- cnes_data_good %>% select(-keep) %>% setDT()
  
  
  cnes_data_good_new <- rbind(cnes_data_good, 
                              cnes_data_low_updated
  )
  
  
  table(cnes_data_good_new$geocode_engine)
  
  
  # save
  readr::write_rds(cnes_data_good_new,
                   sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano1, ano1)) 
  
  
  
  
  
  
  
  # 1.2) Escolas --------------------
  escolas <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps.rds", ano1, ano1))
  # extract low quality
  escolas_low <- escolas[(Addr_type %nin% c('PointAddress', "StreetAddress", "StreetAddressExt", "StreetName",
                                            'street_number', 'route', 'airport', 'amusement_park', 'intersection','premise','town_square',
                                            # these one are exclusive for educacao and saude
                                            'inep', 'cnes', 'PMAQ'))]
  
  escolas_low <- escolas_low %>%
    mutate(across(matches("lon|lat"), ~ ifelse(is.na(.x), 0, .x)))
  
  a <- st_distance(escolas_low %>% st_as_sf(coords = c("lon_inep", "lat_inep"), crs = 4326),
                   escolas_low %>% st_as_sf(coords = c("lon", "lat"), crs = 4326),
                   by_element = TRUE)
  
  escolas_low <- escolas_low %>%
    mutate(dist = as.numeric(a))
  
  
  # oq vamos aceitar?
  # coordenadas do inep com menos de 300 metros em relacao a coordenadas do gmaps
  escolas_low_aceitar <- escolas_low %>% filter(lon_inep != 0 & dist < 300)
  escolas_low_aceitar <- escolas_low_aceitar %>% mutate(lon = lon_inep, lat = lat_inep)
  escolas_low_aceitar <- escolas_low_aceitar %>% select(-dist)
  escolas_low_aceitar <- escolas_low_aceitar %>% mutate(geocode_engine = "inep")
  
  
  # bring previous results
  escolas_previous <- lapply(
    list.files("../../data/acesso_oport/geocode_manual_fix/2017-2019", pattern = "schools", full.names = TRUE),
    fread
  ) %>%
    rbindlist(fill = TRUE)
  
  # table(nchar(escolas_previous$id))
  # table(nchar(escolas_low$co_entidade))
  
  escolas_previous[, co_entidade := as.character(id)]
  escolas_previous[, geocode_engine := "low_quality_corrected"]
  escolas_previous <- escolas_previous[!is.na(lon)] 
  
  # update
  escolas_low_aceitar[escolas_previous, on = c("co_entidade"),
                      c("lon", "lat", "geocode_engine") :=
                        list(i.lon, i.lat, i.geocode_engine)]
  
  table(escolas_low_aceitar$geocode_engine, useNA = 'always')
  
  # corrigir
  escolas_data_good <- readr::read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected.rds", ano1, ano1)) 
  escolas_data_good[, ano := ano1]
  escolas_data_good <- escolas_data_good %>% select(-keep) %>% setDT()
  
  escolas_data_good_new <- rbind(escolas_data_good, 
                                 escolas_low_aceitar
  )
  
  
  table(escolas_data_good_new$geocode_engine, useNA = 'always')
  
  
  # save
  readr::write_rds(escolas_data_good_new,
                   sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano1, ano1)) 
  
  
  
}

purrr::walk(c(2017, 2018, 2019), correct_low_quality)
