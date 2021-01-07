#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.5 Download dos dados geolocalizados dos estabelecimentos de saude
##info
# fonte: Cadastro Nacionl dos Estabelecimentos de Saude (CNES) - DataSus


# carregar bibliotecas
source('./R/fun/setup.R')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2) Ler os dados do CNES ---------------------------------

## 2.1 Ler CNES ativos dos SUS - traz blueprint das intituicoes ativas em 2019

cnes_raw <- readxl::read_xlsx(path = '../../data-raw/hospitais/2019/CNES_NDIS_01_10_2019_BANCO_COMP_08_2019.xlsx',
                            sheet = 'BANCO', skip = 14, 
                            col_types = "text")

# format column names
cnes19 <- janitor::clean_names(cnes_raw)
colnames(cnes19)

# remove 1st NA rows
cnes19 <- cnes19[-c(1:3),]

# rename columns
names(cnes19)[15:30] <- c("instal_fisica_ambu", "instal_fisica_hospt", 
                          "complex_alta_ambu_est", "complex_alta_ambu_mun", 
                          "complex_baix_ambu_est", "complex_baix_ambu_mun", 
                          "complex_medi_ambu_est", "complex_medi_ambu_mun", 
                          "complex_alta_hosp_est", "complex_alta_hosp_mun", 
                          "complex_baix_hosp_est", "complex_baix_hosp_mun", 
                          "complex_medi_hosp_est", "complex_medi_hosp_mun", 
                          "complex_nao_aplic_est", "complex_nao_aplic_mun")
nrow(cnes19) # 340115 obs
colnames(cnes19)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3) Limpar os dados do CNES ---------------------------------


# Filter 0: healthcare nao aplica (pq nao tem servicos de alta/baixa complexidade, e.g. academias de saude, secretarias de saude etc)
cnes_filter0 <- setDT(cnes19)[is.na(complex_nao_aplic_est)] 
cnes_filter0 <- cnes_filter0[is.na(complex_nao_aplic_mun)]


# Filter 1: healthcare facilities operating with the public health system
cnes_filter1 <- setDT(cnes_filter0)[ atende_sus == 'SIM']


# Filter 2: Pessoa juridica
cnes_filter2 <- cnes_filter1[ pessoa_fisica_ou_pessoa_juridi == 'PESSOA_JURÍDICA']


# filter 3: Only municipalities in the project
cnes_filter3 <- cnes_filter2[ibge %in% substr(munis_df$code_muni, 1,6)]


# filter 4: Only atendimento hospitalar ou ambulatorial
cnes_filter4 <- cnes_filter3[ instal_fisica_ambu=="SIM" | instal_fisica_hospt=="SIM"]


# filter 5. Remove special categories of facilities 

# 5.1 Delete prison hospitals, research centers, police hospitals etc
to_remove1 <- 'CENTRO DE ESTUDOS|PSIQUIAT|PRESIDIO|PENAL|JUDICIARIO|PENITENCIARIA|PENITENCIARIO|SEDIT|DETENCAO|PROVISORIA|SANATORIO|POLICIA| PADI|DE REGULACAO|VIGILANCIA|SAMU |ACADEMIA|DEPEND QUIMICO|REEDUCACAO SOCIAL|CAPS|CENTRO DE ATENCAO PSICOSSOCIAL|DISTRIB DE ORGAOS|MILITAR|CADEIA PUBLICA|DOMICILIAR|ARTES MARCIAIS|UBS IPAT|UBS CDPM II'
# PADI = Programa de Atenção Domiciliar ao Idoso
# DE REGULACAO = gestora de servico
# CAPS - CENTRO DE ATENCAO PSICOSSOCIAL - saude mental e drogas
# UBS IPAT e UBS CDPM II - vinculatos a policia



# 5.2 Delete Home care, tele saude, unidades moveis de saude
to_remove2 <- 'TELESSAUDE|UNIDADE MOVEL|DOMICILIAR|PSICOSSOCIAL|FARMACIA|DE ORGAOS'

# apply filter 5
cnes_filter5 <- cnes_filter4[ estabelecimento %nlike% to_remove1 ]
cnes_filter5 <- cnes_filter5[ tipo_unidade %nlike% to_remove2 ]
# test >>> cnes_filter6[ CNES =='6771963']


table(cnes_filter5$complex_baix_ambu_est, useNA = "always")
table(cnes_filter5$complex_baix_hosp_mun, useNA = "always")
table(cnes_filter5$health_low, useNA = "always")

### Organiza Nivel de atencao criando dummy


# convert health facilities Hierarchy into dummy variables
cnes_filter5[, health_low := ifelse(complex_baix_ambu_est=='X'|
                                      complex_baix_ambu_mun=='X' |
                                      complex_baix_hosp_est=='X' |
                                      complex_baix_hosp_mun=='X' , 1, 0)]

cnes_filter5[, health_med := ifelse(complex_medi_ambu_est=='X'|
                                      complex_medi_ambu_mun=='X' |
                                      complex_medi_hosp_est=='X' |
                                      complex_medi_hosp_mun=='X' , 1, 0)]

cnes_filter5[, health_high := ifelse(complex_alta_ambu_est=='X'|
                                       complex_alta_ambu_mun=='X' |
                                       complex_alta_hosp_est=='X' |
                                       complex_alta_hosp_mun=='X' , 1, 0)]


table(cnes_filter5$health_low, useNA = "always")  # 3496
table(cnes_filter5$health_med, useNA = "always")  # 4120
table(cnes_filter5$health_high, useNA = "always") # 854

nrow(cnes_filter5) # 5052 obs


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4)geocode only estabs that werent geocoded in the previous year  ---------------------------------

# geocode only estabs that werent geocoded in the previous year
cnes_previous <- read_rds("../../data/acesso_oport/hospitais/2018/hospitais_geocoded_pmaq_2018.rds") %>%
  select(cnes, lat_digits, lon, lat, ndigitos, latlon, MatchedAddress, SearchedAddress, PrecisionDepth, geocode_engine)


table(nchar(cnes_previous$cnes))
table(nchar(cnes_filter5$cnes))

cnes_filter5 <- cnes_filter5 %>% mutate(cnes = str_pad(cnes, width = 7, side = "left", pad = 0))

cnes_filter5_togeo <- cnes_filter5 %>% 
  mutate(cnes = str_pad(cnes, width = 7, side = "left", pad = 0)) %>%
  filter(cnes %nin% cnes_previous$cnes)


# GEOCODE EVERYONE, I DONT CARE! --------------------------------------------------------------

# lista de enderecos com problema
enderecos <- cnes_filter5_togeo %>% mutate(fim = paste0(logradouro, ", ", numero, " - ", municipio, ", ", uf, " - CEP ", cep)) %>% .$fim

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[4])

# geocode
coordenadas_google1 <- ggmap::geocode(enderecos, output = "all")

# identify list names as cnes
names(coordenadas_google1) <- munis_problema$cnes

# save
write_rds(coordenadas_google1, "../../data/acesso_oport/hospitais/2019/geocode/hospitais_geocode_2019_output_google1.rds")
coordenadas_google1 <- read_rds("../../data/acesso_oport/hospitais/2019/geocode/hospitais_geocode_2019_output_google1.rds")

create_dt <- function(x) {
  
  precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0, 
                             x[["results"]][[1]][["address_components"]], 
                             NA)
  
  # check length from precision depth
  precision_depth <- ifelse(is.na(precision_depth0), NA,
                            ifelse(length(precision_depth0[[1]]$types) > 0,
                                   precision_depth0[[1]]$types[[1]], 
                                   NA))
  
  a <- data.table(
    MatchedAddress = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
    # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
    PrecisionDepth = precision_depth,
    lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
    lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
  )
  
}


estabs_problema_geocoded <- lapply(coordenadas_google1, create_dt)
# estabs_problema_geocoded <- lapply(coordenadas_google1, possibly(create_dt, otherwise = "erro"))

# rbind as data.table
estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "cnes",
                                         use.names = TRUE)

unique(estabs_problema_geocoded_dt$cnes) %>% length() # 151

# identify searchedaddress
estabs_problema_geocoded_dt[, SearchedAddress := enderecos]
# identify problem
estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
# identify quality
estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]

table(estabs_problema_geocoded_dt$PrecisionDepth, useNA = 'always')



#### GOOGLE 2, so ceps ------------------------

# ainda ha escolas mal georreferenciadas!
# identificar essas escolas e separa-las
# convert para sf
hospitais_google_mal_geo <- estabs_problema_geocoded_dt %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps %>% st_buffer(0.0008)) %>%
  # escolas que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni)) %>%
  select(cnes, SearchedAddress)

# mapview(escolas_google_mal_geo)


# identify these address as outside the city
estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[cnes %in% hospitais_google_mal_geo$cnes,
                                                           PrecisionDepth := 'address_outside_city']

table(estabs_problema_geocoded_dt$PrecisionDepth, useNA = 'always')


# bring to the original dataset
# # bring CEP's fixed to the original fixed dataset problems 1 and 2
# rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
#                           c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
#                             list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]

# identify columns 
# cnes19_df_coords_fixed[, co_entidade := as.character(co_entidade)]
cnes_filter5_togeo[, MatchedAddress := paste0(logradouro, ", ", numero, " - ", municipio, ", ", uf, " - CEP ", cep)]
cnes_filter5_togeo[, SearchedAddress := paste0(logradouro, ", ", numero, " - ", municipio, ", ", uf, " - CEP ", cep)]
cnes_filter5_togeo[, PrecisionDepth := "cnes"]
cnes_filter5_togeo[, geocode_engine := "cnes"]


cnes_filter5_togeo[estabs_problema_geocoded_dt, on = "cnes",
                       c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                         list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]  

table(cnes_filter5_togeo$PrecisionDepth, useNA = 'always')
table(cnes_filter5_togeo$geocode_engine, useNA = 'always')


# bring new coordinates to 2018
cnes_filter5_2018_fim <- merge(cnes_filter5,
                               cnes_filter5_togeo[, .(cnes, lon, lat,
                                                          MatchedAddress, SearchedAddress, PrecisionDepth, geocode_engine)],
                               by = "cnes",
                               all.x = TRUE) %>%
  mutate(cnes = str_pad(cnes, width = 7, side = "left", pad = 0))

table(cnes_filter5_2018_fim$PrecisionDepth, useNA = 'always')
table(nchar(cnes_filter5_2018_fim$cnes))
table(nchar(cnes_previous$cnes))

# bring old coordinates from previous year
cnes_filter5_2018_fim[cnes_previous, on = "cnes",
                      c('lon', 'lat', 
                        'MatchedAddress', 'SearchedAddress', 'PrecisionDepth', 'geocode_engine') := 
                        list(i.lon, i.lat, 
                             i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.geocode_engine)]


table(cnes_filter5_2018_fim$PrecisionDepth, useNA = 'always')
table(cnes_filter5_2018_fim$geocode_engine, useNA = 'always')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Salvar dados de saude ------------------------------------------------------------------

cnes_filter5_2018_fim %>%
  rename(code_muni = ibge) %>%
  # select(CNES, code_muni, health_low, health_med, health_high)
  readr::write_rds("../../data/acesso_oport/hospitais/2019/hospitais_geocoded_2019.rds")






# 7) bring pmaq data -----------------------------------------------------------------------------

hospitais <- read_rds("../../data/acesso_oport/hospitais/2019/hospitais_geocoded_2019.rds") %>%
  mutate(cnes = str_pad(cnes, width = 7, side = "left", pad = 0)) %>% setDT()


str(hospitais)

attr(hospitais$cnes, "sorted") <- NULL


###### Usar dados de lat/lon quando eles existirem na PMAQ (estabelecimentos de baixa complexidade)
# Read PMAQ data
pmaq_df_coords_fixed <- fread('../../data-raw/hospitais/2019/PMAQ/pmaq_df_coords_fixed.csv', colClasses = 'character') %>%
  select(code_muni, cnes = CNES_FINAL, lon, lat) %>%
  mutate(cnes = str_pad(cnes, width = 7, side = "left", pad = 0),
         PrecisionDepth = "PMAQ",
         geocode_engine = "PMAQ") %>% 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat)) %>% setDT()

# update 
hospitais[pmaq_df_coords_fixed, on = "cnes",
          c("PrecisionDepth", "lon", "lat", "geocode_engine") := 
            list(i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]


table(hospitais$geocode_engine, useNA = 'always')
table(hospitais$PrecisionDepth, useNA = 'always')


# save it
readr::write_rds(hospitais, "../../data/acesso_oport/hospitais/2019/hospitais_geocoded_pmaq_2019.rds")



