#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Geocode dados do censo escolar

# carregar bibliotecas
source('./R/fun/setup.R')

ano <- 2018


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Ler das escolas do ano -----------------------------------------------------------------------------

escolas <- fread(sprintf("../../data-raw/censo_escolar/%s/censo-escolar_escolas_%s.CSV", ano, ano))

# filter municipalties
escolas <- janitor::clean_names(escolas)
escolas <- escolas[co_municipio %in% munis_df_2019$code_muni]

# only public
escolas <- escolas[tp_dependencia %in% c(1, 2, 3)]
# only active
escolas <- escolas[tp_situacao_funcionamento == 1]

# abrir as coordenadas
escolas_coords <- fread("../../data-raw/censo_escolar/escolas_inep_2020.csv",
                        encoding = "UTF-8")
# filter municipalties
escolas_coords <- janitor::clean_names(escolas_coords)
escolas_coords[, muni_uf := tolower(paste0(municipio, "-", uf))]
escolas_coords[, muni_uf := stringi::stri_trans_general(str = muni_uf, 
                                                        id = "Latin-ASCII")]
escolas_coords <- escolas_coords %>% 
  select(co_entidade = codigo_inep, endereco, lon = longitude, lat = latitude)


# join to create escolas geo
escolas_geo <- merge(
  escolas,
  escolas_coords,
  by = "co_entidade",
  all.x = TRUE
)

filter(escolas_geo, is.na(lon)) %>% nrow() # 169 obs
filter(escolas_geo, is.na(lon) & is.na(endereco)) %>% nrow() # 7 obs


# get coordinatess from 2017
escolas_2017_geo <- read_rds("../../data/acesso_oport/censo_escolar/2017/educacao_inep_final_2017.rds") %>%
  select(co_entidade, SearchedAddress,  PrecisionDepth, MatchedAddress, geocode_engine, lon, lat) %>%
  filter(geocode_engine == "gmaps_prob1") %>%
  filter(!is.na(lon)) %>% setDT()

escolas_2017_geo %>% filter(co_entidade %in% escolas_geo$co_entidade)

escolas_geo[, co_entidade := as.character(co_entidade)]
escolas_geo[escolas_2017_geo, on = "co_entidade", 
            c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
              list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]

filter(escolas_geo, is.na(lon)) %>% nrow() # 169 obs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Identificar CENSO ESCOLAR com lat/long problematicos

# A - poucos digitos
# B - fora dos limites do municipio
# C - com coordenadas NA
# D - mais de 5 estabelecimentos com coordenadas iguais


# qual o nivel de precisao das coordenadas que deve ser aceito?
# 0.01 = 1113.2 m
# 0.001 = 111.32 m
# 0.0001 = 11.132 m
# 0.00001 = 1.1132 m


# A) Numero de digitos de lat/long apos ponto
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Checar isso aqui e nos scripts de saude e educacao

# usao em 2019
#  setDT(escolas_filt)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]

# Acho que esse aqui funciona melhor
setDT(escolas_geo)[, ndigitos := nchar( gsub("^.*\\.","", lat) )]
A_estbs_pouco_digito <- escolas_geo[ ndigitos <=2,]


# B) fora dos limites do municipio

# carrega shapes
shps <- purrr::map_dfr(dir("../../data-raw/municipios/2019", recursive = TRUE, full.names = TRUE), read_rds) %>% 
  as_tibble() %>% 
  st_sf(crs = 4326)

# convert para sf
censoescolar2019_df_coords_fixed_df <- escolas_geo[!(is.na(lat))] %>% 
  st_as_sf( coords = c('lon', 'lat'), crs = 4326)

temp_intersect <- sf::st_join(censoescolar2019_df_coords_fixed_df, shps)

# escolas que cairam fora de algum municipio
B_muni_fora <- subset(temp_intersect, is.na(name_muni))


# C) Lat lon NA (feito mais a frente)



# D) mais de 5 estabelecimentos com coordenadas iguais

# junta lat lon
escolas_geo$latlon <- paste0(escolas_geo$lat, '/', escolas_geo$lon)

# freq de lat lon repetido
tab_latlon <- escolas_geo %>% count(latlon, sort = T)
latlon_problema <- subset(tab_latlon, n >3 & latlon != "NA/NA")



# juntar todos municipios com erro de lat/lon
munis_problemaA <- subset(escolas_geo, co_entidade %in% A_estbs_pouco_digito$co_entidade ) # 0 obs
munis_problemaB <- subset(escolas_geo, co_entidade %in% B_muni_fora$co_entidade ) # 22 obs
munis_problemaC <- escolas_geo[ is.na(lat), ] # 58 obs
munis_problemaD <- subset(escolas_geo, latlon %in% latlon_problema$latlon) # 28 obs

munis_problema <- rbind(munis_problemaA, munis_problemaB, munis_problemaC, munis_problemaD) # 1072 obs
munis_problema <- dplyr::distinct(munis_problema, co_entidade, .keep_all=T) # remove duplicates, 1072 obs


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Usando google maps -----------------------------------------

# select only address that are not NA
munis_problema_enderecos <- munis_problema %>% filter(!is.na(endereco))

# lista de enderecos com problema
enderecos <- munis_problema_enderecos$endereco # 101 obs

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[4])

# geocode
coordenadas_google1 <- ggmap::geocode(enderecos, output = "all")

# identify list names as co_entidade
names(coordenadas_google1) <- munis_problema_enderecos$co_entidade

if (!dir.exists("../../data/acesso_oport/censo_escolar/2018/geocode/")) {
  dir.create("../../data/acesso_oport/censo_escolar/2018/geocode/")
}

# save
write_rds(coordenadas_google1, "../../data/acesso_oport/censo_escolar/2018/geocode/escolas_geocode_2018_output_google1.rds")
coordenadas_google1 <- read_rds("../../data/acesso_oport/censo_escolar/2018/geocode/escolas_geocode_2018_output_google1.rds")


# build df with result from ggmap::geocode
# x <- coordenadas_google1[[11739]]
# x <- coordenadas_google1[[11739]]
# x <- coordenadas_google1[[11738]]
# x <- coordenadas_google1[[11740]]
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
estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "co_entidade",
                                         use.names = TRUE)

unique(estabs_problema_geocoded_dt$co_entidade) %>% length()

# identify searchedaddress
estabs_problema_geocoded_dt[, SearchedAddress := enderecos]
# identify problem
estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
# identify quality
estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]

# bring adress that didnt have an address
without_address <- data.table(
  co_entidade = munis_problema %>% filter(is.na(endereco)) %>% .$co_entidade,
  MatchedAddress = NA,
  SearchedAddress = NA,
  PrecisionDepth = "without_address",
  geocode_engine = "without_address",
  lon = NA,
  lat = NA
)

estabs_problema_geocoded_dt <- rbind(estabs_problema_geocoded_dt,
                                     without_address)



#### so ceps ------------------------

# ainda ha escolas mal georreferenciadas!
# identificar essas escolas e separa-las
# convert para sf
escolas_google_mal_geo <- estabs_problema_geocoded_dt %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps %>% st_buffer(0.0008)) %>%
  # escolas que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni)) %>%
  select(co_entidade, SearchedAddress)

mapview(escolas_google_mal_geo)


# identify these address as outside the city
estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[co_entidade %in% escolas_google_mal_geo$co_entidade,
                            PrecisionDepth := 'address_outside_city']

table(estabs_problema_geocoded_dt$PrecisionDepth, useNA = 'always')

# identify columns 
escolas_geo[, co_entidade := as.character(co_entidade)]
escolas_geo[, MatchedAddress := endereco]
escolas_geo[, SearchedAddress := endereco]
escolas_geo[, PrecisionDepth := "inep"]
escolas_geo[, geocode_engine := "inep"]

escolas_geo[estabs_problema_geocoded_dt, on = "co_entidade",
            c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
              list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]  

table(escolas_geo$PrecisionDepth, useNA = 'always')
table(escolas_geo$geocode_engine, useNA = 'always')

# saveit
write_rds(escolas_geo,
          "../../data/acesso_oport/censo_escolar/2018/censo-escolar_escolas_geocoded_2018.rds")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. trazer escolas do censo escolar ----------------------------------------------------------

# O Censo escolar traz dado codificada da etapa de ensino. Para as informacoes missing, a gente usa
# a info de etapa de ensino informada no dado do INEP geo

# colunas de interesse: 
colunas <- c(tolower(c("co_entidade", "NO_ENTIDADE", "co_municipio",
             "IN_COMUM_CRECHE", "IN_COMUM_PRE", 
             "IN_COMUM_FUND_AI", "IN_COMUM_FUND_AF", 
             "IN_COMUM_MEDIO_MEDIO", "IN_COMUM_MEDIO_NORMAL",
             "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE", 
             "IN_COMUM_MEDIO_INTEGRADO", "IN_PROFISSIONALIZANTE",
             "IN_ESP_EXCLUSIVA_FUND_AI", "IN_ESP_EXCLUSIVA_FUND_AF",
             "IN_ESP_EXCLUSIVA_MEDIO_MEDIO", "IN_ESP_EXCLUSIVA_MEDIO_INTEGR",
             "IN_ESP_EXCLUSIVA_MEDIO_NORMAL","IN_COMUM_EJA_MEDIO","IN_COMUM_EJA_PROF",
             "IN_ESP_EXCLUSIVA_EJA_MEDIO","IN_ESP_EXCLUSIVA_EJA_PROF","IN_COMUM_PROF",
             "IN_ESP_EXCLUSIVA_PROF","IN_COMUM_EJA_FUND","IN_ESP_EXCLUSIVA_EJA_FUND",
             "IN_LOCAL_FUNC_UNID_PRISIONAL", "IN_LOCAL_FUNC_PRISIONAL_SOCIO", # escolas prisionais
             "QT_FUNCIONARIOS")),
             "endereco", "SearchedAddress", "PrecisionDepth", "MatchedAddress", "geocode_engine", "lon", "lat"
             ) 




# abrir
escolas_censo <- read_rds("../../data/acesso_oport/censo_escolar/2018/censo-escolar_escolas_geocoded_2018.rds") %>%
  select(colunas) %>%
  # identificar o tipo de ensino em cada escola
  mutate(mat_infantil = ifelse(in_comum_creche == 1 | 
                                 in_comum_pre == 1 |
                                 in_esp_exclusiva_creche == 1 |
                                 in_esp_exclusiva_pre ==1, 1, 0)) %>%
  
  mutate(mat_fundamental = ifelse(in_comum_fund_ai == 1 | 
                                    in_comum_fund_af == 1 |
                                    in_esp_exclusiva_fund_ai ==1 |
                                    in_esp_exclusiva_fund_af ==1 |
                                    in_comum_eja_fund ==1 |
                                    in_esp_exclusiva_eja_fund ==1, 1, 0)) %>%
  mutate(mat_medio = ifelse(in_comum_medio_medio == 1 |
                              in_comum_medio_normal == 1 |
                              in_comum_medio_integrado ==1 |
                              in_profissionalizante ==1 |
                              in_esp_exclusiva_medio_medio ==1 |
                              in_esp_exclusiva_medio_integr ==1 |
                              in_esp_exclusiva_medio_normal ==1 |
                              in_comum_eja_medio ==1 |
                              in_comum_eja_prof ==1 |
                              in_esp_exclusiva_eja_medio ==1 |
                              in_esp_exclusiva_eja_prof ==1 |
                              in_comum_prof ==1 |
                              in_esp_exclusiva_prof ==1, 1, 0)) %>%
  # Selecionar variaveis
  select(co_entidade, code_muni = co_municipio, no_entidade, mat_infantil, mat_fundamental, mat_medio, 
         in_local_func_unid_prisional, in_local_func_prisional_socio, qt_funcionarios,
         endereco, SearchedAddress, MatchedAddress, PrecisionDepth, geocode_engine, lon, lat)


# Identifica escolas priosionais
escolas_prisionais <- subset(escolas_censo, in_local_func_unid_prisional ==1 | in_local_func_prisional_socio ==1)$co_entidade


# remove escolas prisionais
escolas_etapa_fim <- subset(escolas_censo, co_entidade %nin% escolas_prisionais)
escolas_etapa_fim$in_local_func_unid_prisional <- NULL
escolas_etapa_fim$in_local_func_prisional_socio <- NULL




# # codifica etapa de ensino pela string
# setDT(escolas_etapa_fim)[, mat_infantil := ifelse(is.na(mat_infantil) & OFERTA_ETAPA_MODALIDADE %like% 'Creche|Pré-escola', 1, 
#                                                   mat_infantil)]
# setDT(escolas_etapa_fim)[, mat_fundamental := ifelse(is.na(mat_fundamental) & OFERTA_ETAPA_MODALIDADE %like% 'Ensino Fundamental', 1, 
#                                                      mat_fundamental)]
# setDT(escolas_etapa_fim)[, mat_medio := ifelse(is.na(mat_medio) & OFERTA_ETAPA_MODALIDADE %like% "Ensino Médio|nível Médio|Curso Profissional|Curso Técnico", 1, 
#                                                mat_medio)]



table(escolas_etapa_fim$mat_infantil, useNA = "always")
table(escolas_etapa_fim$mat_fundamental, useNA = "always")
table(escolas_etapa_fim$mat_medio, useNA = "always")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. salvar ----------------------------------------------------------------------------------------

# salvar
write_rds(escolas_etapa_fim, "../../data/acesso_oport/censo_escolar/2018/educacao_inep_final_2018.rds")


