# carregar bibliotecas
source('./R/fun/setup.R')

# leitura da RAIS geocodificada ( output do galileo )
rais_geo <- fread("../../data-raw/rais/2017/rais_2017_georef.csv"
                  # , select = c("id_estab", "codemun", "qt_vinc_ativos", 
                  #              "latitude", 'longitude', 'precisiondepth', 'matchedaddress',
                  #              'logradouro', 'cep', 'uf', 'BA_Nome_do_municipio')
                  , colClasses='character'
                  , nrows = 10
)

unique(rais_geo$id_estab) %>% length() # 8140973 obs

# somente empresas com vinc ativo
rais_geo <- rais_geo[as.numeric(qt_vinc_ativos) > 0]

unique(rais_geo$id_estab) %>% length() # 3436809 estabs

# Filtro selecionar so municipios do projeto (2019 so far)
gc(reset=T)
rais_geo <- rais_geo[codemun %in% substr(munis_df_2019$code_muni, 1, 6) ]
unique(rais_geo$id_estab) %>% length() # 928271 obs

# recode lat lon column names
rais_geo <- rais_geo %>% rename(lat=latitude , lon=longitude,
                                PrecisionDepth = precisiondepth,
                                MatchedAddress = matchedaddress)
head(rais_geo)


# corrigir coordenadas ( substitui , por  .)
rais_geo[, ':='(lon = str_replace(lon, ",", "."),
                lat = str_replace(lat, ",", "."))]

rais_geo[, ':='(lon = as.numeric(lon),
                lat = as.numeric(lat))]


# qual a porcentagem de precisiondepth 1 ou 2 estrelas?
# 4%
rais_geo[ PrecisionDepth %in% c('1 Estrela', '2 Estrelas'), sum(total_corrigido)] / sum(rais_geo$total_corrigido)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### 5) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao  -------------------
# Os estabelecimentos que o Galileo encontrou com 1, 2 ou 3 estrelas serao jogados para o google api



######################### GOOGLE 1 , somente 1 e 2 estrelas, todos, endereco completo 

# Estabs com baixa precisao do Galileo 1 e 2 estrelas
estabs_problema <- rais_geo[ PrecisionDepth %in% c('1 Estrela', '2 Estrelas'), ]
nrow(estabs_problema) # 35262 obs

fwrite(estabs_problema, "../../data/acesso_oport/rais/2017/geocode/geocode_google1_2017.csv")


# lista de enderecos com problema
enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% .$fim

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[4])


# geocode
coordenadas_google1_1 <- lapply(X=enderecos[1:5000], ggmap::geocode, output = "all")
coordenadas_google1_2 <- lapply(X=enderecos[5001:10000], ggmap::geocode, output = "all")
coordenadas_google1_3 <- lapply(X=enderecos[10001:15000], ggmap::geocode, output = "all")
coordenadas_google1_4 <- lapply(X=enderecos[15001:20000], ggmap::geocode, output = "all")
coordenadas_google1_5 <- lapply(X=enderecos[20001:25000], ggmap::geocode, output = "all")
coordenadas_google1_6 <- lapply(X=enderecos[25001:30000], ggmap::geocode, output = "all")
coordenadas_google1_7 <- lapply(X=enderecos[30001:length(enderecos)], ggmap::geocode, output = "all")

# join them all together
coordenadas_google1 <- c(coordenadas_google1_1, coordenadas_google1_2, coordenadas_google1_3,
                         coordenadas_google1_4, coordenadas_google1_5, coordenadas_google1_6,
                         coordenadas_google1_7)

# identify list names as id_estab
names(coordenadas_google1) <- estabs_problema$id_estab

# save
write_rds(coordenadas_google1, "../../data/acesso_oport/rais/2017/geocode/rais_geocode_2017_output_google1.rds")
coordenadas_google1 <- read_rds("../../data/acesso_oport/rais/2017/geocode/rais_geocode_2017_output_google1.rds")


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
estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "id_estab",
                                         use.names = TRUE)

unique(estabs_problema_geocoded_dt$id_estab) %>% length() # 35067 obs

# identify searchedaddress
estabs_problema_geocoded_dt[, SearchedAddress := enderecos]
# identify problem
estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
# identify quality
estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]



filter(estabs_problema_geocoded_dt, is.na(lon)) %>% View()




######################### GOOGLE 2 , somente 3 estrelas, rodovias, endereco completo 
# Estabelecimentos com 3 estrelas que estao localizados em rodovias tendem a ter uma qualidade ruim
# de georreferenciamento!
# Essa etapa busca identificar todos os 3 estrelas que sejam em rodovias, e separa-os para aplicar
# o geocoding do google!


# abrir base de rodovias rodovias
rodovias <- st_read("../../data-raw/rodovias/2014/2014/rodovia_2014.shp") %>%
  # Excluir geometria (desnecessario)
  st_set_geometry(NULL) %>%
  mutate(nome = as.character(DescSeg)) %>%
  # Extrair o nome da rodovia na forma BR-116
  mutate(nome = str_extract(nome, "[[:upper:]]{2}-\\d{3}")) %>%
  # Pegar rodovias unicas
  distinct(nome) %>%
  # Tirar NAs
  filter(!is.na(nome)) %>%
  # Separar por uf da rodovia e numero da rodovia
  separate(nome, c("uf", "numero"), sep = "-", remove = FALSE) %>%
  # Tipo 1 possivel: BR-116;   # Tipo 2 possivel: BR 116
  mutate(tipo1 = nome, tipo2 = paste0(uf, " ", numero))


# pegar so tres estrelas da rais
estabs_problema_3estrelas <- rais_geo[PrecisionDepth == "3 Estrelas"]

# extrair as rodovias de cada tipo possivel, e depois juntar
# criar coluna com rodovias
estabs_problema_3estrelas[, rodovia := str_extract(logradouro, "( |^|,)[[:upper:]]{2}(-| )\\d{3}( |$|,)")]
# tirar virgulas
estabs_problema_3estrelas[, rodovia := str_replace(rodovia, ",", "")]
# tirar espacos em branco
estabs_problema_3estrelas[, rodovia := trimws(rodovia, "both")]

# extrair somente os que sao rodovia
rais_rodovias_tipo1 <- estabs_problema_3estrelas[rodovia %in% rodovias$tipo1]
rais_rodovias_tipo2 <- estabs_problema_3estrelas[rodovia %in% rodovias$tipo2]
rais_rodovias_tipo3 <- estabs_problema_3estrelas[logradouro %like% "(RODOVIA )|(ROD )|(ROD. )(RODOVIARIO )"]
# deletar os duplicados do tipo 1 e 2 no tipo 3
rais_rodovias_tipo3 <- rais_rodovias_tipo3[id_estab %nin% c(rais_rodovias_tipo1$id_estab, rais_rodovias_tipo2$id_estab)]
# juntar todas as rodovias
rais_rodovias <- rbind(rais_rodovias_tipo1, rais_rodovias_tipo2, rais_rodovias_tipo3)
nrow(rais_rodovias) # 9749 obs

fwrite(rais_rodovias, "../../data/acesso_oport/rais/2017/geocode/geocode_google2_rodovias_2017.csv")
rais_rodovias <- fread("../../data/acesso_oport/rais/2017/geocode/geocode_google2_rodovias_2017.csv",
                       colClasses = "character")

# lista de enderecos com problema
enderecos_rodovias <- rais_rodovias %>% mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% .$fim

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[4])

# geocode
coordenadas_google_rodovias_1 <- lapply(X=enderecos_rodovias[1:2500], ggmap::geocode, output = "all")
coordenadas_google_rodovias_2 <- lapply(X=enderecos_rodovias[2501:5000], ggmap::geocode, output = "all")
coordenadas_google_rodovias_3 <- lapply(X=enderecos_rodovias[5001:length(enderecos_rodovias)], ggmap::geocode, output = "all")

# join them all together
coordenadas_google_rodovias <- c(coordenadas_google_rodovias_1, coordenadas_google_rodovias_2,
                                 coordenadas_google_rodovias_3)

# identify list names as id_estab
names(coordenadas_google_rodovias) <- rais_rodovias$id_estab 

# save
write_rds(coordenadas_google_rodovias, "../../data/acesso_oport/rais/2017/geocode/rais_geocode_2017_output_google2.rds")

# create dt 
rodovias_problema_geocoded <- lapply(coordenadas_google_rodovias, create_dt)


# rbind as data.table
rodovias_problema_geocoded_dt <- rbindlist(rodovias_problema_geocoded, idcol = "id_estab",
                                           use.names = TRUE)

# identify searchedaddress
rodovias_problema_geocoded_dt[, SearchedAddress := enderecos_rodovias]
# identify problem
rodovias_problema_geocoded_dt[, geocode_engine := 'gmaps_prob2']
# identify quality
rodovias_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]

filter(rodovias_problema_geocoded_dt, is.na(lon)) %>% View()



# join datasets from problemas 1 and 2 ----------------------------------------------
rais_problema1e2_geocoded <- rbind(estabs_problema_geocoded_dt, rodovias_problema_geocoded_dt)

write_rds(rais_problema1e2_geocoded, "../../data/acesso_oport/rais/2017/geocode/rais_problema1e2_geocoded_2017.rds")
rais_problema1e2_geocoded <- read_rds("../../data/acesso_oport/rais/2017/geocode/rais_problema1e2_geocoded_2017.rds")




######################### GOOGLE 3, so ceps

# ainda ha empresas mal georreferenciadas!
# identificar esses empresas e separa-los
# muitas empresas foram georreferenciadas fora dos municipios
# identifica-las atraves do shape dos municipios e refazer geocode do google considerando so CEP

# carrega shape dos munis
shps <- purrr::map_dfr(munis_df_2019$code_muni, geobr::read_municipality) %>% as_tibble() %>% st_sf()



rais_google_mal_geo <- rais_problema1e2_geocoded %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps %>% st_set_crs(4326)) %>%
  # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni))

# most of these address are geocoded just fine (street_number)
# but they are located at the metro region of the cities
# so were gonna geocode ceps only that are not sreet_number

rais_google_mal_geo <- rais_google_mal_geo %>%
  filter(PrecisionDepth != "street_number") %>%
  # pegar so o estab
  select(id_estab) %>%
  left_join(select(rais_geo, id_estab, BA_Nome_do_municipio, uf, cep), by = "id_estab") %>%
  filter(cep != ".")



# Retorna somente os ceps dos que deram errado para jogar no google API
somente_ceps <- paste0("CEP ", rais_google_mal_geo$cep, " - ", rais_google_mal_geo$name_muni, ", ", rais_google_mal_geo$uf)
length(somente_ceps) # 1369 obs 

# consulta google api
coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode, output = 'all')

# identify list names as id_estab
names(coordenadas_google_cep) <- rais_google_mal_geo$id_estab

# save it
write_rds(coordenadas_google_cep, "../../data/acesso_oport/rais/2017/geocode/rais_geocode_2017_output_google3.rds")
coordenadas_google_cep <- read_rds("../../data/acesso_oport/rais/2017/geocode/rais_geocode_2017_output_google3.rds")

# create list with dt
cep_problema_geocoded <- lapply(coordenadas_google_cep, create_dt)

# rbind as data.table
cep_problema_geocoded_dt <- rbindlist(cep_problema_geocoded, idcol = "id_estab",
                                      fill = TRUE, use.names = TRUE)

nrow(cep_problema_geocoded_dt) # 1369 obs

# identify searchedaddress
cep_problema_geocoded_dt[, SearchedAddress := somente_ceps]
# identify problem
cep_problema_geocoded_dt[, geocode_engine := 'gmaps_prob3']
# identify quality
cep_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]

filter(cep_problema_geocoded_dt, is.na(lon)) %>% View()

mapview(to_spatial(filter(cep_problema_geocoded_dt, !is.na(lon))))

# check again with therey wihtin cities
cep_problema_again <- cep_problema_geocoded_dt %>%
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps %>% st_set_crs(4326)) %>%
  # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni)) %>%
  sfc_as_cols() %>%
  select(id_estab)

# fix these in the final dataset of ceps
cep_problema_geocoded_dt_fixed <- setDT(cep_problema_geocoded_dt)[id_estab %in% cep_problema_again$id_estab, 
                                                                  ':='(MatchedAddress = NA,
                                                                       PrecisionDepth = "address_outside_city",
                                                                       lon = NA,
                                                                       lat = NA)]

filter(cep_problema_geocoded_dt, is.na(lon)) %>% View()

nrow(cep_problema_geocoded_dt_fixed)
table(cep_problema_geocoded_dt_fixed$geocode_engine, useNA = 'always')


# bring CEP's fixed to the original fixed dataset problems 1 and 2
rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
                          c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                            list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]

nrow(rais_problema1e2_geocoded)
table(rais_problema1e2_geocoded$PrecisionDepth, useNA = 'always')
table(rais_problema1e2_geocoded$geocode_engine, useNA = 'always')

filter(rais_problema1e2_geocoded, is.na(PrecisionDepth))


# join the dataset with the fixed coordinates to the original output from galileo
rais_geo_fixed <- data.table::copy(rais_geo)
# identify geocode engine
setDT(rais_geo_fixed)[, geocode_engine := "galileo"]
# join!
rais_geo_fixed[rais_problema1e2_geocoded, on = "id_estab",
                          c("MatchedAddress", "PrecisionDepth", "lon", "lat", "SearchedAddress", "geocode_engine") :=
                            list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.SearchedAddress, i.geocode_engine)]


# identify source input
rais_geo_fixed[, type_input_galileo := "rais_2017"]

table(rais_geo_fixed$PrecisionDepth, useNA = 'always')
table(rais_geo_fixed$geocode_engine, useNA = 'always')
table(rais_geo_fixed$type_input_galileo, useNA = 'always')


# unique addresses
rais_geo_fixed %>% add_count(id_estab) %>% filter(n>=2) %>% View()
rais_geo_fixed <- distinct(rais_geo_fixed, id_estab, .keep_all = TRUE)


# save it
write_rds(rais_geo_fixed, "../../data/acesso_oport/rais/2017/rais_2017_estabs_geocode_final.rds")




a <- read_rds("../../data/acesso_oport/rais/2017/rais_2017_estabs_geocode_final.rds")

filter(a, is.na(SearchedAddress)) %>% nrow()
filter(a, is.na(MatchedAddress)) %>% nrow()

