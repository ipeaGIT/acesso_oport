# carregar bibliotecas
source('./R/fun/setup.R')


# CLEAN UP NEW RAIS ESTAB (2019) --------------------------------------------------------------


# get new rais
rais_estabs_2019_raw <- fread("../../data-raw/rais/2019/rais_estabs_raw_2019.csv",
                              # nrows = 100,
                              select = c("id_estab", "qt_vinc_ativos", 'nat_jur2018',  "logradouro", "bairro", "codemun", "uf", "cep"),
                              colClasses = "character")


# essa base ja esta filtrada para os municipios do projeto, mas vamos aplicar os filtros mesmo assim por consistencia
unique(rais_estabs_2019_raw$id_estab) %>% length() # 2305683 estabs

table(rais_estabs_2019_raw$codemun)

# somente empresas com vinc ativo
rais_estab_2019_0 <- rais_estabs_2019_raw[as.numeric(qt_vinc_ativos) > 0]

unique(rais_estab_2019_0$id_estab) %>% length() # 902049 estabs

# filter municipalities
rais_estab_2019_02 <- rais_estab_2019_0[codemun %in% substr(munis_df$code_muni, 1, 6) ]

unique(rais_estab_2019_02$id_estab) %>% length() # 902049 estabs

# trazer o nome do municipio e a UF
muni_lookup <- geobr::lookup_muni(code_muni = "all")
muni_lookup <- muni_lookup %>%
  select(codemun = code_muni, name_muni, abrev_state) %>%
  mutate(codemun = substr(codemun, 1, 6))

# select and save
rais_estab_2019_02 %>%
  # fix uf and codemun
  left_join(muni_lookup, by = "codemun") %>%
  select(id_estab, qt_vinc_ativos, logradouro, bairro, codemun, name_muni, uf = abrev_state, cep) %>% 
  # save it in rds b
  fwrite("../../data/acesso_oport/rais/2019/rais_estabs_2019_filter.csv")




# JOIN PREVIOUS GEOCODE TO NEW RAIS -----------------------------------------------------------

# get new rais filter
rais_2019_filter <- fread("../../data/acesso_oport/rais/2019/rais_estabs_2019_filter.csv",
                          colClasses = "character",
                          select = c("id_estab", "logradouro", "bairro", "codemun", "name_muni", "uf", "cep"),
                          encoding = "UTF-8")

table(nchar(rais_2019_filter$id_estab))

rais_2019_filter[, id_estab := str_pad(id_estab, side = "left", width = 14, pad = 0)]

# get previous geocode (2018)
rais_etapa8 <- read_rds("../../data/acesso_oport/rais/2018/check/rais_2018_check1.rds")
rais_etapa8 <- rais_etapa8 %>% select(id_estab, logradouro, cep, lon, lat) %>% setDT()
rais_etapa8[, id_estab := str_pad(id_estab, side = "left", width = 14, pad = 0)]

table(nchar(rais_etapa8$id_estab))

# quantos CNPJS estao em 2018 mas nao estao em 2019?
setdiff(rais_etapa8$id_estab, rais_2019_filter$id_estab) %>% length() # 163177

# quantos CNPJS estao em 2019 mas nao estao em 2018?
setdiff(rais_2019_filter$id_estab, rais_etapa8$id_estab) %>% length() # 141481


# filter only estabs that were not geocoded before
rais_2019_geocode_new <- rais_2019_filter[id_estab %nin% rais_etapa8$id_estab]
# identify type
rais_2019_geocode_new[, type_input_galileo := "new_estab_2019"]

# get eestabbss that changed cep from 2018 to 2019
rais_2019_geocode_cep <- left_join(rais_2019_filter, select(rais_etapa8, id_estab, logradouro, cep),
                                  by = "id_estab",
                                  suffix = c("_2018", "_2019")) %>%
  mutate(cep_2018 = str_pad(cep_2018, side = "left", width = 8, pad = 0)) %>%
  mutate(cep_2019 = str_pad(cep_2019, side = "left", width = 8, pad = 0)) %>%
  mutate(cep_2018 = substr(cep_2018, 1, 6)) %>%
  mutate(cep_2019 = substr(cep_2019, 1, 6)) %>%
  mutate(cep_igual = ifelse(cep_2018 == cep_2019, "sim", "nao")) %>%
  filter(cep_igual == "nao") %>%
  rename(cep = cep_2019, logradouro = logradouro_2019) %>%
  select(-cep_2018, -logradouro_2018, -cep_igual) %>%
  # identify type
  mutate(type_input_galileo = "cep_changed_2019")



rais_2019_filter_geocode <- rbind(rais_2019_geocode_new, rais_2019_geocode_cep)

# select columns and create input to galileo
rais_2019_filter_geocode %>%
  write_delim("../../data/acesso_oport/rais/2019/geocode/rais_2019_input_galileo.csv", delim = ";")

# open galileo output
rais_galileo_output <- fread("../../data/acesso_oport/rais/2019/geocode/rais_2019_output_galileo.csv",
                             colClasses = 'character', fill = TRUE)

table(rais_galileo_output$PrecisionDepth, useNA = 'always')

# fix one observation that galileo got wrong
obs_problem <- data.frame(
  PrecisionDepth = "4 Estrelas",
  rais_galileo_output[65704,1],
  rais_galileo_output[65704,2],
  rais_galileo_output[65704,3],
  rais_galileo_output[65704,4],
  rais_galileo_output[65704,5],
  rais_galileo_output[65704,6],
  rais_galileo_output[65704,7],
  rais_galileo_output[65704,8],
  rais_galileo_output[65704,9],
  rais_galileo_output[65704,10],
  rais_galileo_output[65704,11]
)

colnames(obs_problem) <- colnames(rais_galileo_output)

# delete rows with the wrong output
rais_galileo_output <- rais_galileo_output[-c(65703:65704),]
# bring fixed observation
rais_galileo_output <- rbind(rais_galileo_output, obs_problem)


# recode lat lon column names
rais_galileo_output <- rais_galileo_output %>% rename(lat=Latitude , lon=Longitude)

# corrigir coordenadas ( substitui , por  .)
rais_galileo_output[, ':='(lon = str_replace(lon, ",", "."),
                           lat = str_replace(lat, ",", "."))]

rais_galileo_output[, ':='(lon = as.numeric(lon),
                           lat = as.numeric(lat))]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### 5) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao  -------------------
# Os estabelecimentos que o Galileo encontrou com 1, 2 ou 3 estrelas serao jogados para o google api

######################### GOOGLE 1 , somente 1 e 2 estrelas, todos, endereco completo 

# Estabs com baixa precisao do Galileo 1 e 2 estrelas
estabs_problema <- rais_galileo_output[ PrecisionDepth %in% c('1 Estrela', '2 Estrelas'), ]
nrow(estabs_problema) # 18920
unique(estabs_problema$id_estab) %>% length # 18564

# lista de enderecos com problema
enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim

ä# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[3])

# geocode
coordenadas_google1_1 <- lapply(X=enderecos[1:5000], ggmap::geocode, output = "all")
coordenadas_google1_2 <- lapply(X=enderecos[5001:10000], ggmap::geocode, output = "all")
coordenadas_google1_3 <- lapply(X=enderecos[10001:length(enderecos)], ggmap::geocode, output = "all")

# join them all together
coordenadas_google1 <- c(coordenadas_google1_1, coordenadas_google1_2, coordenadas_google1_3)

# identify list names as id_estab
names(coordenadas_google1) <- estabs_problema$id_estab

# save
write_rds(coordenadas_google1, "../../data/acesso_oport/rais/2019/geocode/rais_geocode_2019_output_google1.rds")
coordenadas_google1 <- read_rds("../../data/acesso_oport/rais/2019/geocode/rais_geocode_2019_output_google1.rds")
           

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

unique(estabs_problema_geocoded_dt$id_estab) %>% length()

# identify searchedaddress
searchedaddress <- filter(estabs_problema, id_estab %in% names(coordenadas_google1)) %>%
  mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
  distinct(id_estab, .keep_all = TRUE)
estabs_problema_geocoded_dt <- left_join(estabs_problema_geocoded_dt, searchedaddress, by = "id_estab") %>% setDT()
# identify problem
estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
# identify quality
estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]



filter(estabs_problema_geocoded_dt, is.na(lon)) %>% View()





# view
# subset(output_google_api1_rais, !is.na(lat)) %>%
#   to_spatial() %>%
#   mapview()


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
estabs_problema_3estrelas <- rais_galileo_output[PrecisionDepth == "3 Estrelas"]

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
nrow(rais_rodovias) # 3675 obs

fwrite(rais_rodovias, "../../data/acesso_oport/rais/2019/geocode/geocode_google2_rodovias.csv")
rais_rodovias <- fread("../../data/acesso_oport/rais/2019/geocode/geocode_google2_rodovias.csv",
                       colClasses = "character")

# lista de enderecos com problema
enderecos_rodovias <- rais_rodovias %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)

# geocode
coordenadas_google_rodovias <- lapply(X=enderecos_rodovias, ggmap::geocode, output = "all")

# identify list names as id_estab
names(coordenadas_google_rodovias) <- rais_rodovias$id_estab 

# save
write_rds(coordenadas_google_rodovias, "../../data/acesso_oport/rais/2019/geocode/rais_geocode_2019_output_google2.rds")
coordenadas_google_rodovias <- read_rds("../../data/acesso_oport/rais/2019/geocode/rais_geocode_2019_output_google2.rds")

# create dt 
rodovias_problema_geocoded <- lapply(coordenadas_google_rodovias, create_dt)


# rbind as data.table
rodovias_problema_geocoded_dt <- rbindlist(rodovias_problema_geocoded, idcol = "id_estab",
                                        use.names = TRUE)

# identify searchedaddress
searchedaddress_rodovias <- filter(rais_rodovias, id_estab %in% names(coordenadas_google_rodovias)) %>%
  mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
  distinct(id_estab, .keep_all = TRUE)
rodovias_problema_geocoded_dt <- left_join(rodovias_problema_geocoded_dt, searchedaddress_rodovias, by = "id_estab") %>% setDT()
# identify problem
rodovias_problema_geocoded_dt[, geocode_engine := 'gmaps_prob2']
# identify quality
rodovias_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]

filter(rodovias_problema_geocoded_dt, is.na(lon)) %>% View()



# join datasets from problemas 1 and 2 ----------------------------------------------
rais_problema1e2_geocoded <- rbind(estabs_problema_geocoded_dt, rodovias_problema_geocoded_dt)

write_rds(rais_problema1e2_geocoded, "../../data/acesso_oport/rais/2019/geocode/rais_problema1e2_geocoded.rds")
rais_problema1e2_geocoded <- read_rds("../../data/acesso_oport/rais/2019/geocode/rais_problema1e2_geocoded.rds")





######################### GOOGLE 3, so ceps

# ainda ha empresas mal georreferenciadas!
# identificar esses empresas e separa-los
# muitas empresas foram georreferenciadas fora dos municipios
# identifica-las atraves do shape dos municipios e refazer geocode do google considerando so CEP

# carrega shape dos munis
shps <- purrr::map_dfr(munis_df$code_muni, geobr::read_municipality) %>% as_tibble() %>% st_sf()
  
  

rais_google_mal_geo <- rais_problema1e2_geocoded %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps %>% st_set_crs(4326)) %>%
  # sf::st_join(shps %>% st_set_crs(4326) %>% st_buffer(0.0008)) %>%
  # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni)) %>%
  # pegar so o estab
  select(id_estab) %>%
  left_join(select(rais_galileo_output, id_estab, name_muni, uf, cep), by = "id_estab") %>%
  filter(cep != ".")


# Retorna somente os ceps dos que deram errado para jogar no google API
somente_ceps <- paste0("CEP ", rais_google_mal_geo$cep, " - ", rais_google_mal_geo$name_muni, ", ", rais_google_mal_geo$uf)
length(unique(somente_ceps)) #  2387 obs 

# consulta google api
coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode, output = 'all')

# identify list names as id_estab
names(coordenadas_google_cep) <- rais_google_mal_geo$id_estab

# save it
write_rds(coordenadas_google_cep, "../../data/acesso_oport/rais/2019/geocode/rais_geocode_2019_output_google3.rds")
coordenadas_google_cep <- read_rds("../../data/acesso_oport/rais/2019/geocode/rais_geocode_2019_output_google3.rds")

# create list with dt
cep_problema_geocoded <- lapply(coordenadas_google_cep, create_dt)

# rbind as data.table
cep_problema_geocoded_dt <- rbindlist(cep_problema_geocoded, idcol = "id_estab",
                                      fill = TRUE, use.names = TRUE)

nrow(cep_problema_geocoded_dt) # 1987 obs

# identify searchedaddress
searchedaddress_cep <- filter(rais_google_mal_geo, id_estab %in% names(coordenadas_google_cep)) %>%
  mutate(SearchedAddress = paste0("CEP ", cep, " - ", name_muni, ", ", uf)) %>% 
  select(id_estab, SearchedAddress) %>%
  distinct(id_estab, .keep_all = TRUE)
cep_problema_geocoded_dt <- left_join(cep_problema_geocoded_dt, searchedaddress_cep, by = "id_estab") %>% setDT()
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
                                                                        ':='(PrecisionDepth = "address_outside_city",
                                                                             lon = NA,
                                                                             lat = NA)]

filter(cep_problema_geocoded_dt, is.na(lon)) %>% View()

nrow(cep_problema_geocoded_dt_fixed)
table(cep_problema_geocoded_dt_fixed$geocode_engine, useNA = 'always')


# bring CEP's fixed to the original fixed dataset problems 1 and 2
rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
                          c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                            list(i.MatchedAddress, i.SearchedAddress,  i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]

nrow(rais_problema1e2_geocoded)
table(rais_problema1e2_geocoded$PrecisionDepth, useNA = 'always')
table(rais_problema1e2_geocoded$geocode_engine, useNA = 'always')
filter(rais_problema1e2_geocoded, is.na(MatchedAddress)) %>% View()


# join the dataset with the fixed coordinates to the original output from galileo
rais_galileo_output_fixed <- data.table::copy(rais_galileo_output)
# identify geocode engine
rais_galileo_output_fixed[, geocode_engine := "galileo"]
# join!
rais_galileo_output_fixed[rais_problema1e2_geocoded, on = "id_estab",
                          c("MatchedAddress", "PrecisionDepth", "lon", "lat", "SearchedAddress", "geocode_engine") :=
                          list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.SearchedAddress, i.geocode_engine)]


table(rais_galileo_output_fixed$PrecisionDepth, useNA = 'always')
table(rais_galileo_output_fixed$geocode_engine, useNA = 'always')
table(rais_galileo_output_fixed$type_input_galileo, useNA = 'always')

filter(rais_problema1e2_geocoded, is.na(MatchedAddress)) %>% nrow()
filter(rais_problema1e2_geocoded, is.na(SearchedAddress)) %>% nrow()



# unique addresses
# rais_galileo_output_fixed %>% add_count(id_estab) %>% filter(n>=2) %>% View()
rais_galileo_output_fixed <- distinct(rais_galileo_output_fixed, id_estab, .keep_all = TRUE)

# save it
write_rds(rais_galileo_output_fixed, "../../data/acesso_oport/rais/2019/geocode/rais_2019_estabs_geocode_final.rds")





# merge with the complete base 2019 -----------------------------------------------------------
rais_galileo_output_fixed <- read_rds("../../data/acesso_oport/rais/2019/geocode/rais_2019_estabs_geocode_final.rds")


rais_2019_filter <- fread("../../data/acesso_oport/rais/2019/rais_estabs_2019_filter.csv",
                          colClasses = "character",
                          encoding = "UTF-8") 

# pad everyone to 14 characters
setDT(rais_galileo_output_fixed)[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
rais_2019_filter[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

table(nchar(rais_galileo_output_fixed$id_estab))
table(nchar(rais_2019_filter$id_estab))


rais_2019_filter_geocode_end <- merge(
  setDT(rais_2019_filter),
  setDT(rais_galileo_output_fixed)[, .(id_estab, PrecisionDepth, SearchedAddress, MatchedAddress, 
                              lon, lat, type_input_galileo, geocode_engine)],
  by = "id_estab",
  all.x = TRUE,
  sort = FALSE
  
)


table(rais_2019_filter_geocode_end$type_input_galileo, useNA = 'always')
table(rais_2019_filter_geocode_end$PrecisionDepth, useNA = 'always')


# get previous geocode
rais_etapa8 <- read_rds("../../data/acesso_oport/rais/2018/check/rais_2018_check1.rds")
rais_etapa8 <- rais_etapa8 %>% select(id_estab, lon, lat, 
                                      SearchedAddress, MatchedAddress,
                                      PrecisionDepth, type_input_galileo, geocode_engine)
# filter only estabs that we will keep from 2018
rais_etapa8 <- setDT(rais_etapa8)[id_estab %nin% rais_galileo_output_fixed$id_estab]
# delete the ones that came from inep (we will add them later)
rais_etapa8 <- rais_etapa8[type_input_galileo %nin% "inep"]

table(rais_etapa8$type_input_galileo, useNA = 'always')
filter(rais_etapa8, type_input_galileo == "inep") %>% View()
table(nchar(rais_etapa8$id_estab))



# merge with the previous results
rais_2019_filter_geocode_end[rais_etapa8, on = "id_estab",
                             c('PrecisionDepth', 'SearchedAddress', 'MatchedAddress','lon', 'lat', 'type_input_galileo', 'geocode_engine') :=
                             list(i.PrecisionDepth, i.SearchedAddress, i.MatchedAddress, i.lon, i.lat, i.type_input_galileo, i.geocode_engine)]

table(rais_2019_filter_geocode_end$PrecisionDepth, useNA = 'always')
table(rais_2019_filter_geocode_end$geocode_engine, useNA = 'always')
table(rais_2019_filter_geocode_end$type_input_galileo, useNA = 'always')
colnames(rais_2019_filter_geocode_end)
summary(as.numeric(rais_2019_filter_geocode_end$qt_vinc_ativos))

filter(rais_2019_filter_geocode_end, is.na(MatchedAddress)) %>% nrow()
filter(rais_2019_filter_geocode_end, is.na(SearchedAddress)) %>% nrow()


# filter(rais_2019_filter_geocode_end, PrecisionDepth == "2 Estrelas") %>% 
#   select(PrecisionDepth, type_input_galileo) %>% View()
# 
# filter(rais_2019_filter_geocode_end, type_input_galileo == "rais_2018") %>% 
#   select(PrecisionDepth, type_input_galileo) %>% View()



write_rds(rais_2019_filter_geocode_end, "../../data/acesso_oport/rais/2019/rais_2019_estabs_geocode_final.rds")

