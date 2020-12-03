#> DATASET: Localização dos Centros de Assistência Social
#> Source: Censo do Sistema Único da Assistência Social (SUAS) - Ministério da Cidadania/MDS

### Download, Limpeza dos dados brutos e geolocalização dos CRAS ###

# Setup 
source('R/fun/setup.R')

munis_df <- tibble::tribble(
  ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado, ~modo_2017, ~modo_2018, ~modo_2019, ~modo_2020,
  2304400,    "for",       "Fortaleza",       "CE",          "todos",    "todos",    "todos",    "todos",
  3550308,    "spo",       "Sao Paulo",       "SP",          "todos",    "todos",    "todos",    "todos",
  3304557,    "rio",       "Rio de Janeiro",  "RJ",          "ativo",    "todos",    "todos",    "todos",
  4106902,    "cur",       "Curitiba",        "PR",          "todos",    "todos",    "todos",    "todos",
  4314902,    "poa",       "Porto Alegre",    "RS",          "todos",    "todos",    "todos",    "todos",
  3106200,    "bho",       "Belo Horizonte",  "MG",          "todos",    "todos",    "todos",    "todos",
  5300108,    "bsb",       "Brasilia",        "DF",          "ativo",    "ativo",    "ativo",    "ativo",
  2927408,    "sal",       "Salvador",        "BA",          "ativo",    "ativo",    "ativo",    "ativo",
  1302603,    "man",       "Manaus",          "AM",          "ativo",    "ativo",    "ativo",    "ativo",
  2611606,    "rec",       "Recife",          "PE",          "ativo",    "ativo",    "todos",    "todos",
  5208707,    "goi",       "Goiania",         "GO",          "ativo",    "ativo",    "todos",    "ativo",
  1501402,    "bel",       "Belem",           "PA",          "ativo",    "ativo",    "ativo",    "ativo",
  3518800,    "gua",       "Guarulhos",       "SP",          "ativo",    "ativo",    "ativo",    "ativo",
  3509502,    "cam",       "Campinas",        "SP",          "todos",    "todos",    "todos",    "ativo",
  2111300,    "slz",       "Sao Luis",        "MA",          "ativo",    "ativo",    "ativo",    "ativo",
  3304904,    "sgo",       "Sao Goncalo",     "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  2704302,    "mac",       "Maceio",          "AL",          "ativo",    "ativo",    "ativo",    "ativo",
  3301702,    "duq",       "Duque de Caxias", "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  5002704,    "cgr",       "Campo Grande",    "MS",          "ativo",    "ativo",    "ativo",    "ativo",
  2408102,    "nat",       "Natal",           "RN",          "ativo",    "ativo",    "ativo",    "ativo"
) %>% data.table::setDT()

###### 1. Download arquivos originais ###################

setwd('L:/Proj_acess_oport/')

# 2019 - CRAS

download.file("https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(5).zip" ,
              destfile = "data-raw/CRAS/Censo_SUAS_2019_CRAS.zip")

unzip("data-raw/CRAS/Censo_SUAS_2019_CRAS.zip", exdir = "data-raw/CRAS")

# CRAS 2018 - download dos dados brutos

download.file('https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(3).zip',
              destfile = "data-raw/CRAS/Censo_SUAS_2018_CRAS.zip")

unzip("data-raw/CRAS/Censo_SUAS_2018_CRAS.zip", exdir = "data-raw/CRAS")

# CRAS 2017 - download dos dados brutos

download.file('http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS/2017/Censo_SUAS_2017_CRAS.zip',
              destfile = "data-raw/CRAS/Censo_SUAS_2017_CRAS.zip")

unzip("data-raw/CRAS/Censo_SUAS_2017_CRAS.zip", exdir = "data-raw/CRAS")

######### 2. 2019 ###################

# Read data and filter cities
cras_2019 <- data.table::fread('data-raw/CRAS/CRAS/Censo_SUAS_2019_dados_gerais_RH_CRAS_divulga‡Æo.csv',
                               select = c("NU_IDENTIFICADOR","q0_1","q0_3","q0_4","q0_6","q0_8","q0_9","q0_10", "q0_11",
                                          "q0_12","q0_15", 'Latitude', 'Longitude', 'q39')
                               )
cras_2019 <- data.table::setDT(cras_2019, key = 'NU_IDENTIFICADOR')[q0_9 %in% munis_df$code_muni]

data.table::setnames(cras_2019,
                     old = names(cras_2019),
                     new = c("code_suas","name_suas","logradouro","numero","bairro","cep","code_muni", 'code_uf','email',
                             "telefone","open_date",'lat_suas','long_suas', 'cad_unico')
                     )

cras_2019 <- data.table::merge.data.table(cras_2019,
                                          munis_df[,.(name_muni,code_muni,abrev_muni,abrev_estado)],
                                          all.x = TRUE,
                                          by = 'code_muni')

cras_2019 <- cras_2019[, endereco := paste(paste(paste(paste(paste(logradouro,numero, sep = ", "), bairro,sep = " - "),name_muni, sep=", "),abrev_estado,sep=" - "),cep,sep=", ") ]
cras_2019 <- cras_2019[,!c('logradouro','numero','bairro','cep')]

############### NÃO RODAR NOVAMENTE -- CHECK DAS COORDENADAS COM O API ############################

# Geolocalização das agência com Google API

#library(ggmap)

#my_api <- "ABC123"
#register_google(key = my_api) # registra a key do Google API
#
# Enderecos
#enderecos <- cras_2019$endereco %>% unique() 

# Run Google API
#coords_enderecos <- lapply(enderecos,geocode) %>% data.table::rbindlist()

# Combina output da query do Google com endereços completos
#enderecos <- as.data.frame(enderecos) %>% dplyr::bind_cols(coords_enderecos)
#enderecos <- data.table::setDT(enderecos)[, endereco := enderecos]

# Salva backup dos enderecos geolocalizados
#enderecos %>% dplyr::select(endereco,lon,lat) %>% 
#  data.table::fwrite(here::here('data','geocode_cras2019.csv'))

###########################################################################

# Merge dos enderecos com base original

library(magrittr)
# lEITURA DA BASE COM TODOS OS CRAS GEOLOCALIZADOS
enderecos <- readr::read_csv('data/acesso_oport/cras/geocode_cras.csv') %>% data.table::setDT()
enderecos <- enderecos[,.(lat=mean(lat),lon=mean(lon)),by=endereco]

cras_2019 <- data.table::merge.data.table(cras_2019,
                                          enderecos,
                                          all.x = TRUE,
                                          by = 'endereco')

cras_2019 <- cras_2019 %>% dplyr::select(code_cras = code_suas,
                                         name_cras = name_suas,
                                         name_muni,
                                         abrev_muni,
                                         code_muni,
                                         abrev_estado,
                                         code_estado = code_uf,
                                         cad_unico,
                                         endereco,
                                         telefone,
                                         email,
                                         data_abertura = open_date,
                                         lon,
                                         lat)

data.table::setDT(cras_2019,key = 'code_cras')[, cad_unico := ifelse(cad_unico == 0, 'Não', 'Sim')]

# Salva em formato .csv

readr::write_csv(cras_2019,'data/acesso_oport/cras/cras_2019.csv')

################ 2018 #########

# Leitura e limpeza
cras_2018 <- data.table::fread('data-raw/CRAS/1.CRAS/Censo_SUAS_2018_CRAS_Dados_Gerais_divulgacao.csv',
                               select = c("NU_IDENTIFICADOR","ident_0_1","ident_0_3","ident_0_4","ident_0_6",
                                          "ident_0_8","ident_0_9","ident_0_10", "ident_0_11",
                                          "ident_0_12","ident_0_15", 'Latitude', 'Longitude', 'q_40')
  )

cras_2018 <- data.table::setDT(cras_2018, key = 'NU_IDENTIFICADOR')[ident_0_9 %in% munis_df$code_muni]

data.table::setnames(cras_2018,
                     old = names(cras_2018),
                     new = c("code_cras","name_cras","logradouro","numero","bairro","cep","code_muni", 'code_estado','email',
                             "telefone","data_abertura",'lat_suas','long_suas', 'cad_unico')
)

cras_2018 <- data.table::merge.data.table(cras_2018,
                                          munis_df[,.(name_muni,code_muni,abrev_muni,abrev_estado)],
                                          all.x = TRUE,
                                          by = 'code_muni')

cras_2018 <- cras_2018[, endereco := paste(paste(paste(paste(paste(logradouro,numero, sep = ", "), bairro,sep = " - "),name_muni, sep=", "),abrev_estado,sep=" - "),cep,sep=", ") ]
cras_2018 <- cras_2018[,!c('logradouro','numero','bairro','cep')]

# Etapa 0 do geocode: ver quais enderecos tem grafia diferente da base de 2019, mas são os mesmos

cras_2018 <- data.table::merge.data.table(cras_2018, cras_2019[,.(code_cras,endereco)],
                                          all.x = TRUE, by = 'code_cras')

cras_2018 <- cras_2018[, check := endereco.x == endereco.y]

issues <- cras_2018[check == FALSE | is.na(check)]
issues <- issues[,cep.x := stringr::str_sub(endereco.x,-8)]
issues <- issues[,cep.y := stringr::str_sub(endereco.y,-8)]
issues <- issues[,check := cep.x == cep.y]
issues <- issues[check == FALSE| is.na(check)]

cras_2018 <- cras_2018[, endereco := ifelse(code_cras %nin% issues$code_cras, endereco.y,endereco.x)]
cras_2018 <- cras_2018[,!c('endereco.x','endereco.y', 'check')]

### NÃO RODAR NOVAMENTE - ETAPA DE GEOCODE ##################

#library(ggmap)

#my_api <- "ABC123"
#register_google(key = my_api) # registra a key do Google API

# Run Google API
#coords_enderecos <- lapply(enderecos,geocode) %>% data.table::rbindlist()

#enderecos <- as.data.frame(enderecos) %>% dplyr::bind_cols(coords_enderecos)
#enderecos <- data.table::setDT(enderecos)[, endereco := enderecos]

# Salva backup dos enderecos geolocalizados
#enderecos %>% dplyr::select(endereco,lon,lat) %>% bind_rows(cras_2019[,.(endereco,lon,lat)]) %>% unique() %>% 
#  write_csv(here::here('data','geocode_cras.csv'))

#geocode <- readr::read_csv(here::here('data','geocode_cras.csv'))

###########################################################################################################

# Merge dos enderecos com a base

cras_2018 <- data.table::merge.data.table(cras_2018,
                                          enderecos,
                                          all.x = TRUE,
                                          by = 'endereco')

cras_2018 <- cras_2018 %>% dplyr::select(code_cras,
                                         name_cras,
                                         name_muni,
                                         abrev_muni,
                                         code_muni,
                                         abrev_estado,
                                         code_estado,
                                         cad_unico,
                                         endereco,
                                         telefone,
                                         email,
                                         data_abertura,
                                         lon,
                                         lat)

data.table::setDT(cras_2018,key = 'code_cras')[, cad_unico := stringr::str_sub(cad_unico,1,3)]

# Salva em formato .csv
readr::write_csv(cras_2018,'data/acesso_oport/cras/cras_2018.csv')

#######3 4. 2017 #########################

# Leitura e limpeza
cras_2017 <- data.table::fread('data-raw/CRAS/Censo_SUAS_2017_CRAS/Censo SUAS 2017_CRAS_divulgacao_Base de dados.csv',
                               select = c("NºIDENTIFICADOR","ident.1.Nome","ident.3.Endereço","ident.4.Núm",
                                          "ident.6.Bairro","ident.8.CEP","IBGE7","ident.10.UF", "ident.11.Email",
                                        "ident.12.Tel","ident.15.DTImp", 'Latitude', 'Longitude', 'q35')
)

data.table::setnames(cras_2017,
                     old = names(cras_2017),
                     new = c("code_cras","name_cras","logradouro","numero","bairro","cep","code_muni", 'code_estado','email',
                             "telefone","data_abertura",'lat_suas','long_suas', 'cad_unico')
)

cras_2017 <- data.table::setDT(cras_2017, key = 'code_cras')[code_muni %in% munis_df$code_muni]

cras_2017 <- data.table::merge.data.table(cras_2017,
                                          munis_df[,.(name_muni,code_muni,abrev_muni,abrev_estado)],
                                          all.x = TRUE,
                                          by = 'code_muni')

cras_2017 <- cras_2017[, endereco := paste(paste(paste(paste(paste(logradouro,numero, sep = ", "), bairro,sep = " - "),name_muni, sep=", "),abrev_estado,sep=" - "),cep,sep=", ") ]
cras_2017 <- cras_2017[,!c('logradouro','numero','bairro','cep')]

# Checar enderecos com grafia distinta entre anos

cras_2017 <- data.table::merge.data.table(cras_2017,
                                          dplyr::bind_rows(cras_2018[,.(code_cras,endereco)], 
                                                    cras_2019[,.(code_cras,endereco)]) %>% 
                                            unique() %>% 
                                            data.table::setDT(),
                                          all.x = TRUE, by = 'code_cras')

cras_2017 <- cras_2017[, check := endereco.x == endereco.y]

issues <- cras_2017[check == FALSE | is.na(check)]
issues <- issues[,cep.x := stringr::str_sub(endereco.x,-8)]
issues <- issues[,cep.y := stringr::str_sub(endereco.y,-8)]
issues <- issues[,check := cep.x == cep.y]
issues <- issues[check == FALSE| is.na(check)]

cras_2017 <- cras_2017[, endereco := fifelse(code_cras %nin% issues$code_cras, endereco.y,endereco.x)]
cras_2017 <- cras_2017[,!c('endereco.x','endereco.y', 'check')]

### NÃO RODAR NOVAMENTE - ETAPA DE GEOCODE ##################

#enderecos <- issues$endereco.x %>% unique()

#library(ggmap)

#my_api <- "abc123"
#register_google(key = my_api) # registra a key do Google API

# Run Google API
#coords_enderecos <- lapply(enderecos,geocode) %>% data.table::rbindlist()

#enderecos <- as.data.frame(enderecos) %>% dplyr::bind_cols(coords_enderecos)
#enderecos <- data.table::setDT(enderecos)[, endereco := enderecos]

# Salva backup dos enderecos geolocalizados
#enderecos %>% dplyr::select(endereco,lon,lat) %>% bind_rows(geocode) %>% unique() %>% 
#  write_csv(here::here('data','geocode_cras.csv'))

#geocode <- readr::read_csv(here::here('data','geocode_cras.csv'))

########################################################################

# Merge com enderecos e salva

cras_2017 <- data.table::merge.data.table(cras_2017,
                                          enderecos,
                                          all.x = TRUE,
                                          by = 'endereco')

cras_2017 <- cras_2017 %>% dplyr::select(code_cras,
                                         name_cras,
                                         name_muni,
                                         abrev_muni,
                                         code_muni,
                                         abrev_estado,
                                         code_estado,
                                         cad_unico,
                                         endereco,
                                         telefone,
                                         email,
                                         data_abertura,
                                         lon,
                                         lat)

data.table::setDT(cras_2017,key = 'code_cras')[, cad_unico := stringr::str_sub(cad_unico,1,3)]

# Salva em formato .csv
readr::write_csv(cras_2017,'data/acesso_oport/cras/cras_2017.csv')