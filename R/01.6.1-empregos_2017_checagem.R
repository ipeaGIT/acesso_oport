# esse script agrega os dados de empregos da etapa do censo escolar por hexagono e depois
# identifica hexagonos problemasticos a partir de dois metodos:
# 1) corrigindo quais hexagonos tem uma concentracao indevida de empregos (geralmente acima de 2000 empregos)
# caso esses hexagnons tenham uma alta proporcao de 3 estrelas, todos os estabelecimentos 3 estrelas dele sao
# levados para o gmaps
# 2) comparando a quantidade de empregos dos hex de um ano para o outro (funcao compare_jobs_distribution.R)
# caso o hexagono tenha um diferenca de 1000 empregos de um ano pra outro, todos os estabelecimentos 3 estrelas
# desses hexagonos sao levados para o gmaps

# toda vez que um rodada de correcao eh feita, todos os anos sao atualizados
# dessa forma, toda rais tera um check (check1) correspondente a chechagem 1) e outros n checks a partir
# da checagem a partir de cada ano em comparacao
# assim, a cada ano que for adicionado e checagem realizado, todos os anos vao sofrer um atualizacao com as novas
# coordenadas rodadas no gmaps


# carregar bibliotecas
source('./R/fun/setup.R')




# 1) agrupar empregos nos hexagonos -----------------------------------------------------------


# abrir funcao que agrupar os empregos nos hexagonos para realizacao das correcoes
source("R/fun/agrupar_empregos_hex_teste.R")

# aplicar a funcao para 2017
agrupar_hex_teste("2017", rais = "../../data/acesso_oport/rais/2017/rais_2017_corrigido_geocoded_censoEscolar.rds")



# 2) Corrigindo hexagonos problematicos --------------------------
# No caso de 2017, somente a etapa 1) eh feita porque nao ha ano anterior para comparacao

# abrir rais
rais <- readr::read_rds("../../data/acesso_oport/rais/2017/rais_2017_corrigido_geocoded_censoEscolar.rds")

# # trazer razao social
# estabs <- fread("../../data-raw/rais/2017/rais_2017_raw.csv"), select = c("id_estab", "razao_social"))
# estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
# estabs <- distinct(estabs, id_estab, .keep_all = TRUE)

# rais <- merge(rais, estabs, by = "id_estab", all.x = TRUE)

# carrega funcao que faz o diagnostico dos empregos
source('./R/fun/diagnost_hex_empregos.R')


# lista dos hexagonos problematicos (nesse ano, essa checagem 1) foi feita manualmente)
hex_probs <- data.frame(
  id_hex = c("89a8c0cea23ffff", # goi - Ruas com nome de numero (e.g. "RUA T50, 71", ou "RUA 05, 715")
             "8980088739bffff", # slz, - Avenida dos Holandeses (erro Galileo - mesmo afirmando que encontrou 4 estrelas)
             "898008829a7ffff", # slz, - AVENIDA JOAO PESSOA, 363, São Luís - State of Maranhão
             "898008829afffff", # slz, - AV ENG EMILIANO MACIEIRA
             "898008829abffff", # slz, - AV ENG EMILIANO MACIEIRA de novo
             "89800880d5bffff", # slz, - Av. São Luís Rei de França
             "89800882a3bffff", # slz, AV JERONIMO DE ALBUQUERQUE
             "89800882a0fffff", # slz, AV JERONIMO DE ALBUQUERQUE de novo
             "89800882a53ffff", # slz, - Ruas com nome de numero (e.g. "RUA T50, 71", ou "RUA 05, 715")
             "89a8a2a6413ffff" # sgo, - Avenida Eugênio Borges (rodovia?)
  ),
  sigla_muni = c("goi", "slz","slz", "slz", "slz", "slz", "slz", "slz", "slz", "sgo")
  
)



# ler hex agregados e juntar
hex <- lapply(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds", munis_df$abrev_muni), read_rds) %>%
  rbindlist() %>%
  st_sf() %>%
  select(id_hex) %>%
  # filtra hex ids problematicos
  filter(id_hex %in% hex_probs$id_hex)


# Qual o codigo dos municipio em questao?
# cod_mun_ok <- munis_df_2019[abrev_muni %in% unique(hex_probs$sigla_muni)]$code_muni

# Carrega somente os dados da rais estabes nestes municipios
base <- rais %>%
  filter(!is.na(lon)) %>%
  # filter(codemun %in% substr(cod_mun_ok, 1, 6)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(codemun, id_estab, type_input_galileo)

# Intersecao do hex ids problema com base de uso do solo (p/ saber em quais hexagonos estao os empregos)
fim <- st_join(base, hex) %>%
  filter(!is.na(id_hex)) %>% setDT()

# # get only new estabs in 2019
# fim_year <- fim[type_input_galileo %like% '2019']

# Extrair os estabs 'problematicos' concentrados em hexagonos
oi <- fim %>% distinct(id_estab) %>% .$id_estab

# Filtrar estabs problematicos
rais_prob <- rais %>%
  filter(id_estab %in% oi)

# lista de enderecos com problema
enderecos_etapa7 <- rais_prob %>% 
  mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% 
  .$fim

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[3])

# geocode
coordenadas_google_etapa7 <- lapply(X=enderecos_etapa7, ggmap::geocode, output = 'all')

# identify list names as id_estab
names(coordenadas_google_etapa7) <- rais_prob$id_estab 

# save
write_rds(coordenadas_google_etapa7, "../../data/acesso_oport/rais/2017/geocode/rais_2017_output_google_manyhex.rds")
coordenadas_google_etapa7 <- read_rds("../../data/acesso_oport/rais/2017/geocode/rais_2017_output_google_manyhex.rds")


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

# create dt 
enderecos_google_etapa7 <- lapply(coordenadas_google_etapa7, create_dt)


# rbind as data.table
enderecos_google_etapa7_dt <- rbindlist(enderecos_google_etapa7, idcol = "id_estab",
                                        use.names = TRUE)

# identify searchedaddress
searchedaddress <- filter(rais_prob, id_estab %in% names(enderecos_google_etapa7)) %>%
  mutate(SearchedAddress = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress)
enderecos_google_etapa7_dt <- left_join(enderecos_google_etapa7_dt, searchedaddress, by = "id_estab") %>% setDT()
# identify problem
enderecos_google_etapa7_dt[, geocode_engine := 'gmaps_toomany_hex_2017']
# identify quality
enderecos_google_etapa7_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]



# bring tot he original dataset
rais[enderecos_google_etapa7_dt, on = "id_estab",
     c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
       list(i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]

# check
table(rais$PrecisionDepth, useNA = 'always')
table(rais$geocode_engine, useNA = 'always')
table(rais$type_input_galileo, useNA = 'always')



# trazer razao social
estabs <- fread("../../data-raw/rais/2017/rais_estabs_raw_2017.csv", select = c("id_estab", "razao_social"))
estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
estabs <- distinct(estabs, id_estab, .keep_all = TRUE)
rais <- merge(rais, estabs, by = "id_estab", all.x = TRUE)

# make stabs are unique
rais <- rais %>% distinct(id_estab, .keep_all = TRUE)


# Salvar
write_rds(rais, "../../data/acesso_oport/rais/2017/check/rais_2017_check1.rds")




# # excluir cnpjs problematicos -----------------------------------------------------------------
# rais_2017 <- read_rds("../../data/acesso_oport/rais/2017/check/rais_2017_check1.rds")
# 
# # definir cnpjs problematicos
# cnpjs_prob <- data.frame(id_estab = c("01437408000198", # for - Nordeste Cidadania
#                                       "02137309000153", # spo - Icomon Tecnologia
#                                       "17178195000167", # bho - Sociedade Mineira de Cultura
#                                       "03873484000171", # goi - EMPREZA SERVICE CENTER - Locação de mão-de-obra temporária (EM RECUPERACAO JUDICIAL)
#                                       "67945071000138", # cam - Sapore S/A , 56.2 Serviços de catering, bufê e outros serviços de comida preparada
#                                       "03254082000512", # slz - INSTITUTO ACQUA, com varias unidades em outras cidades
#                                       "00801512000157") # duq - AGILE CORP 56.2 Serviços de catering, bufê e outros serviços de comida preparada
# )
# 
# # export
# write_rds(cnpjs_prob, "../../data/acesso_oport/rais/2017/cnpjs_prob_2017.rds")
# 
# 
# # Excluir esses CNPJs da rais
# rais_2017 <- rais_2017[id_estab %nin% cnpjs_prob$id_estab]
# 
# # Salvar RAIS etapa 8 
# write_rds(rais_2017, "../../data/acesso_oport/rais/2017/check/rais_2017_check1_etapa2.rds")


agrupar_hex_teste("2017", rais = "../../data/acesso_oport/rais/2017/check/rais_2017_check1.rds")