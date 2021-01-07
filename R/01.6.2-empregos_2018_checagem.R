# esse script agrega os dados de empregos da etapa do censo escolar por hexagono e depois
# identifica hexagonos problematicos a partir de dois metodos:
# 1) corrigindo quais hexagonos tem uma concentracao indevida de empregos (geralmente acima de 2000 empregos)
# caso esses hexagnons tenham uma alta proporcao de 3 estrelas, todos os estabelecimentos 3 estrelas dele sao
# levados para o gmaps
# 2) comparando a quantidade de empregos dos hex de um ano para o outro (funcao compare_jobs_distribution.R)
# caso o hexagono tenha um diferenca de 1000 empregos de um ano pra outro, todos os estabelecimentos 3 estrelas
# desses hexagonos sao levados para o gmaps

# toda vez que um rodada de correcao eh feita, todos os anos sao atualizados
# dessa forma, toda rais tera um check (check1) correspondente a chechagem 1 e outros n checks a partir
# da checagem a partir de cada ano em comparacao
# assim, a cada ano que for adicionado e checagem realizado, todos os anos vao sofrer um atualizacao com as novas
# coordenadas rodadas no gmaps


# carregar bibliotecas
source('./R/fun/setup.R')


# 1) agrupar empregos nos hexagonos -----------------------------------------------------------

source("R/fun/agrupar_empregos_hex_teste.R")
agrupar_hex_teste("2018", rais = "../../data/acesso_oport/rais/2018/rais_2018_corrigido_geocoded_censoEscolar.rds")



# 2) Corrigindo hexagonos problematicos --------------------------

# abrir rais
rais <- readr::read_rds("../../data/acesso_oport/rais/2018/rais_2018_corrigido_geocoded_censoEscolar.rds")


# carrega funcoes para correcao
source('./R/fun/diagnost_hex_empregos.R')
source("R/fun/compare_jobs_distribution.R")

# identiicar quais hexagonos tem uma concentracao indevida de empregos (tipo 1)
hex_probs_diag <- data.frame( id_hex = c(
  "8980104c07bffff",
  "8980104c0b7ffff","8980104c1afffff","89a83136c4fffff","8980104c1afffff","89a90e93537ffff",
  "89a8a06a40fffff","89a8a06a40fffff","89a810054b3ffff","89a81007357ffff","89a8100e8b3ffff",
  "89a8103b0b7ffff","89a8c2481a3ffff","89a8c248b07ffff","89a8c248b0fffff","89a8c248b3bffff",
  "89a8c248dbbffff","89a8c249d43ffff","89a8c249d57ffff","89a8c249d8fffff","89a8c249ddbffff",
  "89a8c24aa7bffff","89a8c24e6d7ffff","89a8c24f6dbffff","898116b181bffff","89a8c0ce2cfffff",
  "89a81070ad3ffff","89a81072d67ffff","89800882e27ffff","89a8a06f1a7ffff","89a8a3d666bffff",
  "89819d2cc73ffff","89819d2cc97ffff"),
  sigla_muni = c("for","for","for","cur","gua","poa","rio","rio","spo","spo", "spo","spo",
                 "bsb","bsb","bsb","bsb","bsb","bsb","bsb","bsb","bsb","bsb","bsb","bsb",
                 "sal","goi","gua","gua","slz","duq","duq","nat","nat")
)

# identificar heagonos comparando a quantidade de empregos dos hex de um ano para o anterior
hex_probs_compare <- lapply(munis_df$abrev_muni, compare_jobs_distribution, 
                            corte = 1000, ano_bottom = 2017, ano_top = 2018) %>% rbindlist()



# ler hex agregados e juntar
hex <- lapply(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds", munis_df$abrev_muni), read_rds) %>%
  rbindlist() %>%
  st_sf() %>%
  select(id_hex) %>%
  # filtra hex ids problematicos
  filter(id_hex %in% c(hex_probs_diag$id_hex, hex_probs_compare$id_hex))


# Qual o codigo dos municipio em questao?
# cod_mun_ok <- munis_df_2019[abrev_muni %in% unique(hex_probs$sigla_muni)]$code_muni

# Carrega somente os dados da rais estabes nestes municipios
base <- rais %>%
  filter(!is.na(lon)) %>%
  # filter(codemun %in% substr(cod_mun_ok, 1, 6)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(codemun, id_estab, type_input_galileo, geocode_engine, PrecisionDepth)


# Intersecao do hex ids problema com base de uso do solo
fim <- st_join(base, hex) %>%
  filter(!is.na(id_hex)) %>% setDT()

table(fim$type_input_galileo, useNA = 'always')
table(fim$geocode_engine, useNA = 'always')
table(fim$PrecisionDepth, useNA = 'always')

# # get only new estabs in 2019
# fim_year <- fim[type_input_galileo %like% '2019']

# select only galileo 3 estrelas
fim_filter <- filter(fim, PrecisionDepth == "3 Estrelas")

# Extrair os estabs 'problematicos' concentrados em hexagonos
oi <- fim_filter %>% distinct(id_estab) %>% .$id_estab

# Filtrar estabs
rais_prob <- rais %>%
  filter(id_estab %in% oi)

# lista de enderecos com problema
enderecos_etapa7 <- rais_prob %>% 
  mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% 
  .$fim

# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[3])

# geocode
coordenadas_google_etapa7 <- lapply(X=enderecos_etapa7, ggmap::geocode, output = 'all')

# identify list names as id_estab
names(coordenadas_google_etapa7) <- rais_prob$id_estab 

# save
write_rds(coordenadas_google_etapa7, "../../data/acesso_oport/rais/2018/geocode/rais_2018_output_google_manyhex.rds")
coordenadas_google_etapa7 <- c(read_rds("../../data/acesso_oport/rais/2018/geocode/rais_2018_output_google_manyhex.rds"),
                               read_rds("../../data/acesso_oport/rais/2018/geocode/rais_2018_output_google_etapa9.rds"))

coordenadas_google_etapa7 <- coordenadas_google_etapa7[names(coordenadas_google_etapa7) %in% oi]

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
  mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress)
enderecos_google_etapa7_dt <- left_join(enderecos_google_etapa7_dt, searchedaddress, by = "id_estab") %>% setDT()
# identify problem
enderecos_google_etapa7_dt[, geocode_engine := 'gmaps_toomany_hex_2018']
# identify quality
enderecos_google_etapa7_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]



# bring tot he original dataset
rais[enderecos_google_etapa7_dt, on = "id_estab",
     c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
       list(i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]


table(rais$PrecisionDepth, useNA = 'always')
table(rais$geocode_engine, useNA = 'always')
table(rais$type_input_galileo, useNA = 'always')

# trazer razao social
estabs <- fread("../../data-raw/rais/2018/rais_estabs_raw_2018.csv", select = c("id_estab", "razao_social"))
estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
estabs <- distinct(estabs, id_estab, .keep_all = TRUE)

rais <- merge(rais, estabs, by = "id_estab", all.x = TRUE)

# make stabs are unique
rais <- rais %>% distinct(id_estab, .keep_all = TRUE)


# Salvar
write_rds(rais, "../../data/acesso_oport/rais/2018/check/rais_2018_check1.rds")


# # excluir cnpjs problematicos -----------------------------------------------------------------
# rais_2018 <- read_rds("../../data/acesso_oport/rais/2018/check/rais_2018_check1.rds")
# 
# # definir cnpjs problematicos
# cnpjs_prob <- data.frame(id_estab = c("02685728000120","05305430000135", "01437408000198", "04368898000106","04370282000170","19201128000141",
#                                       "37162435000142","08689024000101","92966571000101","88630413000796","02539959000125",
#                                       "31546484000100","33833880000136","01616929000102","14698658000123","83367342000252",
#                                       "04113174000111","02295753000105","50844182001208","08848807000190","06019070000178",
#                                       "09347229000171","12294708000181","02773312000163","07442731000136","01602361000170")
# )
# 
# # export
# write_rds(cnpjs_prob, "../../data/acesso_oport/rais/2018/cnpjs_prob_2018.rds")
# 
# 
# # Excluir esses CNPJs da rais
# # bring cnpjs from 2017
# cnpjs_prob_2017 <- read_rds("../../data/acesso_oport/rais/2017/cnpjs_prob_2017.rds")
# rais_2018 <- rais_2018[id_estab %nin% c(cnpjs_prob$id_estab, cnpjs_prob_2017$id_estab)]
# 
# # Salvar RAIS etapa 8 
# write_rds(rais_2018, "../../data/acesso_oport/rais/2018/check/rais_2018_check1_etapa2.rds")




# 3) atualizar empregos nos hexagonos para 2018 (check 1) -----------------------------------------------------------
agrupar_hex_teste("2018", rais = "../../data/acesso_oport/rais/2018/check/rais_2018_check1.rds")



# 4) atualizar empregos nos hexagonos para 2017 (check 2) -----------------------------------------------------------
rais_2017 <- read_rds("../../data/acesso_oport/rais/2017/check/rais_2017_check1.rds") %>% setDT()
rais_2018 <- read_rds("../../data/acesso_oport/rais/2018/check/rais_2018_check1.rds")

# get only estabs that were didnt change address
rais_2018 <- rais_2018 %>% filter(type_input_galileo == "rais_2017")
# get only estab that were updated
rais_2018 <- rais_2018 %>% filter(geocode_engine == "gmaps_toomany_hex_2018") %>% setDT()

# update rais 2017
rais_2017[rais_2018, on = "id_estab",
          c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
            list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)
          ]

# save it
write_rds(rais_2017, "../../data/acesso_oport/rais/2017/check/rais_2017_check2.rds")


# run hex agregados from 2017
agrupar_hex_teste("2017", rais = "../../data/acesso_oport/rais/2017/check/rais_2017_check2.rds")




