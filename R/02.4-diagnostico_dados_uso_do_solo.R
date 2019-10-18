


# carregar bibliotecas -----------------------------------------------------------------------------
source('./R/fun/setup.R')





# ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------

# Saude --------------------------------------
cnes <- readr::read_rds("../data/hospitais/health_facilities2019_filtered.rds") 

cnes <- cnes[!is.na(lat),] 
# cnes <- cnes[!is.na(NIV_HIER),] 

cnes <- cnes %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)




# Escolas  -------------------------------------
# abrir censo escolar geo
escolas <- read_rds("../data/censo_escolar/educacao_inep_2019.rds") %>%
  # Deletar escolas q nao foram localizadas
  dplyr::filter(!is.na(lat)) %>%
  # Selecionar variaveis
  dplyr::select(cod_escola = CO_ENTIDADE, uf = CO_UF, municipio = NO_MUNICIPIO, 
                cod_mun = CO_MUNICIPIO, 
                NO_ENTIDADE.x,
                mat_infantil, mat_fundamental, mat_medio, lon, lat)
# tidyr::gather(tipo, mat_n, mat_infantil:mat_medio)




# Empregos ----------------------------------------------------------
# Abrir rais geo
empregos <- readr::read_rds("../data/rais/rais_2017_corrigido_latlon_censoEscolar.rds") # para 2017
# empregos_etapa8 <- readr::read_rds("../data/rais/rais_2017_etapa8.rds") # para 2017

# remove lat lon missing
empregos <- empregos[!is.na(lat), ]
# empregos_etapa8 <- empregos_etapa8[!is.na(lat), ]






# Aplica funcao de diagnostico --------------------------------------------------------------------------------------

# carrega funcao
source('./R/fun/diagnost_hex_uso_solo.R')

# 1/2 Identificar quantidade de empregos por hexagono ( identifica hexagonos problema)
fim <- diagnost_hex_uso_solo("ter", "trabalho", 2000)
# fim8 <- diagnost_hex_uso_solo8("ter", "trabalho", 2000)


View(fim)
mapView(fim, zcol='empregos_total') + mapView(fim8, zcol='empregos_total')



# 2/2 Identifica quais empressas e enderecos estao em cada Hexagno ( identifica empresas/enderecos problema)
oi <- fim %>% filter(id_hex == "89a8c0cea23ffff") %>% .$id_estab

empregos %>% 
  filter(id_estab %in% oi) %>% 
  View()




# Lista de CNPJS problematicos

01437408000198 # for - Nordeste Cidadania
02137309000153 # spo - Icomon Tecnologia
17178195000167 # bho - Sociedade Mineira de Cultura
03873484000171 # goi - EMPREZA SERVICE CENTER - Locação de mão-de-obra temporária (EM RECUPERACAO JUDICIAL)
67945071000138 # cam - Sapore S/A , 56.2 Serviços de catering, bufê e outros serviços de comida preparada
03254082000512 # slz - INSTITUTO ACQUA, com varias unidades em outras cidades
00801512000157 # duq - AGILE CORP 56.2 Serviços de catering, bufê e outros serviços de comida preparada

SERVFAZ


# Lista dos hexagonos problematicos

hex_prob <- 
c("89a8c0cea23ffff", # goi - Ruas com nome de numero (e.g. "RUA T50, 71", ou "RUA 05, 715")
  "8980088739bffff", # slz - Avenida dos Holandeses (erro Galileo - mesmo afirmando que encontrou 4 estrelas)
  "898008829a7ffff", # slz - AVENIDA JOAO PESSOA, 363, São Luís - State of Maranhão
  "898008829afffff", # slz - AV ENG EMILIANO MACIEIRA
  "898008829abffff", # slz - AV ENG EMILIANO MACIEIRA de novo
  "89800880d5bffff", # slz - Av. São Luís Rei de França
  "89800882a3bffff", # slz AV JERONIMO DE ALBUQUERQUE
  "89800882a0fffff", # slz AV JERONIMO DE ALBUQUERQUE de novo
  "89800882a53ffff", # slz - Ruas com nome de numero (e.g. "RUA T50, 71", ou "RUA 05, 715")
  "89a8a2a6413ffff", # sgo - Avenida Eugênio Borges (rodovia?)
  "8980055454bffff", # ter - AV DEPUTADO PAULO FERRAZ
  "89800556a2bffff", # ter - muitas ruas
  "89800554ccfffff", # ter - muitas ruas
  "89800554e17ffff") # ter - muitas ruas


# Hexagonos que serao corrigidos pelo tratamento das rodovias:
# 89804696927ffff # brl - enderecos em Rodovia AUGUSTO MONTENEGRO 
# 89a81070ad3ffff # gua - enderecos em Rodovia
# 89a8a06f1a7ffff # duq- Rodovia Washington Luisa



# SEÇÃO DE TESTE PARA COMPARACAO ENTRE ANTES DE DEPOIS ----------------------------------------


# fim %>% st_set_geometry(NULL) %>% group_by(id_hex) %>% summarise(n(), tot = sum(total_corrigido)) %>% arrange(desc(tot))
# fim8 %>% st_set_geometry(NULL) %>% group_by(id_hex) %>% summarise(n(), tot = sum(total_corrigido)) %>% arrange(desc(tot))
# sum(fim$total_corrigido)
# sum(fim8$total_corrigido)

# # antes e depois para cada hex problematico
# fim_prob <- fim %>% filter(id_hex %in% hex_prob) %>% 
#   st_set_geometry(NULL) %>%
#   select(id_hex, empregos_total) %>% 
#   count(id_hex, empregos_total)
# 
# fim_prob8 <- fim8 %>% filter(id_hex %in% hex_prob) %>% 
#   st_set_geometry(NULL) %>%
#   select(id_hex, empregos_total) %>% 
#   count(id_hex, empregos_total)
# 
# fim_comp <- left_join(fim_prob, fim_prob8, by = "id_hex", suffix = c(".antes", ".depois"))










# Problemas gerais encontrados:
  # - Educacao mesmo estabelecimento duplicado para diferentes níveis
  # - Saude mesmo estabelecimento duplicado para diferentes níveis
  # - Trabalho - geocode em rodovias
  




