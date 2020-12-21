


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
empregos <- readr::read_rds("../../data/acesso_oport/rais/2017/rais_2017_etapa8.rds") # para 2018

# remove lat lon missing
empregos <- empregos[!is.na(lat), ]

# filter only estabs with high wuality geocode
empregos <- empregos[PrecisionDepth %in% c("4 Estrelas", "3 Estrelas", "street_number", "route")]


# trazer razao social
estabs <- fread("../../data-raw/rais/2018/rais_estabs_raw_2018.csv", select = c("id_estab", "razao_social"))
estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

empregos <- merge(empregos, estabs, by = "id_estab", all.x = TRUE)




# Aplica funcao de diagnostico --------------------------------------------------------------------------------------

# carrega funcao
source('./R/fun/diagnost_hex_uso_solo.R')

# 1/2 Identificar quantidade de empregos por hexagono ( identifica hexagonos problema)
fim <- diagnost_hex_uso_solo("for", "trabalho", 3000, ano = 2019)

table(fim$geocode_engine)
table(fim$PrecisionDepth)


# mapView(fim, zcol='empregos_total')

# maybe these rrepreents a too many hex problem
fim %>%
  st_set_geometry(NULL) %>%
  group_by(id_hex, PrecisionDepth) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(id_hex) %>%
  mutate(prop = n/sum(n)) %>%
  filter(PrecisionDepth == "3 Estrelas", prop > 0.5)

  

# lista de hex problematicos p 2018
hex_probS <- data.frame( hexs = c(
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



# maybe these rrepreents a cnpj problem
fim %>%
  st_set_geometry(NULL) %>%
  group_by(id_hex) %>%
  mutate(sum = sum(total_corrigido)) %>%
  mutate(prop = total_corrigido/sum) %>%
  filter(prop > 0.5) %>%
  select(id_estab, razao_social, prop)

02685728000120 # for
05305430000135 # for
01437408000198 # for
04368898000106 # cur
04370282000170 # cur
19201128000141 # bho
37162435000142 # bho
08689024000101 # bho
92966571000101 # poa
88630413000796 # poa
02539959000125 # rio
31546484000100 # bsb
33833880000136 # sal
01616929000102 # goi
14698658000123 # bel
83367342000252 # bel
04113174000111 # bel
02295753000105 # bel
50844182001208 # cam
08848807000190 # cam
06019070000178 # slz
09347229000171 # slz
12294708000181 # mac
02773312000163 # nat
07442731000136 # nat
01602361000170 # nat

# Lista de CNPJS problematicos (2017)

01437408000198 # for - Nordeste Cidadania
02137309000153 # spo - Icomon Tecnologia
17178195000167 # bho - Sociedade Mineira de Cultura
03873484000171 # goi - EMPREZA SERVICE CENTER - Locação de mão-de-obra temporária (EM RECUPERACAO JUDICIAL)
67945071000138 # cam - Sapore S/A , 56.2 Serviços de catering, bufê e outros serviços de comida preparada
03254082000512 # slz - INSTITUTO ACQUA, com varias unidades em outras cidades
00801512000157 # duq - AGILE CORP 56.2 Serviços de catering, bufê e outros serviços de comida preparada

SERVFAZ


# Lista dos hexagonos problematicos (2017)

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
  




