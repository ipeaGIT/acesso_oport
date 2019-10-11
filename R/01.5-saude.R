#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.5 Download dos dados geolocalizados dos estabelecimentos de saude
##info
# fonte: Cadastro Nacionl dos Estabelecimentos de Saude (CNES) - DataSus
  
  
# carregar bibliotecas
source('./R/fun/setup.R')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### 1.1 Download geocoded PMAQ data ------------------------------------
#   # better geocoded info for basic services
#   # source: http://aps.saude.gov.br/ape/pmaq
#   # file  http://189.28.128.100/dab/docs/portaldab/documentos/microdados_pmaq_cliclo3/modulo_I_ubs/UBS_Brasil.xlsx

  # read original Excel sheet
  pmaq_df <- readxl::read_xlsx(path = '../data-raw/hospitais/PMAQ/UBS_Brasil_ciclo3.xlsx',
                   sheet = 'Módulo I', col_types = rep("text", 425))
  

  
# clean PMAQ data
  
# remove invalid lat long info
  head(pmaq_df$LONGITUDE)
  pmaq_df <- subset(pmaq_df, LATITUDE !="0" )
  pmaq_df <- subset(pmaq_df, LATITUDE !="0.0" )
  pmaq_df <- subset(pmaq_df, LATITUDE !="9997" )



### Fix lat long info
  
# identificar cidades do projeto com dois digitos de latitude
munis <- purrr::map_dfr(dir("../data-raw/municipios/", recursive = TRUE, full.names = TRUE), read_rds) %>%
  as_tibble() %>%
  st_sf() %>%
  st_centroid() %>%
  # transformar para lon-lat %>%
  sfc_as_cols() %>%
  # quantos digitos as latitudes tem antes da virgula?
  mutate(lat_digits = sub("^-?(\\d+)[[:punct:]]{1}\\d+$", "\\1", lat)) %>%
  mutate(lat_digits = nchar(lat_digits)) %>%
  # municipio so tem 6 digitos
  mutate(code_muni = substr(code_muni, 1,6)) %>%
  # selecionar so as colunas necessarias
  dplyr::select(code_muni, lat_digits)
  

# trazer a quantidade de digitos para o pmaq_df
pmaq_df_digitos <- pmaq_df %>%
  rename(code_muni = IBGE) %>%
  # selecionar so os municipios do projeto
  filter(code_muni %in% substr(munis_df$code_muni, 1, 6)) %>%
  mutate(code_muni = as.character(code_muni)) %>%
  left_join(munis, by = "code_muni")

# criar dataframe com as coordenadas ajeitadas
pmaq_df_coords_fixed <- pmaq_df_digitos %>%
  # o ponto na longitude vai ser sempre depois do segundo numerico
  mutate(lon = sub("(^?-\\d{2})(\\d+)", "\\1\\.\\2", LONGITUDE)) %>%
  # o ponto na latitude vai depender do nchar
  mutate(lat = ifelse(lat_digits == 1, sub("(^?-\\d{1})(\\d+)", "\\1\\.\\2", LATITUDE),
                          sub("(^?-\\d{2})(\\d+)", "\\1\\.\\2", LATITUDE))) %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))


# # teste
# pmaq_df_coords_fixed %>%
#   to_spatial() %>%
#   mapview()


# save as .csv
fwrite(pmaq_df_coords_fixed, '../data-raw/hospitais/PMAQ/pmaq_df_coords_fixed.csv')
gc(reset = T)











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Leitura dos dados CNES ---------------------------------
  
## 2.1 Ler CNES ativos dos SUS - traz blueprint das intituicoes ativas em 2019

cnes19 <- readxl::read_xlsx(path = '../data-raw/hospitais/CNES_NDIS_01_10_2019_BANCO_COMP_08_2019.xlsx',
                        sheet = 'BANCO', skip = 14, 
                        col_types = c(rep("text", 11), "numeric", "numeric", rep("text", 17)))

# remove 1st NA rows
cnes19 <- cnes19[-c(1:3),]
str(cnes19)
head(cnes19)

# rename columns
names(cnes19)[15:30] <- c("instal_fisica_ambu", "instal_fisica_hospt", "complex_alta_ambu_est", "complex_alta_ambu_mun", "complex_baix_ambu_est", "complex_baix_ambu_mun", "complex_medi_ambu_est", "complex_medi_ambu_mun", "complex_alta_hosp_est", "complex_alta_hosp_mun", "complex_baix_hosp_est", "complex_baix_hosp_mun", "complex_medi_hosp_est", "complex_medi_hosp_mun", "complex_nao_aplic_est", "complex_nao_aplic_mun")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3.Limpeza dos dados CNES ---------------------------------


# Filter 0: healthcare nao aplica (pq nao tem servicos de alta/baixa complexidade, e.g. academias de saude, secretarias de saude etc)
  cnes_filter0 <- setDT(cnes19)[is.na(complex_nao_aplic_est)] 
  cnes_filter0 <- cnes_filter0[is.na(complex_nao_aplic_mun)]


# Filter 1: healthcare facilities operating with the public health system
  cnes_filter1 <- setDT(cnes_filter0)[ `ATENDE SUS`== 'SIM', ]
  
  
# Filter 2: Pessoa juridica
  cnes_filter2 <- cnes_filter1[ PESSOA_FÍSICA_OU_PESSOA_JURÍDI== 'PESSOA_JURÍDICA', ]

  
# filter 3: Only municipalities in the project
  cnes_filter3 <- subset(cnes_filter2, IBGE %in% substr(munis_df$code_muni, 1,6))
  
  
# filter 4: Only atendimento hospitalar ou ambulatorial
  cnes_filter4 <- cnes_filter3[ instal_fisica_ambu=="SIM" | instal_fisica_hospt=="SIM", ]
  
  
# filter 5. Remove special categories of facilities 
   
   # 5.1 Delete prison hospitals, research centers, police hospitals etc
   to_remove1 <- 'CENTRO DE ESTUDOS|PSIQUIAT|PRESIDIO|PENAL|JUDICIARIO|PENITENCIARIA|DETENCAO|PROVISORIA|SANATORIO|POLICIA| PADI|DE REGULACAO|VIGILANCIA|SAMU |ACADEMIA|DEPEND QUIMICO|REEDUCACAO SOCIAL|CAPS|CENTRO DE ATENCAO PSICOSSOCIAL|DISTRIB DE ORGAOS|MILITAR|CADEIA PUBLICA|DOMICILIAR'
                  # PADI = Programa de Atenção Domiciliar ao Idoso
                  # DE REGULACAO = gestora de servico
                  # CAPS - CENTRO DE ATENCAO PSICOSSOCIAL - saude mental e drogas
  
   
    
      
   # 5.2 Delete Home care, tele saude, unidades moveis de saude
   to_remove2 <- 'TELESSAUDE|UNIDADE MOVEL|DOMICILIAR|PSICOSSOCIAL|FARMACIA|DISTRIB DE ORGAOS'
   
 # apply filter 5
   cnes_filter5 <- cnes_filter4[ ESTABELECIMENTO %nlike% to_remove1 ]
   cnes_filter5 <- cnes_filter5[ TIPO_UNIDADE %nlike% to_remove2 ]
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
                                
 
table(cnes_filter5$health_low, useNA = "always")  # 3625
table(cnes_filter5$health_med, useNA = "always")  # 4254
table(cnes_filter5$health_high, useNA = "always") # 881




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4. Corrigir Lat Long  ---------------------------------

  ### Fix lat long info
  
  
  # trazer a quantidade de digitos para o cnes19
  cnes19_df_digitos <- cnes_filter5 %>%
    rename(code_muni = IBGE) %>%
    # selecionar so os municipios do projeto
    filter(code_muni %in% substr(munis_df$code_muni, 1, 6)) %>%
    mutate(code_muni = as.character(code_muni)) %>%
    left_join(munis %>% st_set_geometry(NULL), by = "code_muni")
  
  # criar dataframe com as coordenadas ajeitadas
  cnes19_df_coords_fixed <- cnes19_df_digitos %>%
    # primeiro, tirar tudo que for ponto ou virgula
    mutate(lon = gsub("(\\.|,)", "", LONGITUDE),
           lat = gsub("(\\.|,)", "", LATITUDE)) %>%
    # tirar sinal de negativo
    mutate(lon = gsub("-", "", lon),
           lat = gsub("-", "", lat)) %>%
    # o ponto na longitude vai ser sempre depois do segundo numerico, e vai ser sempre negativo
    mutate(lon = sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lon)) %>%
    # o ponto na latitude vai depender do nchar
    mutate(lat = ifelse(lat_digits == 1, sub("(^\\d{1})(\\d+)", "-\\1\\.\\2", lat),
                        sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lat))) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat))
  
  # # teste
  # cnes19_df_coords_fixed %>%
  #   filter(!is.na(lon)) %>%
  #   to_spatial() %>%
  #   mapview()
  
  
  
###### Usar dados de lat/lon quando eles existirem na PMAQ (estabelecimentos de baixa complexidade)
  # Read PMAQ data
  pmaq_df_coords_fixed <- fread('../data-raw/hospitais/PMAQ/pmaq_df_coords_fixed.csv', colClasses = 'character')
  pmaq_df_coords_fixed[, lat := as.numeric(lat)][, lon := as.numeric(lon)]

  # update lat lon info from PMAQ
  summary(cnes19_df_coords_fixed$lat) # 125 NAs
  setDT(cnes19_df_coords_fixed)[pmaq_df_coords_fixed, on=c('CNES'='CNES_FINAL'), c('lat', 'lon') := list(i.lat, i.lon) ] 
  summary(cnes19_df_coords_fixed$lat) # 124 NAs


      
###### Identificar CNES com lat/long problematicos

# A - poucos digitos
# B - fora dos limites do municipio
# C - com coordenadas NA
# D - mais de 5 estabelecimentos com coordenadas iguais
  
  
# qual o nivel de precisao das coordenadas deve ser aceito?
# 0.01 = 1113.2 m
# 0.001 = 111.32 m
# 0.0001 = 11.132 m
# 0.00001 = 1.1132 m
  

# A) Numero de digitos de lat/long apos ponto
  setDT(cnes19_df_coords_fixed)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  A_estbs_pouco_digito <- cnes19_df_coords_fixed[ ndigitos <=2,]
  
  
# B) fora dos limites do municipio
  
  # carrega shapes
    shps <- purrr::map_dfr(dir("../data-raw/municipios/", recursive = TRUE, full.names = TRUE), read_rds) %>% as_tibble() %>% st_sf()
  
  # convert para sf
    cnes19_df_coords_fixed_df <- cnes19_df_coords_fixed[!(is.na(lat))] %>% st_as_sf( coords = c('lon', 'lat'))
    temp_intersect <- sf::st_join(cnes19_df_coords_fixed_df, shps)
  
  # CNES que cairam fora de algum municipio
    B_muni_fora <- subset(temp_intersect, is.na(name_muni))
  
    
# C) Lat lon NA (feito mais a frente)
    
    
# D) mais de 5 estabelecimentos com coordenadas iguais
  
  # junta lat lon
    cnes19_df_coords_fixed$latlon <- paste0(cnes19_df_coords_fixed$lat, '/', cnes19_df_coords_fixed$lon)
  
  # freq de lat lon repetido
  tab_latlon <- cnes19_df_coords_fixed %>% count(latlon, sort = T)
  C_latlon_problema <- subset(tab_latlon, n >3 & latlon != "NA/NA")
    
    
    
  # juntar todas municipios com erro de lat/lon
    munis_problemaA <- subset(cnes19_df_coords_fixed, CNES %in% A_estbs_pouco_digito$CNES ) 
    munis_problemaB <- subset(cnes19_df_coords_fixed, CNES %in% B_muni_fora$CNES )
    munis_problemaC <- cnes19_df_coords_fixed[ is.na(lat), ]
    munis_problemaD <- subset(cnes19_df_coords_fixed, latlon %in% C_latlon_problema$latlon)
    

    
    munis_problema <- rbind(munis_problemaA, munis_problemaB, munis_problemaC, munis_problemaD)
    munis_problema <- dplyr::distinct(munis_problema, CNES, .keep_all=T) # remove duplicates
    
    # 1385 de 5.283 que vao para o Galileo
    
    
    
######## Gerar input para galileo
    
munis_problema_galileo <- munis_problema %>%
  select(CNES, log = LOGRADOURO, numero = NUMERO, bairro = BAIRRO, cep = CEP, cidade = MUNICÍPIO, uf = UF) %>%
  # juntar logradouro com numero
  mutate(rua = paste0(log, ", ", numero)) %>%
  select(-log, -numero)
    
write_delim(munis_problema_galileo, "../data-raw/hospitais/saude_2019_input_galileo.csv", delim = ";")
    

### RODAR GALILEO--------------
# depois de rodar o galileo.............
    
# abrir output galileo
saude_output_galileo <- fread("../data-raw/hospitais/saude_2019_output_galileo.csv") %>%
  # selecionar so os 4 estrelas
  filter(PrecisionDepth %in% c("4 Estrelas")) %>%
  # substituir virgula por ponto
  mutate(Latitude = str_replace(Latitude, ",", "\\.")) %>%
  mutate(Longitude = str_replace(Longitude, ",", "\\.")) %>%
  # selecionar colunas
  select(CNES, lat = Latitude, lon = Longitude) %>%
  mutate(CNES = as.character(CNES)) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon))
    
# Update lat lon info a partir de resultados do Galileo
summary(cnes19_df_coords_fixed$lon) # 122 NA's
setDT(cnes19_df_coords_fixed)[saude_output_galileo, on='CNES', c('lat', 'lon') := list(i.lat, i.lon) ]
summary(cnes19_df_coords_fixed$lon) # 40 NA's
    

    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Ainda restam lat/lon problematicas. Indentificar elas e jogar no google maps ------------------------------------------------------------------
    

#### GOOGLE 1, endereco completo ------------------------

# A) Escolas com lat/long de baixa precisao (1 ou 2 digitos apos casa decimal)
setDT(cnes19_df_coords_fixed)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
lat_impreciso <- subset(cnes19_df_coords_fixed, ndigitos <=2)$CNES

# continuam imprecisos
A_cnes_lat_impreciso <- subset(cnes19_df_coords_fixed, CNES %in% lat_impreciso)
    
    
# B) Saude com lat/long missing  
cnes_lat_missing <- subset(cnes19_df_coords_fixed, is.na(lat))$CNES
B_cnes_lat_missing <- subset(cnes19_df_coords_fixed, CNES %in% cnes_lat_missing)

# C) Saude com 1, 2 e 3 estrelas do galileo
cnes_galileo_baixo <- fread("../data-raw/hospitais/saude_2019_output_galileo.csv") %>%
  # selecionar so os 1, 2 e 3 estrelas
  filter(PrecisionDepth %in% c("1 Estrela", "2 Estrelas", "3 Estrelas")) %>%
  .$CNES
C_cnes_galileo_baixo <- subset(cnes19_df_coords_fixed, CNES %in% cnes_galileo_baixo)
    

# Saude que permanecem com problema
cnes_problema_gmaps <- rbind(A_cnes_lat_impreciso, B_cnes_lat_missing, C_cnes_galileo_baixo) %>%
  distinct(CNES, .keep_all = TRUE) # 433 casos


# lista de enderecos com problema
# cnes_problema[, endereco := paste0(CEP,", ", MUNICÍPIO) ]
enderecos <- cnes_problema_gmaps %>% mutate(fim = paste0(LOGRADOURO, ", ", NUMERO, " - ", MUNICÍPIO, ", ", UF, " - CEP ", CEP)) %>% .$fim


# registrar Google API Key
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)

# geocode
coordenadas_google <- lapply(X=enderecos, ggmap::geocode) %>% rbindlist()


# Link escolas com lat lon do geocode
cnes_problema_geocoded <- cbind(cnes_problema_gmaps %>% select(-lon, -lat), coordenadas_google)


# atualiza lat lon a partir de google geocode
cnes19_df_coords_fixed[, lat := as.numeric(lat)][, lon := as.numeric(lon)]
setDT(cnes19_df_coords_fixed)[cnes_problema_geocoded, on='CNES', c('lat', 'lon') := list(i.lat, i.lon) ]



#### GOOGLE 2, so ceps ------------------------

# ainda ha hospitais mal georreferenciadas!
# identificar esses hospitais e separa-los
# convert para sf
saude_google_mal_geo <- cnes19_df_coords_fixed %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps %>% st_set_crs(4326)) %>%
  # escolas que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni)) %>%
  # pegar so o cep
  select(CNES, CEP, MUNICÍPIO, UF)

# Retorna somente os ceps dos que deram errado para jogar no google API
somente_ceps <- paste0("CEP ", saude_google_mal_geo$CEP, " - ", saude_google_mal_geo$MUNICÍPIO, ", ", saude_google_mal_geo$UF)
# 12 CEPS

# consulta google api
coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode) %>% rbindlist()


# atualiza lat lon a partir de google geocode
saude_google_bom_geo <- cbind(as.data.frame(saude_google_mal_geo), coordenadas_google_cep)
setDT(cnes19_df_coords_fixed)[saude_google_bom_geo, on='CNES', c('lat', 'lon') := list(i.lat, i.lon) ]



# view
subset(cnes19_df_coords_fixed, !is.na(lat)) %>%
  to_spatial() %>%
  mapview()
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Save data of health facilities ------------------------------------------------------------------
    
cnes19_df_coords_fixed %>%
  # select(CNES, code_muni, health_low, health_med, health_high)
  readr::write_rds("../data/hospitais/health_facilities2019_filtered.rds")
  
  
  


