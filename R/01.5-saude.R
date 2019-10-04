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
### 3.Limpeza dos dados ---------------------------------


# Filter 1: healthcare facilities operating with the public health system
  cnes_filter1 <- setDT(cnes19)[ `ATENDE SUS`== 'SIM', ]
  
  
# Filter 2: Pessoa juridica
  cnes_filter2 <- cnes_filter1[ PESSOA_FÍSICA_OU_PESSOA_JURÍDI== 'PESSOA_JURÍDICA', ]

# filter 3: Only municipalities in the project
  cnes_filter3 <- subset(cnes_filter2, IBGE %in% substr(munis_df$code_muni, 1,6))
  
# filter 4: Only atendimento hospitalar ou ambulatorial
  cnes_filter4 <- cnes_filter3[ instal_fisica_ambu=="SIM" | instal_fisica_hospt=="SIM", ]
  
  
# filter 5. Remove special categories of facilities 
   
   # 6.1 Delete prison hospitals, research centers, police hospitals etc
   to_remove1 <- 'CENTRO DE ESTUDOS|PSIQUIAT|PRESIDIO|PENAL|JUDICIARIO|PENITENCIARIA|DETENCAO|PROVISORIA|SANATORIO|POLICIA| PADI|DE REGULACAO|VIGILANCIA|SAMU |ACADEMIA|DEPEND QUIMICO|REEDUCACAO SOCIAL|CAPS|CENTRO DE ATENCAO PSICOSSOCIAL|DISTRIB DE ORGAOS|MILITAR|CADEIA PUBLICA'
                  # PADI = Programa de Atenção Domiciliar ao Idoso
                  # DE REGULACAO = gestora de servico
                  # CAPS - CENTRO DE ATENCAO PSICOSSOCIAL - saude mental e drogas
  
   

      
   # 6.2 Delete Home care, tele saude, unidades moveis de saude
   to_remove2 <- 'TELESSAUDE|UNIDADE MOVEL|DOMICILIAR|PSICOSSOCIAL|FARMACIA|DISTRIB DE ORGAOS'
   
 # apply filter 6
   cnes_filter5 <- cnes_filter4[ ESTABELECIMENTO %nlike% to_remove1 ]
   cnes_filter5 <- cnes_filter5[ TIPO_UNIDADE %nlike% to_remove2 ]
   # test >>> cnes_filter6[ CNES =='6771963']
   
   

### Organiza Nivel de atencao criando dummy
   
   
# convert health facilities Hierarchy into dummy variables
   cnes_filter5[, health_low := ifelse(complex_baix_ambu_est=='X', 1,
                                ifelse(complex_baix_ambu_mun=='X', 1,
                                ifelse(complex_baix_hosp_est=='X', 1,
                                ifelse(complex_baix_hosp_mun=='X', 1, 0))))]
                                  

   cnes_filter5[, health_med := ifelse(complex_medi_ambu_est=='X', 1,
                                ifelse(complex_medi_ambu_mun=='X', 1,
                                ifelse(complex_medi_hosp_est=='X', 1,
                                ifelse(complex_medi_hosp_mun=='X', 1, 0))))]
   
   
   cnes_filter5[, health_high := ifelse(complex_alta_ambu_est=='X', 1,
                                 ifelse(complex_alta_ambu_mun=='X', 1,
                                 ifelse(complex_alta_hosp_est=='X', 1,
                                 ifelse(complex_alta_hosp_mun=='X', 1, 0))))]
                                
 
table(cnes_filter5$health_low)  # 238
table(cnes_filter5$health_med)  # 546
table(cnes_filter5$health_high) # 273

  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4. Corrigir Lat Long  ---------------------------------

  ### Fix lat long info
  
  
  # trazer a quantidade de digitos para o cnes19
  cnes19_df_digitos <- cnes_filter5 %>%
    rename(code_muni = IBGE) %>%
    # selecionar so os municipios do projeto
    filter(code_muni %in% substr(munis_df$code_muni, 1, 6)) %>%
    mutate(code_muni = as.character(code_muni)) %>%
    left_join(munis, by = "code_muni")
  
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
  
  
  
###### Uusa dados de lat/lon quando eles existirem na PMAQ
  # Read PMAQ data
  pmaq_df_coords_fixed <- fread('../data-raw/hospitais/PMAQ/pmaq_df_coords_fixed.csv', colClasses = 'character')
  pmaq_df_coords_fixed[, lat := as.numeric(lat)][, lon := as.numeric(lon)]

  # update lat lonf info from PMAQ
  summary(cnes19_df_coords_fixed$lat) # 125 NAs
  setDT(cnes19_df_coords_fixed)[pmaq_df_coords_fixed, on=c('CNES'='CNES_FINAL'), c('lat', 'lon') := list(i.lat, i.lon) ] 
  summary(cnes19_df_coords_fixed$lat) # 125 NAs


      
###### Identificar CNES com lat/long problematicos
  # A poucos digitos
  # B fora dos limites do municipio
  
  
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
  
  # juntar todas municipios com erro de lat/lon
    munis_problema1 <- subset(cnes19_df_coords_fixed, CNES %in% A_estbs_pouco_digito$CNES ) 
    munis_problema2 <-  subset(cnes19_df_coords_fixed, CNES %in% B_muni_fora$CNES )
    munis_problema3 <-  cnes19_df_coords_fixed[ is.na(lat), ]
    munis_problema <- rbind(munis_problema1, munis_problema2, munis_problema3)
    munis_problema <- dplyr::distinct(munis_problema, CNES, .keep_all=T) # remove duplicates
    
    
######## Gerar input para galileo
    
    munis_problema_galileo <- munis_problema %>%
      select(CNES, log = LOGRADOURO, numero = NUMERO, bairro = BAIRRO, cep = CEP, cidade = MUNICÍPIO, uf = UF) %>%
      # juntar logradouro com numero
      mutate(rua = paste0(log, ", ", numero)) %>%
      select(-log, -numero)
    
    write_delim(munis_problema_galileo, "../data-raw/hospitais/saude_2019_input_galileo.csv", delim = ";")
    
# depois de rodar o galileo.............
    
    # abrir output galileo
    saude_output_galileo <- fread("../data-raw/hospitais/saude_2019_output_galileo.csv") %>%
      # selecionar so os 3 e 4 estrelas
      filter(PrecisionDepth %in% c("3 Estrelas", "4 Estrelas")) %>%
      # substituir virgula por ponto
      mutate(Latitude = str_replace(Latitude, ",", "\\.")) %>%
      mutate(Longitude = str_replace(Longitude, ",", "\\.")) %>%
      # selecionar colunas
      select(CNES, lat = Latitude, lon = Longitude) %>%
      mutate(CNES = as.character(CNES)) %>%
      mutate(lat = as.numeric(lat)) %>%
      mutate(lon = as.numeric(lon))
    
    # juntar com a base anterior completa
    
    setDT(cnes19_df_coords_fixed)[saude_output_galileo, on='CNES', c('lat', 'lon') := list(i.lat, i.lon) ]
    
    summary(cnes19_df_coords_fixed$lon) # 5 NA's
    

    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 4. Recupera a info lat/long que falta usando google maps ------------------------------------------------------------------
    
    # Escolas com lat/long de baixa precisa (1 ou 2 digitos apos casa decimal)
    setDT(escolas_etapa)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
    lat_impreciso <- subset(escolas_etapa, ndigitos <=2)$CO_ENTIDADE
    escolas_lat_impreciso <- subset(escolas, CO_ENTIDADE %in% lat_impreciso)
    
    
    # Escolas com lat/long missing  
    CO_ENTIDADE_lat_missing <- subset(escolas_etapa, is.na(lat))$CO_ENTIDADE
    escolas_lat_missing <- subset(escolas, CO_ENTIDADE %in% CO_ENTIDADE_lat_missing)
    
    # escolas problema
    escolas_problema <- rbind(escolas_lat_impreciso, escolas_lat_missing)
    
    # lista de enderecom com problema
    enderecos <- escolas_problema$ENDERECO
    
    # registrar Google API Key
    my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
    register_google(key = my_api$V1)
    
    # geocode
    coordenadas_google <- lapply(X=enderecos, ggmap::geocode) %>% rbindlist()
    summary(escolas_lat_missing_geocoded$lat) # Google nao encontrou 3 casos
    
    # Link escolas com lat lon do geocode
    escolas_lat_missing_geocoded <- cbind(escolas_lat_missing, coordenadas_google)
    
    
    # atualiza lat lon a partir de google geocode
    escolas_etapa[, lat := as.numeric(lat)][, lon := as.numeric(lon)]
    setDT(escolas_etapa)[escolas_lat_missing_geocoded, on='CO_ENTIDADE', c('lat', 'lon') := list(i.lat, i.lon) ]
    
    
    summary(escolas$lat)
    summary(escolas_lat_missing_geocoded$lat)
    
    
    subset(escolas_etapa, !is.na(lat)) %>%
      to_spatial() %>%
      mapview()
    
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Save data of health facilities ------------------------------------------------------------------
    
  readr::write_rds(cnes19_df_coords_fixed, "../data/hospitais/health_facilities2019_filtered.rds")
  
  
  


