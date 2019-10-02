ref https://github.com/rafapereirabr/thesis/blob/master/Rscripts/0%20Rio%20places_schools%20and%20hospitals.R



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.5 Download dos dados geolocalizados dos estabelecimentos de saude
##info
# fonte: Cadastro Nacionl dos Estabelecimentos de Saude (CNES) - DataSus
  
  
# carregar bibliotecas
source('./R/fun/setup.R')


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


# teste
pmaq_df_coords_fixed %>%
  to_spatial() %>%
  mapview()


# save as .csv
fwrite(pmaq_df_coords_fixed, '../data-raw/hospitais/PMAQ/pmaq_df_coords_fixed.csv')
gc(reset = T)


# # Read PMAQ data
# pmaq_df_coords_fixed <- fread('../data-raw/hospitais/PMAQ/pmaq_df_coords_fixed.csv')


### 2. Leitura dos dados ---------------------------------
  


## 2.1 Ler CNES ativos dos SUS - traz blueprint das intituicoes ativas em 2019

cnes19 <- readxl::read_xlsx(path = '../data-raw/hospitais/CNES_NDIS_01_10_2019_BANCO_COMP_08_2019.xlsx',
                        sheet = 'BANCO', skip = 14, col_types = rep("text", 30))

# remove 1st NA rows
cnes19 <- cnes19[-c(1:3),]
head(cnes19)

# rename columns
names(cnes19)[15:30] <- c("instal_fisica_ambu", "instal_fisica_hospt", "complex_alta_ambu_est", "complex_alta_ambu_mun", "complex_baix_ambu_est", "complex_baix_ambu_mun", "complex_medi_ambu_est", "complex_medi_ambu_mun", "complex_alta_hosp_est", "complex_alta_hosp_mun", "complex_baix_hosp_est", "complex_baix_hosp_mun", "complex_medi_hosp_est", "complex_medi_hosp_mun", "complex_nao_aplic_est", "complex_nao_aplic_mun")



  
### 3.Limpeza dos dados ---------------------------------
  


# Keep only columns we will use
  cnes_filtered <- cnes19[, .(CNES, CODUFMUN, COD_CEP, PF_PJ, NIV_HIER, VINC_SUS, ATENDAMB, ATENDHOS, abbrev_uf, date) ]
  # vars to keep
    # CNES CHAR (7) Número nacional do estabelecimento de saúde
    # CODUFMUN CHAR (7) Código do município do estabelecimento: UF + MUNIC (sem dígito)
    # COD_CEP CHAR (8) Código do CEP do estabelecimento
    # PF_PJ CHAR (1) Indicador de pessoa: 1-Física 3-Jurídica
    # VINC_SUS CHAR (1) Vínculo com SUS: 1-Sim 0-Não
    # NIV_HIER CHAR (2) Código do nível de hierarquia
    # ATENDHOS CHAR (1) Indica a existência de INSTALAÇÃO FÍSICA de ATENDIMENTO HOSPITALAR para este CNES, onde: 1-sim 0-não
    # ATENDAMB CHAR (1) Indica a existência de INSTALAÇÃO FÍSICA de ATENDIMENTO AMBULATORIAL para este CNES, onde: 1-sim 0-não
    # abbrev_uf - abbreviation of state name
    # date - date of reference
  summary(cnes_filtered$NIV_HIER)
  table(cnes_filtered$NIV_HIER)
  

# Filter 1: healthcare facilities operating with the public health system
  cnes_filter1 <- cnes19[ `ATENDE SUS`== 'SIM', ]
  
  
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
   
   

### Organiza Nivel de atencao
   
   
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

   
# Bring Hierarchy info from 2015 data
  cnes_filter6[cnes15, on='CNES', NIV_HIER := i.NIV_HIER]
  new_cnes19 <- copy(cnes_filter6)
  summary(new_cnes19$NIV_HIER) # 767 NAs
  table(new_cnes19$NIV_HIER)


  
   
### 4. Corrigir Lat Long  ---------------------------------

  
  
  
  ### Fix lat long info
  
  
  # trazer a quantidade de digitos para o cnes19
  cnes19_df_digitos <- cnes19 %>%
    rename(code_muni = IBGE) %>%
    # selecionar so os municipios do projeto
    filter(code_muni %in% substr(munis_df$code_muni, 1, 6)) %>%
    mutate(code_muni = as.character(code_muni)) %>%
    left_join(munis, by = "code_muni")
  
  # criar dataframe com as coordenadas ajeitadas
  cnes19_df_coords_fixed <- cnes19_df_digitos %>%
    # o ponto na longitude vai ser sempre depois do segundo numerico
    mutate(lon = sub("(^?-\\d{2})(\\d+)", "\\1\\.\\2", LONGITUDE)) %>%
    # o ponto na latitude vai depender do nchar
    mutate(lat = ifelse(lat_digits == 1, sub("(^?-\\d{1})(\\d+)", "\\1\\.\\2", LATITUDE),
                        sub("(^?-\\d{2})(\\d+)", "\\1\\.\\2", LATITUDE))) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat))
  
  
  # teste
  cnes19_df_coords_fixed %>%
    to_spatial() %>%
    mapview()
  
  
  
  
  
  
  
  
# subset keep Only municipalities in the project
cnes_geo_filter <- subset(cnes_geo, code_muni %in% substr(munis_df$code_muni, 1,6))


# reorganize cnes geo
  cnes_geo_filter$code_cnes <- as.character(cnes_geo_filter$code_cnes)
  cnes_geo_filter_df <- sfc_as_cols(cnes_geo_filter)
  plot( cnes_geo_filter['code_muni'] )
  
# add lat long from the 2015 database
  new_cnes19 <- left_join(new_cnes19, cnes_geo_filter_df[,.(code_cnes, lon, lat)], by=c('CNES'='code_cnes')) %>% setDT()
  summary(new_cnes19$lat) # 925 NAs
  
  
# update lat lonf info from PMAQ
  new_cnes19[dt_coords_fixed, on=c('CNES'='CNES_FINAL'), c('lat', 'lon') := list(i.lat, i.lon) ] 
  summary(new_cnes19$lat) # 795 NAs
  

### O que fazer com missing??? Rodar no Galileo?
    #   
    # ceps_missing <-   a[is.na(lat)]
    # ceps_missing <- ceps_missing[, .(CNES,  COD_CEP, CODUFMUN)]
    # 
    # 
    # ceps_missing[, code_state := substr(CODUFMUN,1,2)]
    # 
    # # add State abbreviation
    # ceps_missing <- ceps_missing %>% mutate(abbrev_state =  ifelse(code_state== 11, "RO",
    #                                                      ifelse(code_state== 12, "AC",
    #                                                             ifelse(code_state== 13, "AM",
    #                                                                    ifelse(code_state== 14, "RR",
    #                                                                           ifelse(code_state== 15, "PA",
    #                                                                                  ifelse(code_state== 16, "AP",
    #                                                                                         ifelse(code_state== 17, "TO",
    #                                                                                                ifelse(code_state== 21, "MA",
    #                                                                                                       ifelse(code_state== 22, "PI",
    #                                                                                                              ifelse(code_state== 23, "CE",
    #                                                                                                                     ifelse(code_state== 24, "RN",
    #                                                                                                                            ifelse(code_state== 25, "PB",
    #                                                                                                                                   ifelse(code_state== 26, "PE",
    #                                                                                                                                          ifelse(code_state== 27, "AL",
    #                                                                                                                                                 ifelse(code_state== 28, "SE",
    #                                                                                                                                                        ifelse(code_state== 29, "BA",
    #                                                                                                                                                               ifelse(code_state== 31, "MG",
    #                                                                                                                                                                      ifelse(code_state== 32, "ES",
    #                                                                                                                                                                             ifelse(code_state== 33, "RJ",
    #                                                                                                                                                                                    ifelse(code_state== 35, "SP",
    #                                                                                                                                                                                           ifelse(code_state== 41, "PR",
    #                                                                                                                                                                                                  ifelse(code_state== 42, "SC",
    #                                                                                                                                                                                                         ifelse(code_state== 43, "RS",
    #                                                                                                                                                                                                                ifelse(code_state== 50, "MS",
    #                                                                                                                                                                                                                       ifelse(code_state== 51, "MT",
    #                                                                                                                                                                                                                              ifelse(code_state== 52, "GO",
    #                                                                                                                                                                                                                                     ifelse(code_state== 53, "DF",NA))))))))))))))))))))))))))))
    # 
    # ceps_missing <- select(ceps_missing, CNES, CEP=COD_CEP, Estado=abbrev_state)
    # fwrite(ceps_missing, "../data/hospitais/to_galileo.csv", sep=';')
    # 
    # 
    #   to_spatial(a) %>% mapview()
    #   
  

    

# Save data of health facilities
  readr::write_rds(new_cnes19, "../data/hospitais/health_facilities2019_filtered.rds")
  
  
  


