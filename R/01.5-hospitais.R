ref https://github.com/rafapereirabr/thesis/blob/master/Rscripts/0%20Rio%20places_schools%20and%20hospitals.R



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.5 Download dos dados geolocalizados dos estabelecimentos de saude
##info
# fonte: Cadastro Nacionl dos Estabelecimentos de Saude (CNES) - DataSus
  
  
# carregar bibliotecas
source('./R/fun/setup.R')



### 1.1 Dowlonad CNES data from datasus ---------------------------------

#  documentation > ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Doc/IT_CNES_1706.pdf
# tabulacao > ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Auxiliar/TAB_CNES.zip
# complete hierarchy data > ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/STRJ1506.dbc

### Download cnes data with classification of hospitals hierarchy for all states

# Function do download CNES data from data sus
  # adapted from https://gist.github.com/fernandobarbalho/0cf27d994e39700663551b2d14387b08
  download_cnes_datasus <- function(UF, ANO, MES){

                            # URL to download from
                            str_download <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/ST", UF, ANO, MES,".dbc")
                            
                            # create temporary file
                            tempf <- paste0(tempdir(),"/",'cnes_20',ANO,MES,'_',UF,".dbc") 
                            
                            # download data to temporary file
                            download.file(str_download,destfile = tempf, mode='wb') 
                            
                            # read data from temporary file
                            datasus <- read.dbc::read.dbc(tempf)
                            
                            # create State and date columns
                            datasus$abbrev_uf <- UF
                            datasus$date <- paste0('20',ANO,MES)
                            
                            # save data
                            readr::write_rds(datasus, paste0('../data-raw/hospitais/','cnes_20',ANO,MES,'_',UF,".rds"))
                          }


# list of states
  all_states <- c("RO", "AC", "AM", "RR", "PA",
                  "AP", "TO", "MA", "PI", "CE",
                  "RN", "PB", "PE", "AL", "SE",
                  "BA", "MG", "ES", "RJ", "SP",
                  "PR", "SC", "RS", "MS", "MT",
                  "GO", "DF")


# Preparar processamento em paralelo usando future.apply
  future::plan(future::multiprocess)

# Loop over months
  meses <- c(paste0("0", 1:9), 10:12)

# all data
  # ano = '15'
  # mes= '05'
  
  
# download cnes data
  for (i in meses){
    
      # Definir ano para download
      ano = '18'
      message(paste0("Baixando dados do mes ",i,"\n"))
  
      # Aplica funcao para baixar arquivos dos estados em paralelo
      future.apply::future_lapply(X =all_states, FUN=download_cnes_datasus, ANO=ano, MES=i)
  }
    
  
  
  
### 1.2 Dowlonad CNES ativos from http://dados.gov.br ---------------------------------
  # this data base brings more info on health facilitilies
  # dara url
    url_cnes <- "http://i3geo.saude.gov.br/i3geo/ogc.php?service=WFS&version=1.0.0&request=GetFeature&typeName=cnes_ativo&outputFormat=CSV"
  
  # # trying to read directly
  #   cnes <- data.table::fread(url_cnes)
  #   cnes <- read.csv(url_cnes)
  
  # download and save
    download.file(url=url_cnes, destfile = '../data-raw/hospitais/cnes_ativos_201806.csv', mode='w')


  
  
### 1.3 Download geocoded CNES data ------------------------------------
  cnes_geo <- geobr::read_health_facilities(code = "all")
  sf::st_crs(cnes_geo)
  head(cnes_geo)
  
  # salva em data-raw
  readr::write_rds(cnes_geo, "../data-raw/hospitais/cnes_geocoded.rds")
  
  
  
### 1.4 Download geocoded PMAQ data ------------------------------------
  # better geocoded info for basic services
  # source: http://aps.saude.gov.br/ape/pmaq
  # file  http://189.28.128.100/dab/docs/portaldab/documentos/microdados_pmaq_cliclo3/modulo_I_ubs/UBS_Brasil.xlsx
  
  
  
  
  library(xlsx)
  library(readxl)
  
  df <- readxl::read_excel(path = '../data-raw/hospitais/PMAQ/UBS_Brasil_ciclo3.xlsx',
                           sheet = 'Módulo I')
  
  df2 <- read_xlsx(path = '../data-raw/hospitais/PMAQ/UBS_Brasil_ciclo3.xlsx',
                   sheet = 'Módulo I', col_types = rep("text", 425))
  head(df)
  
  df2 <- subset(df2, LATITUDE !="0" )
  
  
### 2. Leitura dos dados ---------------------------------
  
  
# 2.1 Ler CNES ativos dos SUS - traz blueprint das intituicoes ativas em 2019
  
  # Listar arquivos
  rdsfiles <- list.files('../data-raw/hospitais/', pattern = ".rds", full.names = T)
  rds_2019 <- rdsfiles[rdsfiles %like% 2019]
  
  # leitura de todos arquivos
  future::plan(future::multiprocess)
  cnes19 <- future.apply::future_lapply(X=rds_2019, FUN=readr::read_rds) %>% rbindlist(fill=TRUE)
  

# 2.2 ler CNES de 2015 - traz info de nivel hierarquico
  rds_2015 <- rdsfiles[rdsfiles %like% 2015]
  cnes15 <- future.apply::future_lapply(X=rds_2015, FUN=readr::read_rds) %>% rbindlist(fill=TRUE)
  
  
# 2.3 ler CNES ativos de 2018 (baixados de dados.gov.br)- traz nome fantasia das instituicoes
  cnes18 <- fread('../data-raw/hospitais/cnes_ativos_201806.csv')
  
  
# 2.4 CNES geo de traz lat e long (atualizacao em varios anos)
  cnes_geo <- readr::read_rds("../data-raw/hospitais/cnes_geocoded.rds")
  
  
  
  
  
### 3.Limpeza dos dados ---------------------------------
  
# CNES to characther
  setnames(cnes18, old = 'co_cnes', new = 'CNES')
  
  setDT(cnes19)[, CNES := as.character(CNES) ] 
  setDT(cnes15)[, CNES := as.character(CNES) ] 
  setDT(cnes18)[, CNES := as.character(CNES) ] 
  
  

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
  cnes_filter1 <- cnes_filtered[ VINC_SUS==1, ]
  
  
# Filter 2: Pessoa juridica
  cnes_filter2 <- cnes_filter1[ PF_PJ==3, ]

  
# filter 3: Only municipalities in the project
  cnes_filter3 <- subset(cnes_filter2, CODUFMUN %in% substr(munis_df$code_muni, 1,6))
  
# filter 4: Only atendimento hospitalar ou ambulatorial
  cnes_filter4 <- cnes_filter3[ ATENDHOS==1 | ATENDAMB==1]
  
  
# filter 5: remove duplicates and keep CEP of latest date
  
    # # health facilities with more than one cep
    # a <- table(cnes_filter4$CNES) %>% sort()
    # a <- a[a>6]
  
  # format date info
   cnes_filter4[, date := parse_date_time(date, "ym")]
   
  # remove duplicates
   cnes_filter5 <- cnes_filter4[, .(CEP = COD_CEP[which.max(date)]), by=.( CNES, CODUFMUN)]

   
# filter 6. Remove special categories of facilities 
   
   # Add names
   cnes_filter6 <- left_join(cnes_filter5, cnes18, by = "CNES") %>% setDT()
   
   # 6.1 Delete prison hospitals, research centers, police hospitals etc
   to_remove1 <- 'CENTRO DE ESTUDOS|PSIQUIAT|PRESIDIO|PENAL|JUDICIARIO|PENITENCIARIA|DETENCAO|PROVISORIA|SANATORIO|POLICIA| PADI|DE REGULACAO|VIGILANCIA|SAMU |ACADEMIA|DEPEND QUIMICO|REEDUCACAO SOCIAL|CAPS|CENTRO DE ATENCAO PSICOSSOCIAL'
                  # PADI = Programa de Atenção Domiciliar ao Idoso
                  # DE REGULACAO = gestora de servico
                  # CAPS - CENTRO DE ATENCAO PSICOSSOCIAL - saude mental e drogas
  
   

      
   # 6.2 Delete Home care, tele saude, unidades moveis de saude
   to_remove2 <- 'TELESSAUDE|UNIDADE MOVEL|DOMICILIAR'
   
 # apply filter 6
   cnes_filter6 <- cnes_filter6[ no_fantasia %nlike% to_remove1 ]
   cnes_filter6 <- cnes_filter6[ ds_tipo_unidade %nlike% to_remove2 ]
   # test >>> cnes_filter6[ CNES =='6771963']
   
   
   
   
   
### 4. Recupera Nivel hierarquico  ---------------------------------

   cnes_filter6, cnes15
   
   
   
   
   
   
### 5. Recuperar Lat Long  ---------------------------------

# reorganize cnes geo
  cnes_geo$code_cnes <- as.character(cnes_geo$code_cnes)
  cnes_geo <- sfc_as_cols(cnes_geo)
  
  a <- left_join(cnes_filter6, cnes_geo, by=c('CNES'='code_cnes')) %>% setDT()
  
ceps_missing <-   a[is.na(lat)]
ceps_missing <- ceps_missing[, .(CNES,  COD_CEP, CODUFMUN)]


ceps_missing[, code_state := substr(CODUFMUN,1,2)]

# add State abbreviation
ceps_missing <- ceps_missing %>% mutate(abbrev_state =  ifelse(code_state== 11, "RO",
                                                     ifelse(code_state== 12, "AC",
                                                            ifelse(code_state== 13, "AM",
                                                                   ifelse(code_state== 14, "RR",
                                                                          ifelse(code_state== 15, "PA",
                                                                                 ifelse(code_state== 16, "AP",
                                                                                        ifelse(code_state== 17, "TO",
                                                                                               ifelse(code_state== 21, "MA",
                                                                                                      ifelse(code_state== 22, "PI",
                                                                                                             ifelse(code_state== 23, "CE",
                                                                                                                    ifelse(code_state== 24, "RN",
                                                                                                                           ifelse(code_state== 25, "PB",
                                                                                                                                  ifelse(code_state== 26, "PE",
                                                                                                                                         ifelse(code_state== 27, "AL",
                                                                                                                                                ifelse(code_state== 28, "SE",
                                                                                                                                                       ifelse(code_state== 29, "BA",
                                                                                                                                                              ifelse(code_state== 31, "MG",
                                                                                                                                                                     ifelse(code_state== 32, "ES",
                                                                                                                                                                            ifelse(code_state== 33, "RJ",
                                                                                                                                                                                   ifelse(code_state== 35, "SP",
                                                                                                                                                                                          ifelse(code_state== 41, "PR",
                                                                                                                                                                                                 ifelse(code_state== 42, "SC",
                                                                                                                                                                                                        ifelse(code_state== 43, "RS",
                                                                                                                                                                                                               ifelse(code_state== 50, "MS",
                                                                                                                                                                                                                      ifelse(code_state== 51, "MT",
                                                                                                                                                                                                                             ifelse(code_state== 52, "GO",
                                                                                                                                                                                                                                    ifelse(code_state== 53, "DF",NA))))))))))))))))))))))))))))

ceps_missing <- select(ceps_missing, CNES, CEP=COD_CEP, Estado=abbrev_state)
fwrite(ceps_missing, "../data/hospitais/to_galileo.csv", sep=';')


  to_spatial(a) %>% mapview()
  
  
  
  cnes19[COD_CEP=='69065001']
  
# Bring Hierarchy info from 2015 data
  cnes_filtered[cnes15, on='CNES', NIV_HIER := i.NIV_HIER]
  table(cnes_filtered$NIV_HIER)
  summary(cnes_filtered$NIV_HIER)
  
  
  
# Recode health facilities Hierarchy -----
# based on PDF (Manual tecnico do cadastro nacional de estabelecimentos de saude - versao 2) p. 131
  cnes_filtered[ , hierarq := ifelse(NIV_HIER =="01", "Low"
                                  ,ifelse(NIV_HIER =="02", "Medium"
                                  ,ifelse(NIV_HIER =="03", "Medium"
                                  ,ifelse(NIV_HIER =="04", "High"
                                  ,ifelse(NIV_HIER =="05", "Low + Medium"
                                  ,ifelse(NIV_HIER =="06", "Medium"
                                  ,ifelse(NIV_HIER =="07", "Medium + High"
                                  ,ifelse(NIV_HIER =="08", "High", NA))))))))]


# convert health facilities Hierarchy into dummy variables
  cnes_filtered[ , health_low := ifelse( grepl("Low", hierarq), 1, 0) ]
  cnes_filtered[ , health_med := ifelse( grepl("Medium", hierarq), 1, 0) ]
  cnes_filtered[ , health_high := ifelse( grepl("High", hierarq), 1, 0) ]
  

  
  
# Save data of health facilities
  readr::write_rds(hospitals_filtered, "./data/health_facilities_filtered.rds")
  
  
  


