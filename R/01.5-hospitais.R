~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.5 Download dos dados geolicalizados dos estabelecimentos de saude
##info
# fonte: Cadastro Nacionl dos Estabelecimentos de Saude (CNES) - DataSus
  
  
# carregar bibliotecas
source('./R/fun/setup.R')


### Download geocoded CNES data
cnes_geo <- geobr::read_health_facilities(code = "all")
sf::st_crs(cnes_geo)
head(cnes_geo)



### Download CNES data from datasus

# Download documentation
  # ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Doc/IT_CNES_1706.pdf

# tabulacao
  # ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Auxiliar/TAB_CNES.zip

# complete hierarchy data
#  ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/STRJ1506.dbc





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
                            readr::write_rds(datasus, paste0('../data-raw/servicos_saude/','cnes_20',ANO,MES,'_',UF,".rds"))
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

# download cnes data
  for (i in meses){
    
      # Definir ano para download
      ano = '18'
      message(paste0("Baixando dados do mes ",i,"\n"))
  
      # Aplica funcao para baixar arquivos dos estados em paralelo
      future.apply::future_lapply(X =all_states, FUN=download_cnes_datasus, ANO=ano, MES=i)
  }
    
#   
# # download
#   cnes <- lapply(X=all_states, FUN =function(X){hack_datasus(UF=X, ANO=ano, MES=mes)} ) %>% data.table::rbindlist()
#   table(cnes$NIV_HIER)
#   table(cnes$date)
  
  
  
  
  
  
  
  

# clean dataset
  
# CNES to characther
  setDT(cnes)[, CNES := as.character(CNES) ] 
  
# Keep only columns we will use
  cnes_filtered <- cnes[, .(CNES, CODUFMUN, COD_CEP, PF_PJ, NIV_HIER, VINC_SUS, ATENDAMB, ATENDHOS, abbrev_uf) ]
  
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
  summary(cnes$NIV_HIER)
  table(cnes$NIV_HIER)
  


# Filter 1: healthcare facilities operating with the public health system
  cnes_filtered <- cnes_filtered[ VINC_SUS==1, ]
  
# Filter 2: Pessoa juridica
  cnes_filtered <- cnes_filtered[ PF_PJ==3, ]

  
  
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
  
  
  


