#> Esse script faz Download, Limpeza dos dados brutos e geolocalização dos CRAS

# Setup
source('R/fun/setup.R')

###### 1. Download arquivos originais ###################

# 2019 - CRAS

download.file("https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(5).zip" ,
              destfile = "../../sdata-raw/CRAS/Censo_SUAS_2019_CRAS.zip")

unzip("../../data-raw/CRAS/Censo_SUAS_2019_CRAS.zip", exdir = "../../data-raw/CRAS/2019")


# CRAS 2018
download.file('https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(3).zip',
              destfile = "../../data-raw/CRAS/Censo_SUAS_2018_CRAS.zip")

unzip("../../data-raw/CRAS/Censo_SUAS_2018_CRAS.zip", exdir = "../../data-raw/CRAS/2018")

# CRAS 2017

download.file('http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS/2017/Censo_SUAS_2017_CRAS.zip',
              destfile = "../../data-raw/CRAS/Censo_SUAS_2017_CRAS.zip")

unzip("../../data-raw/CRAS/Censo_SUAS_2017_CRAS.zip",exdir = "../../data-raw/CRAS/2017")


########### 2. Limpeza e Geocode

# carregar funcoes
source('./R/fun/cras/cras.R')

# Aplicar funcao 
purrr::walk(.x = c('2017', '2018', '2019'), .f = cras_geocode)



# trazer entao geocode ----------------------------------------------------

update_geocode_cras <- function(ano) {

  # abrir cras do ano
  cras <- fread(sprintf("../../data/acesso_oport/cras/cras_%s.csv", ano))

  # abrir geocode
  cras_geocode <- fread("../../data/acesso_oport/cras/geocode_cras.csv")
  
  
  
  cras[cras_geocode, on = "code_cras",
       c("lon", "lat") :=
         list(i.lon, i.lat)]
  
  
  # output
  write_rds(cras, sprintf("../../data/acesso_oport/cras/cras_%s_geocoded.rds", ano))
  
  
}


update_geocode_cras(2017)
update_geocode_cras(2018)
update_geocode_cras(2019)
