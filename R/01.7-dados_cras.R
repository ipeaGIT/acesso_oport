#> Esse script faz Download, Limpeza dos dados brutos e geolocalização dos CRAS

# Setup 
source('R/fun/setup.R')

###### 1. Download arquivos originais ###################

# 2019 - CRAS

download.file("https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(5).zip" ,
              destfile = "data-raw/CRAS/Censo_SUAS_2019_CRAS.zip")

unzip("../../data-raw/CRAS/Censo_SUAS_2019_CRAS.zip", exdir = "../../data-raw/CRAS/2019")


# CRAS 2018
download.file('https://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/CRAS(3).zip',
              destfile = "data-raw/CRAS/Censo_SUAS_2018_CRAS.zip")

unzip("../../data-raw/CRAS/Censo_SUAS_2018_CRAS.zip", exdir = "../../data-raw/CRAS/2018")

# CRAS 2017

download.file('http://aplicacoes.mds.gov.br/sagi/dicivip_datain/ckfinder/userfiles/files/Censo_SUAS/2017/Censo_SUAS_2017_CRAS.zip',
              destfile = "data-raw/CRAS/Censo_SUAS_2017_CRAS.zip")

unzip("../../data-raw/CRAS/Censo_SUAS_2017_CRAS.zip",exdir = "../../data-raw/CRAS/2017")


########### 2. Limpeza e Geocode

# carregar funcoes
source('./R/fun/cras/cras.R')

# Aplicar funcao 
purrr::walk(.x = c('2017', '2018', '2019'), .f = cras_geocode)
