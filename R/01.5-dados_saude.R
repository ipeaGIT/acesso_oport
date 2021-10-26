#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Geocode dados do censo escolar

# carregar bibliotecas
source('./R/fun/setup.R')

# carregar funcoes
source('./R/fun/saude/saude.R')

# Aplicar funcao filtro
saude_filter(2017)
saude_filter(2018)
saude_filter(2019)

# Aplicar funcao geocode
saude_geocode(2017)
saude_geocode(2018)
saude_geocode(2019)

# Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
lapply(X=2017:2019, geocode_filter, "saude")
