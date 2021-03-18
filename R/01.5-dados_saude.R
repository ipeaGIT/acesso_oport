#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Geocode dados do censo escolar

# carregar bibliotecas
source('./R/fun/setup.R')

# carregar funcoes
source('./R/fun/saude/saude.R')

# Aplicar funcao filtro
lapply(X=2017:2019, FUN=saude_filter)

# Aplicar funcao geocode
lapply(X=2017:2019, FUN=saude_geocode)

# Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
lapply(X=2017:2019, geocode_filter, "saude")