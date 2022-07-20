#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.5 Dados do CNES
# Esse script aplica funcoes que foram definidas na pasta R/fun/saude p/ todos os anos de processamento
# Sao tres funcoes principais:
# 1) Filtra cada base de saude mantendo somente observaoes de equipamentos de saude publicos e e municipios do projeto
# 2) Traz o geocode que foi realizado com o streetmap e aprimora a qualidade do geocode quando necessario
# 3) Filtra somente as observaoes que apresentam boa qualidade de geocode

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
