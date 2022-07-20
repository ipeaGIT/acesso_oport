#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.4 Dados do censo escolar
# Esse script aplica funcoes que foram definidas na pasta R/fun/educacao p/ todos os anos de processamento
# Sao tres funcoes principais:
# 1) Filtra cada base de educacao mantendo somente observaoes de escolas publicas e e municipios do projeto
# 2) Traz o geocode que foi realizado com o streetmap e aprimora a qualidade do geocode quando necessario
# 3) Filtra somente as observaoes que apresentam boa qualidade de geocode

# carregar bibliotecas
source('./R/fun/setup.R')

# carregar funcoes
source('./R/fun/educacao/educacao.R')

# 1) Aplicar funcao filtro ----------------
lapply(X=2017:2019, FUN=educacao_filter)

# 2) Aplicar funcao geocode ---------------
educacao_geocode(run_gmaps = FALSE)

# 3) Selecionar somente as obsservacoes com boa qualidade de geocode ----------
source("R/fun/filter_geocode.R")
lapply(X=2017:2019, geocode_filter, "educacao")
