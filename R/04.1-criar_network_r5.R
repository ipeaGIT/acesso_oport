#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Criacao de graphs no R5R
options(java.parameters = '-Xmx50G')
library(r5r)
source("R/fun/setup.R")



# FUNCAO PARA CONSTRUIR network -------------------------
# graph.obj é salvo na pasta './otp/graphs/ano/cidade

construir_graph_muni <- function(sigla_muni, ano) {
  
  
  path <- sprintf("../../otp/graphs/%s/%s", ano, sigla_muni)
  r5r::setup_r5(data_path = path, use_elevation = TRUE, overwrite = TRUE)
  
}


# aplicar funcao ------------------------------------------------------------------------------
lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, construir_graph_muni, ano = 2017)
lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, construir_graph_muni, ano = 2018)
lapply(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, construir_graph_muni, ano = 2019)


