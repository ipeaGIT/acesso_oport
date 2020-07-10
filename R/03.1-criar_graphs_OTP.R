#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Criacao de graphs no OpenTripPlanner

# Programa OpenTripPlanner.jar deve ser baixado manual desse link
  # https://repo1.maven.org/maven2/org/opentripplanner/otp/1.4.0/
  

  
  
# carregar bibliotecas
source('./R/fun/setup.R')



# FUNCAO PARA CONSTRUIR GRAPH (SO RODAR UMA VEZ!) -------------------------
# graph.obj é salvo na pasta './otp/graphs/cidade
construir_graph <- function(ano, munis = "all") {
  
  
  # Select the corerspondent munis_df
  munis_df <- get(sprintf("munis_df_%s", ano))
  
  construir_graph_muni <- function(sigla_muni) {
    
    message(paste0("Criando Graph da cidade ",sigla_muni, " para o ano ", ano, "\n"))
    
    # Os arquivos de gtfs e .obj devem estar na pasta de cada cidade
    opentripplanner::otp_build_graph(otp = "../otp/programs/otp-1.4.0-shaded.jar", memory = 3,
                                     dir = "../otp", router = sprintf("%s/%s", ano, sigla_muni)) 
    
  }
  
  
  # Aplica funcao para cada municipio
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  # Processamento em paralelo usando future.apply
  options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
  future::plan(future::multiprocess)
  system.time(future.apply::future_lapply(X= x, FUN=construir_graph_muni))
  
}


# aplicar funcao ------------------------------------------------------------------------------
construir_graph(ano = 2019)

# # FUNCAO PARA LIGAR SERVIDOR DO OTP DA CIDADE -----------------------------
# 
# ligar_servidor <- function(sigla_muni) {
#   
#   if (Sys.info()[1] == "Linux") {
#     
#     command <- sprintf("java -Xmx4G -jar ../otp/programs/otp.jar --router %s --graphs ../otp/graphs --server", sigla_muni)
#     
#     system(command, intern = FALSE, wait = FALSE)
#     otp_for <- otp_connect(router = sigla_muni)
#     
#   } else {
#     
#   otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade)
#   otp_for <- otp_connect(router = cidade)
#   
#   }
# }








 
## ----feedvalidator-------------------------------------------------------
# usando feed valitor para checar qualidade do GTFS

source("R/fun/feed_validator.R")

run_validator("gtfs_20190619")



