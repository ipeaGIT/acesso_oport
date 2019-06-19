#' ## Criação dos graphs
#' 
#' 
#' 
## ----graphs--------------------------------------------------------------

# FUNCAO PARA CONSTRUIR GRAPH (SO RODAR UMA VEZ!) -------------------------

construir_graph <- function(cidade) {
  
  # Os arquivos de gtfs e .obj devem estar na pasta "cidade"
  
  otp_build_graph(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade, memory = 6) 
  
}


# FUNCAO PARA LIGAR SERVIDOR DO OTP DA CIDADE -----------------------------

ligar_servidor <- function(cidade) {
  
  if (Sys.info()[1] == "Linux") {
    
    command <- sprintf("java -Xmx4G -jar ../otp/programs/otp.jar --router %s --graphs ../otp/graphs --server", cidade)
    
    system(command, intern = FALSE, wait = FALSE)
    otp_for <- otp_connect(router = cidade)
    
  } else {
    
  otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade)
  otp_for <- otp_connect(router = cidade)
  
  }
}

construir_graph("for")
construir_graph("bel")
construir_graph("rio")
construir_graph("sao")
construir_graph("cur")
construir_graph("por")




#' 
#' 
## ----feedvalidator-------------------------------------------------------

source("R/feed_validator.R")


#' 
#' 
#' 
#' 
## ----obj teste-----------------------------------------------------------

getbb ("belo horizonte")

vai <- available_tags("highway")

q <- opq ("belo horizonte") %>%
  add_osm_feature(key = "highway", value = vai)
  # osmdata_sf()
  
osmdata_pbf(q, "bel_teste.osm.pbf")


meu <- q[["osm_lines"]] %>%
  st_sf()

# Before that
# sudo apt-get install sqlite3 libsqlite3-dev

meu %>%
  st_write("teste_bel.pbf")

ooo <- st_read("../otp/graphs/for/fortaleza_export.pbf")



#' 
