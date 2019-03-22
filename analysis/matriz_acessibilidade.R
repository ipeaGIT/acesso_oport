library(dplyr)
library(tidyr)
library(opentripplanner)
library(sf)
library(purrr)
library(httr)
library(data.table)
library(furrr)
library(parallel)


# FUNCAO PARA CONSTRUIR GRAPH (SO RODAR UMA VEZ!) -------------------------



construir_graph <- function(cidade) {
  
  # Os arquivos de gtfs e .obj devem estar na pasta "cidade"
  
  otp_build_graph(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade) 
  
}

# construir_graph("fortaleza")



# FUNCAO PARA LIGAR SERVIDOR DO OTP DA CIDADE -----------------------------



ligar_servidor <- function(cidade) {
  
  otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade)
  otp_for <- otp_connect(router = cidade)
  
}

ligar_servidor("fortaleza")



# 
# data <- "data/hex_municipio/fortaleza/hex_fortaleza.shp"
# cidade <- "fortaleza"
# 
# data <- "data/hex_municipio/rio_de_janeiro/hex_rio_de_janeiro.shp"
# cidade <- "rio"



# FUNCAO PARA PRODUZIR MATRIZ DE IMPEDANCIA -------------------------------


otp_vai <- function(data, cidade) {
  
  source("R/sfc_as_cols.R")
  
  # ABRIR ARQUIVOS ----------------------------------------------------------
  
  for_hex <- st_read(data, crs = 4326) %>%
    mutate(id = 1:n()) %>%
    select(id) %>%
    # filter(id %in% c(1:10)) %>%
    identity()
  
  for_hex_centroids <- for_hex %>%
    st_centroid() %>%
    sfc_as_cols()
  
  
  # GERAR TABELA DE MATRIZ --------------------------------------------------
  
  for_od <- for_hex_centroids %>%
    expand(id, id) %>%
    left_join(for_hex_centroids) %>%
    left_join(for_hex_centroids, by = c("id1" = "id"), suffix = c(".origem", ".destino")) %>%
    rename(origem = id, destino = id1)
  
  
  # GERAR LISTA COM COORDENADAS ---------------------------------------------
  
  
  origem <- map2(for_od$lat.origem, for_od$lon.origem, c)
  destino <- map2(for_od$lat.destino, for_od$lon.destino, c)
  
  names(origem) <- 1:length(origem)
  names(destino) <- 1:length(destino)
  
  
  url <- paste0("http://localhost:8080/otp/routers/", cidade, "/plan")
  
  request_url <- function(origem, destino, vai) {
    
    # TRATAR AS COORDENADAS ---------------------------------------------------
  
    fromPlace <- paste0(origem, collapse = ",")
    toPlace <- paste0(destino, collapse = ",")
    

    # MAKE REQUEST ------------------------------------------------------------

    req <- httr::GET(
      vai,
      query = list(
        fromPlace = fromPlace,
        toPlace = toPlace,
        mode = "TRANSIT,WALK",
        date = "11-05-2018",
        time = "11:00am",
        maxWalkDistance = "1000",
        walkReluctance = "2",
        arriveBy = "FALSE",
        transferPenalty = "0",
        minTransferTime = "0"
      )
    )
    
    text <- httr::content(req, as = "text", encoding = "UTF-8")
    
    x <- jsonlite::fromJSON(text)
    
  }
  
  plan(multiprocess)
  finni <- future_map2(origem, destino, request_url, vai = url, .progress = TRUE)

  names(finni) <- paste(rep(1:length(for_hex$id), each = length(for_hex$id)),
                        rep(1:length(for_hex$id), length(for_hex$id)), sep = "_")
  

  # FUNCAO PARA ACESSAR CONTEUDO DA CONSULTA --------------------------------

  acessar_consulta <- function(list.consulta) {

    if (is.data.frame(list.consulta[["plan"]][["itineraries"]])) {

      df <- list.consulta[["plan"]][["itineraries"]] %>%
        as.data.frame() %>%
        select(duration, walkTime, transitTime, waitingTime, transfers) %>%
        mutate(option = 1:n())

    } else {

      df <- data.frame(duration = 0, walkTime = 0, transitTime = 0, waitingTime = 0, transfers = 0,
                       option = 0)

    }


  }

  fin_v1 <- future_map(finni, acessar_consulta) %>%
    rbindlist(idcol="origem_destino") %>%
    mutate_at(c("duration", "walkTime", "transitTime", "waitingTime"), ~ round(./60, digits = 1)) %>%
    separate(origem_destino, c("id_origem", "id_destino"), sep = "_")


}



# TESTE !!!!!!!!!1 --------------------------------------------------------

tictoc::tic()
orra <- otp_vai("../data/hex_municipio/fortaleza/hex_fortaleza.shp", "fortaleza")
tictoc::toc()

