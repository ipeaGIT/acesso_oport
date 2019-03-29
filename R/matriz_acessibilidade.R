# FUNCAO PARA PRODUZIR MATRIZ DE IMPEDANCIA -------------------------------


matriz_acessibilidade <- function(municipio, amostra = FALSE, ligar_otp = FALSE) {
  
  source("R/sfc_as_cols.R")
  source("R/otp.R")
  
  
  # se ligar_otp for true, liga-lo antes
  
  if (isTRUE(ligar_otp)) {
    
    ligar_servidor(municipio)
    
  }
  
  # ABRIR ARQUIVOS ----------------------------------------------------------
  
  muni_shortname <- substring(municipio, 1, 3)
  
  dir_muni <- paste0("../data/hex_municipio/hex_", muni_shortname, ".rds")
  
  for_hex <- read_rds(dir_muni)
  
  if (isTRUE(amostra)) {
    for_hex <- slice(for_hex, 1:100)
  }
  
  
  for_hex_centroids <-  for_hex %>%
    st_centroid() %>%
    sfc_as_cols()
  
  
  
  # GERAR TABELA DE MATRIZ --------------------------------------------------
  
  for_od <- for_hex_centroids %>%
    expand(id_hex, id_hex) %>%
    left_join(for_hex_centroids) %>%
    left_join(for_hex_centroids, by = c("id_hex1" = "id_hex"), suffix = c(".origem", ".destino")) %>%
    rename(origem = id_hex, destino = id_hex1)
  
  
  # GERAR LISTA COM COORDENADAS ---------------------------------------------
  
  
  origem <- map2(for_od$lat.origem, for_od$lon.origem, c)
  destino <- map2(for_od$lat.destino, for_od$lon.destino, c)
  
  names(origem) <- 1:length(origem)
  names(destino) <- 1:length(destino)
  
  
  url <- paste0("http://localhost:8080/otp/routers/", municipio, "/plan")
  
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

  names(finni) <- paste(rep(1:length(for_hex$id_hex), each = length(for_hex$id_hex)),
                        rep(1:length(for_hex$id_hex), length(for_hex$id_hex)), sep = "_")
  

  # FUNCAO PARA ACESSAR CONTEUDO DA CONSULTA --------------------------------

  acessar_consulta <- function(list.consulta) {

    if (is.data.frame(list.consulta[["plan"]][["itineraries"]])) {

      # df <- list.consulta[["plan"]][["itineraries"]] %>%
      #   as.data.frame() %>%
      #   select(duration, walkTime, transitTime, waitingTime, transfers) %>%
      #   mutate(option = 1:n())

      df <- setDT(list.consulta[["plan"]][["itineraries"]])
      df <- df[, .(duration, walkTime, transitTime, waitingTime, transfers)]
      df <- df[, option:=1:nrow(df)]
        
      
      
    } else {

      df <- data.table(duration = 0, walkTime = 0, transitTime = 0, waitingTime = 0, transfers = 0,
                       option = 0)

    }


  }

  fin_v1 <- future_map(finni, acessar_consulta) %>%
    rbindlist(idcol="origem_destino") %>%
    mutate_at(c("duration", "walkTime", "transitTime", "waitingTime"), ~ round(./60, digits = 1)) %>%
    separate(origem_destino, c("id_origem", "id_destino"), sep = "_")


}



# TESTE !!!!!!!!!1 --------------------------------------------------------

# tictoc::tic()
# orra <- matriz_acessibilidade("fortaleza", ligar_otp = FALSE, amostra = TRUE)
# tictoc::toc()

