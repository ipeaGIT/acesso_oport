library(dplyr)
library(tidyr)
library(opentripplanner)
library(sf)
library(purrr)
source("R/sfc_as_cols.R")

# devtools::install_local("misc/opentripplanner-master-fork2.zip")

tictoc::tic()

# ABRIR ARQUIVOS ----------------------------------------------------------

for_hex <- st_read("../data/hex_municipio/fortaleza/hex_fortaleza.shp", crs = 4326) %>%
  mutate(id = 1:n()) %>%
  # filter(id %in% c(1:200)) %>%
  select(id)

for_hex_centroids <- for_hex %>%
  st_centroid() %>%
  sfc_as_cols()

# dados de escolas


# # LIGAR O OTP -------------------------------------------------------------
# 
otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = "fortaleza")
otp_for <- otp_connect(router = "fortaleza")


# GERAR TABELA DE MATRIZ --------------------------------------------------

for_od <- for_hex_centroids %>%
  expand(id, id) %>%
  left_join(for_hex_centroids) %>%
  left_join(for_hex_centroids, by = c("id1" = "id"), suffix = c(".origem", ".destino")) %>%
  rename(origem = id, destino = id1)


# GERAR LISTA COM COORDENADAS ---------------------------------------------


fromPlace <- map2(for_od$lat.origem, for_od$lon.origem, c)
toPlace <- map2(for_od$lat.destino, for_od$lon.destino, c)

names(fromPlace) <- 1:length(fromPlace)
names(toPlace) <- 1:length(toPlace)


# # PASSAR PARA O OTP -------------------------------------------------------
# 
# a <- Sys.time()
# for_matriz_access <- map2(fromPlace[1:714], toPlace[1:714], .f = otp_plan, otpcon = otp_for,
#                           date_time = as.POSIXct("2018-11-05"), mode = "TRANSIT")
# b <- Sys.time()
# 
# # Identificar 
# names(for_matriz_access) <- paste(rep(1:2, each = 357), rep(1:357, 2), sep = "_")
# 
# sao_df <- map_chr(for_matriz_access, is.data.frame)
# 
# sao_df_num <- which(sao_df == "TRUE")
# 
# quequeu <- for_matriz_access[sao_df_num]
# 
# opaaaa <- map(quequeu, st_set_geometry, NULL)
# 
# vaaai <- bind_rows(opaaaa, .id = "origem_destino") %>%
#   group_by(origem_destino) %>%
#   filter(route_option == 1) %>%
#   mutate(etapa = 1:n()) %>%
#   select(origem_destino, etapa, modo = mode, duration, walkTime, transitTime, waitingTime)


# # OTP BATCH ---------------------------------------------------------------
# 
# origens_matrix <- as.matrix(for_hex_centroids)
# 
# origens_matrix <- origens_matrix[,c(1,3,2)]
# head(origens_matrix)
# colnames(origens_matrix) <- c("id_hex", "latitude", "longitude" )
# 
# system.time(
# otp_plan_batch_teste <- otp_plan_batch(otpcon = otp_for, 
#                                        fromPlace = origens_matrix[1:2,], toPlace = origens_matrix[],
#                                        date_time = as.POSIXct("2018-11-05"), mode = "TRANSIT"))

# para a nova versão (fork)

fromPlacematrix <- as.matrix(for_od[, c(3, 4)])
toPlacematrix <- as.matrix(for_od[, c(5, 6)])

Sys.setlocale("LC_ALL","English")
Sys.setenv(LANG = "en_US.UTF-8")
otp_plan_teste_batch <- otp_plan(otpcon = otp_for, 
                           fromPlace = fromPlacematrix[1:100,], 
                           toPlace = toPlacematrix[1:100,],
                           date_time = as.POSIXct("2018-11-05 08:00:00"),
                           mode = c("TRANSIT", "WALK"),
                           get_geometry = FALSE)
tictoc::toc()



origem <- c(fromPlacematrix[500,1], fromPlacematrix[500,2])
destino <- c(toPlacematrix[200,1], fromPlacematrix[200,2])

otp_plan_teste <- otp_plan(otpcon = otp_for, 
                          fromPlace = origem, 
                          toPlace = destino,
                          date_time = as.POSIXct("2018-11-05"),
                          mode = "TRANSIT",
                          get_geometry = FALSE)

# usando a url

fromPlacematrix <- as.matrix(for_od[, c(4, 3)])
toPlacematrix <- as.matrix(for_od[, c(6, 5)])

origem <- c(fromPlacematrix[500,1], fromPlacematrix[500,2])
destino <- c(toPlacematrix[200,1], fromPlacematrix[200,2])


origem1 <- paste0(origem, collapse = ",")
destino1 <- paste0(destino, collapse = ",")

req <- httr::GET(
  "http://localhost:8080/otp/routers/fortaleza/plan",
  query = list(
    fromPlace = origem1,
    toPlace = destino1,
    mode = "TRANSIT,WALK",
    date = "11-05-2018",
    time = "08:00am",
    maxWalkDistance = "1600",
    walkReluctance = "2",
    arriveBy = "FALSE",
    transferPenalty = "0",
    minTransferTime = "0"
  )
)

text <- httr::content(req, as = "text", encoding = "UTF-8")

x <- jsonlite::fromJSON(text)




# TESTE CURL --------------------------------------------------------------


