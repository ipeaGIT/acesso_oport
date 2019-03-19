library(dplyr)
library(tidyr)
library(opentripplanner)
library(sf)
library(purrr)
source("R/sfc_as_cols.R")


# ABRIR ARQUIVOS ----------------------------------------------------------

for_hex <- st_read("data/hex_municipio/fortaleza/hex_fortaleza.shp", crs = 4326) %>%
  mutate(id = 1:n()) %>%
  filter(id %in% c(1:200)) %>%
  select(id)

for_hex_centroids <- for_hex %>%
  st_centroid() %>%
  sfc_as_cols()

# dados de escolas


# LIGAR O OTP -------------------------------------------------------------

otp_setup(otp = "otp/programs/otp.jar", dir = "otp", router = "fortaleza")
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


# PASSAR PARA O OTP -------------------------------------------------------

a <- Sys.time()
for_matriz_access <- map2(fromPlace[1:714], toPlace[1:714], .f = otp_plan, otpcon = otp_for,
                          date_time = as.POSIXct("2018-11-05"), mode = "TRANSIT")
b <- Sys.time()

# Identificar 
names(for_matriz_access) <- paste(rep(1:2, each = 357), rep(1:357, 2), sep = "_")

sao_df <- map_chr(for_matriz_access, is.data.frame)

sao_df_num <- which(sao_df == "TRUE")

quequeu <- for_matriz_access[sao_df_num]

opaaaa <- map(quequeu, st_set_geometry, NULL)

vaaai <- bind_rows(opaaaa, .id = "origem_destino") %>%
  group_by(origem_destino) %>%
  filter(route_option == 1) %>%
  mutate(etapa = 1:n()) %>%
  select(origem_destino, etapa, modo = mode, duration, walkTime, transitTime, waitingTime)


# OTP BATCH ---------------------------------------------------------------

origens_matrix <- as.matrix(for_hex_centroids)

origens_matrix <- origens_matrix[,c(1,3,2)]
head(origens_matrix)
colnames(origens_matrix) <- c("id_hex", "latitude", "longitude" )

system.time(
otp_plan_batch_teste <- otp_plan_batch(otpcon = otp_for, 
                                       fromPlace = origens_matrix[1:2,], toPlace = origens_matrix[],
                                       date_time = as.POSIXct("2018-11-05"), mode = "TRANSIT"))
