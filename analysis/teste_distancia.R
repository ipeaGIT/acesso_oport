

sp <- read_rds("../data/hex_agregados/hex_agregado_sao_08.rds")
sp <- read_rds("../data/hex_agregados/hex_agregado_rio_08.rds")

for_hex_centroids <-  sp %>%
  st_centroid() %>%
  sfc_as_cols()



# GERAR TABELA DE MATRIZ --------------------------------------------------

for_od <- for_hex_centroids %>%
  tidyr::expand(id_hex, id_hex) %>%
  left_join(for_hex_centroids) %>%
  left_join(for_hex_centroids, by = c("id_hex1" = "id_hex"), suffix = c(".origem", ".destino")) %>%
  rename(origem = id_hex, destino = id_hex1)

vai_pop <- sp %>%
  filter(pop_total == 0)

vai_atividade <- sp %>%
  filter(saude_total == 0 & escolas_total == 0 & empregos_total == 0)

# Fazer limpeza
for_od_v1 <- for_od %>%
  # Tirar origem == destino
  filter(origem != destino) %>%
  # Tirar hexagonos sem populacao
  filter(origem %nin% vai_pop$id_hex) %>%
  # Tirar hexagonos sem atividade
  filter(destino %nin% vai_atividade$id_hex)


dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}


setDT(for_od_v1)

system.time(for_od_v1[, dist := dt.haversine(lat.origem, lon.origem, lat.destino, lon.destino)])

fim_v1 <- for_od_v1 %>%
  filter(dist < 30000)

str(for_od_v1)

