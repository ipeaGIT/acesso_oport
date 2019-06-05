library(data.table)
library(dplyr)
library(sf)
library(mapview)
library(osmdata)
library(readr)

speeds <- fread("../../data-raw/uber_speed/SAO_uber_speed/movement-speeds-quarterly-by-hod-sao-paulo-2018-Q4/movement-speeds-quarterly-by-hod-sao-paulo-2018-Q4.csv")

head(speeds)

nodes <- fread("../../data-raw/uber_speed/SAO_uber_speed/movement-junctions-to-osm-nodes-sao-paulo-2018/movement-junctions-to-osm-nodes-sao-paulo-2018.csv")
segments <- fread("../../data-raw/uber_speed/SAO_uber_speed/movement-segments-to-osm-ways-sao-paulo-2018/movement-segments-to-osm-ways-sao-paulo-2018.csv")

head(nodes)

# Criar o id do osm no speeds
speeds_v1 <- speeds %>%
  left_join(nodes, by = c("start_junction_id" = "junction_id")) %>%
  rename(start_osm_node_id = osm_node_id) %>%
  left_join(nodes, by = c("end_junction_id" = "junction_id")) %>%
  rename(end_osm_node_id = osm_node_id) %>%
  left_join(segments, by = c("segment_id")) %>%
  select(year, quarter, osm_id = osm_way_id, hour_of_day, start_osm_node_id, end_osm_node_id, speed_kph_mean:speed_kph_p85)

head(speeds_v1)


# DODGR ------------------------------------------------------------------------------
library(dodgr)


# DONT RUN!! --------------------------------------------------------------
# abrir osm stuff
sao1 <- st_read("data-raw/sao_01_export.pbf", layer = "lines")
sao2 <- st_read("data-raw/sao_02_export.pbf", layer = "lines")
fort <- st_read("data-raw/for_teste_planet_osm_line_lines.shp")
# sao1nodes <- st_read("data-raw/sao_01_export.pbf", layer = "points")
# sao2nodes <- st_read("data-raw/sao_02_export.pbf", layer = "points")
# 
# length(which(as.character(nodes_sao) == unique(as.character(sao1nodes$osm_id))))



sao <- rbind(sao1, sao2)
sao_sample <- sao[1:1000,] %>% mutate(osm_id = as.character(osm_id)) %>% select(osm_id, highway) %>%
  filter(!is.na(highway)) %>% as_tibble() %>% st_sf()


# - -----------------------------------------------------------------------



# TEM QUE SER POR AQUI!!!

sao_sf <- opq("são paulo") %>%
  add_osm_feature (key="highway") %>%
  osmdata_sf(quiet = FALSE) %>%
  osm_poly2line()

# salvar
write_rds(sao_sf, "../../data-raw/uber_speed/SAO_uber_speed/osm_network_sao.rds")

# tirar so as linhas e as informacoes q preciso
sao_sf_linhas <- sao_sf$osm_lines %>%
  select(osm_id, bicycle, covered, foot, highway, incline,
         motorcar, motorcycle, motor_vehicle, oneway, surface,
         tracktype, tunnel, width, geometry)

# salvar as linhas
write_rds(sao_sf_linhas, "../../data-raw/uber_speed/SAO_uber_speed/osm_network_sao.rds")

# abrir
sao_sf_linhas <- read_rds("../../data-raw/uber_speed/SAO_uber_speed/osm_network_sao.rds")

# criar dodgr graph
graph <- weight_streetnet(sao_sf_linhas, wt_profile = "motorcar") %>%
  select(-d_weighted, -time_weighted)


# quais nós dos speeds está no dodgr?
nodes_speed <- unique(bit64::as.integer64(speeds_v1$start_osm_node_id))
nodes_dodgr <- unique(bit64::as.integer64(graph$from_id))

# quem ficou de dentro?
nodes_ok <- intersect(nodes_speed, nodes_dodgr)

# quem ficou de fora
nodes_nok <- setdiff(nodes_speed, nodes_ok)


# AGORA EH PRA VALER!! ------------------------------------------------------------------------
setDT(graph)
setDT(speeds_v1)

graph1 <- graph[, from_id := as.numeric(from_id)]
graph1[, to_id := as.numeric(to_id)]
graph1[, way_id := as.numeric(way_id)]
graph1 <- graph1[, .(way_id, edge_id, from_id, to_id, d)]

# pegar so um speed de cada segmento!
speeds_v2 <- speeds_v1[hour_of_day %in% c(7, 8, 9, 10)]
speeds_v2 <- unique(speeds_v2, by = c("start_osm_node_id", "end_osm_node_id"))
speeds_v2 <- speeds_v2[, .(osm_id, start_osm_node_id, end_osm_node_id, 
                           speed_kph_mean)]

# fazer o merge
graph_ok <- merge(graph1, speeds_v2, 
                  all.x = TRUE,
                  by.x = c("from_id", "way_id"), 
                  by.y = c("start_osm_node_id", "osm_id"))

nrow(filter(graph_ok, is.na(speed_kph_mean)))

# pulo do gato
graph_fim <- graph_ok %>%
  # Tirar amostra
  filter(edge_id <= 500) %>%
  arrange(way_id, edge_id) %>%
  # Simple stuff
  mutate(end_osm_node_id = as.character(end_osm_node_id)) %>%
  mutate(to_id = as.character(to_id)) %>%
  # Agrupar tudo por id da via
  group_by(way_id) %>%
  mutate(ok = ifelse(to_id == end_osm_node_id, "sim", "nao")) %>%
  mutate(end_osm_node_id_novo = ifelse(end_osm_node_id == 0, 
                                       NA,
                                       end_osm_node_id)) %>%
  tidyr::fill(end_osm_node_id_novo) %>%
  mutate(eh_esse = ifelse(end_osm_node_id_novo == to_id,
                          "sim", 
                          NA)) %>%
  mutate(n_tenho_ideia = ifelse(ok %in% "nao", end_osm_node_id,
                                ifelse(eh_esse %in% "sim",  to_id,
                                       ifelse(ok %in% "sim", NA,
                                              NA)))) %>%
  tidyr::fill(n_tenho_ideia)

graph_fim_v1 <- graph_fim %>%
  group_by(way_id, n_tenho_ideia) %>%
  summarise(from_id = first(from_id),
            to_id = last(to_id),
            d = sum(d),
            speed_kph_mean = first(speed_kph_mean))
  

  
  




