library(data.table)
library(dplyr)
library(sf)
library(mapview)
library(osmdata)
library(readr)
library(dodgr)

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
head(sao_sf_linhas)

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
graph1 <- graph1[, .(way_id, edge_id, from_id, from_lon, from_lat, to_id, to_lon, to_lat, d, time)]

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
  # Criar coluna com informacao se o segmento esta ok ou nao
  mutate(ok = ifelse(to_id == end_osm_node_id, "sim", "nao")) %>%
  # Preencher o valor da coluna do node do uber
  tidyr::fill(end_osm_node_id) %>%
  # Criar nova coluna identificando onde o node do graph eh igual ao node do uber
  mutate(nodes_iguais = ifelse(end_osm_node_id == to_id,
                          "sim", 
                          NA)) %>%
  
  mutate(end_node_fim = ifelse(ok %in% "nao", end_osm_node_id,
                                ifelse(nodes_iguais %in% "sim",  to_id,
                                       ifelse(ok %in% "sim", NA,
                                              NA)))) %>%
  tidyr::fill(end_node_fim) %>%
  ungroup() %>%
  # Checar se o "nodes_iguais" eh "sim" mesmo
  group_by(end_node_fim) %>%
  mutate(end_node_fim_ok = ifelse(is.na(last(nodes_iguais)),
                                  NA,
                                  end_node_fim)) %>%
  ungroup()



graph_fim_v1 <- graph_fim %>%
  group_by(way_id, end_node_fim_ok) %>%
  summarise(from_id = first(from_id),
            to_id = last(to_id),
            d = sum(d),
            speed_kph_mean = first(speed_kph_mean))


# WITH DATA.TABLE -----------------------------------------------------------------------------

# pulo do gato dt
graph_fim_dt <- graph_ok[order(way_id, edge_id)]
graph_fim_dt[, end_osm_node_id := as.numeric(end_osm_node_id)]
graph_fim_dt[, to_id := as.numeric(to_id)]
graph_fim_dt[, way_id := as.numeric(way_id)]
# Agrupar tudo por id da via e criar coluna com informacao se o segmento esta ok ou nao
graph_fim_dt[, ok := if_else(to_id == end_osm_node_id, 
                             1, 
                             0), 
             by = way_id]
# Preencher o valor da coluna do node do uber
graph_fim_dt[, end_osm_node_id := zoo::na.locf(end_osm_node_id, na.rm=FALSE), by = way_id]
# Criar nova coluna identificando onde o node do graph eh igual ao node do uber
graph_fim_dt[, nodes_iguais := if_else(end_osm_node_id == to_id,
                                  1, 
                                  NA_real_), 
          by = way_id]
# Continuar..
graph_fim_dt[, end_node_fim := if_else(ok == 0, end_osm_node_id,
                                   if_else(nodes_iguais == 1,  to_id,
                                          if_else(ok == 1, NA_real_,
                                                 NA_real_))), 
          by = way_id]
# Preencher..
graph_fim_dt[, end_node_fim := zoo::na.locf(end_node_fim, na.rm=FALSE), by = way_id]
# Checar se o "nodes_iguais" eh "sim" mesmo
# # Comecar criando um id unico pro grupos de nodes
# graph_fim_dt[, group_nodes_id := rleid(end_node_fim)]
graph_fim_dt[, end_node_fim_ok := if_else(is.na(last(nodes_iguais)),
                                          NA_real_,
                                          end_node_fim),
             by = .(way_id, end_node_fim)]

# incorporar os edges que nao foram bem sucedidos!
graph_fim_dt[, end_node_fim_ok_2 := if_else(is.na(end_node_fim_ok),
                                          to_id,
                                          end_node_fim_ok),
             by = .(way_id, end_node_fim)]

# AGRUPAMENTO ---------------------------------------------------------------------------------

graph_fim_v1_dt <- copy(graph_fim_dt)
# Agrupar e pegar as colunas de interesse
graph_fim_v1_dt <- graph_fim_v1_dt[, .(from_id = first(from_id), from_lon = first(from_lon), from_lat = first(from_lat),
                                       to_id = last(to_id), to_lon = first(to_lon), to_lat = first(to_lat),
                                       d = sum(d), time = sum(time), speed_kph_mean = first(speed_kph_mean)),
                                   by = .(way_id, end_node_fim_ok_2)]
# Criar coluna do tempo
graph_fim_v1_dt[, time := if_else(!is.na(speed_kph_mean), 
                                  (d/(speed_kph_mean / 3.6)),
                                  time)]
graph_fim_v1_dt[, ':='(d_weighted = d, time_weighted = time)]

# salvar
write_rds(graph_fim_v1_dt, "../../data/uber_speed/sao_final.rds")


# ROTEAMENTO ----------------------------------------------------------------------------------
library(dodgr)

graph_fim_v1_dt <- read_rds("../../data/uber_speed/sao_final.rds")

# adicionar as colunas la
graph_fim_v2_dt <- graph_fim_v1_dt %>%
  # Adicionar coluna de highway
  left_join(sao_sf_linhas %>% st_set_geometry(NULL) %>% select(osm_id, highway) %>% mutate(osm_id = as.numeric(osm_id)),
            by = c("way_id" = "osm_id")) %>%
  # Criar geom_num
  mutate(geom_num = group_indices(., way_id)) %>%
  # Criar edge_id
  mutate(edge_id = 1:n()) %>%
  mutate(from_id = as.character(from_id)) %>%
  mutate(to_id = as.character(to_id)) %>%
  filter(!is.na(end_node_fim_ok_2)) %>%
  select(-end_node_fim_ok_2, -speed_kph_mean)

from <- sample(graph_fim_v2_dt$from_id, size = 1000)
to <- sample(graph_fim_v2_dt$to_id, size = 1000)

# My coordinates
coords_sao <- read_csv("../../otp/points/points_sao_08.csv")

from <- as.matrix(coords_sao[, c("X", "Y")])
to <- as.matrix(coords_sao[, c("X", "Y")])

t_time <- dodgr_times (graph_fim_v2_dt, from = from, to = to, shortest = FALSE) # fastest paths

# teste
vertices <- dodgr_vertices(graph_fim_v2_dt)

# testar aguns vertices que deram na
vertice_teste <- 2942589635
vertice_teste <- 2332064938


vertices_na <- vertices %>%
  filter(id == vertice_teste) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

mapview(vertices_na)
  



