# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.4 Avaliar qualidade da matriz de tempo de viagem

# carregar bibliotecas
source('./R/fun/setup.R')



## ----avaliar_qualidade_otp, eval=TRUE------------------------------------

# Funcao que identifica quais pontos nao foram roteados em cada modo de transportes 
avaliar_qualidade_otp <- function(sigla_muni, ano) {
  # sigla_muni <- "for"; ano <- 2019

 # listar e ler arquivos de Transporte Publico e Walking
 
   # padrao de nome dos arquivos
    pattern_pt <- sprintf("ttmatrix_%s_%s_pt*.rds", ano, sigla_muni)
    pattern_walk <- sprintf("ttmatrix_%s_%s_walk", ano, sigla_muni)
    pattern_bike <- sprintf("ttmatrix_%s_%s_bike", ano, sigla_muni)
    
  # listar arquivos
  files_pt <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
                         full.names = TRUE, 
                         pattern = pattern_pt)
  
  files_walk <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
                           full.names = TRUE, 
                           pattern = pattern_walk)
  
  files_bike <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
                               full.names = TRUE, 
                               pattern = pattern_bike)
  
  
  otp_matrix_pt <- map_dfr(files_sigla_muni_pt, fread)
  otp_matrix_walk <- map_dfr(files_sigla_muni_walk, fread)
    
  # abrir os pontos
  points_file <- sprintf("../otp/points/points_%s_09.csv", sigla_muni)
  points <- fread(points_file)
  
  # checar os pontos na matrix
  origem_matrix_pt <- unique(otp_matrix_pt$origin)
  destino_matrix_pt <- unique(otp_matrix_pt$destination)
  origem_matrix_walk <- unique(otp_matrix_walk$origin)
  destino_matrix_walk <- unique(otp_matrix_walk$destination)
  
  # quais origens e destinos ficaram fora?
  origem_fora_pt <- setdiff(points$id_hex, origem_matrix_pt)
  destino_fora_pt <- setdiff(points$id_hex, destino_matrix_pt)
  origem_fora_walk <- setdiff(points$id_hex, origem_matrix_walk)
  destino_fora_walk <- setdiff(points$id_hex, destino_matrix_walk)
  
  # quais pontos ficaram fora completamente? tanto a origem como o destino
  pontos_fora_pt <- intersect(origem_fora_pt, destino_fora_pt)
  pontos_fora_walk <- intersect(origem_fora_walk, destino_fora_walk)
  
  fim <- cbind(sigla_muni = sigla_muni, 
               id_hex = c(pontos_fora_pt, pontos_fora_walk), 
               modo = rep(c("pt", "walk"), times = c(length(pontos_fora_pt), length(pontos_fora_walk)))) %>%
    as.data.frame() %>%  
    left_join(points, by = "id_hex") %>%
    group_by(sigla_muni, modo) %>%
    mutate(n = n()) %>%
    mutate(Percentual = n/nrow(points)) %>%
    mutate(Percentual = scales::percent(Percentual)) %>%
    ungroup()
  
}

# # Aplicar funcao
# qualidade_otp_for <- avaliar_qualidade_otp("for")
# qualidade_otp_for <- avaliar_qualidade_otp("bel")
# qualidade_otp_for <- avaliar_qualidade_otp("por")
# qualidade_otp_for <- avaliar_qualidade_otp("rio")
# qualidade_otp_for <- avaliar_qualidade_otp("sao")
# 
# qualidade_otp <- map_dfr(c("for", "bel", "cur", "por", "rio"), avaliar_qualidade_otp)

# visualilzar os pontos que ficaram fora♥

# # para porto alegre
# qualidade_otp %>%
#   dplyr::filter(sigla_muni == "por") %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#   mapview() +
#   read_rds("../data/hex_municipio/hex_por_08.rds") %>% mapview()


#' 
#' 
## ------------------------------------------------------------------------

sigla_muni <- "bel"

snap_points_to_roads <- function(sigla_muni) {
  
  path_network_in <- sprintf("../otp/graphs/%s", sigla_muni)
  
  # abrir street network
  path_network <- dir(path_network_in, full.names = TRUE, pattern = "*.pbf$")
  network <- st_read(path_network, layer = "lines") %>%
    # Selecionar somente vias
    filter(!is.na(highway)) %>%
    filter(highway %nin% c("trunk","trunk_link","motorway","motorway_link","construction"))
  
  # extrair pontos que nao foram roteados pelo otp
  points_fora <- avaliar_qualidade_otp(sigla_muni) %>%
    distinct(id_hex, X, Y) %>%
    to_spatial(c("X", "Y"))
  
  # pegar hexagonos
  path_hex_in <- sprintf("../data/hex_municipio/hex_%s_09.rds", sigla_muni)
  hex <- read_rds(path_hex_in) %>%
    # filtrar somente hexagonos problematicos
    filter(id_hex %in% points_fora$id_hex)
  
  # filtrar somente as ruas que tem intersecoes com os hexagonos
  network_filtrada <- network %>% 
    st_join(hex, left = FALSE)
  
  source("R/fun/snap_point_to_road.R")
  
  points_snap <- map_dfr(points_fora$id_hex, 
                         snap_sf, 
                         points_to_correct = points_fora, streets_buffer = network_filtrada) %>%
    as_tibble() %>%
    st_sf(crs = 4326) %>%
    sfc_as_cols(names = c("X", "Y"))
  
  # Juntar com os pontos totais
  path_points_in <- sprintf("../otp/points/points_%s_09.csv", sigla_muni)
  points_new <- fread(path_points_in) %>%
    filter(id_hex %nin% points_snap$id_hex) %>%
    rbind(points_snap)
  
  # salvar corrigido
  path_out <- sprintf("../otp/points_corrigidos/points_corrigido_%s_09.csv", sigla_muni)
  fwrite(points_new, path_out)
  
  
  
}



#' 
#' 
#' 
## ----for_erro------------------------------------------------------------

snap_points_to_roads("for") # ok
snap_points_to_roads("bel") # ok
snap_points_to_roads("rio") # ok
snap_points_to_roads("sao")
snap_points_to_roads("cur") # ok
snap_points_to_roads("por") # ok



#' 
#' 
## ----rodar_otp_corrigidos------------------------------------------------




#' 
#' 
#' 
## ----for_erro_novo-------------------------------------------------------

# Funcao para abrir resultado da matrix

sigla_muni <- "for"

abrir_resultado_matrix <- function(sigla_muni) {
  
  pattern_sigla_muni_pt <- sprintf("ttmatrix_%s_pt_09_.*.csv$", sigla_muni)
  # pattern_sigla_muni_ative <- sprintf("ttmatrix_%s_(walk|bike)_09.csv$", sigla_muni)
  pattern_sigla_muni_walk <- sprintf("ttmatrix_%s_walk_09.csv$", sigla_muni)
  
  files_sigla_muni_pt <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_sigla_muni_pt)[1]
  
  fim <- fread(files_sigla_muni_pt)
}

ttmatrix_for <- abrir_resultado_matrix("for")

# Quais hexagonos tiverem distancias de caminhada maior que 2000 metros?
hex_problematicos <- ttmatrix_for[walk_distance > 2000] %>%
  .[, .(.N), by = origin] %>%
  .[N > 1000]

# Qual a localizacao deles?
hex_problematicos_sf <- ttmatrix_for %>%
  setDT() %>%
  .[origin %in% hex_problematicos$origin]

hex_problematicos_sf <- unique(hex_problematicos_sf, by = "origin")

# abrir os pontos
points_file <- sprintf("../otp/points/points_%s_09.csv", sigla_muni)
points <- fread(points_file)

hex_problematicos_sf <- merge(hex_problematicos_sf, setDT(points),
                              by.x = "origin",
                              by.y = "id_hex",
                              all.x = TRUE)

hex_problematicos_sf %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  mapview()


#' 
#' 
## ----por_erro------------------------------------------------------------

# abrir street network
st_layers("../otp/graphs/por/por_export.pbf")
por_network <- st_read("../otp/graphs/por/por_export.pbf", layer = "lines")

# extrair pontos de porto alegre que nao foram roteados pelo otp
por_points_fora <- avaliar_qualidade_otp("por") %>%
  select(ponto_fora, X, Y) %>%
  distinct(ponto_fora, .keep_all = TRUE)

# fazer um buffer de 2km em relacao a esses pontos e pegar somente as ruas que estao nesse raio 
points_buffer <- por_points_fora %>%
  to_spatial(c("X", "Y")) %>%
  st_transform(31984) %>%
  st_buffer(1000) %>%
  st_transform(4326)

network_filtrada <- por_network %>% 
  st_join(points_buffer, left = FALSE)

# viz
mapview(por_points_fora %>% to_spatial(c("X", "Y")), zcol = NULL) + mapview(network_filtrada)


#' 
#' 
#' 
## ----tabela_qualidade_otp, eval = TRUE-----------------------------------

qualidade_otp %>%
  mutate(sigla_muni = c("Fortaleza", "Belo Horizonte", "Curitiba", "Porto Alegre")) %>%
  # mutate(Percentual = color_bar("red")(Percentual)) %>%
  select(sigla_muni, n, Percentual) %>%
  kable() %>%
  # column_spec(3, width = "3cm") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


#' 
#' 
#' 
## ----avaliar_qualidade_acessibilidade, eval = TRUE-----------------------

# sigla_muni <- "bel"

avaliar_qualidade_acess <- function(sigla_muni) {
  
  pattern_sigla_muni_pt <- sprintf("ttmatrix_%s_pt_08_.*.csv$", sigla_muni)
  pattern_sigla_muni_ative <- sprintf("ttmatrix_%s_(walk|bike)_08.csv$", sigla_muni)
  
  files_sigla_muni_pt <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_sigla_muni_pt)
  files_sigla_muni_ative <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_sigla_muni_ative)
  
  otp_matrix_pt <- map_dfr(files_sigla_muni_pt, fread)
  
  # abrir oportunidades com hexagonos
  dir_hex <- sprintf("../data/hex_agregados/hex_agregado_%s_%s.rds", sigla_muni, "08")
  hexagonos_for_sf <- read_rds(dir_hex) %>%
    select(id_hex) %>%
    ungroup()
  
  matriz_for <- otp_matrix_pt %>%
    left_join(hexagonos_for_sf, by = c("origin" = "id_hex")) %>%
    select(origin, destination, travel_time) %>%
    mutate(travel_time = travel_time/60) %>%
    mutate(empregos = 1) %>%
    dplyr::filter(travel_time < 60) %>%
    group_by(origin) %>%
    summarise(empregos = sum(empregos))
  
  access_ac_for_fim <- hexagonos_for_sf %>%
    select(id_hex) %>%
    left_join(matriz_for, by = c("id_hex" = "origin"))
  
  
  access_ac_for_fim %>%
    ggplot() +
    geom_sf(aes(fill=empregos), color="gray70") +
    scale_fill_distiller( palette="Oranges", guide = "colorbar", name="Jobs\nDensity", direction = 1) +
    theme_bw() +
    theme(legend.position = "none")
  
  # mapview(access_ac_for_fim, zcol = "empregos")
  
}

avaliar_qualidade_acess("for") +
avaliar_qualidade_acess("bel") +
avaliar_qualidade_acess("por") +
avaliar_qualidade_acess("cur") +
  plot_layout(ncol = 2)



#' 
