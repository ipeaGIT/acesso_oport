#' ## Avaliar qualidade dos resultados
#' 
#' 
## ----avaliar_qualidade_otp, eval=TRUE------------------------------------

cidade <- "bel"

avaliar_qualidade_otp <- function(cidade) {
  
  pattern_cidade_pt <- sprintf("ttmatrix_%s_pt_.*.csv$", cidade)
  # pattern_cidade_ative <- sprintf("ttmatrix_%s_(walk|bike)_09.csv$", cidade)
  pattern_cidade_walk <- sprintf("ttmatrix_%s_walk_09.csv$", cidade)
  
  files_cidade_pt <- dir(sprintf("../data/output_ttmatrix/%s", cidade), 
                         full.names = TRUE, 
                         pattern = pattern_cidade_pt)[1]
  
  files_cidade_walk <- dir(sprintf("../data/output_ttmatrix/%s", cidade), 
                           full.names = TRUE, 
                           pattern = pattern_cidade_walk)
  
  otp_matrix_pt <- map_dfr(files_cidade_pt, fread)
  otp_matrix_walk <- map_dfr(files_cidade_walk, fread)
    
  # abrir os pontos
  points_file <- sprintf("../otp/points/points_%s_09.csv", cidade)
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
  
  fim <- cbind(cidade = cidade, 
               id_hex = c(pontos_fora_pt, pontos_fora_walk), 
               modo = rep(c("pt", "walk"), times = c(length(pontos_fora_pt), length(pontos_fora_walk)))) %>%
    as.data.frame() %>%  
    left_join(points, by = "id_hex") %>%
    group_by(cidade, modo) %>%
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
#   dplyr::filter(cidade == "por") %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#   mapview() +
#   read_rds("../data/hex_municipio/hex_por_08.rds") %>% mapview()


#' 
#' 
## ------------------------------------------------------------------------

cidade <- "bel"

snap_points_to_roads <- function(cidade) {
  
  path_network_in <- sprintf("../otp/graphs/%s", cidade)
  
  # abrir street network
  path_network <- dir(path_network_in, full.names = TRUE, pattern = "*.pbf$")
  network <- st_read(path_network, layer = "lines") %>%
    # Selecionar somente vias
    filter(!is.na(highway)) %>%
    filter(highway %nin% c("trunk","trunk_link","motorway","motorway_link","construction"))
  
  # extrair pontos que nao foram roteados pelo otp
  points_fora <- avaliar_qualidade_otp(cidade) %>%
    distinct(id_hex, X, Y) %>%
    to_spatial(c("X", "Y"))
  
  # pegar hexagonos
  path_hex_in <- sprintf("../data/hex_municipio/hex_%s_09.rds", cidade)
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
  path_points_in <- sprintf("../otp/points/points_%s_09.csv", cidade)
  points_new <- fread(path_points_in) %>%
    filter(id_hex %nin% points_snap$id_hex) %>%
    rbind(points_snap)
  
  # salvar corrigido
  path_out <- sprintf("../otp/points_corrigidos/points_corrigido_%s_09.csv", cidade)
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

cidade <- "for"

abrir_resultado_matrix <- function(cidade) {
  
  pattern_cidade_pt <- sprintf("ttmatrix_%s_pt_09_.*.csv$", cidade)
  # pattern_cidade_ative <- sprintf("ttmatrix_%s_(walk|bike)_09.csv$", cidade)
  pattern_cidade_walk <- sprintf("ttmatrix_%s_walk_09.csv$", cidade)
  
  files_cidade_pt <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_pt)[1]
  
  fim <- fread(files_cidade_pt)
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
points_file <- sprintf("../otp/points/points_%s_09.csv", cidade)
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
  mutate(Cidade = c("Fortaleza", "Belo Horizonte", "Curitiba", "Porto Alegre")) %>%
  # mutate(Percentual = color_bar("red")(Percentual)) %>%
  select(Cidade, n, Percentual) %>%
  kable() %>%
  # column_spec(3, width = "3cm") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


#' 
#' 
#' 
## ----avaliar_qualidade_acessibilidade, eval = TRUE-----------------------

# cidade <- "bel"

avaliar_qualidade_acess <- function(cidade) {
  
  pattern_cidade_pt <- sprintf("ttmatrix_%s_pt_08_.*.csv$", cidade)
  pattern_cidade_ative <- sprintf("ttmatrix_%s_(walk|bike)_08.csv$", cidade)
  
  files_cidade_pt <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_pt)
  files_cidade_ative <- dir("../data/output_ttmatrix", full.names = TRUE, pattern = pattern_cidade_ative)
  
  otp_matrix_pt <- map_dfr(files_cidade_pt, fread)
  
  # abrir oportunidades com hexagonos
  dir_hex <- sprintf("../data/hex_agregados/hex_agregado_%s_%s.rds", cidade, "08")
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
