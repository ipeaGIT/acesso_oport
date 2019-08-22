# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.4 Avaliar qualidade da matriz de tempo de viagem

# carregar bibliotecas
source('./R/fun/setup.R')



# Funcao que identifica quais pontos nao foram roteados em cada modo de transportes 
avaliar_qualidade_otp <- function(sigla_muni, ano) {
  # sigla_muni <- "for"; ano <- 2019

 # listar e ler arquivos de Transporte Publico e Walking
 
  # padrao de nome dos arquivos
  pattern_pt <- sprintf("ttmatrix_%s_%s_pt*.rds", ano, sigla_muni)
  pattern_ativo <- sprintf("ttmatrix_%s_%s_ativo.rds", ano, sigla_muni)

  # listar arquivos
  files_pt <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
                         full.names = TRUE, 
                         pattern = pattern_pt)
  
  files_ativo <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
                           full.names = TRUE, 
                           pattern = pattern_ativo)
  
  
  # abrir arquivos
  otp_matrix_pt <- readr::read_rds(files_pt)
  otp_matrix_ativo <- readr::read_rds(files_ativo)
    
  # abrir os pontos da resolucao 09 ~~~~
  points_file <- sprintf("../otp/points/points_%s_09.csv", sigla_muni)
  points <- fread(points_file)
  
  # checar os pontos na matrix ~~~~
  # para pt
  origem_matrix_pt <- unique(otp_matrix_pt$origin)
  destino_matrix_pt <- unique(otp_matrix_pt$destination)
  # para ativo
  origem_matrix_ativo <- unique(otp_matrix_ativo$origin)
  destino_matrix_ativo <- unique(otp_matrix_ativo$destination)
  
  ## quais origens e destinos ficaram fora? ~~~~
  # para pt
  origem_fora_pt <- setdiff(points$id_hex, origem_matrix_pt) 
  destino_fora_pt <- setdiff(points$id_hex, destino_matrix_pt)
  # para ativo
  origem_fora_ativo <- setdiff(points$id_hex, origem_matrix_ativo)
  destino_fora_ativo <- setdiff(points$id_hex, destino_matrix_ativo)
  
  # quais pontos ficaram fora completamente? tanto a origem como o destino ~~
  # para pt
  pontos_fora_pt <- intersect(origem_fora_pt, destino_fora_pt)
  # para ativo
  pontos_fora_ativo <- intersect(origem_fora_ativo, destino_fora_ativo)
  
  
  # criar dataframe com sumario das informacoes
  fim <- cbind(sigla_muni = sigla_muni, 
               id_hex = c(pontos_fora_pt, pontos_fora_ativo), 
               modo = rep(c("pt", "ativo"), times = c(length(pontos_fora_pt), length(pontos_fora_ativo)))) %>%
    as.data.frame() %>%  
    # trazer a localizacao dos pontos
    left_join(points, by = "id_hex") %>%
    # calcular percentual que ficou fora
    group_by(sigla_muni, modo) %>%
    mutate(n = n()) %>%
    mutate(Percentual = n/nrow(points)) %>%
    mutate(Percentual = scales::percent(Percentual)) %>%
    ungroup()
  
}

# # teste
# fim %>%
#   filter(modo == "pt") %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#   mapview()





# Funcao para realizar snap dos pontos que nao foram roteados pelo OTP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sigla_muni <- "for"

corrigir_pontos_otp_snap <- function(sigla_muni) {
  
  # pegar caminho da rede viaria
  path_network_in <- sprintf("../otp/graphs/%s", sigla_muni)
  
  # pegar caminho do arquivo da rede viaria
  path_network <- dir(path_network_in, full.names = TRUE, pattern = "*.pbf$")
  # abrir street network
  network <- st_read(path_network, layer = "lines") %>%
    # Selecionar somente vias
    filter(!is.na(highway)) %>%
    # excluir vias nao caminhaveis
    filter(highway %nin% c("trunk","trunk_link","motorway","motorway_link","construction"))
  
  # extrair pontos que nao foram roteados pelo otp
  points_fora <- avaliar_qualidade_otp(sigla_muni, ano = 2019) %>%
    # Filtrar somente transporte publico
    filter(modo == "pt") %>%
    # Transformar para sf
    to_spatial(c("X", "Y")) %>%
    # Selecionar colunas de interesse
    select(id_hex)
  
  # pegar hexagonos do municipo ~~~~~~~~~~
  path_hex_in <- sprintf("../data/hex_municipio/hex_%s_09.rds", sigla_muni)
  hex <- read_rds(path_hex_in) %>%
    # filtrar somente hexagonos problematicos
    filter(id_hex %in% points_fora$id_hex)
  
  # filtrar somente as ruas que tem intersecoes com os hexagonos ~~~~~~
  network_filtrada <- network %>% 
    st_join(hex, left = FALSE)
  
  source("R/fun/snap_point_to_road.R")
  
  # Aplicar funcao para fazer snap dos pontos somente se ele tiver rua dentro do mesmo hexagono
  points_snap <- map_dfr(points_fora$id_hex, 
                         snap_sf, 
                         points_to_correct = points_fora, streets_buffer = network_filtrada) %>%
    as_tibble() %>%
    st_sf(crs = 4326) %>%
    # transformar de volta para X e Y
    sfc_as_cols(names = c("X", "Y"))
  
  # Juntar com os pontos totais ~~~~~~
  # pegar caminho dos pontos
  path_points_in <- sprintf("../otp/points/points_%s_09.csv", sigla_muni)
  # abrir caminho do pontos
  points_new <- fread(path_points_in) %>%
    # filtrar somente pontos que nao foram problematicos
    filter(id_hex %nin% points_snap$id_hex) %>%
    # juntar de volta pontos problematicos
    rbind(points_snap)
  
  # salvar corrigido
  path_out <- sprintf("../otp/points_corrigidos/points_corrigido_%s_09.csv", sigla_muni)
  fwrite(points_new, path_out)
  
  
  
}


# Aplicar funcao ~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parallel processing using future.apply
future::plan(future::multiprocess)
#options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
future.apply::future_lapply(X =munis_df$abrev_muni, FUN=corrigir_pontos_otp_snap, future.packages=c('sf', 'dplyr', 'data.table', 'Hmisc'))
