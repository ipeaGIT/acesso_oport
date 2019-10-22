
# Funcao que identifica quais pontos nao foram roteados em cada modo de transportes -------------


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