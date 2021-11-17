# carregar bibliotecas
source('./R/fun/setup.R')


# 1) Identificar e corrigir hexagonos que nao foram roteados e de acessibilidade extrema baixa -----

# Primeiro, sao identificados hexagos que nao foram roteados pelo OTP (etapa 1)
# Segundo, a verificacao eh feita pegando todos os hexagonos que conseguem acessar menos de 10 hex (etapa 2)
# p/ bike. O ponto de roteamento desses hexagonos sao alocados para o hexagono mais proximo 
# que nao tenha menos que 10 hex de acess.

# sigla_muni <- 'for'; ano <- 2017
# sigla_muni <- 'for'; ano <- 201
# sigla_muni <- 'nat'; ano <- 2017
# sigla_muni <- 'man'; ano <- 2019

identificar_e_corrigir_extremos_acess_muni <- function(sigla_muni, ano) {
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano)
  
  
  ttmatrix_allmodes <- fread(sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5.csv", 
                                        ano, ano, sigla_muni))
  
  # ttmatrix_allmodes <- read_rds("../../data/avaliacao_intervencoes/for/ttmatrix/antes/ttmatrix_bike.rds")
  
  # pegar so bike
  ttmatrix_teste <- ttmatrix_allmodes[mode == "bike"]
  # ttmatrix_teste <- ttmatrix_allmodes
  
  # abrir os pontos da resolucao 09 ~~~~
  points_file <- sprintf("../../otp/points/%s/points_%s_09_%s.csv", ano, sigla_muni, ano)
  # points_file <- "../../data/avaliacao_intervencoes/r5/points/points_for_09_2019.csv"
  points <- fread(points_file)
  
  
  # 1) Identificar quais pontos nao foram roteados --------------------
  
  # checar os pontos na matrix ~~~~
  origem_matrix <- unique(ttmatrix_teste$origin)
  # origem_matrix <- unique(ttmatrix_teste$fromId)
  destino_matrix <- unique(ttmatrix_teste$destination)
  # destino_matrix <- unique(ttmatrix_teste$toId)
  
  ## quais origens e destinos ficaram fora? ~~~~
  origem_fora <- setdiff(points$id_hex, origem_matrix) 
  destino_fora <- setdiff(points$id_hex, destino_matrix)
  
  # quais pontos ficaram fora completamente? tanto a origem como o destino ~~
  # para pt
  hex_problematicos_etapa1 <- intersect(origem_fora, destino_fora)
  
  
  
  # 2) Identificar hexagonos de acessibilidade extrema --------------------
  
  # Criar variavel de 1 oportunidade para calculo da "acessibilidade"
  ttmatrix_teste[, var := 1]
  
  # Calcular acess para 90 minutos
  acess_origin <- ttmatrix_teste[,
                                 .(acess = (sum(var[which(travel_time <= 90)], na.rm = T))),
                                 by=.(fromId)]
  # rename id column
  setnames(acess_origin, "fromId", "id_hex")
  
  acess_dest <- ttmatrix_teste[,
                               .(acess = (sum(var[which(travel_time <= 90)], na.rm = T))),
                               by=.(toId)]
  setnames(acess_dest, "toId", "id_hex")
  
  
  # extrair hexagonos que nao consigam acessar mais que 10 hexagonos
  acess_prob <- rbind(acess_origin[acess < 10],
                      acess_dest[acess < 10]) %>%
    distinct(id_hex, .keep_all = TRUE)
  
  hex_problematicos_etapa2 <- acess_prob$id_hex
  
  
  
  
  # 3) Juntar os hexagonos da etapa 1 e 2 fazer o teste de distancia --------------------
  
  # pegar todos os hexagonos problematicos
  points_prob <- points %>%
    filter(id_hex %in% c(hex_problematicos_etapa1, hex_problematicos_etapa2))
  
  
  # 4) Corrigir os hexagonos problematicos nas matrizes originais de tempo de viagem ---------------
  
  
  # # # separar na base os problematicos e nao problematicos
  # # esses sao os hex que vao ser utilizados para a correcao dos problematicos
  # ttmatrix_allmodes_blueprint <- setDT(ttmatrix_allmodes)[origin %in%  points_corrigidos$hex_blueprint |
  #                                                           destination %in% points_corrigidos$hex_blueprint]
  # 
  # esses sao os hex que NAO vao ser utilizados para a correcao dos problematicos
  ttmatrix_allmodes_nprob <- setDT(ttmatrix_allmodes)[!(origin %in%  points_prob$id_hex |
                                                          destination %in% points_prob$id_hex)]
  
  
  # funcao para corrigir os hexagonos nas ttmatrix 
  
  corrigir_hex_ttmatrix <- function(hex_prob) {
    
    # hex_prob <- "89818a593cfffff" # nat
    # hex_prob <- "8980104e91bffff" # for
    # hex_prob <- "89801048d37ffff" # for
    
    # ver quais sao os vizinhos desse hexagono
    # ring_size = 2 vai me trazer todos os hex vizinhos ate o nivel 2, isso da 37 vizinhos
    
    hex_prob_vizinhos <- h3jsr::get_kring(hex_prob, ring_size = 3)[[1]][-1]
    
    # garantir que os vizinhos nao sao problematicos
    hex_prob_vizinhos <- hex_prob_vizinhos[hex_prob_vizinhos %nin% points_prob]
    
    # garantir que os vizinhos estejam dentro da matriz de tempo de viagem da cidade
    # podem acontecer dois casos em que o(s) hex(s) vizinho(s) esteja(m) fora da matriz:
    # 1) o hex problematico ser de borda/perto mar e tem varios vizinhos na cidade vizinha
    # ou no oceano
    # 2) o hex vizinho nao ter pop/atividade e nao ter entrado pro OTP
    hex_prob_vizinhos <- hex_prob_vizinhos[hex_prob_vizinhos %in% ttmatrix_allmodes$origin]
    
    # pegar so os 10 primeiros vizinhos
    hex_prob_vizinhos <- hex_prob_vizinhos[1:10] %>% na.omit()
    
    # ATENCAO: eh necessario corrigir o hex tanto pra quando ele eh origem na matriz
    # como pra quando ele eh destino na matrix
    
    # para a origem:
    # filtrar entao somente a matrix para os hexagonos vizinhos que nao sao problematicos - origem
    ttmatrix_allmodes_blueprint_origin <- ttmatrix_allmodes[origin %in% hex_prob_vizinhos]
    
    # essses hexagonos de origem agora serao representados somente por um hex, o problematico - origem
    ttmatrix_allmodes_blueprint_origin$origin <- hex_prob
    
    # calcular entao a media do tempo de viagem, que vai representar o novo tt do hex prob - origem
    ttmatrix_allmodes_blueprint_origin_mean <- ttmatrix_allmodes_blueprint_origin[, .(travel_time = mean(travel_time, na.rm = TRUE)),
                                                                                  by = .(city, mode, origin, destination, pico)]
    
    # para o destino:
    # filtrar entao somente a matrix para os hexagonos vizinhos que nao sao problematicos - destino
    ttmatrix_allmodes_blueprint_dest <- ttmatrix_allmodes[destination %in% hex_prob_vizinhos]
    
    # essses hexagonos de destino agora serao representados somente por um hex, o problematico - destino
    ttmatrix_allmodes_blueprint_dest$destination <- hex_prob
    
    # calcular entao a media do tempo de viagem, que vai representar o novo tt do hex prob - destino
    ttmatrix_allmodes_blueprint_dest_mean <- ttmatrix_allmodes_blueprint_dest[, .(travel_time = mean(travel_time, na.rm = TRUE)),
                                                                              by = .(city, mode, origin, destination, pico)]
    
    
    # juntar as matrizes de quando eh origem e de quando eh destino
    ttmatrix_allmodes_blueprint_fim <- rbind(ttmatrix_allmodes_blueprint_origin_mean, ttmatrix_allmodes_blueprint_dest_mean)
    
    nrow(ttmatrix_allmodes_blueprint_fim)
    nrow(distinct(ttmatrix_allmodes_blueprint_fim, origin, destination, mode, pico))
    
    # # qual id correto correspondente
    # hex_correto <- subset(points_corrigidos, hex_problema==hex_prob)$hex_blueprint
    # 
    # # subset de todas origem e destino com id correto
    # temp_tt <- setDT(ttmatrix_allmodes_blueprint)[ origin == hex_correto | destination==hex_correto]
    # 
    # # Substituir id correto pelo id q deu errado
    # temp_tt[, origin := fifelse(origin==hex_correto, hex_prob, origin) ]
    # temp_tt[, destination := fifelse(destination==hex_correto, hex_prob, destination) ]
    
    return(ttmatrix_allmodes_blueprint_fim)
    
  }  
  
  
  # aplicar funcao para cada hex problematico
  ttmatrix_hex_prob_corrigidos <- lapply(points_prob$id_hex, corrigir_hex_ttmatrix) %>%
    rbindlist()
  
  
  # juntar com a base original
  # excluir os pares OD dos corrigidos na base original e juntar
  ttmatrix_hex_fim <- rbind(ttmatrix_allmodes_nprob,
                            ttmatrix_hex_prob_corrigidos)
  
  # salvar output corrigido
  write_rds(ttmatrix_hex_fim, sprintf("E:/data/ttmatrix_fix/%s/ttmatrix_fix_%s_%s.rds", ano, ano, sigla_muni))
  
  
}



# aplicar funcao ------------------------------------------------------------------------------
walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, identificar_e_corrigir_extremos_acess_muni, ano = 2017)
walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, identificar_e_corrigir_extremos_acess_muni, ano = 2018)
walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, identificar_e_corrigir_extremos_acess_muni, ano = 2019)