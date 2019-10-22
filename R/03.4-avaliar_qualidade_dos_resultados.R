# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.4 Avaliar qualidade da matriz de tempo de viagem

# carregar bibliotecas
source('./R/fun/setup.R')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1) Juntar output ttmatrix e agrupar as matrizes --------------------------
# As matrizes sao tratadas, agregadas nos intervalos de 15 minutos fazendo a mediana do tempo de viagme
# Em seguida sao juntos todos os modos

agrupar_ttmatrix <- function(sigla_muni, ano = 2019) {
  
  # sigla_muni <- "bho"; ano=2019
  # sigla_muni <- "spo"; ano=2019
  
  # status message
  message('Woking on city ', sigla_muni, '\n')
  
  
  # Listar arquivos de matriz em formato .csv
  tt_files <- dir(path= sprintf("E:/data/output_ttmatrix/%s/", sigla_muni), pattern = '.csv', full.names = T)
  
  # Ler e empilhar ttmatrix
  future::plan(future::multiprocess)
  ttmatrix_allmodes <- future.apply::future_lapply(X =tt_files, FUN=fread, future.packages=c('data.table')) %>% 
    data.table::rbindlist(fill = T)
  # ttmatrix_allmodes <- lapply(X=tt_files, FUN= readr::read_rds) %>% data.table::rbindlist(fill = T)
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem como:
  # transit / walk: 350s equivale ao tempo necessario para cruzar um hexagono a bicicleta (~1 metro/sec = ~3.6 km/h)
  # bike: 110s equivale ao tempo necessario para cruzar um hexagono a de pe (~3.3 metros/sec = ~12 km/h)
  ttmatrix_allmodes[, travel_time := as.numeric(travel_time)]
  ttmatrix_allmodes[mode=='bike', travel_time := ifelse(origin == destination, 110, travel_time)]
  ttmatrix_allmodes[mode %in% 'walk|transit', travel_time := ifelse(origin == destination, 350, travel_time)]
  
  # convert depart_time para formato itime
  ttmatrix_allmodes[, depart_time := as.ITime(depart_time)]
  
  # Classificar informacao de horario de partida como pico ou fora pico
  ttmatrix_allmodes[, pico := ifelse(mode %in% c("bike", "walk"), 1,
                                     ifelse( depart_time %between% c(as.ITime("06:0:00"), as.ITime("08:00:00")),1,0))]
  
  
  
  # Calcular a mediana do tempo de viagem entre cada par OD para pico e fora pico
  
  # Calcular a mediana agrupando por sigla_muni, modo, origin, destination, pico
  ttmatrix_median <- ttmatrix_allmodes[, .(tt_median = median(travel_time, na.rm = TRUE)), 
                                       by = .(city, mode, origin, destination, pico)]
  
  # Transformar o traveltime para minutos
  ttmatrix_median[, tt_median := tt_median/60]
  
  # salvar
  path_out <- sprintf("E:/data/ttmatrix_agregada/ttmatrix_agregada_%s.rds", sigla_muni)
  
  write_rds(ttmatrix_median, path_out)
  
}




# Aplicar funcao ~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parallel processing using future.apply
future::plan(future::multiprocess)
#options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
future.apply::future_lapply(X =munis_df$abrev_muni, FUN=agrupar_ttmatrix, future.packages=c('sf', 'dplyr', 'data.table'))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2) Identificar hexagonos que nao foram roteados e de acessibilidade extrema  --------------------------

# Primeiro, sao identificados hexagos que nao foram roteados pelo OTP (etapa 1)
# Segundo, a verificacao eh feita pegando todos os hexagonos que conseguem acessar menos de 10 hex (etapa 2)
# p/ transporte publico. O ponto de roteamento desses hexagonos sao alocados para o hexagono mais proximo 
# que nao tenha menos que 10 hex de acess.
# Hexagonos que estiverem a menos de 1km das bordas serao desconsiderados


identificar_extremos_acess <- function(sigla_muni) {
  
  # sigla_muni <- "cur"
  
  # status message
  message('Woking on city ', sigla_muni, '\n')
  
  # abrir matrix da cidade
  ttmatrix_allmodes <- read_rds(sprintf("E:/data/ttmatrix_agregada/ttmatrix_agregada_%s.rds", sigla_muni))
  
  # Para fazer o teste, pegar so um modo e so hora pico! 
  # Pegar so pico
  ttmatrix_teste <- ttmatrix_allmodes[pico == 1]
  
  # Se tiver so ativo, pegar so bike; se tiver todos, pegar tp
  if (subset(munis_df, abrev_muni == sigla_muni)$modo == "ativo") {
    
    ttmatrix_teste <- ttmatrix_teste[mode == "bike"]
    
  } else {
    
    ttmatrix_teste <- ttmatrix_teste[mode == "transit"]
    
  }
  
  # abrir os pontos da resolucao 09 ~~~~
  points_file <- sprintf("../otp/points/points_%s_09.csv", sigla_muni)
  points <- fread(points_file)
  
  
  # 1) Identificar quais pontos nao foram roteados --------------------
  
  # checar os pontos na matrix ~~~~
  origem_matrix <- unique(ttmatrix_teste$origin)
  destino_matrix <- unique(ttmatrix_teste$destination)
  
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
  acess <- ttmatrix_teste[,
                           .(acess = (sum(var[which(tt_median <= 90)], na.rm = T))),
                           by=.(city, origin)]
  
  # Trazer a geolocalizacao dos hex
  hex <- read_rds(sprintf("../data/hex_municipio/hex_%s_09.rds", sigla_muni))
  
  # Juntar com o acess
  acess_hex <- left_join(acess, hex, by = c("origin" = "id_hex")) %>% st_sf()
  
  acess_prob <- acess_hex %>% 
  # extrair hexagonos que nao consigam acessar mais que 10 hexagonos
    filter(acess < 10) %>%
    # transformar para pontos
    st_centroid()
  
  hex_problematicos_etapa2 <- acess_prob$origin
  
  
  
  
  # 3) Juntar os hexagonos da etapa 1 e 2 fazer o teste de distancia --------------------
  
  # pegar todos os hexagonos problematicos
  points_prob <- points %>%
    filter(id_hex %in% c(hex_problematicos_etapa1, hex_problematicos_etapa2))
  
  
  # Primeiro, identificar os hexagonos que nao sao problematicos
  # Transformar os hexagonos para centroides e lon lat
  # pegar hexagonos nao problematicos
  acess_nprob <- acess_hex %>%
    # filtrar so hexagonos nao problematicos
    filter(origin %nin% points_prob$id_hex) %>%
    # criar id
    mutate(id = 1:n())
  
  
  acess_nprob_centroides <- acess_nprob %>%
    # transformar para pontos
    st_centroid() %>%
    # transformar para lon lat
    sfc_as_cols()
  
  # pegar os pontos mais proximos nao-problematicos dos problematicos
  uui <- RANN::nn2(select(acess_nprob_centroides, lon, lat), 
                   select(points_prob, X, Y), 1)
  
  # entao identificar quais sao esses hexagonos
  points_corrigidos <- points_prob %>% 
    # # tirar a geometria: ela agora sera nova dos outros hexagonos!
    # st_set_geometry(NULL) %>%
    mutate(id = uui$nn.idx) %>%
    # trazer tambem a distancia
    mutate(dist = uui$nn.dists * 111320) %>%
    # filtrar distancia maxima para o hexagono nao problematico: 1 KM!
    filter(dist < 1000) %>%
    # eu so preciso do id para trazer as coordenadas do acess_nprob!
    select(id_hex, id) %>%
    # trazer o id completo do hex
    left_join(acess_nprob_centroides %>% select(origin, id), by = "id")
    # # selecionar colunas e renomear
    # select(id_hex = origin, X = lon, Y = lat)
  
  
  # # fim: transformar de volta para sf e juntar com os nao-problematicos
  # # abrir pontos roteaveis
  # points <- fread(sprintf("../otp/points_corrigidos/points_%s_09.csv", sigla_muni)) %>%
  #   # selecionar so os nao problematicos
  #   filter(id_hex %nin% acess_prob_corrigidos$id_hex) %>%
  #   # juntar os problematicos corrigidos
  #   rbind(acess_prob_corrigidos)
  
  
  # 4) Corrigir os hexagonos problematicos nas matrizes originais de tempo de viagem ---------------
  
  
  # # separar na base os problematicos e nao problematicos
  # esses sao os hex que vao ser utilizados para a correcao dos problematicos
  ttmatrix_allmodes_prob <- setDT(ttmatrix_allmodes)[origin %in%  points_corrigidos$origin |
                                                        destination %in% points_corrigidos$origin]
  
  # esses sao os hex que NAO vao ser utilizados para a correcao dos problematicos
  ttmatrix_allmodes_nprob <- setDT(ttmatrix_allmodes)[!(origin %in%  points_corrigidos$id_hex |
                                                        destination %in% points_corrigidos$id_hex)]
  
  
  # # criar coluna do par origem-destino
  # setDT(ttmatrix_allmodes)[, par := paste0(origin, "-", destination)]
  # setDT(ttmatrix_allmodes)[, par1 := .I, by = .(origin, destination)]
  
  
  # funcao para corrigir os hexagonos nas ttmatrix 
  
  corrigir_hex_ttmatrix <- function(hex_prob) {
    
    # hex_prob <- points_corrigidos$id_hex[1]
    
    # qual id correto correspondente
    hex_correto <- subset(points_corrigidos, id_hex==hex_prob)$origin
    
    # subset de todas origem e destino com id correto
    temp_tt <- setDT(ttmatrix_allmodes_prob)[ origin == hex_correto | destination==hex_correto]
    
    # Substituir id correto pelo id q deu errado
    temp_tt[, origin := fifelse(origin==hex_correto, hex_prob, origin) ]
    temp_tt[, destination := fifelse(destination==hex_correto, hex_prob, destination) ]
    
  }  
    
  
  # aplicar funcao para cada hex problematico
  ttmatrix_hex_prob_corrigidos <- lapply(points_corrigidos$id_hex, corrigir_hex_ttmatrix) %>%
    rbindlist()
  
  
  # juntar com a base original
  # excluir os pares OD dos corrigidos na base original e juntar
  ttmatrix_hex_fim <- rbind(ttmatrix_allmodes_nprob,
                            ttmatrix_hex_prob_corrigidos)
    
  # salvar output corrigido
  write_rds(sprintf("E:/data/ttmatrix_agregada_cor/ttmatrix_agregada_cor_%s.rds", sigla_muni))
  

}

