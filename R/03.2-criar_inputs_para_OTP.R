#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.2 Criar inputs do OpenTripPlanner
# 1. pontos de origem e destino
# 2. Scripts em Python
  
# carregar bibliotecas
source('./R/fun/setup.R')



### 1) Funcao para gerar pontos de origem e destino -----------------------------------

gerar_pontos_OTP <- function(sigla_muni, ano) {

  # sigla_muni <- "for"
  # ano <- 2019
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano, '\n')
  
  
  # Lista resolucoes disponiveis
  dir <- dir(sprintf("../data/hex_agregados/%s", ano), pattern = sigla_muni)
  res <- str_extract(dir, "\\d{2}(?=_)")
  
  # Lista arquivos de hexagonos
  dir_muni <- paste0("../data/hex_agregados/", ano, "/hex_agregado_", sigla_muni, "_", res, "_", ano, ".rds")
  
  # funcao que aplica por resolucao  
  gerar_por_resolucao <- function(muni_res) {
    # muni_res <- dir_muni[1]
    
    
    # Endereco do hexagono
    dir_muni <- muni_res
    
    # Identifica resolucao utilizada
    res <- str_extract(dir_muni, "\\d{2}(?=_)")
    
    
    # adiciona totais
    # setDT(hex_muni)[, empregos_total := sum(empregos_alta, empregos_media, empregos_baixa), by=id_hex]
    # setDT(hex_muni)[, edu_total := sum(edu_infantil, edu_fundamental, edu_medio), by=id_hex]
    
    
    # Selecoina apenas hexagonos com ao menos uma atividade e gera os centroides
    hex_centroides <- readr::read_rds(dir_muni) %>%
      # Tirar hexagonos sem atividade
      filter(!(pop_total == 0 & 
                 renda_total == 0 & 
                 empregos_total == 0 & 
                 saude_total == 0 & 
                 edu_total == 0)) %>%
      select(id_hex) %>%
      st_centroid() %>%
      sfc_as_cols(names = c("X","Y")) # funcao (dentro de stup.R) que transforma sf em data.frame com lat/long em colunas separadas
  
      
  # salvar centroids
    dir_output <- sprintf("../otp/points/%s/points_%s_%s_%s.csv", ano, sigla_muni, res, ano)
    data.table::fwrite(hex_centroides, dir_output)
    
  }
  
  walk(dir_muni, gerar_por_resolucao)
  
}

# Aplica funcao para todas cidades
future::plan(future::multiprocess)
future.apply::future_lapply(X= munis_df$abrev_muni, FUN=gerar_pontos_OTP, ano = 2019)



### 2) Criar scripts em Python -----------------------------------

# Carregar Funcao para criar o script em python
source("./R/fun/criar_script_python_parallel_multiple.R")

# pico
pblapply(munis_df[modo == "todos"]$abrev_muni, criar_script_python_paral_modes,
         ano = 2019,
         from = 6, until = 8, every = 15, modo = "todos")

# fora pico, # apenas modo transporte publico
pblapply(munis_df[modo == "todos"]$abrev_muni, criar_script_python_paral_modes, 
         ano = 2019,
         from = 14, until = 16, every = 15, modo = 'tp') 

# para as cidades sem gtfs, sera somente transporte ativo
# cidades: bsb, sal, man, rec, goi, bel, gua, cam, slz, sgo, mac, duq, cgr, nat
pblapply(munis_df[modo == "ativo"]$abrev_muni, criar_script_python_paral_modes, 
         ano = 2019,
         modo='ativo') 


# #TEST
# # pico
# pblapply(munis_df$abrev_muni, criar_script_python_paral_modes, from = 6, until = 7, every = 30)
# 
# # fora pico, # apenas modo transporte publico
# pblapply(munis_df$abrev_muni, criar_script_python_paral_modes, from = 14, until = 15, every = 30, modo='tp') 



