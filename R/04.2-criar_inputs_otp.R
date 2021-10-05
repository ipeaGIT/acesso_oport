#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.2 Criar inputs do OpenTripPlanner
# 1. pontos de origem e destino
# 2. Scripts em Python

# carregar bibliotecas
source('./R/fun/setup.R')



### 1) Funcao para gerar pontos de origem e destino -----------------------------------

gerar_pontos_OTP_muni <- function(sigla_muni, ano) {
  
  # sigla_muni <- "for"; ano <- 2017
  # ano <- 2019
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano, '\n')
  
  
  # Lista resolucoes disponiveis
  dir <- dir(sprintf("../../data/acesso_oport/hex_agregados/%s", ano), pattern = sigla_muni)
  res <- str_extract(dir, "\\d{2}(?=_)")
  
  # Lista arquivos de hexagonos
  dir_muni <- paste0("../../data/acesso_oport/hex_agregados/", ano, "/hex_agregado_", sigla_muni, "_", res, "_", ano, ".rds")
  
  # funcao que aplica por resolucao  
  gerar_por_resolucao <- function(muni_res) {
    # muni_res <- dir_muni[2]
    
    
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
    dir_output <- sprintf("../../otp/points/%s/points_%s_%s_%s.csv", ano, sigla_muni, res, ano)
    data.table::fwrite(hex_centroides, dir_output)
    
  }
  
  walk(dir_muni, gerar_por_resolucao)
  
}


walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, gerar_pontos_OTP_muni, ano = 2017)
walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, gerar_pontos_OTP_muni, ano = 2018)
walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, gerar_pontos_OTP_muni, ano = 2019)






### 2) Criar scripts em Python -----------------------------------
source("R/fun/criar_script_python_parallel_multiple.R")



# 2017 ----------------------------------------------------------------------------------------


# remove preivous scripts
purrr::walk(dir(sprintf("../../otp/py/%s", '2017'), full.names = TRUE, recursive = TRUE, pattern = "*.py"), file.remove)

# para as cidades que tem todos os modos de transporte (as que contem GTFS) (tp + ativo) --
# pico
walk(munis_list$munis_modo[ano_modo == 2017 & modo == "todos"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2017, from = 6, until = 8, every = 15, modo = "todos")  

# fora pico, # apenas modo transporte publico
walk(munis_list$munis_modo[ano_modo == 2017 & modo == "todos"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2017, from = 14, until = 16, every = 15, modo = "tp")  

# para as cidades sem gtfs, sera somente transporte ativo ---
walk(munis_list$munis_modo[ano_modo == 2017 & modo == "ativo"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2017, modo = "ativo") 






# 2018 ----------------------------------------------------------------------------------------

# remove preivous scripts
purrr::walk(dir(sprintf("../../otp/py/%s", '2018'), full.names = TRUE, recursive = TRUE, pattern = "*.py"), file.remove)

# para as cidades que tem todos os modos de transporte (as que contem GTFS) (tp + ativo) --
# pico
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2018, from = 6, until = 8, every = 15, modo = "todos")  

# fora pico, # apenas modo transporte publico
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2018, from = 14, until = 16, every = 15, modo = "tp")  

# para as cidades sem gtfs, sera somente transporte ativo ---
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "ativo"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2018, modo = "ativo") 





# 2019 ----------------------------------------------------------------------------------------

# remove preivous scripts
purrr::walk(dir(sprintf("../../otp/py/%s", '2019'), full.names = TRUE, recursive = TRUE, pattern = "*.py"), file.remove)

# para as cidades que tem todos os modos de transporte (as que contem GTFS) (tp + ativo) --
# pico
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2019, from = 6, until = 8, every = 15, modo = "todos")  

# fora pico, # apenas modo transporte publico
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2019, from = 14, until = 16, every = 15, modo = "tp")  

# para as cidades sem gtfs, sera somente transporte ativo ---
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "ativo"]$abrev_muni, criar_script_python_paral_modes_muni,
       ano = 2019, modo = "ativo") 




