~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Funcoes para preparar os inputs do OpenTripPlanner

  
# carregar bibliotecas
source('./R/fun/setup.R')



### 1. Funcao para gerar pontos de origem e destino

# sigla_muni <- "for"

gerar_pontos_OTP <- function(sigla_muni) {
  
  # Lista resolucoes disponiveis
  dir <- dir("../data/hex_agregados/", pattern = sigla_muni)
  res <- str_extract(dir, "\\d+")
  
  # Lista arquivos de hexagonos
  dir_muni <- paste0("../data/hex_agregados/hex_agregado_", sigla_muni, "_", res, ".rds")
  
  # muni_res <- dir_muni[1]
  
  gerar_por_resolucao <- function(muni_res) {
    
    # Endereco do hexagono
    dir_muni <- muni_res
    
    # Resolucao utilizada
    res <- str_extract(dir_muni, "\\d+")
    
    

          
   # adiciona totais
    # setDT(hex_muni)[, empregos_total := sum(empregos_alta, empregos_media, empregos_baixa), by=id_hex]
    # setDT(hex_muni)[, escolas_total := sum(edu_infantil, edu_fundamental, edu_medio), by=id_hex]
      
      
    # criar pontos
    hex_muni <- readr::read_rds(dir_muni) %>%
      # Tirar hexagonos sem atividade
      filter(!(pop_total == 0 & renda_total == 0 & empregos_total == 0 & saude_total == 0 & 
                 escolas_infantil == 0 & escolas_fundamental == 0 & escolas_medio == 0)) %>%
      select(id_hex) %>%
      st_centroid() %>%
      sfc_as_cols(names = c("X","Y"))
    # rename(GEOID = id_hex)
    
    
    # salvar
    dir_output <- sprintf("../otp/points/points_%s_%s.csv", sigla_muni, res)
    
    write_csv(hex_muni, dir_output)
    
  }
  
  walk(dir_muni, seila)
  
}



#' 
#' ###  Criar script em Python
#' 
#' 
#' 
#' 
## ----fun_criar_script_python---------------------------------------------

# Funcao para selecionar a data do gtfs

selecionar_data_gtfs <- function(sigla_muni) {
  
  path_zip <- sprintf("../otp/graphs/%s", sigla_muni)
  file_zip <- dir(path_zip, full.names = TRUE, pattern = "gtfs.*.zip$", ignore.case = TRUE)[1]
  
  unzip(file_zip, files = "calendar.txt", exdir = "../data/temp")
  
  calendar <- read_delim("../data/temp/calendar.txt", delim = ",") %>%
    mutate(end_date = as.character(end_date)) %>%
    mutate(start_date = as.character(start_date)) %>%
    mutate(end_date = as.Date(end_date, format = "%Y%m%d")) %>%
    mutate(start_date = as.Date(start_date, format = "%Y%m%d"))
  
  datas_possiveis <- data.frame(dia = seq.Date(unique(calendar$start_date)[1], unique(calendar$end_date)[1], by="days")) %>%
    # Determinar o dia da semana
    mutate(dia_semana = wday(dia)) %>%
    # Garantir que o dia não é depois do dia de hoje
    filter(dia < Sys.Date()) %>%
    # Selecionar as quartas-feira
    filter(dia_semana == 4) %>%
    # Selecionar a ultima quarta-feira
    slice(n())
  
  dia_fim <- datas_possiveis$dia
  
  
}

# Funcao para criar o script em python
source("R/4-criar_script_python_parallel_multiple.R")


#' 
#' ### Aplicar comando para rodar OTP
#' 
#' 
## ----fun_aplicar_otp-----------------------------------------------------

# sigla_muni <- "for"

aplicar_otp <- function(sigla_muni, data) {
  
  py_nome <- dir("../otp/py", pattern = sprintf("otp_%s", sigla_muni))[1] 
  
  comando <- sprintf("cd ../otp && java -jar programs/jython.jar -Dpython.path=programs/otp.jar py/%s", py_nome)
  
  shell(comando)
  
  

  # colar os arquivos
  
  # pegar os arquivos
  files <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
               pattern = "^ttmatrix_\\w{3}_pt",
               full.names = TRUE)
  
  # extrair os horarios
  horarios <- str_extract(files, "\\d{1,2}-\\d{1,2}") %>% unique()
  
  # funcao para abrir e juntar os arquivos de cada horario
  
  # horarios1 <- horarios[1]
  
  abrir_e_juntar <- function(horarios1) {
    
    files_ok <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
               pattern = sprintf("^ttmatrix_\\w{3}_pt_%s", horarios1),
               full.names = TRUE)
    
    # abrir, juntar e salvar arquivos
    path_out <- sprintf("../data/output_ttmatrix/%s/ttmatrix_%s_%s.csv", sigla_muni, sigla_muni, horarios1)
    
    furrr::future_map(files_ok, fread) %>%
      rbindlist() %>%
      fwrite(path_out)
    
    # remove files?
    walk(files_ok, file.remove)
  }
  
  # aplicar funcao
  plan(multiprocess)
  invisible(furrr::future_map(horarios, abrir_e_juntar))
  
  
}


#' 
