#' ## Metodologia para a construção da matriz
#' 
#' 
#' ### Criar pontos de origem para todas as cidades
#' 
#' 
## ----fun_criar_pontos_allres---------------------------------------------

points_allres <- function(muni_shortname) {
  
  dir <- dir("../data/hex_agregados/", pattern = muni_shortname)
  
  res <- str_extract(dir, "\\d+")
  
  dir_muni <- paste0("../data/hex_agregados/hex_agregado_", muni_shortname, "_", res, ".rds")
  
  seila <- function(muni_res) {
    
    dir_muni <- muni_res
    
    res <- str_extract(dir_muni, "\\d+")
    
    # criar pontos
    hex_muni <- readRDS(dir_muni) %>%
      # Tirar hexagonos sem atividade
      filter(!(pop_total == 0 & renda_total == 0 & empregos_total == 0 & saude_total == 0 & 
                 escolas_total == 0)) %>%
      select(id_hex) %>%
      st_centroid() %>%
      sfc_as_cols(names = c("X","Y"))
    # rename(GEOID = id_hex)
    
    
    # salvar
    dir_output <- sprintf("../otp/points/points_%s_%s.csv", muni_shortname, res)
    
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

selecionar_data_gtfs <- function(cidade) {
  
  path_zip <- sprintf("../otp/graphs/%s", cidade)
  file_zip <- dir(path_zip, full.names = TRUE, pattern = "gtfs.*.zip$", ignore.case = TRUE)
  
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
source("R/4-criar_script_python.R")


#' 
#' ### Aplicar comando para rodar OTP
#' 
#' 
## ----fun_aplicar_otp-----------------------------------------------------

aplicar_otp <- function(cidade, data, res = "08", all_modes = TRUE) {
  
  if (all_modes == FALSE) {
    
    py_nome <- sprintf("otp_%s_%s_%s.py", cidade, data, res) }
  
  else {
    
    py_nome <- sprintf("otp_%s_%s_%s_paral_allmodes.py", cidade, data, res)
    
  }
  
  comando <- sprintf("cd ../otp && java -jar programs/jython.jar -Dpython.path=programs/otp.jar py/%s", py_nome)
  
  shell(comando)
  
}


#' 
