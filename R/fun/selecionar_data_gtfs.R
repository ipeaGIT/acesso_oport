#### Funcao para selecionar a data do gtfs que sera usada no script Python para o OTP

selecionar_data_gtfs <- function(sigla_muni) {
  # sigla_muni <- 'for'
  
  file.remove("../data/temp/calendar.txt")
  
  message(sprintf("working on %s", sigla_muni))
  
  # Leitura do gtfs para pasta temporaria
  path_zip <- sprintf("../otp/graphs/%s", sigla_muni)
  file_zip <- dir(path_zip, full.names = TRUE, pattern = "gtfs.*.zip$", ignore.case = TRUE)[1]
  unzip(file_zip, files = "calendar.txt", exdir = "../data/temp")
  
  if (length(list.files("../data/temp", patter = "calendar.txt")) == 0) {
    
    dia_fim <- "2019-09-20"
    
  } else {
    
    
    # Le calendario
    calendar <- data.table::fread("../data/temp/calendar.txt") %>%
      mutate(end_date = as.character(end_date)) %>%
      mutate(start_date = as.character(start_date)) %>%
      mutate(end_date = as.Date(end_date, format = "%Y%m%d")) %>%
      mutate(start_date = as.Date(start_date, format = "%Y%m%d"))
    
    
    # Criar data frame com todos os dias dentro do intervalo de operacao do GTFS, e
    # retornar a ultima quar-feira disponivel anterior ao dia de hoje
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
    
    if (length(dia_fim) == 0) {
      
      dia_fim <- calendar$start_date
      
    } 
    
  }
  
  
  return(dia_fim)
  
}


lapply(munis_df[modo == "todos"]$abrev_muni[6], selecionar_data_gtfs)
