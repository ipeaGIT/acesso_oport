#### Funcao para selecionar a data do gtfs que sera usada no script Python para o OTP

selecionar_data_gtfs <- function(sigla_muni, ano) {
  # sigla_muni <- 'for'; ano <- 2019
  # sigla_muni <- 'rio'; ano <- 2017
  # sigla_muni <- 'rio'; ano <- 2018
  # sigla_muni <- 'bho'; ano <- 2017
  # sigla_muni <- 'for'; ano <- 2017
  # sigla_muni <- 'poa'; ano <- 2017
  # sigla_muni <- 'cam'; ano <- 2018
  # sigla_muni <- 'bho-flat'; ano <- 2019
  # sigla_muni <- 'sal-flat'; ano <- 2019
  
  file.remove(dir("../../data/acesso_oport/temp/", full.names = TRUE))
  
  message(sprintf("working on %s", sigla_muni))
  
  # Leitura do gtfs para pasta temporaria
  path_zip <- sprintf("../../otp/graphs/%s/%s", ano, sigla_muni)
  file_zip <- dir(path_zip, full.names = TRUE, pattern = "gtfs.*.zip$", ignore.case = TRUE)
  
  if (length(file_zip) == 0) {
    
    dia_fim <- "2019-09-20"
    
  } else if(length(file_zip) == 2) {
    
    
    unzip(file_zip[1], files = "calendar.txt", exdir = "../../data/acesso_oport/temp")
    file.rename(from = "../../data/acesso_oport/temp/calendar.txt", 
                to = "../../data/acesso_oport/temp/calendar1.txt")
    unzip(file_zip[2], files = "calendar.txt", exdir = "../../data/acesso_oport/temp")
    file.rename(from = "../../data/acesso_oport/temp/calendar.txt", 
                to = "../../data/acesso_oport/temp/calendar2.txt")
    
    # Le calendario 1
    calendar1 <- data.table::fread("../../data/acesso_oport/temp/calendar1.txt") %>%
      mutate(end_date = as.character(end_date)) %>%
      mutate(start_date = as.character(start_date)) %>%
      mutate(end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      mutate(start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      # get start and end with most ocurrances
      mutate(pastego = paste0(start_date, "-", end_date)) %>%
      add_count(pastego) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      mutate(end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      mutate(start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%Y%m%d")))
    
    # Le calendario 2
    calendar2 <- data.table::fread("../../data/acesso_oport/temp/calendar2.txt") %>%
      mutate(end_date = as.character(end_date)) %>%
      mutate(start_date = as.character(start_date)) %>%
      mutate(end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      mutate(start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      # get start and end with most ocurrances
      mutate(pastego = paste0(start_date, "-", end_date)) %>%
      add_count(pastego) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      mutate(end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      mutate(start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%Y%m%d")))
    
    calendar1_interval <- interval(calendar1$start_date, calendar1$end_date)
    calendar2_interval <- interval(calendar2$start_date, calendar2$end_date)
    
    
    intersect_intervals <- lubridate::intersect(calendar1_interval, calendar2_interval)
    
    
    # Criar data frame com todos os dias dentro do intervalo de operacao do GTFS, e
    # retornar a ultima quar-feira disponivel anterior ao dia de hoje
    datas_possiveis <- data.frame(dia = seq.Date(as.Date(int_start(intersect_intervals)),
                                                 as.Date(int_end(intersect_intervals)), by="days")) %>%
      # Determinar o dia da semana
      mutate(dia_semana = wday(dia)) %>%
      # Garantir que o dia não é depois do dia de hoje
      filter(dia < Sys.Date()) %>%
      # Selecionar as quartas-feira, quinta e sexta
      filter(dia_semana %in% c(4, 5, 6)) %>%
      # Selecionar o ultimo dia
      slice(n())
    
    dia_fim <- datas_possiveis$dia
    
    
  } else if(length(file_zip) == 1) {
    
    
    unzip(file_zip, files = "calendar.txt", exdir = "../../data/acesso_oport/temp")
    
    # Le calendario
    calendar <- data.table::fread("../../data/acesso_oport/temp/calendar.txt") %>%
      mutate(end_date = as.character(end_date)) %>%
      mutate(start_date = as.character(start_date)) %>%
      mutate(end_date = as.Date(end_date, tryFormats = c("%Y-%m-%d", "%Y%m%d"))) %>%
      mutate(start_date = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%Y%m%d")))
    
    
    # Criar data frame com todos os dias dentro do intervalo de operacao do GTFS, e
    # retornar a ultima quar-feira disponivel anterior ao dia de hoje
    datas_possiveis <- data.frame(dia = seq.Date(min(unique(calendar$start_date)), unique(max(calendar$end_date)), by="days")) %>%
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


# a <- lapply(munis_df[modo_2017 == "todos"]$abrev_muni, safely(selecionar_data_gtfs), ano = 2017)
# 
# 
# selecionar_data_gtfs("bho", 2017)
# selecionar_data_gtfs("cam", 2017)
