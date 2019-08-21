# cidade <- "por"

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
