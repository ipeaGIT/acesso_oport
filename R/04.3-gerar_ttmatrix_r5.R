# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Calcula matriz de tempo de viagem com o R5


# carregar bibliotecas
options(java.parameters = "-Xmx64G")
library(r5r)
source('./R/fun/setup.R')
source("./R/fun/selecionar_data_gtfs.R")


# sigla_munii <- 'bho'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'poa'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'for'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'for'; modo = "walk"; ano <- 2017
# sigla_munii <- 'spo'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'spo'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio'; ano <- 2018; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio-novo'; ano <- 2019; modo <- c("WALK", "TRANSIT")

gerar_tt_matrix_r5 <- function(sigla_munii, ano, modo = c("WALK", "TRANSIT")) {
  
  # choose folder accordindgly
  folder_fine <- sprintf("../../otp/graphs/%s/%s", ano, sigla_munii)
  
  # r5 setup
  setup <- setup_r5(data_path = folder_fine)
  
  # points
  points <- fread(sprintf("../../otp/points/2019/points_%s_09_2019.csv", 
                          substr(sigla_munii, 1, 3)))
  colnames(points) <- c("id", "lon", "lat")
  
  # select date
  if (sigla_munii == "spo" & ano == 2017) {
    
    date <- "2018-05-01"
    
  } else {
    
    date <- selecionar_data_gtfs(sigla_munii, ano)
    
  }
  
  # departure times
  departure_times <- as.POSIXct(paste0(date, c(" 06:00:00",
                                               " 06:15:00",
                                               " 06:30:00",
                                               " 06:45:00",
                                               " 07:00:00",
                                               " 07:15:00",
                                               " 07:30:00",
                                               " 07:45:00")))
  
  # departure_time <- departure_times[1]
  
  # calculate ttmatrix
  calculate_ttmatrix <- function(departure_time) {
    
    # ttmatrix
    df <- travel_time_matrix(setup,
                             origin = points,
                             destination = points,
                             departure_datetime = departure_time,
                             time_window = 1,
                             # percentiles = c(25, 50, 75, 90),
                             max_walk_dist = 800,
                             mode = modo,
                             n_threads = 16)
    
    # rename columns
    df <- df %>% rename(origin = fromId, destination = toId) %>% setDT()
    
    # identify columns
    df[, mode := ifelse("TRANSIT" %in% modo, "transit", ifelse("BICYCLE", "bike", tolower(modo)))]
    df[, pico := 1]
    df[, city := sigla_munii]
    df[, departure_time := str_extract(as.character(departure_time), pattern = "\\d{2}:\\d{2}:\\d{2}")]
    
    table(df$mode)
    table(df$pico)
    table(df$departure_time)
    
    return(df)
    
  }
  
  # apply function to multiple departure times
  tictoc::tic()
  df_fim <- lapply(departure_times, calculate_ttmatrix) %>% rbindlist()
  tictoc::toc() # 3315.87 sec elapsed
  
  table(df_fim$departure_time)
  
  r5r::stop_r5()
  rJava::.jgc(R.gc = TRUE)
  
  # save ttmatrix/
  fwrite(df_fim, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_%s_r5.csv", 
                         ano, 
                         sigla_munii,
                         ifelse("TRANSIT" %in% modo, "pt", ifelse(modo == "BICYCLE", "bike", tolower(modo))),
                         ano))
  
}

gerar_tt_matrix_r5("for", ano = 2018)
gerar_tt_matrix_r5("bho", ano = 2018)
gerar_tt_matrix_r5("cur", ano = 2018)
gerar_tt_matrix_r5("poa", ano = 2018)
gerar_tt_matrix_r5("rio", ano = 2018)
gerar_tt_matrix_r5("spo", ano = 2018)






# apply function
walk(munis_df_2019$abrev_muni,
     gerar_tt_matrix_r5, ano = 2017, modo = "WALK")

walk(munis_df_2019$abrev_muni,
     gerar_tt_matrix_r5, ano = 2017, modo = "BICYCLE")

walk(c("for", "bho", "poa", "cur", "rio", "spo"),
     gerar_tt_matrix_r5, ano = 2017, modo = c("WALK", "TRANSIT"))

walk(c("cur", "rio", "spo"),
     gerar_tt_matrix_r5, ano = 2017, modo = c("WALK", "TRANSIT"))





walk(munis_df_2019$abrev_muni,
     gerar_tt_matrix_r5, ano = 2018, modo = "WALK")

walk(munis_df_2019$abrev_muni,
     gerar_tt_matrix_r5, ano = 2018, modo = "BICYCLE")


walk(c("for", "bho", "poa", "cur", "rio", "spo"),
     gerar_tt_matrix_r5, ano = 2018, modo = c("WALK", "TRANSIT"))






walk(munis_df_2019$abrev_muni,
     gerar_tt_matrix_r5, ano = 2019, modo = "WALK")


walk(munis_df_2019$abrev_muni,
     gerar_tt_matrix_r5, ano = 2019, modo = "BICYCLE")


walk(c("for", "bho", "poa", "cur", "rio", "spo", "rec"),
     gerar_tt_matrix_r5, ano = 2019, modo = c("WALK", "TRANSIT"))


walk(c("cur", "rio", "spo"),
     gerar_tt_matrix_r5, ano = 2019, modo = c("WALK", "TRANSIT"))


# calculate median ttmatrix -------------------------------------------------------------------

# 1) Juntar output ttmatrix e agrupar as matrizes --------------------------
# As matrizes sao tratadas, agregadas nos periodos pico e fora-pico fazendo a mediana do tempo de viagme
# Em seguida sao juntos todos os modos

# sigla_muni <- 'for'; ano <- 2017
# sigla_muni <- 'rio'; ano <- 2017
# sigla_muni <- 'rio'; ano <- 2018
# sigla_muni <- 'rio-novo'; ano <- 2019

gerar_ttmatrix_mediana_muni <- function(sigla_muni, ano) {
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano)
  
  
  # Listar arquivos de matriz em formato .csv
  tt_files <- dir(path= sprintf("E:/data/output_ttmatrix/%s/r5", ano), 
                  pattern = paste0(sigla_muni, "_", "pt"), full.names = T)
  
  # Ler e empilhar ttmatrix
  # future::plan(future::multiprocess)
  # ttmatrix_allmodes <- future.apply::future_lapply(X =tt_files, FUN=fread, future.packages=c('data.table')) %>% 
  #   data.table::rbindlist(fill = T)
  # ttmatrix_allmodes <- lapply(X=tt_files, FUN= readr::read_rds) %>% data.table::rbindlist(fill = T)
  ttmatrix_allmodes <- fread(tt_files)
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem como:
  # transit / walk: 350s equivale ao tempo necessario para cruzar um hexagono a bicicleta (~1 metro/sec = ~3.6 km/h)
  # bike: 110s equivale ao tempo necessario para cruzar um hexagono a de pe (~3.3 metros/sec = ~12 km/h)
  ttmatrix_allmodes[, travel_time := as.numeric(travel_time)]
  ttmatrix_allmodes[mode=='bike', travel_time := fifelse(origin == destination, 1.83, travel_time)]
  ttmatrix_allmodes[mode == "walk", travel_time := fifelse(origin == destination, 5.83, travel_time)]
  ttmatrix_allmodes[mode == "transit", travel_time := fifelse(origin == destination, 5.83, travel_time)]
  
  # # convert depart_time para formato itime
  # ttmatrix_allmodes[, departure_time := as.ITime(departure_time)]
  # 
  # # Classificar informacao de horario de partida como pico ou fora pico
  # ttmatrix_allmodes[, pico := fifelse(mode %in% c("bike", "walk"), 1,
  #                                     fifelse( depart_time %between% c(as.ITime("06:0:00"), as.ITime("08:00:00")),1,0))]
  
  
  
  # Calcular a mediana do tempo de viagem entre cada par OD para pico e fora pico
  
  # Calcular a mediana agrupando por sigla_muni, modo, origin, destination, pico
  ttmatrix_median <- ttmatrix_allmodes[, .(tt_median = median(travel_time, na.rm = TRUE)), 
                                       by = .(city, mode, origin, destination, pico)]
  
  # Transformar o traveltime para minutos
  # ttmatrix_median[, tt_median := tt_median/60]
  
  # salvar
  path_out <- sprintf("E:/data/ttmatrix_agregada/%s/r5/ttmatrix_agregada_%s_%s_r5.rds", ano, sigla_muni, ano)
  
  write_rds(ttmatrix_median, path_out)
  
}


# aplicar funcao ------------------------------------------------------------------------------

gerar_ttmatrix_mediana_muni("for", 2018)
gerar_ttmatrix_mediana_muni("bho", 2018)
gerar_ttmatrix_mediana_muni("cur", 2018)
gerar_ttmatrix_mediana_muni("rio", 2018)
gerar_ttmatrix_mediana_muni("spo", 2018)

gerar_ttmatrix_mediana_muni("rio-novo", 2019)


walk(c("for", "spo", "bho", "rio", "cur", "poa"), gerar_ttmatrix_mediana_muni, ano = 2017)
walk(munis_df_2019$abrev_muni, gerar_ttmatrix_mediana_muni, ano = 2018)
walk(c("for", "spo", "bho", "rio", "cur", "poa"), gerar_ttmatrix_mediana_muni, ano = 2019)
