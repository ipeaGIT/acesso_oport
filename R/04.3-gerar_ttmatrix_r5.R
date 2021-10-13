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
# sigla_munii <- 'for'; ano <- 2017
# sigla_munii <- 'spo'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'spo'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio'; ano <- 2018; modo <- c("WALK", "TRANSIT")
# sigla_munii <- 'rio-novo'; ano <- 2019; modo <- c("WALK", "TRANSIT")


calculate_ttmatrix <- function(sigla_munii, ano) {
  
  
  
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
  
  max_walk_dist <- 1000   # meters
  max_trip_duration <- 180 # minutes
  max_trip_duration_walk <- 60 # minutes
  max_trip_duration_bike <- 90 # minutes
  departure_pico <- paste0(date, " 06:00:00")
  departure_fpico <- paste0(date, " 14:00:00")
  departure_datetime_pico <- as.POSIXct(departure_pico, format = "%Y-%m-%d %H:%M:%S")
  departure_datetime_fpico <- as.POSIXct(departure_fpico, format = "%Y-%m-%d %H:%M:%S")
  
  
  # 3.1) calculate a travel time matrix
  ttm_pico <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 destinations = points,
                                 mode = c("WALK", "TRANSIT"),
                                 departure_datetime = departure_datetime_pico,
                                 time_window = 120,
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration)
  
  ttm_pico[, mode := "transit"]
  ttm_pico[, pico := 1]
  
  ttm_fpico <- travel_time_matrix(r5r_core = setup,
                                  origins = points,
                                  destinations = points,
                                  mode = c("WALK", "TRANSIT"),
                                  departure_datetime = departure_datetime_fpico,
                                  time_window = 120,
                                  max_walk_dist = max_walk_dist,
                                  max_trip_duration = max_trip_duration)
  
  ttm_fpico[, mode := "transit"]
  ttm_fpico[, pico := 0]
  
  ttm_walk <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 destinations = points,
                                 mode = "WALK",
                                 departure_datetime = departure_datetime_pico,
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration_walk)
  
  ttm_walk[, mode := "walk"]
  ttm_walk[, pico := 1]
  
  ttm_bike <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 departure_datetime = departure_datetime_pico,
                                 destinations = points,
                                 mode = "BICYCLE",
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration_bike)
  
  ttm_bike[, mode := "bike"]
  ttm_bike[, pico := 1]
  
  # juntar matrizes
  ttm <- rbind(ttm_pico, ttm_fpico, ttm_walk, ttm_bike)
  
  # rename columns
  ttm <- ttm %>% rename(origin = fromId, destination = toId) %>% setDT()
  
  # identify columns
  ttm[, city := sigla_munii]
  ttm[, ano := ano]
  
  table(ttm$mode, useNA = 'always')
  table(ttm$pico, useNA = 'always')
  
  # save ttmatrix/
  fwrite(ttm, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5.csv", 
                         ano, 
                         ano,
                         sigla_munii))
  
  
}





# apply function
walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni,
     gerar_tt_matrix_r5, ano = 2017)



walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni,
     gerar_tt_matrix_r5, ano = 2018)



walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni,
     gerar_tt_matrix_r5, ano = 2019)



