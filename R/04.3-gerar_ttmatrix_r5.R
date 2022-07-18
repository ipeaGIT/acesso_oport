# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Calcula matriz de tempo de viagem com o R5


# carregar bibliotecas
# options(r5r.montecarlo_draws = 0L)
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
# sigla_munii <- "bho"; ano <- 2017
# sigla_munii <- "spo"; ano <- 2019
# sigla_munii <- "spo"; ano <- 2018
# sigla_munii <- "rio"; ano <- 2018
# sigla_munii <- "man"; ano <- 2017

calculate_ttmatrix <- function(sigla_munii, ano, break_ttmatrix = FALSE) {
  
  # identify transport modes
  modes <- munis_list$munis_modo[abrev_muni == sigla_munii & ano_modo == ano]$modo
  
  # choose folder accordindgly
  folder_fine <- sprintf("../../data/acesso_oport/r5/network/%s/%s", ano, sigla_munii)
  # folder_fine <- normalizePath(folder_fine)
  
  # r5 setup
  setup <- setup_r5(data_path = folder_fine, overwrite = FALSE, verbose = FALSE)
  
  
  
  # points
  points <- fread(sprintf("../../data/acesso_oport/r5/points/%s/points_%s_09_%s.csv", 
                          ano, substr(sigla_munii, 1, 3), ano))
  colnames(points) <- c("id", "lon", "lat")
  
  if (modes == "todos") {
    
    
    # select date
    if (sigla_munii == "bho" & ano == 2019) {
      
      
      date <- "2019-09-20"
      
      
      
    } else {
      
      date <- selecionar_data_gtfs(sigla_munii, ano)
      
    }
    
    
    df <- setup$getTransitServicesByDate(as.character(date))
    df <- jdx::convertToR(df$getDataFrame())
    
    max_walk_dist <- 30   # minutes
    max_trip_duration <- 180 # minutes
    departure_pico <- paste0(date, " 06:00:00")
    departure_fpico <- paste0(date, " 14:00:00")
    departure_datetime_pico <- as.POSIXct(departure_pico, format = "%Y-%m-%d %H:%M:%S")
    departure_datetime_fpico <- as.POSIXct(departure_fpico, format = "%Y-%m-%d %H:%M:%S")
    
    
    
    
    
    # 3.1) calculate a travel time matrix
    message("Running transit peak matrix...")
    a <- Sys.time()
    ttm_pico <- travel_time_matrix(r5r_core = setup,
                                   origins = points,
                                   destinations = points,
                                   mode = c("WALK", "TRANSIT"),
                                   departure_datetime = departure_datetime_pico,
                                   time_window = 120,
                                   max_walk_time = max_walk_dist,
                                   max_trip_duration = max_trip_duration,
                                   n_threads = 39,
                                   draws_per_minute = 1)
    time_ttmatrix_tp_pico <- Sys.time() - a
    print(time_ttmatrix_tp_pico)
    
    ttm_pico[, mode := "transit"]
    ttm_pico[, pico := 1]
    
    # rename columns
    ttm_pico <- ttm_pico %>% rename(origin = from_id, destination = to_id) %>% setDT()
    
    # identify columns
    ttm_pico[, city := sigla_munii]
    ttm_pico[, ano := ano]
    
    if (break_ttmatrix) {
      
      fwrite(ttm_pico, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5_pico.csv",
                               ano,
                               ano,
                               sigla_munii))
      rm(ttm_pico)
      
    }
    
    message("Running transit off-peak matrix...")
    a <- Sys.time()
    ttm_fpico <- travel_time_matrix(r5r_core = setup,
                                    origins = points,
                                    destinations = points,
                                    mode = c("WALK", "TRANSIT"),
                                    departure_datetime = departure_datetime_fpico,
                                    time_window = 120,
                                    max_walk_time = max_walk_dist,
                                    max_trip_duration = max_trip_duration,
                                    n_threads = 39,
                                    draws_per_minute = 1)
    time_ttmatrix_tp_fpico <- Sys.time() - a
    print(time_ttmatrix_tp_fpico)
    
    ttm_fpico[, mode := "transit"]
    ttm_fpico[, pico := 0]
    
    # rename columns
    ttm_fpico <- ttm_fpico %>% rename(origin = from_id, destination = to_id) %>% setDT()
    
    # identify columns
    ttm_fpico[, city := sigla_munii]
    ttm_fpico[, ano := ano]
    
    if (break_ttmatrix) {
      
      
      
      fwrite(ttm_fpico, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5_fpico.csv",
                                ano,
                                ano,
                                sigla_munii))
      
      rm(ttm_fpico)
      
    }
    
  }
  
  max_trip_duration_walk <- 60 # minutes
  max_trip_duration_bike <- 90 # minutes
  
  a <- Sys.time()
  ttm_walk <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 destinations = points,
                                 mode = "WALK",
                                 max_trip_duration = max_trip_duration_walk,
                                 n_threads = 39)
  time_ttmatrix_walk <- Sys.time() - a
  
  ttm_walk[, mode := "walk"]
  ttm_walk[, pico := 1]
  
  # rename columns
  ttm_walk <- ttm_walk %>% rename(origin = from_id, destination = to_id) %>% setDT()
  
  # identify columns
  ttm_walk[, city := sigla_munii]
  ttm_walk[, ano := ano]
  
  if (break_ttmatrix) {
    
    fwrite(ttm_walk, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5_walk.csv",
                             ano,
                             ano,
                             sigla_munii))
    rm(ttm_walk)
    
  }
  
  
  a <- Sys.time()
  ttm_bike <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 destinations = points,
                                 mode = "BICYCLE",
                                 max_trip_duration = max_trip_duration_bike,
                                 n_threads = 39)
  time_ttmatrix_bike <- Sys.time() - a
  
  ttm_bike[, mode := "bike"]
  ttm_bike[, pico := 1]
  
  
  # rename columns
  ttm_bike <- ttm_bike %>% rename(origin = from_id, destination = to_id) %>% setDT()
  
  # identify columns
  ttm_bike[, city := sigla_munii]
  ttm_bike[, ano := ano]
  
  
  if (break_ttmatrix) {
    
    fwrite(ttm_bike, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5_bike.csv",
                             ano,
                             ano,
                             sigla_munii))
    rm(ttm_bike)
    
  }
  
  # formatar os tempos de processamento e juntar matrizes -----------
  if (modes == "todos") {
    
    data.frame(sigla_muni = sigla_munii,
               ano = ano,
               time_ttmatrix_tp_pico =  as.numeric(time_ttmatrix_tp_pico, "mins"),
               time_ttmatrix_tp_fpico = as.numeric(time_ttmatrix_tp_fpico, "mins"),
               time_ttmatrix_walk = as.numeric(time_ttmatrix_walk, "secs"),
               time_ttmatrix_bike = as.numeric(time_ttmatrix_bike, "secs")) %>%
      fwrite(sprintf("E:/data/output_ttmatrix/%s/r5/process_time/proces_ttmatrix_%s_%s.csv",
                     ano, ano, sigla_munii))
    
    
    if (break_ttmatrix == FALSE)
      
    {
      
      
      # juntar matrizes
      ttm <- rbind(ttm_pico, ttm_fpico, ttm_walk, ttm_bike)
      # ttm <- rbind(ttm_pico)
      
      rm(ttm_pico)
      rm(ttm_fpico)
      rm(ttm_walk)
      rm(ttm_bike)
      
    }
    
    
  } else {
    
    data.frame(sigla_muni = sigla_munii,
               ano = ano,
               time_ttmatrix_walk = time_ttmatrix_walk,
               time_ttmatrix_bike = time_ttmatrix_bike) %>%
      fwrite(sprintf("E:/data/output_ttmatrix/%s/r5/process_time/proces_ttmatrix_%s_%s.csv",
                     ano, ano, sigla_munii))
    
    if (break_ttmatrix == FALSE) {
      
      
      # juntar matrizes
      ttm <- rbind(ttm_walk, ttm_bike)
      
      rm(ttm_walk)
      rm(ttm_bike)
    }
    
    
  }
  
  if (break_ttmatrix == FALSE) {
    
    
    # save ttmatrix/
    # colocar em rds com compactacao
    fwrite(ttm, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5.csv",
                        ano,
                        ano,
                        sigla_munii))
  }
  
  # rm(ttm)
  r5r::stop_r5()
  
}





# apply function ----------------
# walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni,
#      calculate_ttmatrix, ano = 2017)

calculate_ttmatrix("spo", 2017, break_ttmatrix = TRUE)
calculate_ttmatrix("rio", 2017)
calculate_ttmatrix("for", 2017) # ok
calculate_ttmatrix("cur", 2017) # ok
calculate_ttmatrix("poa", 2017) # ok
calculate_ttmatrix("bho", 2017) # ok
calculate_ttmatrix("sal", 2017)
calculate_ttmatrix("rec", 2017)
calculate_ttmatrix("goi", 2017)
calculate_ttmatrix("bel", 2017)
calculate_ttmatrix("gua", 2017)
calculate_ttmatrix("cam", 2017)
calculate_ttmatrix("slz", 2017)
calculate_ttmatrix("sgo", 2017)
calculate_ttmatrix("mac", 2017)
calculate_ttmatrix("duq", 2017)
calculate_ttmatrix("nat", 2017)
calculate_ttmatrix("cgr", 2017)
calculate_ttmatrix("man", 2017)
calculate_ttmatrix("bsb", 2017)
# 
# # walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni,
# #      calculate_ttmatrix, ano = 2018)
# # 
calculate_ttmatrix("spo", 2018, break_ttmatrix = TRUE)
calculate_ttmatrix("rio", 2018)
calculate_ttmatrix("for", 2018)
calculate_ttmatrix("cur", 2018)
calculate_ttmatrix("poa", 2018)
calculate_ttmatrix("bho", 2018)
calculate_ttmatrix("bsb", 2018)
calculate_ttmatrix("sal", 2018)
calculate_ttmatrix("man", 2018)
calculate_ttmatrix("rec", 2018)
calculate_ttmatrix("goi", 2018)
calculate_ttmatrix("bel", 2018)
calculate_ttmatrix("gua", 2018)
calculate_ttmatrix("cam", 2018)
calculate_ttmatrix("slz", 2018)
calculate_ttmatrix("sgo", 2018)
calculate_ttmatrix("mac", 2018)
calculate_ttmatrix("duq", 2018)
calculate_ttmatrix("cgr", 2018)
calculate_ttmatrix("nat", 2018)
# # 
# # 
# # 
# # walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni,
# #      calculate_ttmatrix, ano = 2019)
# # 
# # 
calculate_ttmatrix("spo", 2019, break_ttmatrix = TRUE)
calculate_ttmatrix("rio", 2019)
calculate_ttmatrix("goi", 2019)
calculate_ttmatrix("for", 2019)
calculate_ttmatrix("cur", 2019)
calculate_ttmatrix("poa", 2019)
calculate_ttmatrix("bho", 2019)
calculate_ttmatrix("bsb", 2019)
calculate_ttmatrix("sal", 2019)
calculate_ttmatrix("man", 2019)
calculate_ttmatrix("rec", 2019)
calculate_ttmatrix("bel", 2019)
calculate_ttmatrix("gua", 2019)
calculate_ttmatrix("cam", 2019)
calculate_ttmatrix("slz", 2019)
calculate_ttmatrix("sgo", 2019)
calculate_ttmatrix("mac", 2019)
calculate_ttmatrix("duq", 2019)
calculate_ttmatrix("cgr", 2019)
calculate_ttmatrix("nat", 2019)






# juntar ttmatrix somente para sao paulo (2017, 2018, 2019 ----------------


fun

ttm <- lapply(c("E:/data/output_ttmatrix/2018/r5/ttmatrix_2018_spo_r5_pico.csv",
                "E:/data/output_ttmatrix/2018/r5/ttmatrix_2018_spo_r5_fpico.csv",
                "E:/data/output_ttmatrix/2018/r5/ttmatrix_2018_spo_r5_walk.csv",
                "E:/data/output_ttmatrix/2018/r5/ttmatrix_2018_spo_r5_bike.csv"),
              fread) %>% rbindlist()

# save ttmatrix/
# colocar em csv
fwrite(ttm, sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5.csv",
                    2018,
                    2018,
                    'spo'))
