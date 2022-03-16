# carregar bibliotecas
source('./R/fun/setup.R')


# sao paulo: velocidades -----------------------------------


# file <- "../../data-raw/gtfs/spo/2019/gtfs_spo_sptrans_2019-10.zip"; ano <- 2019

# file <- "../../data-raw/gtfs/spo/2018/gtfs_spo_sptrans_2018-11-15.zip"; ano <- 2018

# file <- "../../data-raw/gtfs/spo/2017/gtfs_spo_sptrans_2017-10-16.zip"; ano <- 2017

corrigir_subway_spo <- function(file, ano) {
  
  
  # abrir gtfs
  gtfs <- gtfstools::read_gtfs(file, encoding = "UTF-8")
  
  # extrair linhas de metro
  linhas_metro <- gtfs$routes[route_id %ilike% "met|cptm"]
  
  # extrair as linhas a terem velocidade modificada: linhas 7 e 8 (CPTM) e 5 e 15 (metro)
  linhas_metro_only <- linhas_metro[route_id %like% "L07|L08|L5|15"]
  
  
  
  
  # corrigir as linhas da CPTM: l07 e l08 -------------------------------
  
  # esses valores de tempo de viagem foram baseado em dados da CPTM fornecidos por email
  # no caso, tempos acumulados a partir da primeira parada
  tempos_l07 <- data.table(stop_sequence = 1:18,
                           trip_id = "CPTM L07-0",
                           tempo = as.numeric(lubridate::hms(c( "00:04:00",
                                                                "00:10:30",
                                                                "00:14:00",
                                                                "00:16:30",
                                                                "00:19:30",
                                                                "00:22:30",
                                                                "00:27:30",
                                                                "00:30:30",
                                                                "00:34:00",
                                                                "00:39:00",
                                                                "00:44:00",
                                                                "00:49:30",
                                                                "00:53:00",
                                                                "00:57:00",
                                                                "00:67:00",
                                                                "00:71:30",
                                                                "00:78:00",
                                                                "00:85:00"))))
  # inverter esses tempos pra calcular a volta
  tempos_l07 <- rbind(tempos_l07, data.table(stop_sequence = 1:18, 
                                             trip_id = "CPTM L07-1",
                                             tempo = rev(tempos_l07$tempo)))
  # calcular o tempo de viagem entre cada estacao tanto para ida como para volta
  tempos_l07[trip_id == "CPTM L07-0", dif := tempo - lag(tempo, default = 240)]
  tempos_l07[trip_id == "CPTM L07-1", dif := -(tempo - lag(tempo, default = 5100))]
  
  # esses valores foram baseado em dados da CPTM fornecidos por email
  tempos_l08 <- data.table(stop_sequence = 1:22,
                           trip_id = "CPTM L08-0",
                           tempo = as.numeric(lubridate::hms(c("00:00:00",
                                                               "00:04:00",
                                                               "00:07:00",
                                                               "00:10:00",
                                                               "00:13:00",
                                                               "00:16:00",
                                                               "00:19:00",
                                                               "00:22:00",
                                                               "00:25:00",
                                                               "00:27:00",
                                                               "00:30:00",
                                                               "00:32:00",
                                                               "00:35:00",
                                                               "00:38:00",
                                                               "00:40:00",
                                                               "00:42:00",
                                                               "00:45:00",
                                                               "00:48:00",
                                                               "00:50:00",
                                                               "00:53:00",
                                                               "00:57:30",
                                                               "00:65:00"))) )
  
  # inverter esses tempos pra calcular a volta
  tempos_l08 <- rbind(tempos_l08, data.table(stop_sequence = 1:22, 
                                             trip_id = "CPTM L08-1",
                                             tempo = rev(tempos_l08$tempo)))
  # calcular o tempo de viagem entre cada estacao tanto para ida como para volta
  tempos_l08[trip_id == "CPTM L08-0", dif := tempo - lag(tempo, default = 0)]
  tempos_l08[trip_id == "CPTM L08-1", dif := -(tempo - lag(tempo, default = 3900))]
  
  # juntar os tempos
  tempos <- rbind(tempos_l07, tempos_l08)
  
  # extrair trips dessas linhas
  stop_times_cptm <- gtfs$stop_times[trip_id %in% gtfs$trips[trip_id %ilike% "CPTM L07|CPTM L08"]$trip_id]
  
  # trazer os valores que foram preenchidos a mao anteriormente
  stop_times_cptm_corrigido <- merge(x = stop_times_cptm, 
                                     y = tempos[, .(stop_sequence, trip_id, dif)],
                                     by = c("trip_id", "stop_sequence"),
                                     sort = FALSE
  )
  # transformar para ITIME
  stop_times_cptm_corrigido[, arrival_time := as.ITime(arrival_time)]
  stop_times_cptm_corrigido[, dif := as.ITime(dif)]
  # manter o primeiro arrival time, e substiruir os demais pelo tempo entre paradas que foi inputado
  stop_times_cptm_corrigido[, arrival_time := fifelse(row.names(.SD) == 1, arrival_time, dif), by = trip_id]
  # fazer soma cumulativa a partir dos horarios
  stop_times_cptm_corrigido[, arrival_time := cumsum(arrival_time), by = trip_id]
  # transformar para character
  stop_times_cptm_corrigido[, arrival_time := as.character(arrival_time)]
  # departure vai ser igual ao arrival
  stop_times_cptm_corrigido[, departure_time := arrival_time]
  # selecionar e organizar colunas
  stop_times_cptm_corrigido <- stop_times_cptm_corrigido[, .(trip_id, arrival_time, departure_time, stop_id, stop_sequence)]
  
  # input para um novo stop_times
  gtfs_corrigido <- gtfs
  gtfs_corrigido$stop_times[stop_times_cptm_corrigido, on = c("trip_id", "stop_sequence"), 
                            c("arrival_time", "departure_time") :=
                              list(i.arrival_time, i.departure_time)]
  
  
  
  
  # corrigir as linhas do metro: l05 e l15 -------------------------------
  
  # selecionar viagens a serem corrigidas
  trips_metro <- gtfs$trips[trip_id %ilike% "L5|METRÔ 15"]$trip_id
  # input new speeds
  gtfs_corrigido <- gtfstools::set_trip_speed(gtfs_corrigido, trip_id = trips_metro, speed = 37)
  
  
  # save it 
  gtfstools::write_gtfs(gtfs_corrigido, 
                        path = sprintf("%s_fixed_subway.zip", str_replace(file, pattern = ".zip", replacement = ""))
  )
  
  
}


corrigir_subway_spo(file = "../../data-raw/gtfs/spo/2017/gtfs_spo_sptrans_2017-10-16.zip",
                    ano = 2017)

corrigir_subway_spo(file = "../../data-raw/gtfs/spo/2018/gtfs_spo_sptrans_2018-11-15.zip",
                    ano = 2018)

corrigir_subway_spo(file = "../../data-raw/gtfs/spo/2019/gtfs_spo_sptrans_2019-10.zip",
                    ano = 2019)











# sao paulo: frequencias -----------------------------------
update_frequency_spo_cptm <- function(gtfs_path) {
  
  
  # identificar ano
  ano <- str_extract(gtfs_path, pattern = "\\d{4}")
  
  gtfs <- read_gtfs(gtfs_path)
  
  # extract frequency from cptm lines
  routes_cptm <- gtfs$routes[route_id %like% "CPTM"]
  trips_cptm <- gtfs$trips[route_id %like% "CPTM"]
  
  
  frequencies_ctpm <- gtfs$frequencies[trip_id %in% trips_cptm$trip_id]
  # get only morning peak
  frequencies_cptm_peak <- frequencies_ctpm[start_time %in% c("06:00:00", "07:00:00", "08:00:00")]
  # frequencies_ctpm_peak[, ano := ano]
  
  # gtfs$shapes %>% filter(shape_id == "17854") %>% to_spatial(c("shape_pt_lon", "shape_pt_lat")) %>% mapview()
  
  
  # set frequencies
  frequencies_cptm_peak[, headway_secs := fcase(trip_id == "CPTM L07-0" & ano %in% c(2017, 2018), 660,
                                                trip_id == "CPTM L07-0" & ano %in% c(2019), 802,
                                                trip_id == "CPTM L07-1", 360,
                                                trip_id == "CPTM L08-0", 1800,
                                                trip_id == "CPTM L08-1", 300,
                                                trip_id == "CPTM L09-0", 240,
                                                trip_id == "CPTM L09-1", 240,
                                                trip_id == "CPTM L10-0", 300,
                                                trip_id == "CPTM L10-1", 300,
                                                trip_id == "CPTM L11-0", 240,
                                                trip_id == "CPTM L11-1", 240,
                                                trip_id == "CPTM L12-0", 360,
                                                trip_id == "CPTM L12-1", 360,
                                                trip_id == "CPTM L13-0", 1200,
                                                trip_id == "CPTM L13-1", 1200
  )]
  
  # join
  gtfs$frequencies[frequencies_cptm_peak, on = c("trip_id", "start_time", "end_time"),
                   c("headway_secs") := i.headway_secs]
  
  
  # save
  # create new file
  gtfs_path_exit <- basename(gtfs_path)
  gtfs_path_exit <- str_replace(gtfs_path_exit, ".zip$", replacement = "")
  gtfs_path_exit <- sprintf("../../data-raw/gtfs/spo/%s/%s_freqs.zip", ano, gtfs_path_exit)
  
  # save
  write_gtfs(gtfs, path = gtfs_path_exit)
  
}




update_frequency_spo_cptm("../../data-raw/gtfs/spo/2017/gtfs_spo_sptrans_2017-10-16_fixed_subway.zip")
update_frequency_spo_cptm("../../data-raw/gtfs/spo/2018/gtfs_spo_sptrans_2018-11-15_fixed_subway.zip")
update_frequency_spo_cptm("../../data-raw/gtfs/spo/2019/gtfs_spo_sptrans_2019-10_fixed_subway.zip")









