
# fix gtfs info for some cities
library(gtfstools)

gtfs_path <- "../../data-raw/gtfs/spo/2017/gtfs_spo_sptrans_2017-10-16_fixed_subway.zip"
gtfs_path <- "../../data-raw/gtfs/spo/2019/gtfs_spo_sptrans_2019-10_fixed_subway.zip"
gtfs_path <- "../../data-raw/gtfs/spo/2019/gtfs_spo_sptrans_2019-10_fixed_subway.zip"

# sao paulo:
update_frequency_spo_cptm <- function(gtfs_path) {
  
  
  # identificar ano
  ano <- str_extract(gtfs_path, pattern = "\\d{4}")
  
  gtfs <- read_gtfs(gtfs_path)
  
  # extract frequency from cptm lines
  routes_cptm <- gtfs$routes[route_id %like% "CPTM"]
  trips_cptm <- gtfs$trips[route_id %like% "CPTM"]
  
  
  frequencies_cptm <- gtfs$frequencies[trip_id %in% trips_cptm$trip_id]
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
  gtfs$frequencies <- gtfs$frequencies[frequencies_cptm_peak, on = c("trip_id", "start_time", "end_time"),
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
