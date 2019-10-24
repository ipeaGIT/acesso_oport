# carregar bibliotecas
source('./R/fun/setup.R')

# estabelecer funcoes

read_gtfs <- function(gtfszip){
  if(!file.exists(gtfszip))
    stop(paste0("File '", gtfszip, "' does not exist"))
  
  # Unzip files
  tempd <- file.path(tempdir(), "gtfsdir") # create tempr dir to save GTFS unzipped files
  unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir
  utils::unzip(zipfile = gtfszip, exdir = tempd, overwrite = TRUE) # unzip files
  unzippedfiles <- list.files(tempd) # list of unzipped files
  
  result <- list()
  
  # read files to memory
  if("agency.txt"      %in% unzippedfiles){result$agency      <- data.table::fread(paste0(tempd,"/agency.txt"),      encoding="UTF-8")}  else{stop(message("Error: File routes.txt is missing"))}
  if("routes.txt"      %in% unzippedfiles){result$routes      <- data.table::fread(paste0(tempd,"/routes.txt"),      encoding="UTF-8")}  else{stop(message("Error: File routes.txt is missing"))}
  if("stops.txt"       %in% unzippedfiles){result$stops       <- data.table::fread(paste0(tempd,"/stops.txt"),       encoding="UTF-8")}  else{stop(message("Error: File stops.txt is missing"))}
  if("stop_times.txt"  %in% unzippedfiles){result$stop_times  <- data.table::fread(paste0(tempd,"/stop_times.txt"),  encoding="UTF-8")}  else{stop(message("Error: File stop_times.txt is missing"))}
  if("shapes.txt"      %in% unzippedfiles){result$shapes      <- data.table::fread(paste0(tempd,"/shapes.txt"),      encoding="UTF-8")}  else{stop(message("Error: File shapes.txt is missing"))}
  if("trips.txt"       %in% unzippedfiles){result$trips       <- data.table::fread(paste0(tempd,"/trips.txt"),       encoding="UTF-8")}  else{stop(message("Error: File trips.txt is missing"))}
  if("calendar.txt"    %in% unzippedfiles){result$calendar    <- data.table::fread(paste0(tempd,"/calendar.txt"),    encoding="UTF-8")}  else{stop(message("Error: File calendar.txt is missing"))}
  if("frequencies.txt" %in% unzippedfiles){result$frequencies <- data.table::fread(paste0(tempd,"/frequencies.txt"), encoding="UTF-8")}
  
  mysub <- function(value) sub("^24:", "00:", value)
  
  if(!is.null(result$stop_times)){
    result$stop_times[, departure_time := data.table::as.ITime(mysub(departure_time), format = "%H:%M:%OS")]
    result$stop_times[, arrival_time := data.table::as.ITime(mysub(arrival_time), format ="%H:%M:%OS")]
  }
  
  if(!is.null(result$frequencies)){
    result$frequencies[, start_time := data.table::as.ITime(mysub(start_time), format = "%H:%M:%OS")]
    result$frequencies[, end_time := data.table::as.ITime(mysub(end_time), format = "%H:%M:%OS")]
  }
  
  return(result)
}

#' @title Convert GTFS shapes to simple feature
gtfs_shapes_as_sf <- function(gtfs, crs = 4326){
  temp_shapes <- gtfs$shapes[,
                             {
                               geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2))
                               geometry <- sf::st_sfc(geometry)
                               geometry <- sf::st_sf(geometry = geometry)
                             }
                             , by = shape_id
                             ]
  
  # add shape length
  temp_shapes[, length := sf::st_length(geometry) %>% units::set_units("km"), by = shape_id]
  
  # back to sf
  sf::st_as_sf(temp_shapes, crs = crs)  %>% sf::as_Spatial() %>% sf::st_as_sf()
}

#' @title Filter GTFS data by shape ids
filter_by_shape_id <- function(gtfs_data, shape_ids){
  gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
  gtfs_data$trips <- subset(gtfs_data$trips, shape_id %in% shape_ids)
  
  trip_ids <- unique(gtfs_data$trips$trip_id)
  
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %in% trip_ids)
  
  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  
  gtfs_data$stops <- subset(gtfs_data$stops, stop_id %in% stop_ids)
  
  route_ids <- unique(gtfs_data$trips$route_id)
  
  gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)
  
  return(gtfs_data)
}



# abrir gtfs
gtfs_novo <- read_gtfs('../data-raw/gtfs/rio/gtfs_rio_fetranspor_2019-09.zip')

# converter os shapes para sf
shapes_sf <- gtfs_shapes_as_sf(gtfs_novo)

# abrir limits do rio
rio_limits <- read_rds("../data-raw/municipios/rio/municipio_rio.rds") %>%
  st_set_crs(4326) %>%
  select(code_muni)

# fazer o join para saber quais shapes estao completamente fora do rio
shapes_sf_muni <- st_join(shapes_sf, rio_limits) %>%
  # deleter os que estao completamente fora
  filter(!is.na(code_muni))

# filtrar so esses shapes_id
gtfs_novo_filtrado <- filter_by_shape_id(gtfs_novo, shapes_sf_muni$shape_id)

# salvar

if(!is.null(gtfs_novo_filtrado$agency))      data.table::fwrite(gtfs_novo_filtrado$agency,      paste0("../data-raw/gtfs/rio", "/agency.txt"))
if(!is.null(gtfs_novo_filtrado$routes))      data.table::fwrite(gtfs_novo_filtrado$routes,      paste0("../data-raw/gtfs/rio", "/routes.txt"))
if(!is.null(gtfs_novo_filtrado$stops))       data.table::fwrite(gtfs_novo_filtrado$stops,       paste0("../data-raw/gtfs/rio", "/stops.txt"))
if(!is.null(gtfs_novo_filtrado$stop_times))  data.table::fwrite(gtfs_novo_filtrado$stop_times,  paste0("../data-raw/gtfs/rio", "/stop_times.txt"))
if(!is.null(gtfs_novo_filtrado$shapes))      data.table::fwrite(gtfs_novo_filtrado$shapes,      paste0("../data-raw/gtfs/rio", "/shapes.txt"))
if(!is.null(gtfs_novo_filtrado$trips))       data.table::fwrite(gtfs_novo_filtrado$trips,       paste0("../data-raw/gtfs/rio", "/trips.txt"))
if(!is.null(gtfs_novo_filtrado$calendar))    data.table::fwrite(gtfs_novo_filtrado$calendar,    paste0("../data-raw/gtfs/rio", "/calendar.txt"))
if(!is.null(gtfs_novo_filtrado$frequencies)) data.table::fwrite(gtfs_novo_filtrado$frequencies, paste0("../data-raw/gtfs/rio", "/frequencies.txt"))
  
# depois zipei na mao mesmo...