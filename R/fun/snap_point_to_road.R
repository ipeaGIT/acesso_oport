##### function to Snap points to closest road segment

snap_sf <- function(i, points_to_correct, streets_buffer){ 
  
  # # points_to_correct <- copy(points_got)
  # # street_points <- copy(street_points_got)
  # # streets_buffer <- streets_buffer_got
  # # cut_dist = 450 # meters
  # # i = 16
  # 
  # select a point
  temp_point <- subset(points_to_correct, id_hex == i)
  # 
  # # subset street buffer around that point
  temp_buffer <- subset(streets_buffer, id_hex == i)
  # 
  # # make sure streets are walkable and driveable
  # table(temp_buffer$type)
  # table(temp_buffer$access)
  # table(temp_buffer$service)
  # table(temp_buffer$class)
  # 
  # # pedestrian and bike restrictions
  # # http://docs.opentripplanner.org/en/latest/Troubleshooting-Routing/
  # 
  # p_restrictions <- c("trunk","trunk_link","motorway","motorway_link","construction")
  # # car restrictions
  # c_restrictions <- c("cycleway","footway","sidewalk","platform", "rail", "tram")
  # 
  # temp_buffer <- subset(temp_buffer, !(class %in% "railway"))           # remove railways
  # temp_buffer <- subset(temp_buffer, !(access %in% c("no"))) # "private" ??? # remove segments with restricted car access
  # temp_buffer <- subset(temp_buffer, !(type %in% c(p_restrictions, c_restrictions))) # remove car, bike and pedestrian restrictions
  
  
  # if there are no streets within the buffer, return original point
  if( nrow(temp_buffer) ==0 ){ return(temp_point) 
  } else {
    
    # find nearest street-point
    nrst <- st_nearest_points(temp_point, temp_buffer) # find closest distance to all streets in buffer
    nrst <- nrst[which.min(st_length(nrst))] # keep the closest of all
    nearest_snap = st_cast(nrst, "POINT")[2] # get second point of that line distance and convert it to a point
    nearest_snap <-  st_sf( data.frame( id_hex =  i, geom=nearest_snap)) # convert to sf object
    return(nearest_snap)
  }
}



# # Make function work in parallel
# 
# # Calculate the number of cores
# no_cores <- parallel::detectCores() - 1
# 
# 
# 
# 
# #  Initiate cluster 
# cl <- makeCluster(no_cores)
# clusterEvalQ(cl, {library(data.table); library(sf); library(dplyr)})
# clusterExport(cl=cl, c('points_got', 'streets_buffer_got', 'snap_sf'), envir=environment())
# 
# 
# system.time(
#   correct_got <- parallel::parLapply(cl=cl, 
#                                      X=  points_got$idhex, #list(16,   37,   68,   69,   70, 2366, 2370),  
#                                      points_to_correct= points_got, 
#                                      streets_buffer = streets_buffer_got,
#                                      fun=snap_sf
#   )  %>% data.table::rbindlist() %>% sf::st_as_sf() 
# )
# stopCluster(cl)
