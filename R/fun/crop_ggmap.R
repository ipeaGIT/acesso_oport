ps_ggmap_to_raster <- function(x){
  if(!inherits(x, "ggmap")) ps_error("x must be a ggmap object (e.g. from ps_bbox_ggmap)")
  
  map_bbox <- attr(x, 'bb')
  .extent <- raster::extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster::raster(.extent, nrow= nrow(x), ncol = ncol(x))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(x))), c('red','green','blue'))
  red <- my_map
  raster::values(red) <- rgb_cols[['red']]
  green <- my_map
  raster::values(green) <- rgb_cols[['green']]
  blue <- my_map
  raster::values(blue) <- rgb_cols[['blue']]
  ras <- raster::stack(red,green,blue)
  ras
  
  crs(ras) <- CRS('+init=EPSG:3857')
  
  ras
  
}



# funcao de bounding box para ggmap
ggmap_bbox <- function(map, muni_limits) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # buffer muni
  muni_buffer <- st_buffer(muni_limits, dist = 0.003)
  bbox_muni <- st_bbox(st_transform(st_as_sfc(st_bbox(muni_buffer, crs = 4326)), 3857))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  
  # converter ggmap para raster
  map <- ps_ggmap_to_raster(map)
  
  # Cria bounding box do municipio para fazer crop do raster
  bb1 <- c(bbox_muni[1], bbox_muni[3], bbox_muni[2], bbox_muni[4])
  
  # Converte para spatial polygon para fazer o crop
  e <- as(extent(bb1), 'SpatialPolygons')
  
  
  # crop do municipio com raster de topografia
  elev_img_fim <- raster::crop(map, e)
  
  # # transformar de volta para 4326
  # elev_img_fim <- projectRaster(elev_img_fim, crs = CRS('+init=EPSG:4326'))
  
  # para rgb
  test_spdf <- as(elev_img_fim, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  names(test_df) <- c("red", "green", "blue", "x", "y")
  test_df$hex <- rgb(test_df$red, test_df$green, test_df$blue, maxColorValue = 255)
  
  
  test_df
  
}

