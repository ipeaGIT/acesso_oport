
library(sf)
library(ggmap)
library(ggplot2)

fun{}
# read shape
temp_sf <- geobr::read_municipality(code_muni = 3304557)

# get bounding box
bbbox <- st_bbox(rio)

centroid <- st_centroid(rio)

if(muni_abbrev == 'SP'){ my_zoom =xx}


# get ggmap tile
map <- get_googlemap(center = c(lon = -43.4, lat = -22.9), zoom = my_zoom,  scale = 2,
                     style = c('feature:administrative.locality|element:labels|visibility:off',
                               'feature:administrative.neighborhood|element:labels|visibility:off'))

# map <- get_googlemap(center = c(lon = -43.4, lat = -22.9), zoom = 10,  scale = 2,
#                      style = c('feature:administrative.locality|element:labels|visibility:off',
#                                'feature:administrative.neighborhood|element:labels|visibility:off'))



# save tile
saveRDS(map, 'test_tile_rio.rds')

}







ggplot() +
ggmap(map2) + 
  
  geom_sf(data=rio, color='red')



rio2 <- c(left = -43.79653853, bottom = -23.07665345, right = -43.15022410, top = -22.77554842)

x <- get_stamenmap( rio2, zoom = 10,  scale = 2, maptype = "terrain")
ggmap(map2)



x <- get_stamenmap( bbox = c(left = 110, bottom = -40, right = 160, top = -10), zoom = 4, maptype = "watercolor")
ggmap(map) 
  
  



ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}



# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
rio <- st_transform(rio, 3857)


# Use the function:
map <- ggmap_bbox(map)

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = rio, fill = 'red', inherit.aes = FALSE, alpha=.2)

my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)


