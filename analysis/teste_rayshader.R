library(raster)
library(rayshader)
library(osmplotr)

elev_img <- raster("../data/elevation/elevation_for.tif")

bb = c(-38.63656796,-3.88812428,-38.40154132,-3.69197903)

bbox <- get_bbox (bb)

dat_B <- extract_osm_objects (key = 'building', bbox = bbox)
dat_H <- extract_osm_objects (key = 'highway', bbox = bbox)
dat_P <- extract_osm_objects (key = 'park', bbox = bbox)
dat_G <- extract_osm_objects (key = 'landuse', value = 'grass', bbox = bbox)

map <- osm_basemap (bbox = bbox, bg = 'gray95')
map <- add_osm_objects (map, dat_B, col = 'gray40')
map <- add_osm_objects (map, dat_H, col = 'gray80')
map <- add_osm_objects (map, dat_P, col = 'darkseagreen')
map <- add_osm_objects (map, dat_G, col = 'darkseagreen1')
print_osm_map (map)
print_osm_map (map, file = "map_for.png", width = 846, height = 706, units = "px")

overlay_img <- png::readPNG("map_for.png")

# agora vai
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30)
watermap <- detect_water(elev_matrix)

# plot 2D
elev_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  plot_map()

# plot 3d
elev_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(watermap, color="desert") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat,0.5) %>%
  plot_3d(elev_matrix,zscale=30,fov=0,theta=45,zoom=0.75,phi=45, windowsize = c(1000,800))

render_snapshot()
