library(raster)
library(rayshader)
library(ggmap)
library(purrr)
library(dplyr)
library(mapview)
library(readr)

# Belo Horizonte
files <- dir("../data-raw/elevation/bel", full.names = T)
# Rio
files <- dir("../data-raw/elevation/rio", full.names = T)
# For
files <- dir("../data-raw/elevation/for", full.names = T)

# Para for
elev_img_bind <- raster::raster("../data-raw/elevation/for/s04_w039_1arc_v3.tif")
# Para demais
elev_img <- map(files, raster)
elev_img_bind <- do.call(raster::merge, elev_img)

# Get bounding box from https://boundingbox.klokantech.com/
# Aqui tambem pode ser usado osmdata::getbb()
# BB for
bb <- c(-38.63656796,-3.88812428,-38.40154132,-3.69197903)
# BB bel
bb <- c(-44.06329161,-20.0594646,-43.85721992,-19.77654377)
# BB rio
bb <- c(-43.79625205,-23.08270518,-43.09908114,-22.74608786)

bb1 <- c(bb[1], bb[3], bb[2], bb[4])

# crop fortaleza first?
e <- as(extent(bb1), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
mapview(e)
elev_img_fim <- crop(elev_img_bind, e)

# salvar
writeRaster(elev_img_fim, "../data/elevation/elevation_for.tif", format="GTiff", overwrite=TRUE)



# FUNCAO ------------------------------------------------------------------

crop_save_raster <- function(municipio, bb) {
  
  dir <- sprintf("../data-raw/elevation/%s", municipio)
  
  files <- dir(dir, full.names = T)
  
  if (length(files) == 1) {
    
    elev_img_bind <- raster::raster(files)
    
  } else {
    
    elev_img <- map(files, raster)
    elev_img_bind <- do.call(raster::merge, elev_img)
    
    
  }
  
  bb1 <- c(bb[1], bb[3], bb[2], bb[4])
  
  e <- as(extent(bb1), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  elev_img_fim <- crop(elev_img_bind, e)
  
  # salvar
  output <- sprintf("../data/elevation/elevation_%s.tif", municipio)
  
  writeRaster(elev_img_fim, output, format="GTiff", overwrite=TRUE)
  
}

crop_save_raster("for", bb = c(-38.63656796,-3.88812428,-38.40154132,-3.69197903))
crop_save_raster("bel", bb = c(-44.06329161,-20.0594646,-43.85721992,-19.77654377))
crop_save_raster("rio", bb = c(-43.79625205,-23.08270518,-43.09908114,-22.74608786))





# TESTES ------------------------------------------------------------------

elev_img_fim <- raster("../data/elevation/elevation_for.tif")

# testar
# agora vai
elev_matrix <- matrix(
  raster::extract(elev_img_fim, raster::extent(elev_img_fim), buffer = 1000), 
  nrow = ncol(elev_img_fim), ncol = nrow(elev_img_fim)
)

elev_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elev_matrix), color = "imhof4") %>%
  # add_shadow(raymat, max_darken = 0.5) %>%
  # add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()

# plot 3d
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(detect_water(elev_matrix), color="desert") %>%
  # add_shadow(raymat) %>%
  # add_shadow(ambmat,0.5) %>%
  plot_3d(elev_matrix,zscale=30,fov=60,theta=45,zoom=0.75,phi=45, windowsize = c(1000,800))

render_snapshot()
