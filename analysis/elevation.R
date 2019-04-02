library(raster)
library(rayshader)
library(ggmap)

# Download elevation data
# https://cgiarcsi.community/data/srtm-90m-digital-elevation-database-v4-1/
# http://srtm.csi.cgiar.org/srtmdata/
# https://earthexplorer.usgs.gov/

# load elevation data
# Somente fortaleza, baixado diretamente do earthexplorer
elev_img <- raster::raster("../data-raw/elevation/for/s04_w039_1arc_v3.tif")

# Get bounding box from https://boundingbox.klokantech.com/
# Aqui tambem pode ser usado osmdata::getbb()
bb_for <- c(-38.63656796,-3.88812428,-38.40154132,-3.69197903)
bb_bel <- c(-44.06329161,-20.0594646,-43.85721992,-19.77654377)
bb_rio <- c(-43.79625205,-23.08270518,-43.09908114,-22.74608786)
bb1 <- c(bb_for[1], bb_for[3], bb_for[2], bb_for[4])

# crop fortaleza first?
e <- as(extent(bb1), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
elev_img <- crop(elev_img, e)

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
  plot_map()

# plot 3d
elev_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(watermap, color="desert") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat,0.5) %>%
  plot_3d(elev_matrix,zscale=30,fov=0,theta=45,zoom=0.75,phi=45, windowsize = c(1000,800))

render_snapshot()

