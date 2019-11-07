source('./R/fun/setup.R')
library(ceramic)

# register api
Sys.setenv(MAPBOX_API_KEY = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")

# read shape
temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == "for"]$code_muni)

# get centroid
centroid <- st_centroid(temp_sf) %>%
  # st_transform(3857) %>%
  sfc_as_cols()

zoom <- 11


# Cria bounding box do municipio para fazer crop do raster
bb1 <- st_bbox(temp_sf, crs = 4326)
bb1 <- c(bb1[1], bb1[3], bb1[2], bb1[4])

# Converte para spatial polygon para fazer o crop
e <- extent(bb1)


# get tile
tile_for <- get_tiles_zoom(e, type = "v4/mapbox.light", zoom = 10)


# ou
tile_for <- cc_location(temp_sf, 
                        type = "v4/mapbox.light", 
                        debug = TRUE)


tile_for <- cc_location(temp_sf, 
                        type = "styles/v1/kauebraga/cjykmhzgo12421cpjk9qos202/tiles" 
                        , verbose = TRUE
                        # , debug = TRUE
                        )

plotRGB(tile_for)

# teste com baseurl
# as coordenadas tem que estar em 3857

tile_for <- cc_location(temp_sf, 
                        base_url = "https://api.mapbox.com/styles/v1/kauebraga/cjykmhzgo12421cpjk9qos202/tiles/512/12/-4288884/-421724")



raster::plotRGB(tile_for)


# https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
slippymath::lonlat_to_tilenum(-38.5277, -3.785656, 10)
# utilizando bbox
bbox_fim <- slippymath::bbox_to_tile_grid(bb1, 11)

# funcao para criar url de download do tile
criar_url <- function(x, y, zoom) {
  
  url <- sprintf("https://api.mapbox.com/styles/v1/kauebraga/cjykmhzgo12421cpjk9qos202/tiles/512/%s/%s/%s/", zoom, x, y)
  
}

# funcao para criar url de download do tile
criar_url_completa <- function(x, y, zoom) {
  
  url <- sprintf("https://api.mapbox.com/styles/v1/kauebraga/cjykmhzgo12421cpjk9qos202/tiles/512/%s/%s/%s?access_token=pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ", 
                 zoom, x, y)
  
}


# aplicar funcao
urls <- pmap(bbox_fim$tiles, criar_url, zoom = bbox_fim$zoom)
urls_completa <- pmap(bbox_fim$tiles, criar_url_completa, zoom = bbox_fim$zoom)

# aplicar funcao de baixar tiles para todas as urls
my_cc_location <- function(url, location, ...) {
  
  x <- cc_location(loc = location, base_url = url, ...)
  
}

tile_for <- map(urls, my_cc_location, location = temp_sf) 


elev_img_bind <- do.call(raster::merge, tile_for)

raster::plotRGB(tile_for[[1]])
raster::plotRGB(elev_img_bind)


vai <- map(urls, curl::curl_download, "test_ceramic.png")

curl::curl_download(urls_completa[[1]], "test_ceramic1.png")
curl::curl_download(urls_completa[[2]], "test_ceramic2.png")
curl::curl_download(urls_completa[[3]], "test_ceramic3.png")
curl::curl_download(urls_completa[[4]], "test_ceramic4.png")

vai1 <- png::readPNG("test_ceramic1.png")
vai2 <- png::readPNG("test_ceramic2.png")
vai3 <- png::readPNG("test_ceramic3.png")
vai4 <- png::readPNG("test_ceramic4.png")

brick <- brick(vai1)

projection(brick) <- "+proj=merc +a=6378137 +b=6378137"

plotRGB(brick)


plot(vai4)

ui <- list(vai1, vai2, vai3, vai4)

elev_img_bind <- do.call(raster::merge, ui)

raster::plot(elev_img_bind)
