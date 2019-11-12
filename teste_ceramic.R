source('./R/fun/setup.R')
library(ceramic)

# register api
Sys.setenv(MAPBOX_API_KEY = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")

# read shape
temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == "rio"]$code_muni)

# ou
tile_for <- cc_location(temp_sf, 
                        type = "v4/mapbox.light", 
                        debug = TRUE)


tile_for <- cc_location(temp_sf, 
                        type = "styles/v1/kauebraga/ck2qc9zd22g2x1dqs9qxgfh26/tiles" 
                        # , debug = TRUE
                        )

plotRGB(tile_for)


# teste plot
tab <- as.data.frame(tile_for, xy = TRUE)
names(tab) <- c("x", "y", "red", "green", "blue")
tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)

ggplot() + 
  geom_raster(data = tab, aes(x, y, fill = hex)) + 
  coord_equal()  +   
  scale_fill_identity() +
  new_scale_fill() +
  geom_sf(data = st_transform(acess_rio_pt_pico, "+proj=merc +a=6378137 +b=6378137"), aes(fill = valor), color = NA, alpha=.7)  +
  geom_sf(data = st_transform(linhas_hm_rio, "+proj=merc +a=6378137 +b=6378137"), size=0.3, color="gray70")+
  viridis::scale_fill_viridis( direction = -1,
                               breaks = c(0, 10, 20, 30, 40),
                               labels = c(0, 10, 20, 30, "+40 min")) +
  labs(fill = "Tempo até a oportunidade\n mais próxima")+
  facet_wrap(~ind, ncol = 1)+
  theme_for_TMI()

ggsave(file="../figures/td/fig1-TMI_SM_TP_mapbox.png", dpi = 300, width = 8, height = 10, units = "cm")


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
