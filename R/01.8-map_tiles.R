# carregar bibliotecas
source('./R/fun/setup.R')
source("R/fun/crop_ggmap.R")


library(sf)
library(ggmap)
library(ggplot2)

# 1) GOOGLE MAPS MAP TILES --------------------------------

# register google api key
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)

# 1.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile <- function(muni_sigla) {
  
  # muni_sigla <- "rio"
  
  # read shape
  temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == muni_sigla]$code_muni)
  
  # get centroid
  centroid <- st_centroid(temp_sf) %>%
    sfc_as_cols()
  
  if(muni_sigla %in% c("bsb", "man", "cgr")) { 
    my_zoom = 9
  } else if (muni_sigla %in% c("spo", "bel", "slz", "rio")) {
    my_zoom = 10
  } else {
    my_zoom = 11 # for
  }
  
  
  
  # get ggmap tile
  map <- get_googlemap(center = c(lon = centroid$lon, lat = centroid$lat), zoom = my_zoom,  scale = 2,
                       color = "bw",
                       style = c('feature:administrative.locality|element:labels|visibility:off',
                                 'feature:administrative.neighborhood|element:labels|visibility:off'))
  
  # ggmap(map)
  
  
  # save tile
  readr::write_rds(map, sprintf("../data-raw/map_tiles/map_tile_%s.rds", muni_sigla))
  
}

# aplicar funcao
lapply(munis_df$abrev_muni, baixar_map_tile)





# 1.2) Fazer bounding box e transformar para data.frame RGB xy (easier to plot) ---------------------


crop_e_converter_maps <- function(muni_sigla) {
  
  # muni_sigla <- "for"
  
  # abrir tiles
  map <- read_rds(sprintf("../data-raw/map_tiles/map_tile_%s.rds", muni_sigla))
  
  # read shape
  temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == muni_sigla]$code_muni)
  
  # aplicaf funcao de bbox
  map <- ggmap_bbox(map, temp_sf)
  
  # save tile
  readr::write_rds(map, sprintf("../data/map_tiles_crop/map_tile_crop_%s.rds", muni_sigla))
  
}


# aplicar funcao
lapply(munis_df$abrev_muni, crop_e_converter_maps)










# 2) MAPBOX MAP TILES --------------------------------


library(ceramic)


# register mapbox api key
my_api <- data.table::fread("../data-raw/mapbox_key.txt", header = F)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)



# 2.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(muni_sigla) {
  
  # muni_sigla <- "for"
  
  # read shape
  temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == muni_sigla]$code_muni)
  
  
  tile_for <- cc_location(temp_sf, 
                          type = "styles/v1/kauebraga/ck34n83gd0dli1cnvtnrlgber/tiles" 
                          # , debug = TRUE
  )
  
  
  
  # plotRGB(tile_for)
  
  
  # as rgb data.frame
  tab <- as.data.frame(tile_for, xy = TRUE)
  names(tab) <- c("x", "y", "red", "green", "blue")
  tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)
  
  
  # save tile
  readr::write_rds(tab, sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", muni_sigla))
  
}

# aplicar funcao
lapply(munis_df$abrev_muni, baixar_map_tile_ceramic)

