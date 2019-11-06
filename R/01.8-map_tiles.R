# carregar bibliotecas
source('./R/fun/setup.R')
source("R/fun/crop_ggmap.R")


library(sf)
library(ggmap)
library(ggplot2)

# register google api key
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)

# 1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile <- function(muni_sigla) {
  
  # muni_sigla <- "bel"
  
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
                       style = c('feature:administrative.locality|element:labels|visibility:off',
                                 'feature:administrative.neighborhood|element:labels|visibility:off'))
  
  # ggmap(map)
  
  
  # save tile
  readr::write_rds(map, sprintf("../data-raw/map_tiles/map_tile_%s.rds", muni_sigla))
  
}

# aplicar funcao
lapply(munis_df$abrev_muni, baixar_map_tile)





# 2) Fazer bounding box e transformar para data.frame RGB xy (easier to plot) ---------------------


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
