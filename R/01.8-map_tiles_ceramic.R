source('./R/fun/setup.R')
library(ceramic)

# register api
Sys.setenv(MAPBOX_API_KEY = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")



# 1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(muni_sigla) {
  
  # muni_sigla <- "for"
  
  # read shape
  temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == muni_sigla]$code_muni)
  
  
  tile_for <- cc_location(temp_sf, 
                          type = "styles/v1/kauebraga/ck2qc9zd22g2x1dqs9qxgfh26/tiles" 
                          # , debug = TRUE
  )
  
  
  
  # plotRGB(tile_for)
  
  
  # teste plot
  tab <- as.data.frame(tile_for, xy = TRUE)
  names(tab) <- c("x", "y", "red", "green", "blue")
  tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)
  
  
  # save tile
  readr::write_rds(tab, sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", muni_sigla))
  
}

# aplicar funcao
lapply(munis_df$abrev_muni, baixar_map_tile_ceramic)
