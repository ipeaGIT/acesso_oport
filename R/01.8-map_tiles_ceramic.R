source('./R/fun/setup.R')
library(ceramic)


# register mapbox api key
my_api <- data.table::fread("../data-raw/mapbox_key.txt", header = F)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)



# 1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(muni_sigla) {
  
  # muni_sigla <- "for"
  
  # read shape
  temp_sf <- geobr::read_municipality(code_muni = munis_df[abrev_muni == muni_sigla]$code_muni)
  
  
  tile_for <- cc_location(temp_sf, 
                          type = "styles/v1/kauebraga/ck34n83gd0dli1cnvtnrlgber/tiles" 
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
