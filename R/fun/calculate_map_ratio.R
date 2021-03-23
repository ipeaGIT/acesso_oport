
# sigla_muni <- 'for'

calculate_map_ratio <- function(sigla_muni) {
  
  # open tile
  muni <- read_rds(sprintf("../../data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_muni))
  
  # calulate bbox
  x_range <- max(muni$x) - min(muni$x) 
  y_range <- max(muni$y) - min(muni$y) 
  
  # calcualte w / h
  w_h <- x_range / y_range
  
  
}


# a <- map_chr(munis_df$abrev_muni, calculate_map_ratio)
