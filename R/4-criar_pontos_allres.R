points_allres <- function(muni_shortname) {
  
  dir <- dir("../data/hex_municipio/", pattern = muni_shortname)
  
  res <- str_extract(dir, "\\d+")
  
  dir_muni <- paste0("../data/hex_municipio/hex_", muni_shortname, "_", res, ".rds")
  
  seila <- function(muni_res) {
    
    dir_muni <- muni_res
    
    res <- str_extract(dir_muni, "\\d+")
    
    # criar pontos
    hex_muni <- readRDS(dir_muni) %>%
      select(id_hex) %>%
      st_centroid() %>%
      sfc_as_cols(names = c("X","Y"))
    # rename(GEOID = id_hex)
    
    
    # salvar
    dir_output <- sprintf("../otp/points/points_%s_%s.csv", muni_shortname, res)
    
    write_csv(hex_muni, dir_output)
    
  }
  
  walk(dir_muni, seila)
  
}