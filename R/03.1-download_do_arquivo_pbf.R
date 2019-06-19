#' ## Download do arquivo .pbf
#' 
#' 
#' 
## ----download_pbf--------------------------------------------------------

cidade <- "porto alegre"

download_pbf <- function(cidade) {
  
  cidade_string <- paste0(cidade, ", brazil")
  
  # Tags disponiveis
  vai <- available_tags("highway")
  
  features <- opq (cidade_string) %>%
    add_osm_feature(key = "highway", value = vai)
  
  # Exportar arquivo .pbf para o disco
  cidade_short <- substr(cidade, 1, 3)
  path_out <- sprintf("../otp/graphs/%s/%s.pbf", cidade_short, cidade_short)
  osmdata_pbf(features, path_out)

}


#' 
