# carregar bibliotecas
source('./R/fun/setup.R')
source("R/fun/crop_ggmap.R")

devtools::install_github('kauebraga/ceramic')

library(ggmap)

# 1) GOOGLE MAPS MAP TILES --------------------------------

# register google api key
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)


# 1.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile <- function(ano, munis = "all") {
  
  # Criar pasta para salvar arquivos
  dir.create(sprintf("../data-raw/maptiles/gmaps/%s", ano), recursive = TRUE)
  
  baixar_map_tile_muni <- function(sigla_muni) {
    
    # sigla_muni <- "rio"
    
    # read shape
    temp_sf <- read_rds(sprintf("../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla_muni, ano))
    
    # get centroid
    centroid <- st_centroid(temp_sf) %>%
      sfc_as_cols()
    
    if(sigla_muni %in% c("bsb", "man", "cgr")) { 
      my_zoom = 9
    } else if (sigla_muni %in% c("spo", "bel", "slz", "rio")) {
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
    readr::write_rds(map, sprintf("../data-raw/maptiles/%s/gmaps/maptile_gmaps_%s_%s.rds", ano, sigla_muni, ano))
    
  }
  
  # aplicar funcao
  
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  
  lapply(x, baixar_map_tile_muni)
}


baixar_map_tile(2019)


# 1.2) Fazer bounding box e transformar para data.frame RGB xy (easier to plot) ---------------------


crop_e_converter_maps <- function(ano, munis = "all") {
  
  # Criar pasta para salvar arquivos
  dir.create(sprintf("../data/maptiles_crop/gmaps/%s", ano), recursive = TRUE)
  
  crop_e_converter_maps_muni <- function(sigla_muni) {
    
    # muni_sigla <- "for"
    
    # abrir tiles
    map <- read_rds(sprintf("../data-raw/maptiles/%s/gmaps/maptile_gmaps_%s_%s.rds", ano, sigla_muni, ano))
    
    # read shape
    temp_sf <- read_rds(sprintf("../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla_muni, ano))
    
    # aplicaf funcao de bbox
    map <- ggmap_bbox(map, temp_sf)
    
    # save tile
    readr::write_rds(map, sprintf("../data/maptiles_crop/%s/gmaps/maptile_crop_gmaps_%s_%s.rds", ano, sigla_muni, ano))
    
  }
  
  
  # aplicar funcao 
  
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  lapply(x, crop_e_converter_maps_muni)
  
}

# aplicar funcao
crop_e_converter_maps(ano = 2019)





# 2) MAPBOX MAP TILES --------------------------------


library(ceramic)


# register mapbox api key
my_api <- data.table::fread("../data-raw/mapbox_key.txt", header = F)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)



# 2.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(ano, munis = "all") {
  
  
  # Criar pasta para salvar arquivos
  dir.create(sprintf("../data/maptiles_crop/%s/mapbox", ano), recursive = TRUE)
  
  baixar_map_tile_ceramic_muni <- function(sigla_muni) {
    
    # muni_sigla <- "for"
    
    # read shape
    temp_sf <- read_rds(sprintf("../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla_muni, ano))
    
    # download tile based on custom template (style)
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
    readr::write_rds(tab, sprintf("../data/maptiles_crop/%s/mapbox/maptile_crop_mapbox_%s_%s.rds", ano, sigla_muni, ano))
    
  }
  
  # aplicar funcao
  
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  lapply(munis_df$abrev_muni, baixar_map_tile_ceramic_muni)
    
}



# 2.2) Aplicar funcao -----------------------------------------------------------------------------

baixar_map_tile_ceramic(ano = 2019)
