### Este script faz download e edicao dos tiles que serao utilizados como fundo
## dos mapas do projeto


# carregar bibliotecas
source('./R/fun/setup.R')
source("R/fun/crop_ggmap.R")

# devtools::install_github('kauebraga/ceramic')

# 2) MAPBOX MAP TILES --------------------------------


library(ceramic)


# register mapbox api key
my_api <- data.table::fread("../../data-raw/mapbox_key.txt", header = F)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)



# 2.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(ano, munis = "all") {
  
  
  # Criar pasta para salvar arquivos
  dir.create(sprintf("../../data/acesso_oport/maptiles_crop/%s/mapbox", ano), recursive = TRUE)
  
  baixar_map_tile_ceramic_muni <- function(sigla_muni) {
    
    # muni_sigla <- "for"
    
    # read shape
    temp_sf <- read_rds(sprintf("../../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla_muni, ano))
    
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
    readr::write_rds(tab, sprintf("../../data/acesso_oport/maptiles_crop/%s/mapbox/maptile_crop_mapbox_%s_%s.rds", ano, sigla_muni, ano))
    
  }
  
  # aplicar funcao
  
  if (munis == "all") {
    
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  lapply(x, baixar_map_tile_ceramic_muni)
    
}



# 2.2) Aplicar funcao -----------------------------------------------------------------------------

baixar_map_tile_ceramic(ano = 2017)
baixar_map_tile_ceramic(ano = 2018)
baixar_map_tile_ceramic(ano = 2019)
