# info on Uber's H3 hexagonal gird:  https://github.com/obrl-soil/h3jsr/blob/master/vignettes/intro-to-h3jsr.Rmd
# https://github.com/uber/h3/issues/87


# devtools::install_github("obrl-soil/h3jsr")


library(h3jsr)
library(sf)
library(ggplot2)
library(tidyverse)


shape_to_hexagon <- function(municipio, uf_sigla) {
    
    dir_muni <- paste0("data-raw/municipios/", uf_sigla,  "/", "municipios_", uf_sigla, ".shp")
    
    muni <- read_sf(dir_muni, crs = 4326) %>%
      filter(NM_MUNICIP == toupper(gsub( "_", " ", municipio)))
    
    # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
    hex_ids <- h3jsr::polyfill(muni, res = 8, simple = FALSE)
    
    # Available resolutions considerinf length of short diagonal - https://uber.github.io/h3/#/documentation/core-library/resolution-table
    # 10 ~136 meters
    # 09 ~357
    # 08 ~960
    # 07 ~2510 meters
    
    
    # pass the h3 ids to return the hexagonal grid
    hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
      h3jsr::h3_to_polygon(simple = FALSE)
    
    # plot(hex_grid)
    

    # salvar ------------------------------------------------------------------

    
    # criar pasta para o municipio
    dir.create(paste0("data/hex_municipio/", municipio))
    
    # salvar no disco
    st_write(hex_grid, 
             paste0("data/hex_municipio/", municipio, "/hex_", municipio, ".shp"))

}



# # # aplicando ---------------------------------------------------------------
# 
# shape_to_hexagon("fortaleza", "ce")
# 
# 
# # abrir
# 
# fortaleza <- st_read("data/hex_municipio/fortaleza/hex_fortaleza.shp", crs = 4326)
# mapview::mapview(fortaleza)
