# info on Uber's H3 hexagonal gird:  https://github.com/obrl-soil/h3jsr/blob/master/vignettes/intro-to-h3jsr.Rmd
# https://github.com/uber/h3/issues/87


# devtools::install_github("obrl-soil/h3jsr")


library(h3jsr)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)


shape_to_hexagon <- function(municipio, uf_sigla, resolution = 8) {
    
    dir_muni <- paste0("../data/municipios/", "municipios_", uf_sigla, ".rds")
    
    muni <- read_rds(dir_muni) %>%
      filter(NM_MUNICIP == toupper(gsub( "_", " ", municipio)))
    
    # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
    hex_ids <- h3jsr::polyfill(muni, res = resolution, simple = FALSE)
    
    # Available resolutions considerinf length of short diagonal - https://uber.github.io/h3/#/documentation/core-library/resolution-table
    # 10 ~136 meters
    # 09 ~357
    # 08 ~960
    # 07 ~2510 meters
    
    
    # pass the h3 ids to return the hexagonal grid
    hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
      h3jsr::h3_to_polygon(simple = FALSE) %>%
      mutate(id_hex = 1:n())
    
    

    # salvar ------------------------------------------------------------------
    
    municipio_nome_salvar <- substring(municipio, 1, 3)
    
    # salvar no disco
    write_rds(hex_grid, 
             paste0("../data/hex_municipio/hex_", municipio_nome_salvar, ".rds"))

}



# # # aplicando ---------------------------------------------------------------
# 
# munis <- c("fortaleza", "rio de janeiro", "recife", "belo horizonte")
# ufs <- c("ce", "rj", "pe", "mg")
# 
# purrr::walk2(munis, ufs, shape_to_hexagon)
# 
# # abrir
# 
# rj <- read_rds("../data/hex_municipio/hex_rio.rds")
# mapview::mapview(rj)
