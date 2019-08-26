#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.2.1 Cria grade de haxogonos para os municipios

  
# carregar bibliotecas
source('./R/fun/setup.R')



#### 1 Funcao para criar hexagonos

shape_to_hexagon <- function(sigla_muni) {

dir_muni <- paste0("../data-raw/municipios/",sigla_muni, "/municipio_", sigla_muni, ".rds")

muni <- read_rds(dir_muni) %>% 
  st_transform(crs=4326) %>% # projecao
  # Buffer para extender a area do municipio e assim evitar que os hexagonos nao considerem areas de borda
  st_buffer(dist = 0.003)

# resolucoes desejadas
  # Available resolutions considerinf length of short diagonal - https://uber.github.io/h3/#/documentation/core-library/resolution-table
  # 10 ~136 meters
  # 09 ~357
  # 08 ~960
  # 07 ~2510 meters
  res_todas <- c(8, 9)

  

make_hex <- function(resolution, muninho) {
  
  # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
  hex_ids <- h3jsr::polyfill(muni, res = resolution, simple = FALSE)
  
  # pass the h3 ids to return the hexagonal grid
  hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
    h3jsr::h3_to_polygon(simple = FALSE) %>%
    rename(id_hex = h3_address) %>%
    as_tibble() %>% 
    st_sf()
  
  
  
# salvar ------------------------------------------------------------------
  
  # rsolution
  if (nchar(resolution) == 1) res_fim <- paste0("0", resolution) else res_fim <- resolution
    
  
  # salvar no disco
  write_rds(hex_grid, 
            paste0("../data/hex_municipio/hex_", sigla_muni, "_", res_fim, ".rds"))
}
# salve arquivo de cada resolucao
purrr::walk(res_todas, make_hex, muni)


}




#### Aplicando funcao em paralelo para salvar grades de hexagonos ---------------------------------------------------------------

# Parallel processing using future.apply
future::plan(future::multiprocess)
future.apply::future_lapply(X =munis_df$abrev_muni, FUN=shape_to_hexagon, future.packages=c('h3jsr','sf', 'dplyr'))

