#' ## Criação de hexágonos
#' 
#' 
## ----hexagonos-----------------------------------------------------------

shape_to_hexagon <- function(municipio, uf_sigla) {
  
  dir_muni <- paste0("../data/municipios/", "municipios_", uf_sigla, ".rds")
  
  muni <- read_rds(dir_muni) %>%
    dplyr::filter(NM_MUNICIP == toupper(gsub( "_", " ", municipio))) %>%
    # Buffer para extender a area do municipio e assim evitar que os hexagonos nao considerem areas de borda
    st_buffer(dist = 0.003)
  
  
  res_todas <- c(7, 8, 9, 10)
  
  # Teste:
  # resolution <- 8
  
  make_hex <- function(resolution, muninho) {
    
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
      rename(id_hex = h3_address) %>%
      as_tibble() %>% 
      st_sf()
    
    
    
    # salvar ------------------------------------------------------------------
    
    municipio_nome_salvar <- substring(municipio, 1, 3)
    if (nchar(resolution) == 1) res_fim <- paste0("0", resolution) else res_fim <- resolution
      
    
    # salvar no disco
    write_rds(hex_grid, 
              paste0("../data/hex_municipio/hex_", municipio_nome_salvar, "_", res_fim, ".rds"))
    
    
    
  }
  
  purrr::walk(res_todas, make_hex, muni)
  
  
}

# # aplicando ---------------------------------------------------------------

shape_to_hexagon("fortaleza", "ce")
shape_to_hexagon("rio de janeiro", "rj")
shape_to_hexagon("belo horizonte", "mg")
shape_to_hexagon("porto alegre", "rs")
shape_to_hexagon("curitiba", "pr")
shape_to_hexagon("teresina", "pi")
shape_to_hexagon("são paulo", "sp")


# # Eu poderia simplesmente usar walk...
# purrr::walk2(munis, ufs, shape_to_hexagon)


#' 
