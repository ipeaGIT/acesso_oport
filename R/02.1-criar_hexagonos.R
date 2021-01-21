
# # adiciona sigla do municipio
# hex_grid$muni <- sigla_muni



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.2.1 Cria grade de haxogonos para os municipios


# carregar bibliotecas
source('./R/fun/setup.R')
library(h3jsr)



#### 1) Funcao para criar hexagonos ------------

criar_hexagonos <- function(ano, munis = "all") {
  
  # Select the corerspondent munis_df
  # munis_df <- get(sprintf("munis_df_%s", ano))
  # munis_df <- munis_df_2019
  
  # sigla_muni <- 'slz'
  # sigla_muni <- 'poa'
  
  shape_to_hexagon <- function(sigla_muni) {
    
    code <- filter(munis_df, abrev_muni == sigla_muni)
    
    code <- code$code_muni
    
    muni <- geobr::read_municipality(code, year = 2019) %>% 
      st_transform(crs=4326) %>%
      # Buffer para extender a area do municipio e assim evitar que os hexagonos nao considerem areas de borda
      st_buffer(dist = 0.003)
    
    # resolucoes desejadas
    # Available resolutions considerinf length of short diagonal - https://uber.github.io/h3/#/documentation/core-library/resolution-table
    # 10 ~136 meters
    # 09 ~357
    # 08 ~960
    # 07 ~2510 meters
    res_todas <- c(8, 9)
    
    # resolution <- res_todas[2]
    
    
    
    make_hex <- function(resolution) {
      
      # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
      hex_ids <- polyfill(muni, res = resolution, simple = TRUE)
      
      # pass the h3 ids to return the hexagonal grid
      hex_grid <- hex_ids %>%  # hex_ids[[1]] %>%  # change necessary to accommodate multiple municipalities
        h3_to_polygon(simple = FALSE) %>%
        rename(id_hex = h3_address) %>%
        as_tibble() %>% 
        mutate(sigla_muni = sigla_muni)
      
      # delete possible duplicates
      hex_grid <- distinct(hex_grid, id_hex, .keep_all = TRUE) %>%
        st_sf()
      
      # mapview(select(hex_grid, geometry))
      
      # salvar ------------------------------------------------------------------
      
      # rsolution
      if (nchar(resolution) == 1) res_fim <- paste0("0", resolution) else res_fim <- resolution
      
      # dir.create(sprintf("../../data/acesso_oport/hex_municipio/%s", ano))
      
      # salvar no disco
      write_rds(hex_grid, 
                sprintf("../../data/acesso_oport/hex_municipio/%s/hex_%s_%s_%s.rds", ano, sigla_muni, res_fim, ano))
    }
    
    # salve arquivo de cada resolucao
    purrr::walk(res_todas, make_hex)
    
    
  }
  
  
  
  
  #### Aplicando funcao em paralelo para salvar grades de hexagonos ---------------------------------------------------------------
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  future::plan(future::multiprocess)
  future.apply::future_lapply(X = x, FUN=shape_to_hexagon, future.packages=c('h3jsr','sf', 'dplyr'))
  
}


#### 2) Aplicar funcao ------------
criar_hexagonos(ano = 2017)
criar_hexagonos(ano = 2018)
criar_hexagonos(ano = 2019)
criar_hexagonos(ano = 2020)
