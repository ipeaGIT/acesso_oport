# sigla_muni <- 'for'; ano <- 2017

agrupar_hex_teste <- function(ano, rais) {
  
  # agrupar empregos por hexagonos para teste ---------------------------------------------------
  
  # empregos <- readr::read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido_geocoded_censoEscolar.rds", ano, ano))
  empregos <- readr::read_rds(rais)
  
  # remove lat lon missing
  empregos <- empregos[!is.na(lat), ]
  
  # filter only estabs with high wuality geocode
  empregos <- empregos[PrecisionDepth %in% c("4 Estrelas", "3 Estrelas", "street_number", "route")]
  
  # select columns
  empregos <- empregos[, .(codemun, id_estab, total_corrigido, lon, lat)]
  
  
  
  agrupar_hex_teste_muni <- function(sigla_muni) {
    
    code_muni <- munis_df[abrev_muni == sigla_muni]$code_muni
    
    # endereco do hexagono na resolucao
    hexf <- sprintf("../../data/acesso_oport/hex_municipio/%s/hex_%s_09_%s.rds", ano, sigla_muni, ano)
    
    # Ler arquivo de hexagono  
    hex_muni <- readr::read_rds(hexf)
    
    
    # filter municipality
    empregos_filtrado <- empregos[codemun == substr(code_muni, 1, 6)] %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # join espacial 
    hex_rais <- hex_muni %>% st_join(empregos_filtrado)
    
    # Summarize
    hex_rais <- setDT(hex_rais)[, .(empregos_total = sum(total_corrigido, na.rm = TRUE),
                                    geometry = geometry[1]), 
                                by = id_hex ] %>%
      mutate(sigla_muni = sigla_muni) %>%
      st_sf()
    
    # save it
    write_rds(hex_rais, sprintf("../../data/acesso_oport/rais/%s/hex_agregados_teste/hex_agregados_teste_%s_%s.rds", 
                                ano, sigla_muni, ano))
    
    
  }
  
  plan(multiprocess)
  furrr::future_walk(munis_df$abrev_muni, agrupar_hex_teste_muni)
  
  
}