# AGRUPAR POPULACAO!!! ----------------------------------------------------



pop_por_hex <- function(shape, hex) {
  
  if (tools::file_ext(shape) == "shp") {
    
    points <- st_read(shape) %>%
      st_centroid()
    
  }  else if (tools::file_ext(shape) == "rds") {
    
    points <- readr::read_rds(shape) %>%
      st_centroid()
    
  }
  
  if (tools::file_ext(hex) == "shp") {
    
    hex <- st_read(hex) %>%
      mutate(id_hex = 1:n())
    
  } else if (tools::file_ext(hex) == "rds") {
    
    hex <- readr::read_rds(hex)
    mutate(id_hex = 1:n())
    
  }
  
  
  # SIMBORA!!! --------------------------------------------------------------
  
  vai <- st_join(points, hex) %>%
    group_by(id_hex) %>%
    summarise(n = sum(POP)) %>%
    st_set_geometry(NULL)
  
  
  vai_final <- hex %>%
    left_join(vai)
  

  # SALVAR ------------------------------------------------------------------
  
  

  
  
  
}




# AGRUPAR HOSPITAIS!!! ----------------------------------------------------

hospitais_por_hex <- function(municipio, cnes = "../data-raw/hospitais/cnesnone_2018.csv") {
  
  # ABRIR ARQUIVOS ----------------------------------------------------------
  
  hospitais <- read_csv(cnes) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    identity()
  
  muni_short_name <- substring(municipio, 1, 3)
  
  dir_muni <- paste0("../data/hex_municipio/hex_", muni_short_name, ".rds")
  
  hex_muni <- read_rds(dir_muni) %>%
    select(id_hex)
  
  
  # AGRUPAR TODAS OS HOSPITAIS POR HEXAGONO -----------------------------------
  
  hex_muni_v1 <- hospitais %>%
    st_join(hex_muni) %>%
    # filter(id_hex == 278)
    st_set_geometry(NULL) %>%
    count(id_hex)
  
  hex_final <- hex_muni %>%
    left_join(hex_muni_v1)
  
  

  # SALVAR ------------------------------------------------------------------
  
  write_rds(hex_final, "..data/")

  
  
  
}



# AGRUPAR ESCOLAS!!! ------------------------------------------------------

escolas_por_hex <- function(shape, hex) {
  
  # ABRIR ARQUIVOS ----------------------------------------------------------
  
  educacao <- read_csv(shape) %>%
    # Selecionar somente municipio de Fortaleza
    filter(municipio == "Fortaleza") %>%
    # Deletar as que nao tem localizacao
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    # Selecionar somente o nome da escola
    select(cod_escola)
  
  hex_muni <- st_read(hex) %>%
    mutate(id_hex = 1:n()) %>%
    select(id_hex)
  
  
  # AGRUPAR TODAS AS ESCOLAS POR HEXAGONO -----------------------------------
  
  hex_muni_v1 <- educacao %>%
    st_join(hex_muni) %>%
    st_set_geometry(NULL) %>%
    count(id_hex)
  
  hex_final <- hex_muni %>%
    left_join(hex_muni_v1)
  
  
}
