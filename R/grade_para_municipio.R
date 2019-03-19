library(tidyverse)
library(sf)
library(mapview)

# essa funcao buscar extrair os grades referentes aos municipios

# extrair_municipio_grade <- function(grade, municipio, nome_municipio) {
extrair_municipio_grade <- function(nome_municipio, uf_sigla) {
  

    # extrair municipio -------------------------------------------------------
    
    municipio_ok <- toupper(nome_municipio)
    
      
    # abrir arquivos ----------------------------------------------------------
    
    dir_grade <- paste0("data-raw/grade_censo/", nome_municipio, "/grade_raw_", nome_municipio, ".shp")
    dir_muni <- paste0("data-raw/municipios/", uf_sigla,  "/", "municipios_", uf_sigla, ".shp")
    
    grade_estado <- read_sf(dir_grade, crs = 4326) %>%
      mutate(id_grade = 1:n()) %>%
      select(id_grade, MASC, FEM, POP, DOM_OCU)
    
    grade_estado_centroids <- grade_estado %>%
      st_centroid()
      
    
    cidade <- read_sf(dir_muni, crs = 4326) %>%
      filter(NM_MUNICIP == municipio_ok) %>%
      select(municipio = NM_MUNICIP)
    
    
    # geoprocessamento --------------------------------------------------------
    
    vai <- st_join(grade_estado_centroids, cidade) %>%
      filter(!is.na(municipio))
      
    
    grade_municipio <- grade_estado %>%
      filter(id_grade %in% vai$id_grade) %>%
      mutate(municipio = municipio_ok)
    
    
    # salvar ------------------------------------------------------------------
    
    # tirar os espaços e colocar underscore
    municipio_nome_salvar <- tolower(gsub( " ", "_", municipio_ok))
    
    # criar pasta para o municipio
    dir.create(paste0("data/grade_municipio/", municipio_nome_salvar))
    
    # salvar no disco
    st_write(grade_municipio, 
             paste0("data/grade_municipio/", municipio_nome_salvar, "/grade_", tolower(municipio_ok), ".shp"))

}
