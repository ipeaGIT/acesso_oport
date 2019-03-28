  


grade_para_municipio <- function(muni, uf_input) {
  
  files <- read_csv("../data-raw/lookup_grade_ufs.csv") %>%
    filter(uf == uf_input) %>%
    mutate(Quadrante = paste0("grade_", Quadrante)) %>%
    .$Quadrante
  
  arquivos <- paste0("../data-raw/dadosrds/", files, ".rds")
  
  # abrir quadrantes da uf
  
  grades <- map_dfr(arquivos, read_rds) %>%
    as_tibble() %>%
    st_sf(crs = 4326)
  
  # extrair municipio -------------------------------------------------------
  
  municipio_ok <- toupper(muni)
  
  
  # abrir arquivos ----------------------------------------------------------
  
  dir_muni <- paste0("../data/municipios/municipios_", uf_input, ".rds")
  
  grade_estado <- grades %>%
    mutate(id_grade = 1:n()) %>%
    select(id_grade, MASC, FEM, POP, DOM_OCU)
  
  grade_estado_centroids <- grade_estado %>%
    st_centroid()
  
  cidade <- read_rds(dir_muni) %>%
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
  municipio_nome_salvar <- substring(municipio_ok, 1, 3)
  
  # # criar pasta para o municipio
  # dir.create(paste0("data/grade_municipio/", municipio_nome_salvar))
  
  # salvar no disco
  write_rds(grade_municipio, 
           paste0("../data/grade_municipio/grade_", tolower(municipio_nome_salvar), ".rds"))
  
  
  
}

# grade_para_municipio("recife", "pe")
# 
# 
# hmmm <- read_rds("../data/grade_municipio/grade_rec.rds")
# 
# mapview::mapview(hmmm)

