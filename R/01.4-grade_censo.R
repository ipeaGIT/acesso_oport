#' ## Grade censo
#' 
#' 
## ----grade_censo---------------------------------------------------------

# TRATAMENTO DO ARQUIVO COM OS IDs

# criar encoding para abrir arquivo
brazil <- locale("pt", encoding = "Windows-1252")

# abrir tabela de ids
ids_corresp <- read_delim("../data-raw/Tabela_UF_ID.csv", delim = ";", locale = brazil) %>%
  arrange(Estados) %>%
  mutate(Estados = ifelse(Estados == "Pernanbuco", "Pernambuco", Estados))

lookup_ufs <- data.frame(stringsAsFactors=FALSE,
                      Estados = c("Acre", "Alagoas", "Amazonas",
                                          "Amapá", "Bahia", "Ceará",
                                          "Distrito Federal", "Espírito Santo", "Goiás",
                                          "Maranhão", "Minas Gerais",
                                          "Mato Grosso do Sul", "Mato Grosso", "Pará",
                                          "Paraíba", "Pernambuco", "Piauí", "Paraná",
                                          "Rio de Janeiro", "Rio Grande do Norte",
                                          "Rondônia", "Roraima",
                                          "Rio Grande do Sul", "Santa Catarina", "Sergipe",
                                          "São Paulo", "Tocantins"),
                         uf = c("AC", "AL", "AM", "AP", "BA", "CE",
                                          "DF", "ES", "GO", "MA", "MG", "MS",
                                          "MT", "PA", "PB", "PE", "PI", "PR",
                                          "RJ", "RN", "RO", "RR", "RS", "SC", "SE",
                                          "SP", "TO")
)



ids_corresp_v1 <- ids_corresp %>%
  left_join(lookup_ufs) %>%
  mutate(uf = tolower(uf),
         Quadrante = tolower(Quadrante)) %>%
  mutate(Quadrante = gsub("_", "", Quadrante))

write_csv(ids_corresp_v1, "../data-raw/lookup_grade_ufs.csv")
# write_rds(ids_corresp_v1, "../data-raw/lookup_grade_ufs.rds")


#' 
#' 
#' 
## ----funcao_grade_p_municipio--------------------------------------------

# muni <- "porto alegre"
# uf_input <- "rs"


grade_para_municipio <- function(muni, uf_input) {
  
  files <- read_csv("../data-raw/lookup_grade_ufs.csv") %>%
    # Corrigir esse valor
    mutate(Quadrante = ifelse(Quadrante == "id4", "id04", Quadrante)) %>%
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
    dplyr::select(id_grade, MASC, FEM, POP, DOM_OCU)
  
  # grade_estado_centroids <- grade_estado %>%
  #   st_centroid()
  
  cidade <- read_rds(dir_muni) %>%
    filter(NM_MUNICIP == municipio_ok) %>%
    dplyr::select(municipio = NM_MUNICIP)
  
  
  # geoprocessamento --------------------------------------------------------
  
  vai <- st_join(grade_estado, cidade) %>%
    filter(!is.na(municipio))
  
  
  grade_municipio <- grade_estado %>%
    dplyr::filter(id_grade %in% vai$id_grade) %>%
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


#' 
#' 
## ----aplicar_grades------------------------------------------------------

municipios <- c("fortaleza", "rio de janeiro", "belo horizonte", "recife", "porto alegre", "são paulo", "curitiba")
ufs <- c("ce", "rj", "mg", "pe", "rs", "sp", "pr")

grade_para_municipio("são paulo", "sp")
grade_para_municipio("curitiba", "pr")
grade_para_municipio("teresina", "pi")
grade_para_municipio("porto alegre", "rs")

walk2(municipios, ufs, grade_para_municipio)


#' 
#' 
#' 
