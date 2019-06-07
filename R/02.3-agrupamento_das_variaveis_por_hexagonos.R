#' ## Agrupamento das variáveis por hexágonos
#' 
#' A função ``agrupar_variaveis`` aceita como _input_ o nome do município desejado e retorna uma tabela com o shape dos hexágonos, para cada resolução espacial, e a quantidade de estabelecimentos de saúde, educação e população agregados, salvos em disco.
#' 
## ----agregar variaveis---------------------------------------------------


# FUNCAO --------------------------------------------------------------------------------------

agrupar_variaveis <- function(munis) {
  
  # ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------
  
  # saude 
  cnes <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # educacao
  escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
    dplyr::filter(!is.na(lat)) %>%
    # mutate(municipio == tolower(municipio)) %>%
    # filter(municipio == muni) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # empregos, por enquanto para 2015
  # deu problemas no fread, entao tentando com o readr
  # empregos <- fread("../data-raw/rais/rais_2015_rafa_franco.csv", fill = TRUE) %>%
  empregos <- read_rds("../data/rais/rais_2015.rds") 
  
  # Criar tabela de lookup
  cidades_lookup <- tibble(municipio = c("for", "rec", "bel", "rio", "por", "cur", "ter"),
                           cidade_uf = c("fortaleza, ce", "recife, pe", "belo horizonte, mg", "rio de janeiro, rj",
                                         "porto alegre, rs", "curitiba, pr", "teresina, pi"))
  
  
  
  
  # FUNCAO PARA REALIZAR EM CADA MUNICIPIO ----------------------------------
  
  por_municipio <- function(munis) {
    
    dir <- dir("../data/hex_municipio/", pattern = munis)
    
    res <- str_extract(dir, "\\d+")
    
    dir_muni <- paste0("../data/hex_municipio/hex_", munis, "_", res, ".rds")
    
    dir_grade <- paste0("../data/grade_municipio_com_renda/grade_renda_", munis, ".rds")
    
    pop <- read_rds(dir_grade) %>%
      dplyr::select(id_grade, pop_total, renda) %>%
      mutate(renda = as.numeric(renda)) %>%
      st_centroid()
    
    # Extrair o nome da cidade de acordo com a base da RAIS
    # cidade_ufs <- filter(cidades_lookup, municipio == munis) %>% .$cidade_uf
    
    # setDT(empregos)
    # 
    # empregos_v1 <- empregos[cidade_uf == cidade_ufs]
    
    # muni_res <- dir_muni[1]
    
    # FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES ------------------------------
    
    seila <- function(muni_res, cidade_uf) {
      
      dir_muni <- muni_res
      
      res <- str_extract(dir_muni, "\\d+")
      
      hex_muni <- readRDS(dir_muni)
      
      hex_muni_fim <- hex_muni %>%
        # Agrupar populacao e renda
        st_join(pop) %>%
        group_by(id_hex) %>%
        summarise(pop_total = sum(pop_total), renda_total = sum(renda)) %>%
        ungroup() %>%
        # Agrupar empregos (agora somando a quantidade de vinculos!)
        st_join(empregos) %>%
        # mutate(indice = ifelse(is.na(id_estab), 0, 1)) %>%
        group_by(id_hex, pop_total, renda_total) %>%
        summarise(empregos_total = sum(qt_vinc_ativos, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(empregos_total = ifelse(is.na(empregos_total), 0, empregos_total)) %>%
        # agrupar saude
        st_join(cnes) %>%
        mutate(indice = ifelse(is.na(co_cnes), 0, 1)) %>%
        group_by(id_hex, pop_total, renda_total, empregos_total) %>%
        summarise(saude_total = sum(indice)) %>%
        ungroup() %>%
        # agrupar educacao
        st_join(escolas) %>%
        mutate(indice = ifelse(is.na(cod_escola), 0, 1)) %>%
        group_by(id_hex, pop_total, renda_total, empregos_total, saude_total) %>%
        summarise(escolas_total = sum(indice)) %>%
        ungroup()
      
      
      dir_output <- sprintf("../data/hex_agregados/hex_agregado_%s_%s.rds", munis, res)
      
      write_rds(hex_muni_fim, dir_output)
      
    }
    
    # aplicar para cada resolucao
    
    walk(dir_muni, seila)
    
  }
  
  # aplicar para cada municipio
  
  map(munis, por_municipio)
  
  
  
}



# APLICAR FUNCAO ------------------------------------------------------------------------------


agrupar_variaveis("for")
agrupar_variaveis("bel")
agrupar_variaveis("rio")
agrupar_variaveis("por")
agrupar_variaveis("cur")
agrupar_variaveis("sao")


#' 
