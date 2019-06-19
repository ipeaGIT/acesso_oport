# 
# 
# 
# hex_for <- readRDS("../data/hex_municipio/hex_for.rds")
# 
# cnes <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326)
# 
# escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
#   filter(!is.na(lon)) %>%
#   filter(municipio == "Fortaleza") %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
#   
# pop <- read_rds("../data/grade_municipio/grade_for.rds") %>%
#   select(id_grade, POP) %>%
#   st_centroid()
# 
# 
# hex_for_temp <- hex_for %>%
#   st_join(pop) %>%
#   group_by(id_hex) %>%
#   summarise(pop_total = sum(POP)) %>%
#   ungroup() %>%
#   st_join(cnes) %>%
#   group_by(id_hex, pop_total) %>%
#   summarise(saude_total = n()) %>%
#   ungroup() %>%
#   st_join(escolas) %>%
#   group_by(id_hex, pop_total, saude_total) %>%
#   summarise(escolas_total = n())
# 
# 
# mapview(hex_for_temp, zcol = "pop_total")
# mapview(hex_for_temp, zcol = "saude_total")
# mapview(hex_for_temp, zcol = "escolas_total")


# FUNCAO!!!!!!!!!!!!!!!! --------------------------------------------------



agrupar_variaveis <- function(munis) {
  
  # ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------
  
  # # saude 
  # cnes <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
  #   st_as_sf(coords = c("long", "lat"), crs = 4326)
  # 
  # # educacao
  # escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
  #   filter(!is.na(lon)) %>%
  #   # mutate(municipio == tolower(municipio)) %>%
  #   # filter(municipio == muni) %>%
  #   st_as_sf(coords = c("lon", "lat"), crs = 4326)
  # 
  # # empregos, por enquanto para 2015
  # # deu problemas no fread, entao tentando com o readr
  # # empregos <- fread("../data-raw/rais/rais_2015_rafa_franco.csv", fill = TRUE) %>%
  # empregos <- read_rds("../data/rais/rais_2015.rds") 
  
  # Criar tabela de lookup
  cidades_lookup <- tibble(municipio = c("for", "rec", "bel", "rio", "por", "cur", "ter"),
                           cidade_uf = c("fortaleza, ce", "recife, pe", "belo horizonte, mg", "rio de janeiro, rj",
                                         "porto alegre, rs", "curitiba, pr", "teresina, pi"))
  
  
  
  
  # FUNCAO PARA REALIZAR EM CADA MUNICIPIO ----------------------------------
  
  por_municipio <- function(munis) {
    
    dir <- dir("../data/hex_municipio/", pattern = munis)
    
    res <- str_extract(dir, "\\d+")
    
    dir_muni <- paste0("../data/hex_municipio/hex_", munis, "_", res, ".rds")
    
    dir_grade <- paste0("../data/grade_municipio/grade_", munis, ".rds")
    
    pop <- read_rds(dir_grade) %>%
      dplyr::select(id_grade, POP) %>%
      st_centroid()
    
    # Extrair o nome da cidade de acordo com a base da RAIS
    # cidade_ufs <- filter(cidades_lookup, municipio == munis) %>% .$cidade_uf
    
    # setDT(empregos)
    # 
    # empregos_v1 <- empregos[cidade_uf == cidade_ufs]
    
    # FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES ------------------------------
    
    seila <- function(muni_res, cidade_uf) {
      
      dir_muni <- muni_res
      
      res <- str_extract(dir_muni, "\\d+")
      
      hex_muni <- readRDS(dir_muni)
      
      hex_muni_fim <- hex_muni %>%
        st_join(pop) %>%
        group_by(id_hex) %>%
        summarise(pop_total = sum(POP)) %>%
        ungroup()
        # # Agrupar empregos
        # st_join(empregos) %>%
        # mutate(indice = ifelse(is.na(id_estab), 0, 1)) %>%
        # group_by(id_hex, pop_total) %>%
        # summarise(empregos_total = sum(indice)) %>%
        # ungroup() %>%
        # # agrupar saude
        # st_join(cnes) %>%
        # mutate(indice = ifelse(is.na(co_cnes), 0, 1)) %>%
        # group_by(id_hex, pop_total, empregos_total) %>%
        # summarise(saude_total = sum(indice)) %>%
        # ungroup() %>%
        # # agrupar educacao
        # st_join(escolas) %>%
        # mutate(indice = ifelse(is.na(cod_escola), 0, 1)) %>%
        # group_by(id_hex, pop_total, empregos_total, saude_total) %>%
        # summarise(escolas_total = sum(indice)) %>%
        # ungroup()
      
      
      dir_output <- sprintf("../data/hex_agregados/hex_agregado_%s_%s_TEMP.rds", munis, res)
      
      write_rds(hex_muni_fim, dir_output)
      
    }
    
    # aplicar para cada resolucao
    
    walk(dir_muni, seila)
    
  }
  
  # aplicar para cada municipio
  
  map(munis, por_municipio)
  
  
  
}

  agrupar_variaveis(c("for", "rec", "bel", "rio", "por", "cur", "ter"))
agrupar_variaveis("sao")
agrupar_variaveis("cur")


