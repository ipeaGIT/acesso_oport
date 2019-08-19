#' ## Agrupamento das variáveis por hexágonos
#' 
#' 
## ----agregar variaveis---------------------------------------------------

# munis <- "for"

agrupar_variaveis <- function(munis) {
  
  # ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------
  
  # saude 
  cnes <- fread("../data-raw/hospitais/cnesnone_2018.csv") %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # educacao
  escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
    dplyr::filter(!is.na(lat)) %>%
    select(cod_escola, uf, municipio, cod_mun = CO_MUNICIPIO, rede, mat_infantil, mat_fundamental, mat_medio, lon, lat) %>%
    gather(tipo, mat_n, mat_infantil:mat_medio)
    # # Ajeitar nome do municipio
    # mutate(municipio = tolower(municipio)) %>%
    # mutate(municipio = iconv(municipio, to="UTF-8")) %>%
    # mutate(municipio = iconv(municipio, to="ASCII//TRANSLIT")) %>%
  
  # empregos
  # empregos <- read_rds("../data/rais/rais_2015.rds") # para 2015
  empregos <- read_rds("../data/rais/rais_2017_corrigido_escol.rds") # para 2017
  # Transformar o id_estab para caracter pra evitar problemas
  empregos[, id_estab := as.character(id_estab)]
  
  # Criar tabela de lookup
  cidades_lookup <- tibble(municipio = c("for", "rec", "bel", "rio", "por", "cur", "ter"),
                           cidade_uf = c("fortaleza, ce", "recife, pe", "belo horizonte, mg", "rio de janeiro, rj",
                                         "porto alegre, rs", "curitiba, pr", "teresina, pi"))
  
  # abrir tabela com o nome e codigo dos municipios
  muni_codigos <- fread("../data-raw/tabela_muni_codigos_2010.csv")
  muni_codigos <- muni_codigos[, .(cod_mun = municipio, nome_municipio)]
  muni_codigos[, nome_municipio := tolower(nome_municipio)]
  muni_codigos[, nome_municipio := iconv(nome_municipio, to="ASCII//TRANSLIT")]
  muni_codigos[, nome_municipio := substr(nome_municipio, 1, 3)]
  
  
  
  # FUNCAO PARA REALIZAR EM CADA MUNICIPIO ----------------------------------
  
  por_municipio <- function(munis) {
    
    dir <- dir("../data/hex_municipio/", pattern = munis)
    
    res <- str_extract(dir, "\\d+")
    
    dir_muni <- paste0("../data/hex_municipio/hex_", munis, "_", res, ".rds")
    
    dir_grade <- paste0("../data/grade_municipio_com_renda/grade_renda_", munis, ".rds")
    
    # Pegar a populacao do centroide de cada municipio
    pop <- read_rds(dir_grade) %>%
      dplyr::select(id_grade, pop_total, renda) %>%
      st_centroid()
    
    # Qual o codigo do municipio em questao?
    cod_mun_ok <- muni_codigos[nome_municipio %in% munis]
    
    # Filtrar somente as atividades referentes a cada municipio
    # Para RAIS 2015
    # empregos_filtrado <- empregos[cod_mun %in% cod_mun_ok$cod_mun] %>% 
    #   st_as_sf(coords = c("lon", "lat"), crs = 4326)
    # para rais 2017
    empregos_filtrado <- empregos[codemun %in% substr(cod_mun_ok$cod_mun, 1, 6)] %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    escolas_filtrado <- setDT(escolas)[cod_mun %in% cod_mun_ok$cod_mun] %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # Extrair o nome da cidade de acordo com a base da RAIS
    # cidade_ufs <- filter(cidades_lookup, municipio == munis) %>% .$cidade_uf
    
    # setDT(empregos)
    # 
    # empregos_v1 <- empregos[cidade_uf == cidade_ufs]
    
    # muni_res <- dir_muni[3]
    
    # FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES ------------------------------
    
    por_resolucao <- function(muni_res, cidade_uf) {
      
      dir_muni <- muni_res
      
      res <- str_extract(dir_muni, "\\d+")
      
      hex_muni <- readRDS(dir_muni)
      
      # A criaca da coluna de indice buscar dar valor 0 para se nao houver oportunidades e valor 1
      # para o caso de houver
      
      # Dica para melhorar: fazer st_join, dps agrupar com o data.table, dps st_sf() e st_join de nv
      
      hex_muni_fim <- hex_muni %>%
        # Agrupar populacao e renda
        st_join(pop) %>%
        group_by(id_hex) %>%
        summarise(pop_total = sum(pop_total, na.rm = TRUE), renda_total = sum(renda, na.rm = TRUE)) %>%
        ungroup() %>%
        # Agrupar empregos (agora somando a quantidade de vinculos!)
        st_join(empregos_filtrado %>% select(id_estab)) %>%
        # Trazer a quantidade de vinculos 
        left_join(empregos_filtrado %>% st_set_geometry(NULL) %>% select(id_estab, baixo, medio, alto)) %>%
        mutate(alto = ifelse(is.na(alto), 0, alto),
               medio = ifelse(is.na(medio), 0, medio),
               baixo = ifelse(is.na(baixo), 0, baixo)) %>%
        group_by(id_hex, pop_total, renda_total) %>%
        # summarise(empregos_total = sum(qt_vinc_ativos2, na.rm = TRUE)) %>% # para rais 2017
        summarise(empregos_baixa = sum(baixo, na.rm = TRUE),
                  empregos_media = sum(medio, na.rm = TRUE),
                  empregos_alta = sum(alto, na.rm = TRUE)) %>% # para rais 2015
        ungroup() %>%
        # agrupar saude
        st_join(cnes) %>%
        mutate(indice = ifelse(is.na(co_cnes), 0, 1)) %>%
        group_by(id_hex, pop_total, renda_total, empregos_alta, empregos_media, empregos_baixa) %>%
        summarise(saude_total = sum(indice)) %>%
        ungroup() %>%
        # agrupar educacao
        # agrupar educacao infantil
        st_join(escolas_filtrado %>% filter(tipo == "mat_infantil")) %>%
        mutate(indice = ifelse(is.na(cod_escola), 0, 
                               ifelse(mat_n == 0, 0, 
                                      1))) %>%
        group_by(id_hex, pop_total, renda_total, empregos_alta, empregos_media, empregos_baixa, saude_total) %>%
        summarise(escolas_infantil = sum(indice)) %>%
        ungroup() %>%
        # agrupar educacao fundamental
        st_join(escolas_filtrado %>% filter(tipo == "mat_fundamental")) %>%
        mutate(indice = ifelse(is.na(cod_escola), 0, 
                               ifelse(mat_n == 0, 0, 
                                      1))) %>%
        group_by(id_hex, pop_total, renda_total, empregos_alta, empregos_media, empregos_baixa, 
                 saude_total, escolas_infantil) %>%
        summarise(escolas_fundamental = sum(indice)) %>%
        ungroup() %>%
        # agrupar educacao media
        st_join(escolas_filtrado %>% filter(tipo == "mat_medio")) %>%
        mutate(indice = ifelse(is.na(cod_escola), 0, 
                               ifelse(mat_n == 0, 0, 
                                      1))) %>%
        group_by(id_hex, pop_total, renda_total, empregos_alta, empregos_media, empregos_baixa, 
                 saude_total, escolas_infantil, escolas_fundamental) %>%
        summarise(escolas_medio = sum(indice)) %>%
        ungroup()
      
      
      dir_output <- sprintf("../data/hex_agregados/hex_agregado_%s_%s.rds", munis, res)
      
      write_rds(hex_muni_fim, dir_output)
      
    }
    
    # aplicar para cada resolucao
    
    walk(dir_muni[1:3], por_resolucao)
    
  }
  
  # aplicar para cada municipio
  
  map(munis, por_municipio)
  
  
  
}


# Aplicar funcao
agrupar_variaveis("for")
agrupar_variaveis("bel")
agrupar_variaveis("rio")
agrupar_variaveis("por")
agrupar_variaveis("cur")
agrupar_variaveis("sao")

# ou
plan(multiprocess)
furrr::future_map(c("for", "bel", "rio", "por", "cur", "sao"), agrupar_variaveis)

# # Calculate the number of cores
# no_cores <- 6
# 
# #  Initiate cluster
# library(parallel)
# cl <- parallel::makeCluster(no_cores)
# 
# clusterEvalQ(cl, {library(data.table); library(sf); library(dplyr)})
# clusterExport(cl=cl, c('points_got', 'streets_buffer_got', 'snap_sf'), envir=environment())
# 
# invisible(parallel::parLapply(cl = cl, c("for", "bel", "rio", "por", "cur", "sao"), agrupar_variaveis))


invisible(parallel::mclapply(c("for", "bel", "rio", "por", "cur", "sao"), agrupar_variaveis, mc.cores = 6))


#' 
