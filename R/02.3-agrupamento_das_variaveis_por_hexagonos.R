# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.2.2 Agrega informacoes demograficas e uso do solo nos hexagonos

# carregar bibliotecas
source('./R/fun/setup.R')




# ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------

# Saude --------------------------------------
cnes <- fread("../data-raw/hospitais/cnesnone_2018.csv") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


# Escolas  -------------------------------------
# abrir censo escolar geo
escolas <- fread("../data/censo_escolar/censo_escolar_2015.csv") %>%
  # Deletar escolas q nao foram localizadas
  dplyr::filter(!is.na(lat)) %>%
  # Selecionar variaveis
  dplyr::dplyr::select(cod_escola, uf, municipio, cod_mun = CO_MUNICIPIO, rede, mat_infantil, mat_fundamental, mat_medio, lon, lat) %>%
  # Transformar para formato longo
  tidyr::gather(tipo, mat_n, mat_infantil:mat_medio)


# Empregos ----------------------------------------------------------
# Abrir rais geo
empregos <- readr::read_rds("../data/rais/rais_2017_corrigido.rds") # para 2017

# Transformar o id_estab para string pra evitar problemas
empregos[, id_estab := as.character(id_estab)]



# FUNCAO PARA REALIZAR EM CADA MUNICIPIO ----------------------------------

# sigla_muni <- "for"
# Funcao para agregar dados de uso do solo na grade de hexagonos
agrupar_variaveis <- function(sigla_muni) {
  
  # resolucoes disponiveis
  res <- str_extract(dir("../data/hex_municipio/", pattern = sigla_muni), "\\d+")
  
  # Pegar endereco das grades e hexagonos em todas resolucoes
  grade_file <- paste0("../data/grade_municipio_com_renda_cor/grade_renda_cor_", sigla_muni, ".rds")
  
  # Gerar centroide da grade grade de cada municipio
  centroide_pop <- readr::read_rds(grade_file) %>%
    dplyr::dplyr::select(id_grade, pop_total, renda,  cor_branca, cor_amarela, cor_indigena, cor_negra) %>%
    st_centroid()
  
  # Qual o codigo do municipio em questao?
  cod_mun_ok <- munis_df[abrev_muni == sigla_muni]$code_muni
  
# Filtrar somente as atividades referentes a cada municipio e transforma em sf
  # para rais 2017
  empregos_filtrado <- empregos[codemun == substr(cod_mun_ok, 1, 6)] %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # escolas
  escolas_filtrado <- setDT(escolas)[cod_mun == cod_mun_ok] %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # saude
  cnes_filtrado <- setDT(cnes)[co_ibge == substr(cod_mun_ok, 1, 6)] %>% st_sf()
  
  
  
# FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES ------------------------------
  
  por_resolucao <- function(muni_res) {
    # muni_res <- '08'
    
    # endereco do hexagono na resolucao
    hexf <- paste0("../data/hex_municipio/hex_",sigla_muni,'_',muni_res, '.rds')
    
    # Ler arquivo de hexagono  
    hex_muni <- readr::read_rds(hexf)
    
    
# Agrupar populacao, cor e renda
    hex_muni_parcial <- hex_muni %>% st_join(centroide_pop) %>%
      group_by(id_hex) %>%
      summarise(cor_branca = sum(round(cor_branca,0), na.rm = TRUE),
                cor_amarela = sum(round(cor_amarela,0), na.rm = TRUE),
                cor_indigena = sum(round(cor_indigena,0), na.rm = TRUE),
                cor_negra = sum(round(cor_negra,0), na.rm = TRUE),
                pop_total = sum(round(pop_total,0), na.rm = TRUE),
                renda_total = sum(renda, na.rm = TRUE)) %>%
      ungroup()
    
    
# Agrupar empregos (agora somando a quantidade de vinculos!)
    groupcols <- names(hex_muni_parcial)[1:ncol(hex_muni_parcial)-1]
    # apagar groupcols <- paste(groupcols, collapse=", ")   %>% noquote()
    
    
    hex_muni_parcial <- hex_muni_parcial %>% st_join(empregos_filtrado) %>%
      # Trazer a quantidade de vinculos 
      group_by(groupcols) %>%
      # summarise(empregos_total = sum(qt_vinc_ativos2, na.rm = TRUE)) %>% # para rais 2017
      summarise(empregos_baixa = sum(baixo, na.rm = TRUE),
                empregos_media = sum(medio, na.rm = TRUE),
                empregos_alta = sum(alto, na.rm = TRUE)) %>% # para rais 2015
      ungroup() %>%
66666666666666666666666666666666666666666666666666666666666666666666666666      
      
     a <-  setDT(hex_muni_parcial)[, .(empregos_baixa = sum(baixo, na.rm = TRUE),
                                       empregos_media = sum(medio, na.rm = TRUE),
                                       empregos_alta = sum(alto, na.rm = TRUE),
                                       geometry = st_union(geometry)
                                       ), by = groupcols ]
      a <- st_sf(a)
      
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

map(sigla_muni, por_municipio)



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
