# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.2.2 Agrega informacao de renda dos setores censitários para a grade do censo
  
  

# carregar bibliotecas
source('./R/fun/setup.R')



## Funcao para inputar renda do setor censitario para grade estatistica  -------------------------------------------
renda_de_setor_p_grade <- function(sigla_muni) {
  
  # sigla_muni <- 'for'

  # status message
  message('Woking on city ', sigla_muni, '\n')
  
  # endereco dos arquivos
  path_setor <- sprintf("../data/setores_agregados/setores_agregados_%s.rds", sigla_muni)
  path_grade <- sprintf("../data-raw/grade_municipio/grade_%s.rds", sigla_muni)
  
  # leitura de shapes de setores censitarios e grade estatistica
  setor <- readr::read_rds(path_setor)
  grade <- readr::read_rds(path_grade)
  
  
  # mesma projecao
  setor <- sf::st_transform(setor, sf::st_crs(grade))
  
  # Criar id unico de cada grade e filtra colunas
  grade$id_grade <- 1:nrow(grade)
  
  # corrigir grades de borda
    grade_corrigida <- grade %>%
      mutate(area_antes = as.numeric(st_area(.))) %>%
      st_intersection(setor %>% dplyr::select(code_tract)) %>%
      group_by(id_grade) %>%
      summarise(pop_total = first(POP),
                area_antes = first(area_antes))
                    

# corrigir populacao das grades de borda que foram cortadas (porque parte da grade caia fora do municipio)
  grade_corrigida <- grade_corrigida %>%    
    mutate(area_depois = as.numeric(st_area(.))) %>%
    mutate(prop = area_depois/area_antes) %>%
    mutate(pop_total = prop * pop_total)
  
  # Criar id unico de cada grade e filtra colunas
  grade_corrigida <- grade_corrigida %>%
    rename(area_grade = area_depois) %>%
    dplyr::select(id_grade, pop_total, area_grade)
  
  # Criar id unico de cada setor e filtra colunas
  setor <- setor %>%
    mutate(id_setor = 1:n()) %>%
    mutate(area_setor = st_area(.)) %>%
    dplyr::select(id_setor, renda_total, area_setor, cor_branca, cor_preta, cor_amarela, cor_parda, cor_indigena)
  
  # agrega cor negra
    setDT(setor)[, cor_negra := sum(cor_preta, cor_parda), by=id_setor]
    setor[, c('cor_preta', 'cor_parda') := NULL]

  # Calcular a proporcao que cada cor em cada setor censitario
  setDT(setor)[,  pop_total := sum(cor_branca, cor_amarela, cor_indigena, cor_negra),  by=id_setor]
  setor[,  ":="(cor_b_prop = cor_branca/pop_total,
                cor_a_prop = cor_amarela/pop_total,
                cor_i_prop = cor_indigena/pop_total,
                cor_n_prop = cor_negra/pop_total), by=id_setor]
  # volta para sf
  setor <- st_sf(setor)
  


# funcao de reaportion com duas variaveis de referencia (populacao e area)
# Resultado (ui_fim) eh uma grade estatistica com informacao de renda inputada a partir do setor censitario
### aplicacao para renda --------------------------
ui <- sf::st_intersection(grade_corrigida, setor) %>%
  # tip from https://rpubs.com/rural_gis/255550
  
  # Calcular a area de cada pedaco
  dplyr::mutate(area_pedaco = st_area(.)) %>%
  
  # Calcular a proporcao de cada setor que esta naquele pedaco (essa sera a area a ponderar pela renda)
  dplyr::mutate(area_prop_setor = area_pedaco/area_setor) %>%
  
  # Calcular a proporcao de cada grade que esta naquele pedacao
  dplyr::mutate(area_prop_grade =  area_pedaco/area_grade) %>%
    
    # Calcular a quantidade de populacao em cada pedaco (baseado na grade)
    dplyr::mutate(pop_prop_grade = pop_total * area_prop_grade) %>%
    
    # Calcular a proporcao de populacao de cada grade que esta dentro do setor
    group_by(id_setor) %>%
    dplyr::mutate(sum = sum(pop_prop_grade, na.rm = TRUE)) %>%
    ungroup() %>%
    
    # Calcular a populacao proporcional de cada pedaco dentro do setor
    dplyr::mutate(pop_prop_grade_no_setor =  pop_prop_grade/sum) %>%
    # Calcular a renda dentro de cada pedaco
    dplyr::mutate(renda_pedaco = renda_total * pop_prop_grade_no_setor) %>%
    dplyr::mutate(branca_pedaco = cor_b_prop * area_prop_grade * pop_total) %>%
    dplyr::mutate(amarela_pedaco = cor_a_prop * area_prop_grade * pop_total) %>%
    dplyr::mutate(indigena_pedaco = cor_i_prop * area_prop_grade * pop_total) %>%
    dplyr::mutate(negra_pedaco = cor_n_prop * area_prop_grade * pop_total)
  
  # Grand Finale (uniao dos pedacos) - Agrupar por grade e somar a renda
  ui_fim <- ui %>%
    st_set_geometry(NULL) %>%
    group_by(id_grade, pop_total) %>%
    dplyr::summarise(renda = sum(renda_pedaco, na.rm = TRUE),
                     cor_branca = as.numeric(sum(branca_pedaco, na.rm = TRUE)),
                     cor_amarela = as.numeric(sum(amarela_pedaco, na.rm = TRUE)),
                     cor_indigena = as.numeric(sum(indigena_pedaco, na.rm = TRUE)),
                     cor_negra = as.numeric(sum(negra_pedaco, na.rm = TRUE))) %>%
    dplyr::mutate(renda = as.numeric(renda)) %>%
    ungroup()
  
  ui_fim_sf <- grade_corrigida %>%
    dplyr::select(id_grade) %>%
    left_join(ui_fim, by = "id_grade")
  
  
  # Salvar em disco
  path_out <- sprintf("../data/grade_municipio_com_renda_cor/grade_renda_cor_%s.rds", sigla_muni)
  readr::write_rds(ui_fim_sf, path_out)
  
}

#### Aplicando funcao em paralelo para salvar grades com info de renda ---------------------------------------------------------------

# Parallel processing using future.apply
future::plan(future::multiprocess)
future.apply::future_lapply(X =munis_df$abrev_muni, FUN=renda_de_setor_p_grade, future.packages=c('sf', 'dplyr'))


