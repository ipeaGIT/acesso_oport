# Agrega informacao de renda dos setores censitários para a grade do censo
  
  

# carregar bibliotecas
source('./R/fun/setup.R')

#' A funcao `renda_de_setor_p_grade` passa todas as variaveis que foram coletadas
#' dos setores censitarios para as grades estatisticas  

renda_de_setor_p_grade <- function(ano, munis = "all") {
  
  
  renda_de_setor_p_grade_muni <- function(sigla_muni) {
    
    # sigla_muni <- 'for'; ano <- 2019
    
    # status message
    message('Woking on city ', sigla_muni, '\n')
    
    # endereco dos arquivos
    path_setor <- sprintf("../../data/acesso_oport/setores_agregados/%s/setores_agregados_%s_%s.rds", ano, sigla_muni, ano)
    path_grade <- sprintf("../../data-raw/grade_municipio/%s/grade_%s_%s.rds", ano, sigla_muni, ano)
    
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
                pop_homens = first(MASC),
                pop_mulheres = first(FEM),
                area_antes = first(area_antes))
    
    
    # corrigir populacao das grades de borda que foram cortadas (porque parte da grade caia fora do municipio)
    grade_corrigida <- grade_corrigida %>%    
      mutate(area_depois = as.numeric(st_area(.))) %>%
      mutate(prop = area_depois/area_antes) %>%
      mutate(pop_total = prop * pop_total,
             pop_homens = prop * pop_homens,
             pop_mulheres = prop * pop_mulheres)
    
    # Criar id unico de cada grade e filtra colunas
    grade_corrigida <- grade_corrigida %>%
      rename(area_grade = area_depois) %>%
      dplyr::select(id_grade, pop_total, pop_homens, pop_mulheres, area_grade)
    
    # Criar id unico de cada setor e filtra colunas
    setor <- setor %>%
      mutate(id_setor = 1:n()) %>%
      mutate(area_setor = st_area(.)) %>%
      dplyr::select(id_setor, renda_total, area_setor, 
                    # cores
                    cor_branca, cor_preta, cor_amarela, cor_parda, cor_indigena,
                    # idade
                    matches("idade")
                    )
    
    # agrega cor negra
    setDT(setor)[, cor_negra := sum(cor_preta, cor_parda), by=id_setor]
    setor[, c('cor_preta', 'cor_parda') := NULL]
    
    # Calcular a proporcao que cada cor em cada setor censitario
    setDT(setor)[,  pop_total := sum(cor_branca, cor_amarela, cor_indigena, cor_negra),  by=id_setor]
    setor[,  ":="(cor_b_prop = cor_branca/pop_total,
                  cor_a_prop = cor_amarela/pop_total,
                  cor_i_prop = cor_indigena/pop_total,
                  cor_n_prop = cor_negra/pop_total,
                  idade_1_prop = idade_0a5/pop_total,
                  idade_2_prop = idade_6a14/pop_total,
                  idade_3_prop = idade_15a18/pop_total,
                  idade_4_prop = idade_19a24/pop_total,
                  idade_5_prop = idade_25a39/pop_total,
                  idade_6_prop = idade_40a69/pop_total,
                  idade_7_prop = idade_70/pop_total
                  ), 
          
          by=id_setor]
    
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
      dplyr::mutate(negra_pedaco = cor_n_prop * area_prop_grade * pop_total) %>%
      
      # Calcular proporcionais para idade
      dplyr::mutate(idade_1_pedaco = idade_1_prop * area_prop_grade * pop_total) %>%
      dplyr::mutate(idade_2_pedaco = idade_2_prop * area_prop_grade * pop_total) %>%
      dplyr::mutate(idade_3_pedaco = idade_3_prop * area_prop_grade * pop_total) %>%
      dplyr::mutate(idade_4_pedaco = idade_4_prop * area_prop_grade * pop_total) %>%
      dplyr::mutate(idade_5_pedaco = idade_5_prop * area_prop_grade * pop_total) %>%
      dplyr::mutate(idade_6_pedaco = idade_6_prop * area_prop_grade * pop_total) %>%
      dplyr::mutate(idade_7_pedaco = idade_7_prop * area_prop_grade * pop_total)
    
    # Grand Finale (uniao dos pedacos) - Agrupar por grade e somar a renda
    ui_fim <- ui %>%
      st_set_geometry(NULL) %>%
      group_by(id_grade, pop_total, pop_homens, pop_mulheres) %>%
      dplyr::summarise(renda = sum(renda_pedaco, na.rm = TRUE),
                       cor_branca = as.numeric(sum(branca_pedaco, na.rm = TRUE)),
                       cor_amarela = as.numeric(sum(amarela_pedaco, na.rm = TRUE)),
                       cor_indigena = as.numeric(sum(indigena_pedaco, na.rm = TRUE)),
                       cor_negra = as.numeric(sum(negra_pedaco, na.rm = TRUE)),
                       # para idade
                       idade_0a5   = as.numeric(sum(idade_1_pedaco, na.rm = TRUE)),
                       idade_6a14  = as.numeric(sum(idade_2_pedaco, na.rm = TRUE)),
                       idade_15a18 = as.numeric(sum(idade_3_pedaco, na.rm = TRUE)),
                       idade_19a24 = as.numeric(sum(idade_4_pedaco, na.rm = TRUE)),
                       idade_25a39 = as.numeric(sum(idade_5_pedaco, na.rm = TRUE)),
                       idade_40a69 = as.numeric(sum(idade_6_pedaco, na.rm = TRUE)),
                       idade_70    = as.numeric(sum(idade_7_pedaco, na.rm = TRUE))
                       ) %>%
      dplyr::mutate(renda = as.numeric(renda)) %>%
      ungroup()
    
    ui_fim_sf <- grade_corrigida %>%
      dplyr::select(id_grade) %>%
      left_join(ui_fim, by = "id_grade") %>%
      # arredodandar valores
      mutate_at(vars(matches("pop|renda|cor|idade")), round)
    
    
    # Salvar em disco
    path_out <- sprintf("../../data/acesso_oport/grade_municipio_com_renda_cor/%s/grade_renda_cor_%s_%s.rds", ano, sigla_muni, ano)
    readr::write_rds(ui_fim_sf, path_out)
    
  }
  
  #### Aplicando funcao em paralelo para salvar grades com info de renda ---------------------------------------------------------------
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  future::plan(future::multiprocess)
  future.apply::future_lapply(X = x, FUN=renda_de_setor_p_grade_muni, future.packages=c('sf', 'dplyr'))
  
}


# aplicar funcao ------------------------------------------------------------------------------
renda_de_setor_p_grade(ano = 2017)
renda_de_setor_p_grade(ano = 2018)
renda_de_setor_p_grade(ano = 2019)


