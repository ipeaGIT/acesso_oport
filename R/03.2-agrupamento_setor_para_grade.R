# Agrega informacao ddos setores censitários (renda, idade) para a grade estatistica

# carregar bibliotecas
sf::sf_use_s2(FALSE)
source('./R/fun/setup.R')

#' A funcao `renda_de_setor_p_grade` passa todas as variaveis que foram coletadas
#' dos setores censitarios para as grades estatisticas  

renda_de_setor_p_grade <- function(ano, munis = "all") {
  
  
  renda_de_setor_p_grade_muni <- function(sigla_muni) {
    
    # sigla_muni <- 'nat'; ano <- 2019
    # sigla_muni <- 'for'; ano <- 2017
    # sigla_muni <- 'rio'; ano <- 2018
    
    # status message
    message('Woking on city ', sigla_muni, '\n')
    
    # endereco dos arquivos
    path_setor <- sprintf("../../data/acesso_oport/setores_agregados/%s/setores_agregados_%s_%s.rds", ano, sigla_muni, ano)
    path_grade <- sprintf("../../data-raw/grade_municipio/%s/grade_%s_%s.rds", ano, sigla_muni, ano)
    
    # leitura de shapes de setores censitarios e grade estatistica
    setor <- readr::read_rds(path_setor)
    grade <- readr::read_rds(path_grade)
    
    # Drop grades vazias
    grade <- subset(grade, POP >0)
    
    # mesma projecao
    # setor <- sf::st_transform(setor, crs = 31984)
    # grade <- sf::st_transform(grade, crs = 31984)
    setor <- sf::st_transform(setor, sf::st_crs(grade))
    
    setor_junto <- st_union(setor)
    
    # precision
    # grade <- grade %>% st_set_precision(units::set_units(10, nm))
    # setor <- setor %>% st_set_precision(units::set_units(10, nm))
    
    # Criar id unico de cada grade e filtra colunas
    grade$id_grade <- 1:nrow(grade)
    
    # corrigir grades de borda
    # cortar as grades da borda e tira rebarbas
    # e divide a grade segundo recorte dos setores
    grade_corrigida <- grade %>%
      mutate(area_antes = as.numeric(st_area(.))) %>%
      st_intersection(setor %>% dplyr::select(code_tract)) %>%
      group_by(id_grade) %>%
      summarise(pop_total = first(POP),
                pop_homens = first(MASC),
                pop_mulheres = first(FEM),
                area_antes = first(area_antes))

    # corrigir populacao das grades de borda que foram cortadas (porque parte da grade
    # cai fora do municipio)
    grade_corrigida <- grade_corrigida %>%
      mutate(area_depois = as.numeric(st_area(.))) %>%
      mutate(prop = area_depois/area_antes) %>%
      mutate(prop_homens = pop_homens/pop_total,
             prop_mulheres = pop_mulheres/pop_total) %>%
      mutate(pop_total =    prop * pop_total) %>%
      mutate(pop_homens = pop_total * prop_homens,
             pop_mulheres = pop_total * prop_mulheres)


    # Seleciona colunas da GRADE
    grade_corrigida <- grade_corrigida %>%
      rename(area_grade = area_depois) %>%
      dplyr::select(id_grade, pop_total, pop_homens, pop_mulheres, area_grade)

    
    # Criar id unico de cada setor e filtra colunas DO SETOR
    # calcula area de cada setor
    setor <- setor %>%
      mutate(id_setor = 1:n()) %>%
      mutate(area_setor = as.numeric(st_area(.))) %>%
      dplyr::select(id_setor, renda_total, area_setor, 
                    # domicilios por renda
                    matches("moradores_SM"),
                    # cores
                    matches("cor_"),
                    # idade
                    matches("idade")
      )
    
    
    # agrega cor negra
    setDT(setor)[, cor_negra := cor_preta + cor_parda ]
    setor[, c('cor_preta', 'cor_parda') := NULL]
    
    
    
    
    # Calcular a proporcao que cada cor em cada setor censitario
    # aqui, eh calculada a proporcao que cada segmento de populacao tem em relacao
    # a populacao dentro do proprio setor
    # a variavel renda total nao esta aqui pq ela ja representa o total da renda,
    # nao sendo segmentada
    setDT(setor)[,  pop_total := sum(cor_branca, cor_amarela, cor_indigena, cor_negra),  by=id_setor]
    setor[,  ":="(
      # renda
      moradores_SM_0_1Q = moradores_SM_0_1Q/pop_total,
      moradores_SM_1Q_1M = moradores_SM_1Q_1M/pop_total,
      moradores_SM_1M_1 = moradores_SM_1M_1/pop_total,
      moradores_SM_1_2 = moradores_SM_1_2/pop_total,
      moradores_SM_2 = moradores_SM_2/pop_total,
      
      # cor
      cor_b_prop = cor_branca/pop_total,
      cor_a_prop = cor_amarela/pop_total,
      cor_i_prop = cor_indigena/pop_total,
      cor_n_prop = cor_negra/pop_total,
      
      # idade
      idade_1_prop = idade_0a5/pop_total,
      idade_2_prop = idade_6a14/pop_total,
      idade_3_prop = idade_15a18/pop_total,
      idade_4_prop = idade_19a24/pop_total,
      idade_5_prop = idade_25a39/pop_total,
      idade_6_prop = idade_40a69/pop_total,
      idade_7_prop = idade_70/pop_total
    ), 
    
    by=id_setor]
    
    setor$pop_total <- NULL
    
    # volta para sf
    setor <- st_sf(setor)
    head(setor)
    
    
    # funcao de reaportion com duas variaveis de referencia (populacao e area)
    # Resultado (ui_fim) eh uma grade estatistica com informacao de renda inputada a partir do setor censitario
    ### aplicacao para renda --------------------------
    ui <- sf::st_intersection(grade_corrigida, setor) %>%
      # tip from https://rpubs.com/rural_gis/255550
      
      # Calcular a area de cada pedaco
      dplyr::mutate(area_pedaco = as.numeric(st_area(.))) %>%
      
      # Calcular a proporcao de cada setor que esta naquele pedaco (essa sera a area a ponderar pela renda)
      dplyr::mutate(area_prop_setor = area_pedaco/area_setor) %>%
      
      # Calcular a proporcao de cada grade que esta naquele pedacao
      dplyr::mutate(area_prop_grade =  area_pedaco/area_grade) %>%
      
      # Calcular a quantidade (absoluto) de populacao em cada pedaco (baseado na grade)
      dplyr::mutate(pop_sub_grade = as.numeric(pop_total * area_prop_grade)) %>%
      
      # Calcular a populacao do setor somando-se a pop das grades
      # necessario pq populacao da grade nao bate 100% com pop do setor
      group_by(id_setor) %>%
      dplyr::mutate(total_pop_setor = sum(pop_sub_grade, na.rm = TRUE)) %>%
      ungroup() %>%
      
      # Calcular a populacao proporcional de cada pedaco dentro do setor
      dplyr::mutate(pop_prop_grade_no_setor =  pop_sub_grade/total_pop_setor) %>%
      
      # Calcular a renda dentro de cada pedaco
      # assume que renda do setor eh distribuida igualmente para cada pessoa dentro do setor
      dplyr::mutate(renda_pedaco = renda_total * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_0_1Q_pedaco = moradores_SM_0_1Q * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_1Q_1M_pedaco = moradores_SM_1Q_1M * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_1M_1_pedaco = moradores_SM_1M_1 * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_1_2_pedaco = moradores_SM_1_2 * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_2_pedaco = moradores_SM_2 * pop_prop_grade_no_setor) %>%
      
      # Calcular cor/raca dentro de cada pedaco
      # como essas variaveis estao agora como proporcoes
      dplyr::mutate(branca_pedaco = cor_b_prop * pop_sub_grade) %>%
      dplyr::mutate(amarela_pedaco = cor_a_prop * pop_sub_grade) %>%
      dplyr::mutate(indigena_pedaco = cor_i_prop * pop_sub_grade) %>%
      dplyr::mutate(negra_pedaco = cor_n_prop * pop_sub_grade) %>%
      
      ## exemplo visual para entender o que esta sendo feito
      # subset(ui, id_grade %in% c(1306) ) %>% select(., id_grade, cor_b_prop, area_prop_grade, pop_total)
      
      # Calcular proporcionais para idade
      dplyr::mutate(idade_1_pedaco = idade_1_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_2_pedaco = idade_2_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_3_pedaco = idade_3_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_4_pedaco = idade_4_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_5_pedaco = idade_5_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_6_pedaco = idade_6_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_7_pedaco = idade_7_prop * pop_sub_grade)
    
    # # grades p/ teste: 2229 (problematica), 2230 (ok)
    # a1 <- ui %>% filter(id_grade == 2229)
    # a2 <- ui %>% filter(id_grade == 2230)
    # mapview(a1)
    
    # Grand Finale (uniao dos pedacos) - Agrupar por grade e somar a renda
    ui_fim <- ui %>% st_set_geometry(NULL) %>% setDT()
    
    ui_fim <- ui_fim[, .( 
      # renda
      renda = sum(renda_pedaco, na.rm = TRUE),
      # moradores_SM_0_1Q = sum(moradores_SM_0_1Q_pedaco, na.rm = TRUE),
      # moradores_SM_1Q_1M = sum(moradores_SM_1Q_1M_pedaco, na.rm = TRUE),
      # moradores_SM_1M_1 = sum(moradores_SM_1M_1_pedaco, na.rm = TRUE),
      # moradores_SM_1_2 = sum(moradores_SM_1_2_pedaco, na.rm = TRUE),
      # moradores_SM_2 = sum(moradores_SM_2_pedaco, na.rm = TRUE),
      # cor
      cor_branca =   sum(branca_pedaco, na.rm = TRUE),
      cor_amarela =  sum(amarela_pedaco, na.rm = TRUE),
      cor_indigena = sum(indigena_pedaco, na.rm = TRUE),
      cor_negra =    sum(negra_pedaco, na.rm = TRUE),
      # para idade
      idade_0a5   = sum(idade_1_pedaco, na.rm = TRUE),
      idade_6a14  = sum(idade_2_pedaco, na.rm = TRUE),
      idade_15a18 = sum(idade_3_pedaco, na.rm = TRUE),
      idade_19a24 = sum(idade_4_pedaco, na.rm = TRUE),
      idade_25a39 = sum(idade_5_pedaco, na.rm = TRUE),
      idade_40a69 = sum(idade_6_pedaco, na.rm = TRUE),
      idade_70    = sum(idade_7_pedaco, na.rm = TRUE)),
      by = .(id_grade, pop_total, pop_homens, pop_mulheres)]
    
    ui_fim[, renda := as.numeric(renda)]
    
    # sum(grade_corrigida$pop_total) 
    # sum(grade_corrigida$pop_homens)+ sum(grade_corrigida$pop_mulheres) 
    # sum(ui_fim$pop_total)
    # sum(ui_fim$pop_homens)+ sum(ui_fim$pop_mulheres) 
    # a soma das cores sum(ui_fim$cor_branca, ui_fim$cor_amarela, ui_fim$cor_indigena, ui_fim$cor_negra) 
    # nao esta batendo com sum(ui_fim$pop_total)
    # a soma das idades sum(ui_fim$idade_0a5, ui_fim$idade_6a14, ui_fim$idade_15a18, ui_fim$idade_19a24, ui_fim$idade_25a39, ui_fim$idade_40a69, ui_fim$idade_70) 
    # nao esta batendo com sum(ui_fim$pop_total)
    # corrigir
    
    # ha casos em que pop_total != 0 e a soma das cores ou idades eh igual a 0. corrigir
    # a1 <- ui_fim[pop_total > 0 & idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70 == 0]
    # a2 <- ui_fim[pop_total > 0 & cor_branca + cor_amarela + cor_indigena + cor_negra == 0]
    # a1[, pop_idade := cor_branca + cor_amarela + cor_indigena + cor_negra]
    # a1[, pop_idade := idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70]
    
    ui_fim[, pop_total := fifelse(pop_total > 0 & idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70 == 0, 0, pop_total)]
    ui_fim[, pop_total := fifelse(pop_total > 0 & cor_branca + cor_amarela + cor_indigena + cor_negra == 0, 0, pop_total)]
    ui_fim[, pop_homens := fifelse(pop_total == 0, 0, pop_homens)]
    ui_fim[, pop_mulheres := fifelse(pop_total == 0, 0, pop_mulheres)]
    ui_fim[, renda := fifelse(pop_total == 0, 0, renda)]
    
    # ha casos em que pop_total == 0 e a soma das cores ou idades eh maior que 0. corrigir
    # b1 <- ui_fim[pop_total == 0 & idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70 > 0]
    # b2 <- ui_fim[pop_total == 0 & cor_branca + cor_amarela + cor_indigena + cor_negra > 0]
    # ui_fim[, ":="(cor_branca =   fifelse(pop_total == 0 & cor_branca != 0, 0, cor_branca),
    #               cor_amarela =  fifelse(pop_total == 0 & cor_amarela != 0, 0, cor_amarela),
    #               cor_indigena = fifelse(pop_total == 0 & cor_indigena != 0, 0, cor_indigena),
    #               cor_negra =    fifelse(pop_total == 0 & cor_negra != 0, 0, cor_negra),
    #               idade_0a5 =   fifelse(pop_total == 0 & idade_0a5 != 0, 0, idade_0a5),
    #               idade_6a14 =  fifelse(pop_total == 0 & idade_6a14 != 0, 0, idade_6a14),
    #               idade_15a18 = fifelse(pop_total == 0 & idade_15a18 != 0, 0, idade_15a18),
    #               idade_19a24 = fifelse(pop_total == 0 & idade_19a24 != 0, 0, idade_19a24),
    #               idade_25a39 = fifelse(pop_total == 0 & idade_25a39 != 0, 0, idade_25a39),
    #               idade_40a69 = fifelse(pop_total == 0 & idade_40a69 != 0, 0, idade_40a69),
    #               idade_70 =    fifelse(pop_total == 0 & idade_70 != 0, 0, idade_70)
    # )]
    
    # o shape do setores nao se encaixam perfeitamente - ha muitos vazios e sobreposicao.
    # isso de alguma forma prejudica a interpolacao das variaveis, fazendo com que a soma
    # das pops por cores e das idades muitas vezes nao seja igual a pop_total - principalmente
    # em casos em que ha algum vazio ou sobreposicao na intersecao entre as grades
    # e os setores.
    ui_fim[, ":="(prop_branca   = cor_branca/    (cor_branca + cor_amarela + cor_indigena + cor_negra),
                  prop_amarela  = cor_amarela/   (cor_branca + cor_amarela + cor_indigena + cor_negra),
                  prop_indigena = cor_indigena/  (cor_branca + cor_amarela + cor_indigena + cor_negra),
                  prop_negra    = cor_negra/     (cor_branca + cor_amarela + cor_indigena + cor_negra),
                  prop_idade_0a5   = idade_0a5/  (idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70),
                  prop_idade_6a14  = idade_6a14/ (idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70),
                  prop_idade_15a18 = idade_15a18/(idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70),
                  prop_idade_19a24 = idade_19a24/(idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70),
                  prop_idade_25a39 = idade_25a39/(idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70),
                  prop_idade_40a69 = idade_40a69/(idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70),
                  prop_idade_70    = idade_70/   (idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70)
                  )]
    
    # quando eh 0/0, o resultado da como NaN. corrigir
    ui_fim[, ":="(prop_branca =   fifelse(is.nan(prop_branca), 0, prop_branca),
                  prop_amarela =  fifelse(is.nan(prop_amarela), 0, prop_amarela),
                  prop_indigena = fifelse(is.nan(prop_indigena), 0, prop_indigena),
                  prop_negra =    fifelse(is.nan(prop_negra), 0, prop_negra),
                  prop_idade_0a5 =    fifelse(is.nan(prop_idade_0a5), 0, prop_idade_0a5),
                  prop_idade_6a14 =   fifelse(is.nan(prop_idade_6a14), 0, prop_idade_6a14),
                  prop_idade_15a18 =  fifelse(is.nan(prop_idade_15a18), 0, prop_idade_15a18),
                  prop_idade_19a24 =  fifelse(is.nan(prop_idade_19a24), 0, prop_idade_19a24),
                  prop_idade_25a39 =  fifelse(is.nan(prop_idade_25a39), 0, prop_idade_25a39),
                  prop_idade_40a69 =  fifelse(is.nan(prop_idade_40a69), 0, prop_idade_40a69),
                  prop_idade_70 =     fifelse(is.nan(prop_idade_70), 0, prop_idade_70)
                  )]
    
    ui_fim[, ":="(cor_branca =   prop_branca * pop_total,
                  cor_amarela =  prop_amarela * pop_total,
                  cor_indigena = prop_indigena * pop_total,
                  cor_negra =    prop_negra * pop_total,
                  idade_0a5 =    prop_idade_0a5 * pop_total,
                  idade_6a14 =   prop_idade_6a14 * pop_total,
                  idade_15a18 =  prop_idade_15a18 * pop_total,
                  idade_19a24 =  prop_idade_19a24 * pop_total,
                  idade_25a39 =  prop_idade_25a39 * pop_total,
                  idade_40a69 =  prop_idade_40a69 * pop_total,
                  idade_70 =     prop_idade_70 * pop_total
                  )]
    
    
    # delete grade without pop
    ui_fim <- ui_fim[pop_total > 0]
    
    # exlucir variaeveis de proporcao
    ui_fim <- ui_fim %>% select(-starts_with("prop_"))
    
    
    
    # finalizar trazendo as geometrias    
    ui_fim_sf <- grade_corrigida %>%
      dplyr::select(id_grade) %>%
      left_join(ui_fim, by = "id_grade")
      # arredodandar valores
      # mutate_at(vars(matches("pop|renda|moradores|cor|idade")), round)
    
    
    
    # # Renomear as colunas 
    # ui_fim_sf <- ui %>%
    #   dplyr::select(
    #     id_grade, pop_total, pop_homens, pop_mulheres,
    #     # renda
    #     renda = renda_pedaco,
    #     # moradores_SM_0_1Q = moradores_SM_0_1Q_pedaco,
    #     # moradores_SM_1Q_1M = moradores_SM_1Q_1M_pedaco,
    #     # moradores_SM_1M_1 = moradores_SM_1M_1_pedaco,
    #     # moradores_SM_1_2 = moradores_SM_1_2_pedaco,
    #     # moradores_SM_2 = moradores_SM_2_pedaco,
    # 
    #     # cor
    #     cor_branca = branca_pedaco,
    #     cor_amarela = amarela_pedaco,
    #     cor_indigena = indigena_pedaco,
    #     cor_negra = negra_pedaco,
    # 
    #     # para idade
    #     idade_0a5   = idade_1_pedaco,
    #     idade_6a14  = idade_2_pedaco,
    #     idade_15a18 = idade_3_pedaco,
    #     idade_19a24 = idade_4_pedaco,
    #     idade_25a39 = idade_5_pedaco,
    #     idade_40a69 = idade_6_pedaco,
    #     idade_70    = idade_7_pedaco
    #   )
    
    # Salvar em disco
    path_out <- sprintf("../../data/acesso_oport/grade_municipio_com_renda_cor/%s/grade_renda_cor_%s_%s.rds", ano, sigla_muni, ano)
    readr::write_rds(ui_fim_sf, path_out)
    
  }
  
  #### Aplicando funcao em paralelo para salvar grades com info de renda ---------------------------------------------------------------
  if (munis == "all") {
    
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  # plan(multiprocess, workers = 10)
  # furrr::future_walk(x, renda_de_setor_p_grade_muni)
  walk(x, renda_de_setor_p_grade_muni)
  
}


# aplicar funcao ------------------------------------------------------------------------------
# renda_de_setor_p_grade(ano = 2017)
renda_de_setor_p_grade(ano = 2018)
renda_de_setor_p_grade(ano = 2019)


