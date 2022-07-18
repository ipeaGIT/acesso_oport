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
    # sigla_muni <- 'rio'; ano <- 2017
    # sigla_muni <- 'bho'; ano <- 2017
    # sigla_muni <- 'cam'; ano <- 2019
    # sigla_muni <- 'gua'; ano <- 2017
    # sigla_muni <- 'sal'; ano <- 2017
    
    # status message
    message('Woking on city ', sigla_muni, '\n')
    
    # endereco dos arquivos
    path_setor <- sprintf("../../data/acesso_oport/setores_agregados/%s/setores_agregados_%s_%s.rds", ano, sigla_muni, ano)
    path_grade <- sprintf("../../data-raw/grade_municipio/%s/grade_%s_%s.rds", ano, sigla_muni, ano)
    
    # leitura de shapes de setores censitarios e grade estatistica
    setor <- readr::read_rds(path_setor)
    grade <- readr::read_rds(path_grade)
    
    
    # valid
    setor <- st_make_valid(setor) # %>% st_set_precision(1) 
    grade <- st_make_valid(grade) # %>% st_set_precision(1) 
    
    # Drop grades vazias e setores vazios
    grade <- subset(grade, POP >0)
    setor <- subset(setor, cor_branca + cor_amarela + cor_preta + cor_parda + cor_indigena > 0)
    # mapview(grade, zcol = "POP")
    
    
    # # DELETE OVERLAP FROM SECTORS
    setor <- st_difference(setor)
    # # acontecia overlap entre os setores, o que acabava prejudicando a
    # # reagregacao espacial - a populacao total (da grade) acabava
    # # nao sendo a soma da populacao por cor/idade
    # sum(setor$moradores_total) # 1073872
    # setor <- setor %>%
    #   # identificar setores temporariamente
    #   mutate(id = 1:n()) %>%
    #   # fazer intersecao com ele proprio
    #   st_intersection() %>%
    #   # a coluna 'origins' identifica com quais setores houve intersecao
    #   # vamos transformar essa coluna (q esta como lista) em um vector
    #   mutate(originss = purrr::map_chr(origins, paste0, collapse = ",")) %>%
    #   # filtrar a intersecao de cada setor SOMENTE com si. isso exclui qualquer
    #   # pedacinho de intersecao que acontecer com outro setor que nao seja o proprio,
    #   # garantindo que nao aconteca nenhum overlap
    #   filter(id == originss) %>%
    #   select(-id, -n.overlaps, -origins, -originss)
    # sum(setor$moradores_total) # 1073410
    # # checar se a populacao eh a mesma antes e depois OK
    
    # mesma projecao
    # setor <- sf::st_transform(setor, crs = 31984)
    # grade <- sf::st_transform(grade, crs = 31984)
    setor <- sf::st_transform(setor, sf::st_crs(grade))
    
    # Criar id unico de cada grade e filtra colunas
    grade$id_grade <- 1:nrow(grade)
    
    # outra premissa da reagregacao eh que aconteca uma correspondencia espacial
    # entre a grade e os setores, nao podendo ter grade/setor com pedaco sobrando
    # essa operacao garante que exista uma correspondecia espacial perfeita
    # entre grades e setores
    grade_corrigida <- grade %>%
      # mutate(area_antes = as.numeric(st_area(.))) %>%
      # st_intersection(limits %>% dplyr::select(code_state)) %>%
      st_intersection(setor %>% dplyr::select(code_tract)) %>%
      group_by(id_grade) %>%
      summarise(pop_total = first(POP),
                pop_homens = first(MASC),
                pop_mulheres = first(FEM)) %>%
      mutate(area_grade = st_area(.))
    
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
    ui_fim[, pop_total := round(pop_total)]
    # delete grade without pop
    ui_fim <- ui_fim[pop_total > 0]
    
    # sum(grade_corrigida$pop_total) 
    # sum(grade_corrigida$pop_homens)+ sum(grade_corrigida$pop_mulheres) 
    # sum(ui_fim$pop_total)
    # sum(ui_fim$pop_homens)+ sum(ui_fim$pop_mulheres) 
    # a soma das cores sum(ui_fim$cor_branca, ui_fim$cor_amarela, ui_fim$cor_indigena, ui_fim$cor_negra) 
    # nao esta batendo com sum(ui_fim$pop_total)
    # a soma das idades sum(ui_fim$idade_0a5, ui_fim$idade_6a14, ui_fim$idade_15a18, ui_fim$idade_19a24, ui_fim$idade_25a39, ui_fim$idade_40a69, ui_fim$idade_70) 
    # nao esta batendo com sum(ui_fim$pop_total)
    # corrigir / redistribuir
    
    # distribute
    # identify within each class the hierarquy of proportions (non-zero)
    # vector_sexo <- purrr::pmap(ui_fim[, .(-pop_homens, -pop_mulheres)], 
    #                            c)
    vector_cor <- purrr::pmap(ui_fim[, .(-cor_branca, -cor_amarela, -cor_indigena, -cor_negra)], 
                              c)
    vector_idade <- purrr::pmap(ui_fim[, .(-idade_0a5, -idade_6a14, -idade_15a18, -idade_19a24,-idade_25a39,-idade_40a69,-idade_70)], 
                                c)
    # rank them (highest rank means highest value)
    # rank_sexo <- lapply(vector_sexo, frank, ties.method = "first")
    # rank_sexo <- do.call(rbind, rank_sexo) %>% as.data.frame()
    rank_cor <- lapply(vector_cor, frank, ties.method = "first")
    rank_cor <- do.call(rbind, rank_cor) %>% as.data.frame()
    rank_idade <- lapply(vector_idade, frank, ties.method = "first")
    rank_idade <- do.call(rbind, rank_idade) %>% as.data.frame()
    
    # apply theses rans as columns
    # ui_fim[, ':='(rank_homens = rank_sexo$V1, rank_mulheres = rank_sexo$V2)]
    ui_fim[, ':='(rank_branca   = rank_cor$V1, 
                  rank_amarela  = rank_cor$V2, 
                  rank_indigena = rank_cor$V3, 
                  rank_negra    = rank_cor$V4)]
    ui_fim[, ':='(rank_idade_0a5   = rank_idade$V1, 
                  rank_idade_6a14  = rank_idade$V2, 
                  rank_idade_15a18 = rank_idade$V3, 
                  rank_idade_19a24 = rank_idade$V4,
                  rank_idade_25a39 = rank_idade$V5, 
                  rank_idade_40a69 = rank_idade$V6, 
                  rank_idade_70    = rank_idade$V7
    )]
    
    
    
    # now we will rounbd the variables to calculate the difference
    ui_fim[, ":="(
      # pop_homens = round(pop_homens),
      # pop_mulheres = round(pop_mulheres),
      cor_branca = round(cor_branca),
      cor_amarela = round(cor_amarela),
      cor_indigena = round(cor_indigena),
      cor_negra = round(cor_negra),
      idade_0a5   = round(idade_0a5),
      idade_6a14  = round(idade_6a14),
      idade_15a18 = round(idade_15a18),
      idade_19a24 = round(idade_19a24),
      idade_25a39 = round(idade_25a39),
      idade_40a69 = round(idade_40a69),
      idade_70    = round(idade_70))
    ]
    
    # calculate dif
    # ui_fim[, dif_sexo := pop_total - (pop_homens + pop_mulheres)]
    ui_fim[, dif_cor := pop_total - (cor_branca + cor_amarela + cor_indigena + cor_negra)]
    ui_fim[, dif_idade := pop_total - (idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70)]
    
    summary(ui_fim$dif_cor)
    summary(ui_fim$dif_idade)
    
    # # para rio de janeiro: grade 13688 problematica
    # grade_prob <- ui %>% filter(id_grade == 13688)
    # setores_prob <- setor %>% filter(id_setor %in% grade_prob$id_setor)
    # mapview(grade_prob) + setores_prob
    
    # |> now we have to distribute the difference based on the ranks -----------------
    # divide the number to be distributed by the number of non-zero classes
    # ui_fim[, a1_sexo  := abs(dif_sexo) / 2]
    teste <- function(oi) {
      if (oi > 0)  1 else 0
    }
    
    ui_fim <- ui_fim %>% group_by(id_grade) %>% mutate(across(cor_branca:cor_negra, teste, .names = "class_{.col}")) %>% setDT()
    ui_fim <- ui_fim %>% group_by(id_grade) %>% mutate(across(idade_0a5:idade_70, teste, .names = "class_{.col}")) %>% setDT()
    
    # ui_fim %>% filter(id_grade == 2297) # p/ fortaleza
    
    ui_fim[, classes_cor :=   class_cor_branca + class_cor_amarela + class_cor_indigena + class_cor_negra, by = id_grade]
    ui_fim[, classes_idade := class_idade_0a5 + class_idade_6a14 + class_idade_15a18 + class_idade_19a24 + class_idade_25a39  + class_idade_40a69 + class_idade_70, by = id_grade]
    # se nao tiver nenhuma classe, significa que nao cor e idade eh 0. precisamos de pelo menos uma classe
    ui_fim[, classes_cor :=   ifelse(classes_cor == 0 & dif_cor > 0, 1, classes_cor), by = id_grade]
    ui_fim[, classes_idade :=   ifelse(classes_idade == 0 & dif_idade > 0, 1, classes_idade), by = id_grade]
    
    # create variables with sum of color and age
    ui_fim[, sum_cor := cor_branca + cor_amarela + cor_indigena + cor_negra]
    ui_fim[, sum_idade := idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70]
    
    
    ui_fim[, a1_cor   := abs(dif_cor) / classes_cor]
    ui_fim[, a1_idade := abs(dif_idade) / classes_idade ]
    
    # calculate the number of rounds that is necessary
    # ui_fim[, rounds_sexo := ceiling(a1_sexo)]
    ui_fim[, rounds_cor := ceiling(a1_cor)]
    ui_fim[, rounds_idade := ceiling(a1_idade)]
    
    # determine the position of each rank at each round
    ui_fim[, ':='(
      # rank_homens_new =   ifelse(rounds_sexo > 1, rank_homens   + ((rounds_sexo - 1) * 2), rank_homens), 
      # rank_mulheres_new = ifelse(rounds_sexo > 1, rank_mulheres + ((rounds_sexo - 1) * 2), rank_mulheres),
      rank_branca_new   = ifelse(cor_branca   <= 0 & sum_cor != 0, 0, 
                                 ifelse(rounds_cor > 1, rank_branca   + ((rounds_cor - 1) * classes_cor), rank_branca)),
      rank_amarela_new  = ifelse(cor_amarela  <= 0 & sum_cor != 0, 0, 
                                 ifelse(rounds_cor > 1, rank_amarela  + ((rounds_cor - 1) * classes_cor), rank_amarela)),
      rank_indigena_new = ifelse(cor_indigena <= 0 & sum_cor != 0, 0, 
                                 ifelse(rounds_cor > 1, rank_indigena + ((rounds_cor - 1) * classes_cor), rank_indigena)),
      rank_negra_new    = ifelse(cor_negra    <= 0 & sum_cor != 0, 0, 
                                 ifelse(rounds_cor > 1, rank_negra    + ((rounds_cor - 1) * classes_cor), rank_negra)),
      rank_idade_0a5_new   = ifelse(idade_0a5   <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_0a5   + ((rounds_idade - 1) * classes_idade), rank_idade_0a5)),
      rank_idade_6a14_new  = ifelse(idade_6a14  <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_6a14  + ((rounds_idade - 1) * classes_idade), rank_idade_6a14)),
      rank_idade_15a18_new = ifelse(idade_15a18 <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_15a18 + ((rounds_idade - 1) * classes_idade), rank_idade_15a18)),
      rank_idade_19a24_new = ifelse(idade_19a24 <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_19a24 + ((rounds_idade - 1) * classes_idade), rank_idade_19a24)),
      rank_idade_25a39_new = ifelse(idade_25a39 <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_25a39 + ((rounds_idade - 1) * classes_idade), rank_idade_25a39)),
      rank_idade_40a69_new = ifelse(idade_40a69 <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_40a69 + ((rounds_idade - 1) * classes_idade), rank_idade_40a69)),
      rank_idade_70_new    = ifelse(idade_70    <= 0 & sum_idade != 0, 0,
                                    ifelse(rounds_idade > 1, rank_idade_70    + ((rounds_idade - 1) * classes_idade), rank_idade_70))
    )]
    
    # ui_fim %>% filter(id_grade == 2297) # p/ fortaleza
    
    
    ui_fim[, ':='(
      # dif_homens =   ifelse(rank_homens_new   <= (abs(dif_sexo)), ceiling(a1_sexo), floor(a1_sexo)),
      # dif_mulheres = ifelse(rank_mulheres_new <= (abs(dif_sexo)), ceiling(a1_sexo), floor(a1_sexo)),
      dif_branca   = ifelse(cor_branca   <= 0 & sum_cor != 0, 0, ifelse(rank_branca_new   <= (abs(dif_cor)),  ceiling(a1_cor), floor(a1_cor))),
      dif_amarela  = ifelse(cor_amarela  <= 0 & sum_cor != 0, 0, ifelse(rank_amarela_new  <= (abs(dif_cor)),  ceiling(a1_cor), floor(a1_cor))),
      dif_indigena = ifelse(cor_indigena <= 0 & sum_cor != 0, 0, ifelse(rank_indigena_new <= (abs(dif_cor)),  ceiling(a1_cor), floor(a1_cor))),
      dif_negra    = ifelse(cor_negra    <= 0 & sum_cor != 0, 0, ifelse(rank_negra_new    <= (abs(dif_cor)),  ceiling(a1_cor), floor(a1_cor))),
      dif_idade_0a5   = ifelse(idade_0a5   <= 0 & sum_idade != 0, 0, ifelse(rank_idade_0a5_new   <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade))),
      dif_idade_6a14  = ifelse(idade_6a14  <= 0 & sum_idade != 0, 0, ifelse(rank_idade_6a14_new  <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade))),
      dif_idade_15a18 = ifelse(idade_15a18 <= 0 & sum_idade != 0, 0, ifelse(rank_idade_15a18_new <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade))),
      dif_idade_19a24 = ifelse(idade_19a24 <= 0 & sum_idade != 0, 0, ifelse(rank_idade_19a24_new <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade))),
      dif_idade_25a39 = ifelse(idade_25a39 <= 0 & sum_idade != 0, 0, ifelse(rank_idade_25a39_new <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade))),
      dif_idade_40a69 = ifelse(idade_40a69 <= 0 & sum_idade != 0, 0, ifelse(rank_idade_40a69_new <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade))),
      dif_idade_70    = ifelse(idade_70    <= 0 & sum_idade != 0, 0, ifelse(rank_idade_70_new    <= (abs(dif_idade)), ceiling(a1_idade), floor(a1_idade)))
    )]
    
    
    # ui_fim %>% filter(id_grade == 1490 ) %>% View()
    # ui_fim %>% filter(id_grade == 3546 ) %>% View()
    
    # convert to negative if necessary
    ui_fim[, ':='(
      # dif_homens =   ifelse(dif_sexo > 0, dif_homens, -dif_homens),
      # dif_mulheres = ifelse(dif_sexo > 0, dif_mulheres, -dif_mulheres),
      dif_branca   = ifelse(dif_cor    >0, dif_branca, -dif_branca),
      dif_amarela  = ifelse(dif_cor    >0, dif_amarela, -dif_amarela),
      dif_indigena = ifelse(dif_cor    >0, dif_indigena, -dif_indigena),
      dif_negra    = ifelse(dif_cor    >0, dif_negra, -dif_negra),
      dif_idade_0a5   = ifelse(dif_idade   > 0, dif_idade_0a5, -dif_idade_0a5),
      dif_idade_6a14  = ifelse(dif_idade   > 0, dif_idade_6a14, -dif_idade_6a14),
      dif_idade_15a18 = ifelse(dif_idade   > 0, dif_idade_15a18, -dif_idade_15a18),
      dif_idade_19a24 = ifelse(dif_idade   > 0, dif_idade_19a24, -dif_idade_19a24),
      dif_idade_25a39 = ifelse(dif_idade   > 0, dif_idade_25a39, -dif_idade_25a39),
      dif_idade_40a69 = ifelse(dif_idade   > 0, dif_idade_40a69, -dif_idade_40a69),
      dif_idade_70    = ifelse(dif_idade   > 0, dif_idade_70, -dif_idade_70)
    )]
    
    # ui_fim %>% filter(id_grade == 1490 ) %>% View()
    
    # se a soma da cor/idade for zero e o dif for maior que zero, jogar tudo pra uma cateoria so
    ui_fim[, ':='(
      # dif_homens =   ifelse(dif_sexo > 0, dif_homens, -dif_homens),
      # dif_mulheres = ifelse(dif_sexo > 0, dif_mulheres, -dif_mulheres),
      dif_branca   = ifelse(sum_cor    == 0 & dif_cor > 0, dif_cor, dif_branca),
      dif_amarela  = ifelse(sum_cor    == 0 & dif_cor > 0, 0, dif_amarela),
      dif_indigena = ifelse(sum_cor    == 0 & dif_cor > 0, 0, dif_indigena),
      dif_negra    = ifelse(sum_cor    == 0 & dif_cor > 0, 0, dif_negra),
      dif_idade_0a5   = ifelse(sum_idade   == 0 & dif_idade >0, dif_idade, dif_idade_0a5),
      dif_idade_6a14  = ifelse(sum_idade   == 0 & dif_idade >0, 0, dif_idade_6a14),
      dif_idade_15a18 = ifelse(sum_idade   == 0 & dif_idade >0, 0, dif_idade_15a18),
      dif_idade_19a24 = ifelse(sum_idade   == 0 & dif_idade >0, 0, dif_idade_19a24),
      dif_idade_25a39 = ifelse(sum_idade   == 0 & dif_idade >0, 0, dif_idade_25a39),
      dif_idade_40a69 = ifelse(sum_idade   == 0 & dif_idade >0, 0, dif_idade_40a69),
      dif_idade_70    = ifelse(sum_idade   == 0 & dif_idade >0, 0, dif_idade_70)
    )]
    
    
    
    # so add up these variables
    ui_fim[, ":="(
      # pop_homens = pop_homens + dif_homens,
      # pop_mulheres = pop_mulheres + dif_mulheres,
      cor_branca = cor_branca + dif_branca,
      cor_amarela = cor_amarela + dif_amarela,
      cor_indigena = cor_indigena + dif_indigena,
      cor_negra = cor_negra + dif_negra,
      idade_0a5 = idade_0a5 + dif_idade_0a5,
      idade_6a14 = idade_6a14 + dif_idade_6a14,
      idade_15a18 = idade_15a18 + dif_idade_15a18,
      idade_19a24 = idade_19a24 + dif_idade_19a24,
      idade_25a39 = idade_25a39 + dif_idade_25a39,
      idade_40a69 = idade_40a69 + dif_idade_40a69,
      idade_70 = idade_70 + dif_idade_70
      
      
    )]
    
    # ai <- ui_fim %>% filter(id_grade == 2057 )
    # sum(ai$pop_total)
    # sum(ai$cor_branca, ai$cor_amarela, ai$cor_indigena, ai$cor_negra) 
    # sum(ai$idade_0a5, ai$idade_6a14, ai$idade_15a18, ai$idade_19a24, ai$idade_25a39, ai$idade_40a69, ai$idade_70) 
    
    # sum(ui_fim$pop_total)
    # sum(ui_fim$pop_homens)+ sum(ui_fim$pop_mulheres) 
    # sum(ui_fim$cor_branca, ui_fim$cor_amarela, ui_fim$cor_indigena, ui_fim$cor_negra) 
    # sum(ui_fim$idade_0a5, ui_fim$idade_6a14, ui_fim$idade_15a18, ui_fim$idade_19a24, ui_fim$idade_25a39, ui_fim$idade_40a69, ui_fim$idade_70) 
    # ui_fim[pop_total > 0 & cor_branca + cor_amarela + cor_indigena + cor_negra == 0]
    # ui_fim[pop_total > 0 & renda == 0]
    # ui_fim[pop_total != cor_branca + cor_amarela + cor_indigena + cor_negra]
    # ui_fim[pop_total != idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70]
    # median(ui_fim$renda / ui_fim$pop_total)
    # ui_fim[pop_total <= 0 & renda != 0]
    # summary(ui_fim$pop_total)
    
    
    
    
    
    # exlucir variaeveis de proporcao
    ui_fim <- ui_fim %>% select(-starts_with("prop_"))
    ui_fim <- ui_fim %>% select(-starts_with("dif_"))
    ui_fim <- ui_fim %>% select(-starts_with("rank_"))
    ui_fim <- ui_fim %>% select(-starts_with("a1_"))
    ui_fim <- ui_fim %>% select(-starts_with("rounds_"))
    ui_fim <- ui_fim %>% select(-starts_with("classes_"))
    ui_fim <- ui_fim %>% select(-starts_with("sum_"))
    ui_fim <- ui_fim %>% select(-starts_with("class_"))
    
    
    
    # finalizar trazendo as geometrias    
    ui_fim_sf <- grade_corrigida %>%
      dplyr::select(id_grade) %>%
      left_join(ui_fim, by = "id_grade")
    # arredodandar valores
    # mutate_at(vars(matches("pop|renda|moradores|cor|idade")), round)
    
    
    # Salvar em disco
    message("salvando...")
    path_out <- sprintf("../../data/acesso_oport/grade_municipio_com_renda_cor/%s/grade_renda_cor_%s_%s.rds", ano, sigla_muni, ano)
    readr::write_rds(ui_fim_sf, path_out)
    
  }
  
  #### Aplicando funcao em paralelo para salvar grades com info de renda ---------------------------------------------------------------
  if (munis == "all") {
    
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  # plan(multisession, workers = 20)
  # furrr::future_walk(x, renda_de_setor_p_grade_muni)
  walk(x, renda_de_setor_p_grade_muni)
  
}


# aplicar funcao ------------------------------------------------------------------------------
# renda_de_setor_p_grade(ano = 2017)
# renda_de_setor_p_grade(ano = 2018)
# renda_de_setor_p_grade(ano = 2019)


renda_de_setor_p_grade(2019, c("rio", "cur", "poa", "bho", "bsb", "sal", "man", "rec", "goi", "bel", "gua", "cam", "slz", "sgo", "mac", "duq", "cgr", "nat"))

# 
# 
# 
# 
# 
# 
# check_grade <- function(ano) {
#   
#   
#   check_grade_ano <- function(sigla_muni) {
#     
#     
#     # check
#     grade <- read_rds(sprintf("../../data/acesso_oport/grade_municipio_com_renda_cor/%s/grade_renda_cor_%s_%s.rds", ano, sigla_muni, ano))
#     grade <- setDT(grade)
#     
#     # checks
#     var1 <- sum(grade$pop_total, na.rm = TRUE)
#     var2 <- sum(grade$pop_homens, na.rm = TRUE)+ sum(grade$pop_mulheres, na.rm = TRUE) 
#     var3 <- sum(grade$cor_branca, grade$cor_amarela, 
#                 grade$cor_indigena, grade$cor_negra, na.rm = TRUE) 
#     var4 <- sum(grade$idade_0a5, grade$idade_6a14, 
#                 grade$idade_15a18, grade$idade_19a24, 
#                 grade$idade_25a39, grade$idade_40a69, grade$idade_70, na.rm = TRUE) 
#     var5 <- grade[pop_total > 0 & cor_branca + cor_amarela + cor_indigena + cor_negra == 0]
#     var6 <- grade[pop_total != cor_branca + cor_amarela + cor_indigena + cor_negra]
#     var6 <- grade[pop_total != idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70]
#     var7 <- grade[pop_total > 0 & renda == 0]
#     var8 <- median(grade$renda / grade$pop_total)
#     
#     check1 <- if (var1 == var2) "OK" else "NAO OK"
#     check2 <- if (var1 == var3) "OK" else "NAO OK"
#     check3 <- if (var1 == var4) "OK" else "NAO OK"
#     check4 <- if (nrow(var5) == 0) "OK" else "NAO OK"
#     check5 <- if (nrow(var6) == 0) "OK" else "NAO OK"
#     check6 <- if (nrow(var7) == 0) "OK" else "NAO OK"
#     
#     message("Sum pop total == sum by sex: ", check1)
#     message("Sum pop total == sum by color: ", check2)
#     message("Sum pop total == sum by age: ", check3)
#     message("Any case where pop total >0 and sum by color = 0: ", check4)
#     message("Any case where pop total =! sum pop by color: ", check5)
#     message("Any case where pop total > 0 and income = 0: ", check6)
#     message("Median income: ", var8)
#     
#     
#   }
#   
#   walk(munis_list$munis_df$abrev_muni, check_grade_ano)
#   
#   
#   
# }
