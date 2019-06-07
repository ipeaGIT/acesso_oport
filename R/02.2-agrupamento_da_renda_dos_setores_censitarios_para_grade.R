#' ## Agrupamento da renda dos setores censitários para a grade do censo
#' 
#' A grade do censo, tratada na etapa anterior, apresenta somente informação de população (homens e mulheres) por cada uma das entidades. Há então a necessidade de incorporar a variável da renda daquela grade. Para isso, será utilizada a informação de renda de cada setor censitário.
#' 
## ----renda_de_setor_para_grade-------------------------------------------

# cidade <- "bel"

renda_de_setor_p_grade <- function(cidade) {
  
  path_setor <- sprintf("../data/setores_agregados/setores_agregados_%s.rds", cidade)
  path_grade <- sprintf("../data/grade_municipio/grade_%s.rds", cidade)
  
  setor <- read_rds(path_setor)
  grade <- read_rds(path_grade)
  
    # abrir setores
  setor <- setor %>%
    mutate(id_setor = 1:n()) %>%
    mutate(area_setor = st_area(.)) %>%
    dplyr::select(id_setor, renda_total, area_setor)
  
  # abrir grade
  grade <- grade %>%
    mutate(area_grade = st_area(.)) %>%
    mutate(id_grade = 1:n()) %>%
    dplyr::select(id_grade, pop_total = POP, area_grade)
  
  ui <- st_intersection(grade, setor) %>%
    # tip from https://rpubs.com/rural_gis/255550
    # Calcular a area de cada pedaco
    mutate(area_pedaco = st_area(.)) %>%
    # Calcular a proporcao de cada setor que esta naquele pedaco (essa sera a area a ponderar pela renda)
    mutate(area_prop_setor = area_pedaco/area_setor) %>%
    # Calcular a proporcao de cada grade que esta naquele pedacao
    mutate(area_prop_grade =  area_pedaco/area_grade) %>%
    # Calcular a quantidade de populacao em cada pedaco (baseado na grade)
    mutate(pop_prop_grade = pop_total * area_prop_grade) %>%
    # Calcular a proporcao de populacao de cada grade que esta dentro do setor
    group_by(id_setor) %>%
    mutate(sum = sum(pop_prop_grade)) %>%
    ungroup() %>%
    # Calcular a populacao proporcional de cada pedaco dentro do setor
    mutate(pop_prop_grade_no_setor =  pop_prop_grade/sum) %>%
    # Calcular a renda dentro de cada pedaco
    mutate(renda_pedaco = renda_total* pop_prop_grade_no_setor)
  
  # Grand Finale
  ui_fim <- ui %>%
    # Agrupar por grade e somar a renda
    group_by(id_grade, pop_total) %>%
    summarise(renda = sum(renda_pedaco, na.rm = TRUE)) %>%
    mutate(renda_capta = renda/pop_total)
  
  path_out <- sprintf("../data/grade_municipio_com_renda/grade_renda_%s.rds", cidade)
  
  # Salvar em disco
  write_rds(ui_fim, path_out)
  
}

# Aplicar funcao
renda_de_setor_p_grade("for")
renda_de_setor_p_grade("bel")
renda_de_setor_p_grade("rio")
renda_de_setor_p_grade("por")
renda_de_setor_p_grade("cur")
renda_de_setor_p_grade("sao")





#' 
#' 
