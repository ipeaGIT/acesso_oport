setor_path <- '../data/setores_agregados/setores_agregados_ce.rds'
grade_path <- '../data/grade_municipio/grade_for.rds'

setor_para_hex <- function(setor_path, hex_path) {
  
  # abrir setores
  setor <- read_rds(setor_path) %>%
    filter(muni == "FORTALEZA") %>%
    mutate(id_setor = 1:n()) %>%
    mutate(area_total = st_area(.)) %>%
    dplyr::select(id_setor, renda_total, moradores_total, area_total)
  
  # abrir grade
  grade <- read_rds(grade_path) %>%
    dplyr::select(id_grade, pop_total = POP)
    # mutate(pop_total = ifelse(is.na(pop_total), 0, pop_total))
  
  
  ui <- st_intersection(grade, setor) %>%
  # tip from https://rpubs.com/rural_gis/255550
    mutate(area = st_area(.)) %>%
    mutate(prop_area = area/area_total)
  

  # distribuir moradores e renda proporcionalmente a area
  ui_fim <- ui %>%
    group_by(id_grade) %>%
    summarise(moradores_prop = sum(moradores_total * prop_area, na.rm = TRUE),
              renda_prop = sum(renda_total * prop_area, na.rm = TRUE)) %>%
    mutate(renda_capta_prop = renda_prop / moradores_prop)
    
}

ui %>%
  filter(id_setor %in% c(6)) %>%
  mapview()
  

# quais sao os setores que incorporam completamente mais de uma grade?
aaah <- st_contains_properly(setor, grade)



# OUTRA FUNCAO ------------------------------------------------------------

# Essa funcao vai fazer o overlay em duas etapas: primeiros, vamos pegar os setores que sao muito grandes e que incorporam completamente mais de duas quadriculas:

setor_path <- '../data/setores_agregados/setores_agregados_ce.rds'
grade_path <- '../data/grade_municipio/grade_for.rds'


setor_para_hex <- function(setor_path, hex_path) {
  
  # abrir setores
  setor <- read_rds(setor_path) %>%
    filter(muni == "FORTALEZA") %>%
    mutate(id_setor = 1:n()) %>%
    mutate(area_setor = st_area(.)) %>%
    dplyr::select(id_setor, renda_total, area_setor)
  
  # abrir grade
  grade <- read_rds(grade_path) %>%
    mutate(area_grade = st_area(.)) %>%
    mutate(id_grade = 1:n()) %>%
    dplyr::select(id_grade, pop_total = POP, area_grade)
  # mutate(pop_total = ifelse(is.na(pop_total), 0, pop_total))
  
  
  # # abrir grade
  # grade <- read_rds(grade_path) %>%
  #   dplyr::select(id_grade, pop_total = POP)
  # # mutate(pop_total = ifelse(is.na(pop_total), 0, pop_total))
  # 
  # quais sao os setores que incorporam completamente mais de uma grade?
  aaah <- st_contains_properly(setor, grade) %>% as.list()
  names(aaah) <- 1:length(aaah)

  que <- data.frame(id_setor = rep(names(aaah), map_dbl(aaah, length)),
                    id_grade = unlist(aaah[c(which(lengths(aaah) > 0))]))
  # 
  # 
  # setor_novo <- setor %>%
  #   filter(id_setor %in% que$id_setor)
  
  # Para as quadriculas que estao completamente inseridas em um setor (tipo 1):
  
  ui_tipo1 <- grade %>%
    filter(id_grade %in% que$id_grade)
  
  # viz
  mapview(ui_tipo1) + mapview(setor %>% filter(id_setor %in% que$id_setor))
  
  ui_tipo1_v2 <- ui_tipo1 %>%
    st_intersection(setor) %>%
    # tip from https://rpubs.com/rural_gis/255550
    # Calcular a area de cada pedaco
    mutate(area_pedaco = st_area(.)) %>%
    # Calcular a proporcao de cada setor que esta naquele pedaco (essa sera a area a ponderar pela renda)
    mutate(area_prop_setor = area_pedaco/area_setor) %>%
    # # Calcular a proporcao de cada grade que esta naquele pedacao
    # mutate(area_prop_grade =  area_pedaco/area_grade) %>%
    # # Calcular a quantidade de populacao em cada pedaco (baseado na grade)
    # mutate(pop_prop_grade = pop_total * area_prop_grade) %>%
    # Calcular a proporcao de populacao de cada grade que esta dentro do setor
    group_by(id_setor) %>%
    mutate(sum_pop = sum(pop_total)) %>%
    ungroup() %>%
    # Calcular a populacao proporcional de cada pedaco dentro do setor
    mutate(pop_prop_grade =  pop_total/sum_pop) %>%
    mutate(pop_prop_grade = ifelse(is.nan(pop_prop_grade), 0, pop_prop_grade))
    
  
  # distribuir renda proporcionalmente a area e populacao do setor
  ui_tipo1_fim <- ui_tipo1_v2 %>%
    mutate(renda = renda_total * area_prop_setor * pop_prop_grade) %>%
    mutate(renda_capta_prop = renda / pop_total %>% as.numeric) %>%
    select(id_grade, pop_total, renda_total, renda, renda_capta_prop)
  
  
  # Para as quadriculas que sao divididas por um setor (tipo 2):
  `%nin%` = Negate(`%in%`)
  
  ui_tipo2 <- grade %>%
    filter(id_grade %nin% que$id_grade)
  
  ui_tipo2_v2 <- ui_tipo2 %>%
    st_intersection(setor) %>%
    # tip from https://rpubs.com/rural_gis/255550
    # Calcular a area de cada pedaco
    mutate(area_pedaco = st_area(.)) %>%
    # Calcular a proporcao de cada setor que esta naquele pedaco (essa sera a area a ponderar pela renda)
    mutate(area_prop_setor = area_pedaco/area_setor)
    # # Calcular a proporcao de cada grade que esta naquele pedacao
    # mutate(area_prop_grade =  area_pedaco/area_grade) %>%
    # # Calcular a quantidade de populacao em cada pedaco (baseado na grade)
    # mutate(pop_prop_grade = pop_total * area_prop_grade) %>%
    # # Calcular a proporcao de populacao de cada grade que esta dentro do setor
    # group_by(id_setor) %>%
    # mutate(sum_pop = sum(pop_total)) %>%
    # ungroup() %>%
    # # Calcular a populacao proporcional de cada pedaco dentro do setor
    # mutate(pop_prop_grade =  pop_total/sum_pop) %>%
    # mutate(pop_prop_grade = ifelse(is.nan(pop_prop_grade), 0, pop_prop_grade))
  
  
  # distribuir renda proporcionalmente a area e populacao do setor
  ui_tipo2_fim <- ui_tipo2_v2 %>%
    group_by(id_grade, pop_total) %>%
    summarise(renda = sum(renda_total * area_prop_setor, na.rm = TRUE)) %>%
    mutate(renda_capta_prop = renda_prop / pop_total)
  
}


# TESTAR O MEU RESULTADO!!!


# AGORA VAI!!!!!! ---------------------------------------------------------

# DEU CERTO AQUI!! --------------------------------------------------------

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




ui_teste <- filter(ui, id_grade == 709528)
ui_fim_teste <- ui_fim %>% filter(id_grade == 709528)
mapview(ui_teste)

mapview(grade %>% filter(id_grade == 709528))
mapview(setor %>% filter(id_setor == 93))


vai_setor <- setor %>%
  filter(id_setor %in% c(2656, 2855, 2854))

bbox_setor <- st_bbox(vai_setor)

vai_ui <- st_crop(ui_fim, bbox_setor)

mapview(vai_ui) + mapview(vai_setor)


  
  



