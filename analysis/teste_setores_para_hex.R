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
    mutate(area_total = st_area(.)) %>%
    dplyr::select(id_setor, renda_total, moradores_total, area_total)
  
  # abrir grade
  grade <- read_rds(grade_path) %>%
    dplyr::select(id_grade, pop_total = POP)
  # mutate(pop_total = ifelse(is.na(pop_total), 0, pop_total))
  
  
  # # abrir grade
  # grade <- read_rds(grade_path) %>%
  #   dplyr::select(id_grade, pop_total = POP)
  # # mutate(pop_total = ifelse(is.na(pop_total), 0, pop_total))
  # 
  # # quais sao os setores que incorporam completamente mais de uma grade?
  # aaah <- st_contains_properly(setor, grade) %>% as.list()
  # names(aaah) <- 1:length(aaah) 
  # 
  # que <- data.frame(id_setor = rep(names(aaah), map_dbl(aaah, length)),
  #                   id_grade = unlist(aaah[c(which(lengths(aaah) > 0))]))
  # 
  # 
  # setor_novo <- setor %>%
  #   filter(id_setor %in% que$id_setor)
  
  # agora comeca tudo
  
  ui <- st_intersection(grade, setor) %>%
    # tip from https://rpubs.com/rural_gis/255550
    mutate(area = st_area(.)) %>%
    mutate(prop_area = area/area_total) %>%
    mutate(pop_prop_grade = prop_area * pop_total) %>%
    # mutate(prop_pop_setor = prop_area * moradores_total)
    group_by(id_setor) %>%
    mutate(sum = sum(pop_prop_grade)) %>%
    ungroup() %>%
    mutate(prop_pop_grade_v1 =  prop_pop_grade/sum)
  
  
  # distribuir moradores e renda proporcionalmente a area
  ui_fim <- ui %>%
    group_by(id_grade) %>%
    summarise(renda_prop = sum(renda_total * prop_area * prop_pop_grade_v1, na.rm = TRUE)) %>%
    mutate(renda_capta_prop = renda_prop / pop_total)
    
  
  
  
}


ui_fim %>% 
  # filter(id_grade == 717753) %>%
  mapview(zcol = "renda_prop")
