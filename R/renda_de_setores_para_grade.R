

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