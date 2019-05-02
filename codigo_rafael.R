options(scipen=999)
ui_df <- copy(ui) 
setDT(ui_df)


# calcular pop da grade em cada setor
ui_df[, pop_grade_in_setor := as.numeric(pop_total * area_prop_grade)]
summary(ui_df$pop_grade_in_setor)
class(ui_df$pop_grade_in_setor)


# exclui pares de grade-setor com pop zero na intersecao
ui_df <- filter(ui_df, pop_grade_in_setor > 0)



# qual eh a proporocao da pop/area do setor que cai na grade


setDT(ui_df)[, pop_total_setor := sum(pop_grade_in_setor), by = id_setor]
setDT(ui_df)[, pro_popgrade_in_setor := pop_grade_in_setor /pop_total_setor, by = id_grade]
summary(ui_df$pro_popgrade_in_setor)





# distribui renda dos setores para cada grade de acordo com proporcao da pop/area do setor que cai na grade
setDT(ui_df)[, renda_setor_in_grade := renda_total[1L] * pro_popgrade_in_setor, by = id_grade]

summary(ui_df$renda_setor_in_grade)







# TESTE de validade
test <- ui_df[, .(rendatotal_grade = as.numeric(sum(renda_setor_in_grade, na.rm=T))), by = id_grade]  

class(test$rendatotal_grade)
sum(test$rendatotal_grade)


reda_total_dos_setores <- 
reda_total_das_grades <- 
  
  
  
  
reda_total_dos_setores == reda_total_das_grades 