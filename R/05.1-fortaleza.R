#' ## Fortaleza
#' 
## ----acess acumulativa for-----------------------------------------------


# Aplicar
# calcular_acess("for")

# Abrir
acess_for <- read_rds("../data/output_access/acess_for.rds")


# for_tile <- annotation_map_tile(data = acess_for$transit)
# Fazer mapa para fortaleza ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_cidade(acess_for, indicador = "TMI", modo = "transit", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "TMI", modo = "walk", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "TMI", modo = "bike", atividade = "ST",
                        salvar = TRUE)

# Para indicador CMA
# transit
fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "transit", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "transit", atividade = "TT",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "transit", atividade = "EI",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "transit", atividade = "EF",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "transit", atividade = "EM",
                        salvar = TRUE)

# walk
fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "walk", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "walk", atividade = "TT",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "walk", atividade = "EI",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "walk", atividade = "EF",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "walk", atividade = "EM",
                        salvar = TRUE)

# bike
fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "bike", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "bike", atividade = "TT",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "bike", atividade = "EI",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "bike", atividade = "EF",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_for, indicador = "CMA", modo = "bike", atividade = "EM",
                        salvar = TRUE)





#' 
