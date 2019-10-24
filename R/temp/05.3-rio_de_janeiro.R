#' ## Rio de Janeiro
#' 
#' 
## ----acess acumulativa rio-----------------------------------------------

# Aplicar
# calcular_acess("rio")

# Abrir
acess_rio <- read_rds("../data/output_access/acess_rio.rds")

# Fazer mapa para rio ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_cidade(acess_rio, indicador = "TMI", modo = "transit", atividade = "ST",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_rio, indicador = "TMI", modo = "walk", atividade = "ST",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_rio, indicador = "TMI", modo = "bike", atividade = "ST",
                        salvar = TRUE, nrow = 2)

# Para indicador CMA
fazer_mapa_acess_cidade(acess_rio, indicador = "CMA", modo = "transit", atividade = "ST",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_rio, indicador = "CMA", modo = "transit", atividade = "TT",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_rio, indicador = "CMA", modo = "transit", atividade = "EI",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_rio, indicador = "CMA", modo = "transit", atividade = "EF",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_rio, indicador = "CMA", modo = "transit", atividade = "EM",
                        salvar = TRUE, nrow = 2)


#' 
