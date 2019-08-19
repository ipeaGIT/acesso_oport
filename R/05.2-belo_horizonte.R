#' ## Belo Horizonte
#' 
#' 
## ----acess acumulativa bel-----------------------------------------------

# Aplicar
# calcular_acess("bel")

# Abrir
acess_bel <- read_rds("../data/output_access/acess_bel.rds")

# Fazer mapa para bel ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_cidade(acess_bel, indicador = "TMI", modo = "transit", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_bel, indicador = "TMI", modo = "walk", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_bel, indicador = "TMI", modo = "bike", atividade = "ST",
                        salvar = TRUE)

# Para indicador CMA
fazer_mapa_acess_cidade(acess_bel, indicador = "CMA", modo = "transit", atividade = "ST",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_bel, indicador = "CMA", modo = "transit", atividade = "TT",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_bel, indicador = "CMA", modo = "transit", atividade = "EI",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_bel, indicador = "CMA", modo = "transit", atividade = "EF",
                        salvar = TRUE)

fazer_mapa_acess_cidade(acess_bel, indicador = "CMA", modo = "transit", atividade = "EM",
                        salvar = TRUE)



#' 
