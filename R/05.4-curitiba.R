#' ## Curitiba
#' 
#' 
## ----acess acumulativa cur-----------------------------------------------

# Aplicar
# calcular_acess("cur")

# Abrir
acess_cur <- read_rds("../data/output_access/acess_cur.rds")

# Fazer mapa para cur ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_cidade(acess_cur, indicador = "TMI", modo = "transit", atividade = "ST",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_cur, indicador = "TMI", modo = "walk", atividade = "ST",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_cur, indicador = "TMI", modo = "bike", atividade = "ST",
                        salvar = TRUE, nrow = 2)

# Para indicador CMA
fazer_mapa_acess_cidade(acess_cur, indicador = "CMA", modo = "transit", atividade = "ST",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_cur, indicador = "CMA", modo = "transit", atividade = "TT",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_cur, indicador = "CMA", modo = "transit", atividade = "EI",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_cur, indicador = "CMA", modo = "transit", atividade = "EF",
                        salvar = TRUE, nrow = 2)

fazer_mapa_acess_cidade(acess_cur, indicador = "CMA", modo = "transit", atividade = "EM",
                        salvar = TRUE, nrow = 2)


#' 
