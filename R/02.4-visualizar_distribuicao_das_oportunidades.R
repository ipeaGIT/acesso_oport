#' ## Visualizar distribuição das oportunidades
#' 
#' 
## ----for-----------------------------------------------------------------

fort <- read_rds("../data/hex_agregados/hex_agregado_for_09.rds")


mapview(fort, zcol = "empregos_total")


#' 
#' 
