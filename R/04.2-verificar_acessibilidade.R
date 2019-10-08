# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.2 Verificar acessibilidade extrema

# carregar bibliotecas
source('./R/fun/setup.R')

# Isso sera feito individualmente para cada cidade por inspecao visual

inspecao_visual_acess <- function(sigla_muni) {
 
  # sigla_muni <- "bho"; ano <- 2019
  
  # path in da acessibilidade
  path_in <- sprintf("../data/output_access/acess_%s_%s.rds", sigla_muni, ano)
  
  # abrir
  acess <- read_rds(path_in)
  
  # Fazer teste para dois indicadores: CMA para trabalho 60 TP e TMI Saude -----------
  
  # CMA TT 60
  
  acess_cma <- acess %>% filter(mode == "transit") %>% select(origin, city, CMATT90, CMPPT90) 
  
  # trazer us
  us <- read_rds(sprintf("../data/hex_agregados/hex_agregado_%s_09.rds", sigla_muni)) %>%
    st_set_geometry(NULL)
  
  # juntar
  acess_cma_us <- left_join(acess_cma, us, by = c("origin" = "id_hex"))
  
  # visualizar
  
  mapview(acess_cma, zcol = "CMATT90", lwd = 0, alpha = 0.6)
  
  
  pal <- colorNumeric(
    palette = "inferno",
    domain = acess_cma$CMPPT90,
    na.color = "black")
  
  popup_hex <- paste0("<b>População: </b>", acess_cma_us$pop_total, "<br/>",
                      "<b>Oportunidades: </b>", paste0(acess_cma_us$CMPPT90*100, "%"), "<br/>",
                      acess_cma_us$origin)
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data = acess_cma,
                fillColor = ~pal(CMPPT90),
                fillOpacity = 0.3,
                stroke = FALSE,
                popup = popup_hex)
  
  
  library(leaflet)
  library(leafgl)
  library(colourvalues)
  
  cols = colour_values_rgb(acess_cma$CMATT60, include_alpha = FALSE) / 255
  
   
}


# ATENCAO
# ATENCAO
# ATENCAO
# ATENCAO
# ATENCAO
# ATENCAO
# PEGAR TODOS OS HEXAGONOS COM ACESSIBILIDADE MENOR QUE 5% DO CMATT90



