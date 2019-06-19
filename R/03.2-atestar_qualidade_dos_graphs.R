#' ## Atestar qualidade dos graphs
#' 
#' 
## ------------------------------------------------------------------------

cidade <- "sao"
uf <- "sp"

atestar_qualidade_graph <- function(cidade, uf) {
  
  # Abrir graph
  graph_dir <- sprintf("../otp/graphs/%s/%s_export.pbf", cidade, cidade)
  graph <- st_read(graph_dir, layer = "multilines")
  
  graph_junto <- graph %>%
    st_combine()
  
  plot(graph_junto)
  
  # Abrir limites do municipio
  municipio_dir <- sprintf("../data/municipios/municipios_%s.rds", uf)
  municipio <- read_rds(municipio_dir) %>%
    filter(CD_GEOCODM == 3550308)
  
  plot(municipio, add = TRUE, fill = FALSE)
}

graph %>%
  st_set_geometry(NULL) %>%
  count(highway, sort = T)

graph %>%
  filter(highway == "service") %>%
  View()

graph %>%
  filter(is.na(highway)) %>%
  View

graph %>%
  filter(highway == "trunk") %>%
  View()


#' 
## ------------------------------------------------------------------------

# get bb para sao paulo

bb_sao <- osmdata::getbb("são paulo, brazil")


#' 
#' 
#' 
#' 
