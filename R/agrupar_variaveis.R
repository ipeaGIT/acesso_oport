# 
# 
# 
# hex_for <- readRDS("../data/hex_municipio/hex_for.rds")
# 
# cnes <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326)
# 
# escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
#   filter(!is.na(lon)) %>%
#   filter(municipio == "Fortaleza") %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
#   
# pop <- read_rds("../data/grade_municipio/grade_for.rds") %>%
#   select(id_grade, POP) %>%
#   st_centroid()
# 
# 
# hex_for_temp <- hex_for %>%
#   st_join(pop) %>%
#   group_by(id_hex) %>%
#   summarise(pop_total = sum(POP)) %>%
#   ungroup() %>%
#   st_join(cnes) %>%
#   group_by(id_hex, pop_total) %>%
#   summarise(saude_total = n()) %>%
#   ungroup() %>%
#   st_join(escolas) %>%
#   group_by(id_hex, pop_total, saude_total) %>%
#   summarise(escolas_total = n())
# 
# 
# mapview(hex_for_temp, zcol = "pop_total")
# mapview(hex_for_temp, zcol = "saude_total")
# mapview(hex_for_temp, zcol = "escolas_total")


# FUNCAO!!!!!!!!!!!!!!!! --------------------------------------------------



agrupar_variaveis <- function(municipio, salvar_no_disco = TRUE) {
  
  muni_shortname <- substring(muni, 1, 3)
  
  dir_muni <- paste0("../data/hex_municipio/hex_", muni_shortname, ".rds")
    
  hex_muni <- readRDS(dir_muni)
  
  cnes <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
    filter(!is.na(lon)) %>%
    # mutate(municipio == tolower(municipio)) %>%
    # filter(municipio == muni) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  dir_grade <- paste0("../data/grade_municipio/grade_", muni_shortname, ".rds")
  
  pop <- read_rds(dir_grade) %>%
    select(id_grade, POP) %>%
    st_centroid()
  
  
  hex_muni_fim <- hex_muni %>%
    st_join(pop) %>%
    group_by(id_hex) %>%
    summarise(pop_total = sum(POP)) %>%
    ungroup() %>%
    st_join(cnes) %>%
    group_by(id_hex, pop_total) %>%
    summarise(saude_total = n()) %>%
    ungroup() %>%
    st_join(escolas) %>%
    group_by(id_hex, pop_total, saude_total) %>%
    summarise(escolas_total = n())
  
  if (salvar_no_disco == TRUE) {
    
    dir_output <- sprintf("../data/hex_agregados/hex_agregado_%s.rds", muni_shortname)
    
    write_rds(hex_muni_fim, dir_output)
    
    
  }
  
  
}
