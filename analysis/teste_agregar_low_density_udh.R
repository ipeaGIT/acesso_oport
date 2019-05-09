source("R/setup.R")

# cidade <- "sao"

extract_lowdensity_areas <- function(cidade) {
  
  udh_path <- sprintf("../data-raw/udh/%s", cidade)
  udh_path_shape <- dir(udh_path, full.names = TRUE, pattern = "*.shp")
  udh_path_vars <- dir(udh_path, full.names = TRUE, pattern = "*.xlsx")
  
  hex_path <- sprintf("../data/hex_agregados/hex_agregado_%s_08.rds", cidade)
  
  # ABRIR UDH ---------------------------------------------------------------
  
  # Abrir shape
  udh_cur_shape <- st_read(udh_path_shape) %>%
    na.omit() %>%
    mutate(UDH_Atlas = as.character(UDH_ATLAS)) %>%
    mutate(UDH_Atlas = as.numeric(UDH_Atlas))
  
  # Abrir variaveis
  udh_cur_vars <- readxl::read_xlsx(udh_path_vars, sheet = 2) %>%
    filter(ANO == 2010) %>%
    select(-ANO)
  
  # Juntar os dataframes
  
  udh_cur_fim <- udh_cur_shape %>%
    left_join(udh_cur_vars, by = "UDH_Atlas") %>%
    select(UDH_Atlas, POP) %>%
    # Calcular densidade populacional
    mutate(area = st_area(.) %>% as.numeric()) %>%
    mutate(area = area * 10^(-6)) %>%
    mutate(pop_dens = POP/area)
  
  
  # ABRIR HEXAGONOS ---------------------------------------------------------
  
  hex_cur <- read_rds(hex_path) %>%
    filter(pop_total > 0)
    
    
    # ANALISAR DISTRIBUICAO POPULACIONAL DAS UDHS -----------------------------
  
  # udh_cur_fim %>%
  #   ggplot()+
  #   geom_histogram(aes(pop_dens))
  
  # mapview(udh_cur_fim, zcol = "pop_dens")
  
  # Qual o 10 percentil?
  
  percentil <- quantile(udh_cur_fim$pop_dens, 0.1)
  
  # Filtrar as UDHs que estao abaixo desse percentil
  
  udh_cur_low <- udh_cur_fim %>%
    filter(pop_dens <= percentil)
  
  
  # EXTRAIR OS HEXAGONOS QUE ESTAO INSERIDOS NAS UDHS LOW -------------------
  
  hex_cur_low <- hex_cur %>%
    st_join(udh_cur_low, join = st_intersects, left = FALSE, largest = TRUE)
  
  # mapview(udh_cur_low) + mapview(hex_cur_low)
  
  # Tirar esses hexagonos do hexagonos totais
  
  hex_cur_fim <- hex_cur %>%
    filter(id_hex %nin% hex_cur_low$id_hex)
  
  # Agrupar os hexagonos low
  hex_cur_low_group <- hex_cur_low %>%
    group_by(UDH_Atlas) %>%
    summarise(pop_total = sum(pop_total), empregos_total = sum(empregos_total), 
              saude_total = sum(saude_total), escolas_total = sum(escolas_total)) %>%
    rename(id_hex = UDH_Atlas)
  
  # mapview(hex_cur_low_group)
  
  # Juntar com o hex normal
  hex_cur_end <- rbind(hex_cur_fim, hex_cur_low_group)
  
  # mapview(hex_cur_end)
}



# # APLICAR -----------------------------------------------------------------
# 
# # Sao Paulo
# novo_sao <- extract_lowdensity_areas("sao")
# 
# mapview(novo_sao)
# 
# # Curitiba
# novo_cur <- extract_lowdensity_areas("cur")
# 
# mapview(novo_cur)
  
