source("R/setup.R")

cidade <- "cur"

extract_lowdensity_areas <- function(cidade) {
  
  udh_path <- sprintf("../data-raw/udh/%s", cidade)
  udh_path_shape <- dir(udh_path, full.names = TRUE, pattern = "*.shp")
  udh_path_vars <- dir(udh_path, full.names = TRUE, pattern = "*.xlsx")
  
  hex_path <- sprintf("../data/hex_agregados/hex_agregado_%s_08.rds", cidade)
  
  # ABRIR UDH ---------------------------------------------------------------
  
  # Abrir shape
  udh_cur_shape <- st_read(udh_path_shape) %>%
    na.omit() %>%
    # filter(CD_GEOCODM == "355030") %>%
    mutate(UDH_Atlas = as.character(UDH_ATLAS)) %>%
    mutate(UDH_Atlas = as.numeric(UDH_Atlas))
  
  # Abrir variaveis
  udh_cur_vars <- readxl::read_xlsx(udh_path_vars, sheet = 2) %>%
    # filter(CODMUN6 == 355030) %>%
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
    mutate(pop_total = ifelse(is.na(pop_total), 0, pop_total)) %>%
    # filter(pop_total > 0 | empregos_total > 0 | saude_total > 0 | escolas_total > 0) %>%
    # Calcular densidade populacional
    mutate(area = st_area(.) %>% as.numeric()) %>%
    mutate(area = area * 10^(-6)) %>%
    mutate(pop_dens = pop_total/area)
  
    
    
    # ANALISAR DISTRIBUICAO POPULACIONAL DAS UDHS -----------------------------
  
  # udh_cur_fim %>%
  #   ggplot()+
  #   geom_histogram(aes(pop_dens))
  
  # mapview(udh_cur_fim, zcol = "pop_dens")
  
  # Qual o 10 percentil?
  
  percentil <- quantile(hex_cur$pop_dens, 0.2, na.rm = TRUE)
  
  # # Filtrar as UDHs que estao abaixo desse percentil
  # 
  # udh_cur_low <- udh_cur_fim %>%
  #   filter(pop_dens <= percentil)
  
  # Filtrar os hexagonos que estao abaixo desse percentil
  
  hex_cur_low <- hex_cur %>%
    filter(pop_dens <= percentil)
  
  # mapview(udh_cur_low)
  
  
  # EXTRAIR OS HEXAGONOS QUE ESTAO INSERIDOS NAS UDHS LOW -------------------
  
  hex_cur_low_v1 <- hex_cur_low %>%
    st_join(udh_cur_fim, join = st_intersects, left = FALSE, largest = TRUE)
    # st_join(udh_cur_low, join = st_intersects, left = FALSE, largest = TRUE)
  
  # mapview(hex_cur_low_v1)
  
  # mapview(udh_cur_low) + mapview(hex_cur_low)
  
  # Tirar esses hexagonos do hexagonos totais
  
  hex_cur_fim <- hex_cur %>%
    filter(id_hex %nin% hex_cur_low$id_hex) %>%
    select(- area, -pop_dens)
  
  # Agrupar os hexagonos low
  hex_cur_low_group <- hex_cur_low_v1 %>%
    group_by(UDH_Atlas) %>%
    summarise(pop_total = sum(pop_total), empregos_total = sum(empregos_total), 
              saude_total = sum(saude_total), escolas_total = sum(escolas_total)) %>%
    rename(id_hex = UDH_Atlas)
  
  # mapview(hex_cur_low_group)
  
  # Juntar com o hex normal
  hex_cur_end <- rbind(hex_cur_fim, hex_cur_low_group) %>%
    mutate(id_hex = 1:n())
  
  # mapview(hex_cur_end)
}



# SUGESTAO ----------------------------------------------------------------

# Eliminar hexagonos vazios (pop e atividades)
# Fazerp percentil dos hexagonos de baixa densidade
# Vai mudar o percentil!




# # APLICAR -----------------------------------------------------------------
# 
# Sao Paulo
novo_sao <- extract_lowdensity_areas("sao")
# 
mapview(novo_sao)
# 
# Curitiba
novo_cur <- extract_lowdensity_areas("cur")
# 
mapview(novo_cur) + mapview(udh_cur_fim)


# SALVAR EM KML -----------------------------------------------------------

st_write(novo_sao, "../data/temp/agregacoes_uber_sao.shp")  
st_write(novo_cur, "../data/temp/agregacoes_uber_cur.shp")  
