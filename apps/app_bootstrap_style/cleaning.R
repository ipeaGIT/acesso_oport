library(sf)
library(readr)
library(dplyr)
library(mapview)

source("../../R/fun/setup.R")

# abrir base final
acess <- read_rds("../../../data/output_base_final/acess_oport_2019.rds")

# filtrar hexagonos vazios
acess <- acess %>% filter(!is.na(modo))

# por enquanto, so transporte publico
acess_tp <- acess %>% filter(modo == "tp")
# por enquanto, so pico
acess_tp <- acess_tp %>% filter(pico == 1)

# filter columns
acess_tp <- acess_tp %>% dplyr::select(nome_muni, P001, matches("30|60|90|120"), starts_with("TMI"))

# separate between indicators
acess_tp_cum <- acess_tp %>% dplyr::select(nome_muni, P001, matches("30|60|90|120"))
acess_tp_min <- acess_tp %>% dplyr::select(nome_muni, P001, starts_with("TMI"))

# from wide to long cum
acess_tp_cum_long <- acess_tp_cum %>%
  # wide to long
  tidyr::gather(tipo, valor, CMATT30:CMAEM120) %>%
  # extract time threshld (separate at the position 5 of the string)
  tidyr::separate(tipo, c("indicador", "tempo_viagem"), sep = 5) %>%
  # extract activity
  tidyr::separate(indicador, c("indicador", "atividade"), sep = 3) %>%
  # multiply percent
  mutate(valor = valor * 100)
  
acess_tp_min_long <- acess_tp_min %>%
  # wide to long
  tidyr::gather(tipo, valor, TMIST:TMIEM) %>%
  # extract activity
  tidyr::separate(tipo, c("indicador", "atividade"), sep = 3) %>%
  # ajeitar infinitos
  mutate(valor = ifelse(is.infinite(valor), 120, valor)) %>%
  # truncar valores acima de 30 minutos
  mutate(valor = ifelse(valor > 30, 30, valor))


# salvar
write_rds(acess_tp_cum_long,   # tirar sao paulo e rio para teste
          # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro"))
          "acess_tp_cum_app.rds") 

write_rds(acess_tp_min_long,  # tirar sao paulo e rio para teste
            # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
          "acess_tp_min_app.rds") 

# salvar sem geometria
write_rds(acess_tp_cum_long %>% st_set_geometry(NULL),   # tirar sao paulo e rio para teste
          # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
          "acess_tp_cum_app_sgeo.rds") 

write_rds(acess_tp_min_long %>%  st_set_geometry(NULL),   # tirar sao paulo e rio para teste
          # filter(nome_muni %nin% c("São Paulo", "Rio de Janeiro")), 
          "acess_tp_min_app_sgeo.rds") 


# geometrias ----------------------------------------------------------------------------------

# dados hex agregados
hex_agreg <- lapply(dir("../data/hex_agregados/", full.names = TRUE, pattern = "09"), read_rds) %>% rbindlist(fill = TRUE)

# pegar pontos nao-vazios
points <- read_csv("../otp/")



# para abrir
system.time(read_rds("acess_tp_cum_app.rds")) # 4.3s

system.time(read_rds("acess_tp_cum_app_sgeo.rds")) # 1.14s
            
            read_r
            ) # 0.8s
