library(sf)
library(mapview)
library(dplyr)
library(ggplot2)



setores1 <- fread("../data-raw/dados_censo2010A.csv")

names(setores1)

# Renda 6.19 - variavel escolhida: V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
setores_renda <-  setores1 %>% 
  select(cod_muni = Cod_municipio, cod_setor = Cod_setor, renda_total = DomRend_V003)
  
# Moradores 6.3 - variavel escolhida: V002 = Moradores em domicílios particulares permanentes
setores_moradores <- setores1 %>% 
  select(cod_setor = Cod_setor, moradores_total = Dom2_V002)

# juntar

setores_total <- setores_renda %>%
  left_join(setores_moradores, by = "cod_setor") %>%
  mutate(renda_per_capta = renda_total / moradores_total) %>%
  mutate(cod_setor = as.numeric(cod_setor))

write_rds(setores_total, "../data/renda_por_setor/renda_por_setor.rds")
  

# TESTE PARA FORTALEZA ----------------------------------------------------


ai <- read_sf("../data-raw/municipios/ce/ce_setores_censitarios/23SEE250GC_SIR.shp") %>%
  filter(NM_MUNICIP == "FORTALEZA") %>%
  select(cod_setor = CD_GEOCODI, muni = NM_MUNICIP) %>%
  mutate(cod_setor = as.numeric(cod_setor))
  
  
setores_fortaleza <- ai %>%
  left_join(setores_total, by = c("cod_setor"))


setores_fortaleza %>%
  mutate(renda_v1 = ifelse(renda_per_capta > 4000, 4000, renda_per_capta)) %>%
  ggplot()+
  geom_sf(aes(fill = renda_v1))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd"),
                       breaks = c(0, 1000, 2000, 3000, 4000),
                       labels = c("0", "1000", "2000", "3000", "4000+"))
