# carregar bibliotecas
source('./R/fun/setup.R')

library(ggalt)
library(hrbrthemes)


# FIGURAS A FAZER

# MAPAS:

# 1) TMI Rio SaudeMedia/Alta TP
# 2) TMI Bel Ed Infantil Walk
# 3) CMA For Trabalho Bike 15/45
# 4) CMA Cur Trabalho/Escola TP 60



# GRAFICOS:

# 5) TMI Ponto Decil Todos EdMedia Bike
# 6) CMA Boxplot Decil cam/poa/goi Trabalho TP 30
# 7) CMA Palma Renda Todos Trabalho  Walk 30
# 8) CMA Palma Cor SaudeAlta TP 60



###### A. Carrega dados ---------------------------

# dados hex agregados
hex_agreg <- lapply(dir("../data/hex_agregados/", full.names = TRUE, pattern = "09"), read_rds) %>% rbindlist(fill = TRUE)
head(hex_agreg)

# dados acessibilidade
acess <- lapply(dir("../data/output_access/", full.names = TRUE), read_rds) %>% rbindlist(fill = TRUE)
setDT(acess)
setnames(acess, 'origin', 'id_hex' )

# join data sets
hex_dt <- left_join(acess, hex_agreg[, -"geometry", with =F], by=c("id_hex", "quintil", "decil"))
setDT(hex_dt)                                                               


###### B. temas para mapas ---------------------------

theme_for_CMA <- function(base_size, ...) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      # plot.title = element_text(hjust = 0, vjust = 4),
      ...
      
      
    )
}

theme_for_TMI <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4)
      # legend.key.width=unit(0.5,"cm")
      
    )
}


# base plot

baseplot <- theme_minimal() +
  theme( 
    #axis.text.y  = element_text(face="bold")
    #,axis.text.x  = element_text(face="bold")
    #,
    panel.grid.minor = element_blank()
    ,strip.text.x = element_text(size = 9, face ="bold")
    ,legend.text = element_text(size = 9)
    , axis.text = element_text(size=7)
    , axis.title = element_text(size=9)
  )





### 0) Mapa com cidades do projeto   ---------------------------------------

# get World map
worldMap <- rworldmap::getMap(resolution = "low") %>% st_as_sf()


# load map of Brazil and municipalities
states_sf <- geobr::read_state(code_state = "all", year = 2018)
munis_sf <- lapply(munis_df$code_muni, geobr::read_municipality) %>% rbind_list() %>% st_sf()
st_crs(munis_sf) <- st_crs(states_sf)

# get centroids of municipalities
munis_centroids <- st_centroid(munis_sf)
munis_tp_centroids <- subset(munis_centroids, code_muni %in% munis_df$code_muni[which(munis_df$modo=='todos')])


# create map

temp_map1 <- 
  ggplot() + 
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.5), fill="steelblue4", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.5), fill="#469c77", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
  theme(panel.background = element_rect(fill = "gray98", colour = NA)) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), panel.background=element_rect(fill = "gray98")) +
  coord_sf(expand = F, xlim = c(st_bbox(states_sf)[[1]], st_bbox(states_sf)[[3]]),ylim = c(st_bbox(states_sf)[[2]], st_bbox(states_sf)[[4]])) # coord_cartesian Coordinates Zoom


# save map
ggsave(temp_map1, file="../figures/td/fig1_munis_all.png", dpi = 300, width = 16.5, height = 16.5, units = "cm")
beepr::beep()







### 1) Mapa TMI saude de media e alta - PT - rio  ---------------------------------------


# linhas capacidade
linhas_hm_rio <- read_rds("../data/linhas_HMcapacidade/linhas_HMcapacidade.rds") %>%
  filter(Cidade == "Rio de Janeiro")

# abrir acess rio
acess_rio <- read_rds("../data/output_access/acess_rio_2019.rds")

# abrir muni
muni_rio <- read_rds("../data-raw/municipios/rio/municipio_rio.rds")

muni_rio <- geobr::read_municipality(code_muni = munis_df$code_muni[which(munis_df$abrev_muni=="rio")])


# tirar so pt e pico e colocar em format long
acess_rio_pt_pico <- acess_rio %>%
  filter(mode == "transit" & pico == 1) %>%
  # tirar so saude medio e alta
  select(TMISM, TMISA) %>%
  gather(ind, valor, TMISM:TMISA)

acess_rio$saude

# fazer grafico

acess_rio_pt_pico %>%
  mutate(valor = ifelse(valor > 40, 40, valor)) %>%
  mutate(ind = factor(ind, 
                      levels = c("TMISM", "TMISA"), 
                      labels = c("Saúde Média Complexidade", "Saúde Alta Complexidade"))) %>%
  ggplot()+
  geom_sf(data = muni_rio, fill = NA) +
  geom_sf(aes(fill = valor), color = NA, alpha=.9)  +
  geom_sf(data = linhas_hm_rio, size=0.3, color="gray40")+
  viridis::scale_fill_viridis( direction = -1,
                               breaks = c(0, 10, 20, 30, 40),
                               labels = c(0, 10, 20, 30, "+40 min")) +
  labs(fill = "Tempo até a oportunidade\n mais próxima")+
  facet_wrap(~ind, ncol = 1)+
  theme_for_TMI()

# save map
ggsave(file="../figures/td/fig1-TMI_SM_TP.png", dpi = 300, width = 8, height = 10, units = "cm")
beep()





# 2) TMI Bel Ed Infantil Walk -------------------------

# abrir acess rio
acess_bel <- read_rds("../data/output_access/acess_bel_2019.rds")

# abrir muni
muni_bel <- read_rds("../data-raw/municipios/bel/municipio_bel.rds")

muni_bel <- geobr::read_municipality(code_muni = munis_df$code_muni[which(munis_df$abrev_muni=="bel")])


# tirar so pt e pico e colocar em format long
acess_bel_pt_pico <- acess_bel %>%
  filter(mode == "walk" & pico == 1) %>%
  # tirar so saude medio e alta
  select(TMIEI)

# fazer grafico
acess_bel_pt_pico %>%
  mutate(valor = ifelse(TMIEI > 30, 30, TMIEI)) %>%
  ggplot()+
  # geom_sf(data = muni_bel, fill = NA) +
  geom_sf(aes(fill = valor), color = NA, alpha=.9)  +
  viridis::scale_fill_viridis( direction = -1,
                               breaks = c(0, 15, 30),
                               labels = c(0, 15,"+30 min")) +
  labs(fill = "Tempo até a oportunidade\n mais próxima")+
  theme_for_TMI()



# save map
ggsave(file="../figures/td/fig2-TMI_EI_bel_walk.png", dpi = 300, width = 8, height = 10, units = "cm")
beep()



# # 3) CMA For Trabalho Bike 15/45 ----------------------------------

acess_for <- read_rds("../data/output_access/acess_for_2019.rds") %>% 
  filter(mode == "bike") %>%
  select(city, CMATQ15, CMATQ45) %>%
  gather(ind, valor, CMATQ15:CMATQ45)

# fazer plots
acess_for %>%
  ggplot()+
  geom_sf(aes(fill = valor), color = NA, alpha=.9)+
  viridis::scale_fill_viridis(option = "B"
                              , limits = c(0, 0.72),
                              , breaks = c(0.001, 0.35, 0.7)
                              , labels = c(0, "35", "70%")
  ) +
  facet_wrap(~ind, nrow = 1)+
  theme_for_CMA()+
  labs(fill = "",
       title = "Fortaleza") +
  theme(plot.title = element_text(hjust = 0.5))



ggsave(file="../figures/td/fig3-CMA_TQ_for_1545.png", dpi = 200, width = 14, height = 10, units = "cm")
beep()





# 4) CMA Cur Trabalho/Escola TP 60 ---------------------

acess_cur <- read_rds("../data/output_access/acess_cur_2019.rds") %>% 
  filter(mode == "transit") %>%
  select(city, CMATQ60, CMAEM60) %>%
  gather(ind, valor, CMATQ60:CMAEM60)

# fazer plots
acess_cur %>%
  ggplot()+
  geom_sf(aes(fill = valor), color = NA, alpha=.9)+
  viridis::scale_fill_viridis(option = "B"
                              , limits = c(0, 0.9),
                              , breaks = c(0.001, 0.45, 0.9)
                              , labels = c(0, "45", "90%")
  )+
  facet_wrap(~ind, nrow = 1)+
  theme_for_CMA()+
  labs(fill = "",
       title = "Curitiba") +
  theme(plot.title = element_text(hjust = 0.5))



ggsave(file="../figures/td/fig4-CMA_TQEM_cur_60.png", dpi = 300, width = 14, height = 10, units = "cm")
beep()







# 5) TMI Ponto Decil Todos EdMedia Bike  --------------------------------

# abrir o acess de todas as cidades e juntar

# seleciona so bike
acess_bike <- setDT(hex_dt)[ mode == "bike" ]

# Se nenhuma atividade acessivel (TMIEM==Inf), entao imputar TMIEM de 90 min.
acess_bike[, TMIEM := fifelse(TMIEM==Inf, 90, TMIEM)]

# tempo medio ponderapo pela populacao de cada hexagono
df4 <- acess_bike[, .(Total = weighted.mean(x = TMIEM[which(pop_total>0)], w = pop_total[which(pop_total>0)], na.rm=T),
                      Negra = weighted.mean(TMIEM[which(cor_negra>0)], w = cor_negra[which(cor_negra>0)], na.rm=T),
                      Branca = weighted.mean(TMIEM[which(cor_branca>0)], w = cor_branca[which(cor_branca>0)], na.rm=T),
                      Q1 = weighted.mean(TMIEM[which(quintil==1)], w = pop_total[which(quintil==1)], na.rm=T),
                      Q5 = weighted.mean(TMIEM[which(quintil==5)], w = pop_total[which(quintil==5)], na.rm=T)), by=city]



# ajeitar nome das cidade
df4 <- df4 %>%
  mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni))



df4 %>%
  ggplot() + 
  geom_dumbbell(aes(x = Q5, xend = Q1, y = forcats::fct_reorder(city, Q1)), 
                size=3, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4") +
  geom_point(aes(x = Total, y = city), color = "black", size = 2)+
  scale_color_manual(values=c('#f0a150', '#f48020', '#f0750f'), 
                     name="", 
                     labels=c('Pobres Q1', 'Média', 'Ricos Q5')) +
  scale_x_continuous(name="", limits = c(0, 24),
                     breaks = c(0, 5, 10, 15, 20),
                     labels = c(0, 5,  10, 15,"20 minutos")) +
  geom_text(data = filter(df4, city == "Goiania"),
            aes(x = Q1, y = city),
            label = "Pobres Q1", fontface = "bold",
            color = "springgreen4",
            hjust = -0.5) +
  geom_text(data = filter(df4, city == "Goiania"),
            aes(x = Q5, y = city),
            label = "Ricos Q5", fontface = "bold",
            color = "steelblue4",
            hjust = 1.5) +
  geom_text(data = filter(df4, city == "Goiania"),
            aes(x = Total, y = city),
            label = "Total", fontface = "bold",
            color = "black",
            vjust = -1) +
  expand_limits(y = 21)+
  theme_ipsum_rc(grid= "X") +
  labs(x = "", y = "", title = "")

# TME bike
# tme walk



ggsave(file="../figures/td/fig5_TMIEM_bike_renda90median.png", dpi = 300, width = 16, height = 16, units = "cm")
beep()







# 6) CMA Boxplot Decil cam/poa/goi Trabalho TP 30 ------------------------------

acess_walk <- hex_dt %>%
  # pegar so TP
  filter(mode == "walk")



baseplot2 <- theme_minimal() +
  theme( 
    axis.text.y  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
    , legend.position = "top"
    , axis.text.x = element_blank()
    
  )

acess_walk %>% 
  filter(decil >0) %>%
  # filtrar so as cidades de tamanho semelhante
  filter(city %in% c("cam", "poa", "goi")) %>%
  mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(decil), y = CMATQ30, weight=pop_total, color = factor(decil)),
               # size = 1, 
               outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
  # facet_grid(threshold_name ~ ., scales = "free_y") +
  facet_wrap(~city) +
  scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
  scale_y_percent() +
  hrbrthemes::theme_ipsum_rc(grid = "Y") +
  labs(x = "",
       y = "Porcentagem de oportunidades acessíveis") + 
  guides(color=guide_legend(nrow=1)) +
  theme( 
    axis.text.y  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
    , legend.position = "top"
    , axis.text.x = element_blank()
    
  )


ggsave(file="../figures/td/fig6-boxplot_CMA_rendadecil_walk_TQ_30.png", dpi = 300, width = 25, height = 15, units = "cm")
beep()






# 7) CMA Palma Renda Todos Trabalho  Walk 30  --------------------------------

# calcular acessibilidade media do 90 percentil e 40 percentil de renda
# usar caminhada pico CMATQ30

acess_palma <- hex_dt %>%
  filter(mode == "bike" & pico == 1) %>%
  select(city, decil, pop_total, CMATQ30) %>%
  # pegar so decis 4 e 9
  filter(decil %in% c(1, 2, 3, 4, 10)) %>%
  # definir ricos e pobres
  mutate(classe = ifelse(decil %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
  group_by(city, classe) %>%
  summarise(acess_media = weighted.mean(CMATQ30, pop_total)) %>%
  ungroup() %>%
  spread(classe, acess_media) %>%
  # calcular palma ratio
  group_by(city) %>%
  mutate(palma_ratio = rico/pobre) %>%
  ungroup()

# visualizar
acess_palma %>%
  mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  mutate(city = fct_reorder(city, palma_ratio)) %>%
  ggplot()+
  geom_col(aes(y = palma_ratio, x = city))+
  scale_y_continuous(breaks = c(0, 3, 6, 9))+
  coord_flip()+
  theme_ipsum_rc(grid = "X")+
  labs(x = "", y = "Palma Ratio")


ggsave(file="../figures/td/fig7-palma_ratio_CMA_TQ_walk_30.png", dpi = 300, width = 16.5, height = 15, units = "cm")




# 8) CMA Palma Cor SaudeAlta TP 60  --------------------------------

# calcular acessibilidade media do 90 percentil e 40 percentil de renda
# usar TP pico CMASA60

acess_palma_2 <- hex_dt %>%
  filter(mode == "transit" & pico == 1) %>%
  select(city, decil, pop_total, cor_negra, cor_branca, CMASA60) %>%
  group_by(city) %>%
  summarise(acess_brancos = weighted.mean(CMASA60, cor_branca),
            acess_negros = weighted.mean(CMASA60, cor_negra)) %>%
  # calcular palma ratio
  mutate(palma_ratio = acess_brancos/acess_negros) %>%
  ungroup()

# visualizar
acess_palma_2 %>%
  mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  mutate(city = fct_reorder(city, palma_ratio)) %>%
  ggplot()+
  geom_col(aes(y = palma_ratio, x = city))+
  scale_y_continuous(breaks = c(0, 0.75, 1.5))+
  coord_flip()+
  theme_ipsum_rc(grid = "X")+
  labs(x = "", y = "Palma Ratio")


ggsave(file="../figures/td/fig8-palma_ratio_cor_CMA_SA_TP_60.png", dpi = 300, width = 16.5, height = 15, units = "cm")
