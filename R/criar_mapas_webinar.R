# carregar bibliotecas
source('./R/fun/setup.R')

# FIGURAS A FAZER

# 0) Ok - Mapa com municipios no Projeto


# 1) TMI saude de media e alta - PT  (uma cidade)- rio 
 # - so firula


# 2) CMA empregos 60 min - PT (duas cidades) - BHe Fortaleza
# - so firula

# 3) CMP - Gráfico de pontos do acesso a pé  educação infantil - população negra e branca (TODAS CIDADES)

# 4) Um gráfico de pontos do CMA de bicicleta de educação média (TODAS CIDADES) - decil, 1, 10  e média
# rafa

# 5) Box plot por renda - acesso a emprego de PT - 60 min
- firula


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
    st_crs(munis_sf) <- st_crs(brasil_sf)
  
  # get centroids of municipalities
    munis_centroids <- st_centroid(munis_sf)
    munis_tp_centroids <- subset(munis_centroids, code_muni %in% munis_df$code_muni[which(munis_df$modo=='todos')])
    
    
  
  
  # create map
  temp_map1 <- 
  ggplot() + 
    geom_sf(data=worldMap, fill="white", color="gray90") +
#    geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
#    geom_sf(data=st_buffer(munis_centroids, dist =.5), fill="steelblue4", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
   geom_sf(data=st_buffer(munis_tp_centroids, dist =.5), fill="steelblue4", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
    theme(panel.background = element_rect(fill = "gray98", colour = NA)) + 
    theme_map() +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    coord_sf(xlim = c(st_bbox(brasil_sf)[[1]], st_bbox(brasil_sf)[[3]]),ylim = c(st_bbox(brasil_sf)[[2]], st_bbox(brasil_sf)[[4]])) # coord_cartesian Coordinates Zoom
    # spherical mal?>>> coord_sf(crs = "+proj=laea +lat_0=-24 +lon_0=300 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +no_defs") 
    
  
  
  
  # save map
  ggsave(temp_map1, file="./figures/fig1_munis1.png", dpi = 200, width = 16.5, height = 15, units = "cm")
  beepr::beep()







### 1) TMI saude de media e alta - PT - rio  ---------------------------------------
  
  
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


# fazer grafico
fig2 <- 
  acess_rio_pt_pico %>%
  mutate(valor = ifelse(valor > 40, 40, valor)) %>%
  mutate(ind = factor(ind, 
                      levels = c("TMISM", "TMISA"), 
                      labels = c("Saúde Média Complexidade", "Saúde Alta Complexidade"))) %>%
  ggplot()+
  geom_sf(data = muni_rio, fill = NA) +
  geom_sf(aes(fill = valor), color = NA, alpha=.9)  +
#  geom_sf(data = linhas_hm_rio, size=0.7, color="#2340e7")+
  viridis::scale_fill_viridis( direction = -1,
                               breaks = c(0, 10, 20, 30, 40),
                               labels = c(0, 10, 20, 30, "+40 min")) +
  labs(fill = "Tempo até a oportunidade\n mais próxima")+
  facet_wrap(~ind, ncol = 1)+
  theme_for_TMI()

# save map
ggsave(fig2, file="./figures/fig2-TMI_SM_TP.png", dpi = 300, width = 8, height = 10, units = "cm")
beep()



# 2) CMA empregos 60 min - PT (duas cidades) - BHe Fortaleza ----------------------------------

# abrir acess for e bho e for e tirar so empregos 60 min pico pt
acess_for <- read_rds("../data/output_access/acess_for_2019.rds") %>% 
  filter(mode == "transit" & pico == 1)

acess_bho <- read_rds("../data/output_access/acess_bho_2019.rds") %>%
  filter(mode == "transit" & pico == 1)

# linhas capacidade
linhas_hm_bho <- read_rds("../data/linhas_HMcapacidade/linhas_HMcapacidade.rds") %>%
  filter(Cidade == "Belo Horizonte")

library(patchwork)
library(cowplot)

# fazer plots
plot1 <- acess_for %>%
  ggplot()+
  geom_sf(aes(fill = CMATT30), color = NA, alpha=.9)+
  viridis::scale_fill_viridis(option = "B",
                              limits = c(0, 0.7),
                              breaks = c(0.001, 0.35, 0.7),
                              labels = scales::percent) +
  theme_for_CMA()+
  labs(fill = "",
       title = "Fortaleza") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))



plot2 <- acess_bho %>%
  ggplot()+
  geom_sf(aes(fill = CMATT30), color = NA, alpha=.9)+
 # geom_sf(data = linhas_hm_bho, size=0.9, color="#d7301f")+
  viridis::scale_fill_viridis(option = "B",
                              limits = c(0, 0.7),
                              breaks = c(0.001, 0.35, 0.7),
                              labels = c(0, 35, "70%")) +
  theme_for_CMA(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Porcentagem de\n oportunidades acessíveis",
       title = "Belo Horizonte")


legenda <- get_legend(plot2 + theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))


temp <- plot_grid(plot1, plot2 + theme(legend.position = "none"))


plot_grid(temp, legenda,  ncol = 1, rel_heights = c(1, .2))


ggsave(file="./figures/fig3-CMA_for_bho.png", dpi = 300, width = 14, height = 10, units = "cm")
beep()


# 3) CMP - Gráfico de pontos do acesso a pé educação infantil - população negra e bra --------

# abrir o acess de todas as cidades e juntar

acess_cmp_todas_walk <- lapply(dir("../data/output_access/", full.names = TRUE), read_rds) %>%
  rbindlist(fill = TRUE) %>%
  # tirar so caminhada
  filter(mode == "walk")


# cor e renda
pop_totais <- hex_agreg[, .(pop_negra_total = sum(cor_negra, na.rm=T),
                             pop_branca_total = sum(cor_branca, na.rm=T),
                             pop_total_total = sum(pop_total, na.rm=T),
                             quintil1  = sum(pop_total[which(quintil==1)], na.rm=T),
                             quintil5  = sum(pop_total[which(quintil==5)], na.rm=T)
                             ), by= muni]





# tem que filtrar zonas que tenha alguma escola de edu infantil!
hex_escolas <- hex_agreg %>% filter(edu_infantil > 0)
  
acess_cmp_todas_edu <- acess_cmp_todas_walk %>%
  filter(origin %in% hex_escolas$id_hex) %>%
  # selecionar so acess passiva negros e brancos 15 min
  select(city, CMPPB30, CMPPN30)



acess_cmp_todas_walk$TMIEI


# grafico!


# grafico!
acess_cmp_todas_edu %>%
  # testar algumas cidades
  # filter(city %in% c("for", "bho", "rio")) %>%
  gather(indicador, valor, CMPPB30:CMPPN30) %>%
  ggplot()+
  geom_boxplot(aes(y = valor, x = indicador)) +
  coord_flip()+
  facet_wrap(~city, scales = "free_x")

  
  


  
  
# 4) Um gráfico de pontos do acesso de bicicleta de educação média (TODAS CIDADES) - decil, 1, 10  e média  --------------------------------

# abrir o acess de todas as cidades e juntar

# selciona so Walking
acess_walk <- hex_dt[ mode == "walk" ]

# Se nenhuma atividade acessivel (TMIEF==Inf), entao imputar TMIEF de 120 min.
acess_walk[, TMIEF := ifelse(TMIEF==Inf, NA, TMIEF)]

acess_walk$TMIEF
df4 <- acess_walk[, .(Total = weighted.mean(x = TMIEF[which(pop_total>0)], w = pop_total[which(pop_total>0)], na.rm=T),
                      Negra = weighted.mean(TMIEF[which(cor_negra>0)], w = cor_negra[which(cor_negra>0)], na.rm=T),
                      Branca = weighted.mean(TMIEF[which(cor_branca>0)], w = cor_branca[which(cor_branca>0)], na.rm=T),
                      Q1 = weighted.mean(TMIEF[which(quintil==1)], w = pop_total[which(quintil==1)], na.rm=T),
                      Q5 = weighted.mean(TMIEF[which(quintil==5)], w = pop_total[which(quintil==5)], na.rm=T)), by=city]


# ajeitar nome das cidade
  df4 %<>%
    mutate(city = ifelse(city == "bel", "bho", ifelse(city == "sao", "spo", ifelse(city == "por", "poa", city)))) %>%
    mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni))
    

  



library(ggalt)
library(hrbrthemes)
  
  
temp_fig4 <- 
  
  df4 %>%
  ggplot() + 
  geom_dumbbell(aes(x = Negra    , xend = Branca        , y = forcats::fct_reorder(city, Q1)), 
                           size=3, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4") +
  geom_point(aes(x = Total, y = city), color = "black", size = 2)+
  scale_color_manual(values=c('#f0a150', '#f48020', '#f0750f'), 
                     name="", 
                     labels=c('Pobres Q1', 'Média', 'Ricos Q5')) +
  scale_x_continuous(name="", limits = c(0, 40),
                     breaks = c(0, 10, 20, 30, 40),
                     labels = c(0, 10, 20, 30, "40 minutos")) +
  geom_text(data = filter(df4, city == "Sao Goncalo"),
            aes(x = Q1, y = city),
            label = "Pobres Q1", fontface = "bold",
            color = "steelblue4",
            hjust = -0.5) +
  geom_text(data = filter(df4, city == "Sao Goncalo"),
            aes(x = Q5, y = city),
            label = "Ricos Q5", fontface = "bold",
            color = "springgreen4",
            hjust = 1.5) +
  geom_text(data = filter(df4, city == "Sao Goncalo"),
            aes(x = Total, y = city),
            label = "Total", fontface = "bold",
            color = "black",
            vjust = -1) +
  expand_limits(y = 21)+
  theme_ipsum_rc(grid= "X") +
  labs(x = "", y = "", title = "")



ggsave(temp_fig4, file="./figures/fig4_TMIEI_walk_renda.png", dpi = 300, width = 16, height = 16, units = "cm")
beep()




# grafico!


# grafico!
library(ggalt)
library(hrbrthemes)






# 5) Box plot por renda - acesso a emprego de PT - 60 min --------------------------------


# abrir acessibilidade
path_acess <- dir("../data/output_access/", 
                  full.names = TRUE, 
                  pattern = paste0(munis_df[modo == "todos"]$abrev_muni, collapse = "|"))

acess_bike <- hex_dt %>%
  # pegar so TP
  filter(mode == "bike")



# title <- bquote("Distribuição da acessibilidade por"~bold(.("transporte público"))~"à"~bold(.("oportunidades de trabalho")))

baseplot2 <- theme_minimal(base_family = "Roboto Condensed") +
  theme( 
    # plot.background = element_rect(fill = "grey85"),
    axis.text.y  = element_text(face="bold")
    # ,axis.text.x  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 8, face ="bold")
    ,legend.text = element_text(size = 8)
    , legend.position = "top"
    , axis.title = element_text(size = 8)
    , axis.text.x = element_blank()
    , title = element_text(size = 9)
    , plot.margin=unit(c(2,0,0,0),"mm")
    , axis.ticks.x = element_blank()
    , axis.line.x = element_blank()
  )


acess_bike %>% 
  filter(quintil >0) %>%
  mutate(city = ifelse(city == "bel", "bho", ifelse(city == "sao", "spo", ifelse(city == "por", "poa", city)))) %>%
  # filtrar so as cidades de tamanho semelhante
  filter(city %in% c("for", "bho", "cur")) %>%
  # # Wide to long
  # gather(threshold, acess_abs, CMA_TT_15:CMA_TT_90) %>%
  # mutate(threshold1 = as.integer(str_extract(threshold, "\\d+$"))) %>%
  # Refactor quintil
  # mutate(quintil1 = quintil - 1) %>%
  mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(quintil), y = CMATQ45, fill = factor(quintil)),
               # size = 1, 
               color = "black",
               outlier.colour=rgb(.5,.5,.5, alpha=0.1)) +
  # facet_grid(threshold_name ~ ., scales = "free_y") +
  facet_wrap(~city) +
  scale_fill_brewer(palette = "YlOrBr") +
  scale_y_percent()+
  # hrbrthemes::theme_ipsum_rc() +
  labs(fill = "Quintil de renda",
       x = "",
       y = "Porcentagem de oportunidades acessíveis",
       title = "Acessibilidade por bicicleta em 45 minutos para oportunidades de trabalho por quintil de renda") + 
  guides(color=guide_legend(nrow=1)) +
  baseplot2


ggsave(file="./figures/fig5-boxplot_renda_bike_CMATQ45.png", dpi = 300, width = 16.5, height = 15, units = "cm")





# PLOT TESTE

teste <- setDT(acess_bike)[, .(Negra=sum(cor_negra[which(TMIEM>30)] ,na.rm=T) /sum(cor_negra, na.rm=T),
               Branca=sum(cor_branca[which(TMIEM>30)] ,na.rm=T) /sum(cor_branca, na.rm=T)), 
           by=city]




teste %>%
  mutate(city = ifelse(city == "bel", "bho", ifelse(city == "sao", "spo", ifelse(city == "por", "poa", city)))) %>%
  mutate(city = factor(city, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  gather(tipo, valor, Negra:Branca) %>%
  ggplot()+
  geom_bar(aes(x = forcats::fct_reorder(factor(city), valor), y = valor, fill = tipo), 
               stat = "identity", position=position_dodge())+
  coord_flip()+
  scale_y_percent()+
  theme_ipsum_rc(grid = "X")+
  labs(title = "Proporção da população que leva mais que 30 min\npara acessar escola pública de ensino fundamental",
       y = "", x = "",
       fill = "")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))



ggsave(file="./figures/fig6-teste_pop.png", dpi = 300, width = 16.5, height = 15, units = "cm")
  