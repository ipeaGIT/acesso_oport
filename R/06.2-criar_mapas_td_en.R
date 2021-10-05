# carregar bibliotecas ----------------
source('./R/fun/setup.R')

library(ggalt)
library(hrbrthemes)
library(ggnewscale) # install.packages("ggnewscale")
library(ggsn)



# FIGURAS A FAZER

# MAPAS (feitos para todas as cidades):

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

# abrir dados
acess_final <- read_rds("../data/output_base_final/2019/dados2019_AcessOport_v1.0_20200116_interno.rds")


###### B. temas para mapas ---------------------------

theme_for_CMA <- function(base_size, ...) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
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
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 6)
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
munis_sf <- lapply(munis_df_2019$code_muni, geobr::read_municipality) %>% rbind_list() %>% st_sf()
st_crs(munis_sf) <- st_crs(states_sf)

munis_sf <- munis_sf %>%
  left_join(munis_df_2019, by = "code_muni") %>%
  # number it according to order
  mutate(n = case_when(abrev_muni == "bho" ~ 1,
                       abrev_muni == "cur" ~ 2,
                       abrev_muni == "for" ~ 3,
                       abrev_muni == "poa" ~ 4,
                       abrev_muni == "rec" ~ 5,
                       abrev_muni == "rio" ~ 6,
                       abrev_muni == "spo" ~ 7,
                       abrev_muni == "bel" ~ 8,
                       abrev_muni == "bsb" ~ 9,
                       abrev_muni == "cam" ~ 10,
                       abrev_muni == "cgr" ~ 11,
                       abrev_muni == "duq" ~ 12,
                       abrev_muni == "goi" ~ 13,
                       abrev_muni == "gua" ~ 14,
                       abrev_muni == "mac" ~ 15,
                       abrev_muni == "man" ~ 16,
                       abrev_muni == "nat" ~ 17,
                       abrev_muni == "sal" ~ 18,
                       abrev_muni == "sgo" ~ 19,
                       abrev_muni == "slz" ~ 20
  )) %>%
  # format
  mutate(text = paste0(n, ".", " ", name_muni.x)) %>%
  mutate(type = ifelse(abrev_muni %in% c("bho", "cur", "for", "poa", "rec", "rio", "spo"), 
                       "Active and Public Transport",
                       "Active Transport")) %>%
  mutate(color = ifelse(abrev_muni %in% c("bho", "cur", "for", "poa", "rec", "rio", "spo"), 
                        "#469c77", "steelblue4"))

# get centroids of municipalities
munis_centroids <- st_centroid(munis_sf)
munis_tp_centroids <- subset(munis_centroids, code_muni %in% munis_df_2019$code_muni[which(munis_df_2019$modo=='todos')])

munis_tp_centroids_df <- sfc_as_cols(munis_centroids)
  


# create sp map
sp <- ggplot()+
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.1), fill="steelblue4", color="gray95", alpha=.8) +
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.1), fill="#469c77", color="gray95", alpha=.8) +
  ggrepel::geom_text_repel(data = filter(munis_tp_centroids_df, abbrev_state == "SP"), aes(x = lon, y = lat, label = n),
                           segment.size = 3, size=2.5)+
  theme_void() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        panel.background=element_rect(fill = "gray98"),
        panel.border = element_rect(fill = NA))+
  coord_sf(expand = FALSE,
           xlim = c(filter(munis_tp_centroids_df, abrev_muni == "spo")$lon-1.2, 
                    filter(munis_tp_centroids_df, abrev_muni == "spo")$lon+1.2),
           ylim = c(filter(munis_tp_centroids_df, abrev_muni == "spo")$lat-0.8, 
                    filter(munis_tp_centroids_df, abrev_muni == "spo")$lat+1.2))

# create rio map
rio <- ggplot()+
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.1), fill="steelblue4", color="gray95", alpha=.8) +
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.1), fill="#469c77", color="gray95", alpha=.8) +
  ggrepel::geom_text_repel(data = filter(munis_tp_centroids_df, abbrev_state == "RJ"), aes(x = lon, y = lat, label = n),
                           segment.size = 3, size=2.5)+
  theme_void() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        panel.background=element_rect(fill = "gray98"),
        panel.border = element_rect(fill = NA))+
  coord_sf(expand = FALSE,
           xlim = c(filter(munis_tp_centroids_df, abrev_muni == "rio")$lon-1.2, 
                    filter(munis_tp_centroids_df, abrev_muni == "rio")$lon+1.2),
           ylim = c(filter(munis_tp_centroids_df, abrev_muni == "rio")$lat-0.5, 
                    filter(munis_tp_centroids_df, abrev_muni == "rio")$lat+1))
  
  

# create map

temp_map1 <- 
  ggplot() + 
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.5), fill="steelblue4", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.5), fill="#469c77", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
  ggrepel::geom_text_repel(data = filter(munis_tp_centroids_df, abbrev_state %nin% c("SP", "RJ")), 
                                         aes(x = lon, y = lat, label = n),
                           segment.size = 3, size=3)+
  theme(panel.background = element_rect(fill = "gray98", colour = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank()) + 
  labs(x = '', y = '')+
  coord_sf(expand = FALSE, 
           xlim = c(st_bbox(states_sf)[[1]], st_bbox(states_sf)[[3]]),
           ylim = c(st_bbox(states_sf)[[2]], st_bbox(states_sf)[[4]])) # coord_cartesian Coordinates Zoom
  # guides(colour = guide_legend(override.aes = list(size=8)))

arrowA <- data.frame(x1 = 14.5, x2 = 10, 
                     y1 = 6.1,  y2 = 5) # 1: arrow!

arrowB <- data.frame(x1 = 16.9, x2 = 17.7, 
                     y1 = 6.1,  y2 = 5.3) # 1: arrow!

library(cowplot)
library(gridExtra)

t1 <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 4), "bold.italic")),
  bg_params = list(fill=c(rep(c("grey95", "grey90"),
                              length.out=4), "#6BAED6"),
                   alpha = rep(c(1,0.5), each=5))
))

tt1 <- ttheme_minimal(
  
  padding = unit(c(2, 2), "mm"),
  base_size = 8.5,
  core=list(fg_params=list(col="#469c77", hjust=0, x=0))
  
  )

tt2 <- ttheme_minimal(
  
  padding = unit(c(2, 2), "mm"),
  base_size = 8.5,
  core=list(fg_params=list(col="steelblue4", hjust=0, x=0))
  
  )

# textos
t1 <- textGrob("Active and Public\n Transport",  gp=gpar(col="#469c77", fontsize=10, fontface = "bold"),
               just = c("right"))
t2 <- textGrob("Active\n Transport", gp=gpar(col="steelblue4", fontsize=10, fontface = "bold"), 
               just = c("right"))
# t <- textGrob("[", gp=gpar(fontsize=50))

# tabelas
table1 <- munis_tp_centroids_df %>% arrange(n) %>% filter(modo == "todos") %>% .$text
table2 <- munis_tp_centroids_df %>% arrange(n) %>% filter(modo == "ativo") %>% .$text

fim <- ggplot() +
  coord_equal(xlim = c(0, 35), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(temp_map1), 
                    xmin = 0, xmax = 25, 
                    ymin = 0, ymax = 20) +
  annotation_custom(ggplotGrob(sp), 
                    xmin = 1.5, xmax = 9, 
                    ymin = 0, ymax = 9) +
  annotation_custom(t1, 
                    xmin = 21, xmax = 24,
                    ymin = 17, ymax = 18)+
  annotation_custom(t2, 
                    xmin = 21, xmax = 24,
                    ymin = 8, ymax = 9)+
  # annotation_custom(t, 
  #                   xmin = 22, xmax = 23,
  #                   ymin = 0, ymax = 12)+
  annotation_custom(gridExtra::tableGrob(table1,
                                         rows = NULL, cols = NULL, theme = tt1),
                    xmin = 19, xmax = Inf,
                    ymin = 12, ymax = 20.5)+
  annotation_custom(gridExtra::tableGrob(table2, 
                                         rows = NULL, cols = NULL, theme = tt2),
                    xmin = 20.3, xmax = Inf,
                    ymin = 0, ymax = 12.5)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(length = unit(0.02, "npc")), lineend = "round")+
  annotation_custom(ggplotGrob(rio), 
                    xmin = 15, xmax = 21, 
                    ymin = 0, ymax = 5)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
               arrow = arrow(length = unit(0.02, "npc")), lineend = "round")+
  theme(panel.background = element_rect(fill = NA, colour = NA),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  labs(x = "", y = "")



# save map
ggsave(fim, 
       file="../publicacoes/2020_access_inequalities_paper/figures/fig0_munis_all_test.png", 
       dpi = 300, width = 16, height = 12, units = "cm")
beepr::beep()



# criar diretorio
# dir.create("../figures/td_todas")


# 1) Mapa TMI saude de media e alta - PT  ---------------------------------------

# sigla_munii <- "bsb"; cols = 2; width = 14; height = 10

fazer_plot_1 <- function(sigla_munii, cols = 2, width = 14, height = 11) {
  
  # abrir acess
  # acess <- read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_muni))
  acess <- acess_final %>% filter(sigla_muni == sigla_munii)
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_munii))
  
  
  # tirar so pt e pico e colocar em format long
  acess_pt_pico <- acess %>%
    filter(modo == "tp" & pico == 1) %>%
    # tirar so saude medio e alta
    select(TMISM, TMISA) %>%
    gather(ind, valor, TMISM:TMISA)
  
  # fazer grafico
  acess_pt_pico <- acess_pt_pico %>%
    mutate(valor = ifelse(valor > 30, 30, valor)) %>%
    mutate(ind = factor(ind, 
                        levels = c("TMISM", "TMISA"), 
                        labels = c("Medium Complexity Health Care", 
                                   "High Complexity Health Care")))
  
  # determine scale size
  dist_scale <- ifelse(sigla_munii %in% c("spo", "rio"), 8, 4)
  
  plot1 <- ggplot() + 
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    geom_sf(dat = st_transform(acess_pt_pico, 3857), aes(fill = valor), color = NA, alpha=.7)  +
    viridis::scale_fill_viridis( direction = -1
                                 # , breaks = c(0, 10, 20, 30, 40)
                                 , labels = c(0, 10, 20, "+30 min")
    ) +
    labs(fill = "Time to reach\n the closest opportunity",
         title = munis_df_2019[abrev_muni == sigla_munii]$name_muni)+
    facet_wrap(~ind, ncol = cols)+
    theme_for_TMI()+
    theme(plot.title = element_text(hjust = 0.5))+
    ggsn::scalebar(st_transform(acess_pt_pico, 3857), dist = dist_scale, dist_unit = "km", st.dist = 0.03,
             transform = FALSE, model = "WGS84", st.size = 2.5, height=0.01,
             facet.var = 'ind',
             border.size = 0.3,
             facet.lev = 'High Complexity Health Care')
  
  
  # save map
  ggsave(plot = plot1, 
         filename = sprintf("../publicacoes/2020_access_inequalities_paper/figures/fig1/fig1-%s_TMI_SM_TP.png", sigla_munii), 
         device  = "png",
         dpi = 300, width = width, height = height, units = "cm")
}





# 2) TMI Ed Infantil Walk -------------------------

fazer_plot_2 <- function(sigla_munii, cols = 2, width = 14, height = 13) {
  
  # abrir acess
  acess <- acess_final %>% filter(sigla_muni == sigla_munii)
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_munii))
  
  # tirar so pt e pico e colocar em format long
  acess_pt_pico <- acess %>%
    filter(modo == "caminhada" & pico == 1) %>%
    # tirar so educacao
    select(TMIEI, TMIEF) %>%
    gather(ind, valor, TMIEI:TMIEF)
  
  # fazer grafico
  acess_pt_pico <- acess_pt_pico %>%
    mutate(valor = ifelse(valor > 30, 30, valor)) %>%
    mutate(ind = factor(ind, 
                        levels = c("TMIEI", "TMIEF"), 
                        labels = c("Early Childhood Education", 
                                   "Elementary Education")))
  
  # determine scale size
  dist_scale <- ifelse(sigla_munii %in% c("man"), 16,
                       ifelse(sigla_munii %in% c("man", "cgr", "bsb"), 12,
                              ifelse(sigla_munii %in% c("spo", "rio", "man", "bsb", "cgr"), 8, 
                                     ifelse(sigla_munii %in% c("goi"), 6,
                                            ifelse(sigla_munii %in% c("nat"), 3, 4)))))
  
  plot2 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    geom_sf(data = st_transform(acess_pt_pico, 3857), aes(fill = valor), color = NA, alpha=.7)  +
    viridis::scale_fill_viridis( direction = -1
                                 # , breaks = c(0, 15, 30)
                                 , labels = c(0, 10, 20, "+30 min")
    ) +
    labs(fill = "Time to reach\n the closest opportunity",
         title = munis_df_2019[abrev_muni == sigla_munii]$name_muni)+
    facet_wrap(~ind, ncol = cols)+
    theme_for_TMI()+
    theme(plot.title = element_text(hjust = 0.5))+
    ggsn::scalebar(st_transform(acess_pt_pico, 3857), dist = dist_scale, dist_unit = "km", st.dist = 0.03,
             transform = FALSE, model = "WGS84", st.size = 2.5, height=0.01,
             facet.var = 'ind',
             border.size = 0.3,
             facet.lev = 'Elementary Education')

  
  # save 
  ggsave(plot = plot2, 
         filename = sprintf("../publicacoes/2020_access_inequalities_paper/figures/fig2/fig2-%s_TMI_EI_walk.png", sigla_munii), 
         device  = "png",
         dpi = 300, width = width, height = height, units = "cm")
}



# 3) CMA Trabalho Bike 15/45 ----------------------------------


fazer_plot_3 <- function(sigla_munii, cols = 2, width = 14, height = 11) {
  
  # abrir acess
  acess <- acess_final %>% filter(sigla_muni == sigla_munii) %>%
    # read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_munii)) %>%
    filter(modo == "bicicleta") %>%
    select(sigla_muni, CMATT15, CMATT45) %>%
    gather(ind, valor, CMATT15:CMATT45)
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_munii))
  
  # ajustar levels
  acess <- acess %>%
    mutate(ind = factor(ind, 
                        levels = c("CMATT15", "CMATT45"), 
                        labels = c("15 Minutes", "45 Minutes")))
  
  # determine scale size
  dist_scale <- ifelse(sigla_munii %in% c("man"), 16,
                       ifelse(sigla_munii %in% c("man", "cgr", "bsb"), 12,
                              ifelse(sigla_munii %in% c("spo", "rio", "man", "bsb", "cgr"), 8, 
                                     ifelse(sigla_munii %in% c("goi"), 6,
                                            ifelse(sigla_munii %in% c("nat"), 3, 4)))))
  
  
  # fazer plots
  plot3 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
    viridis::scale_fill_viridis(option = "B", labels = scales::percent
                                # , limits = c(0, 0.72)
                                # , breaks = c(0.001, 0.35, 0.7)
                                # , labels = c(0, "35", "70%")
    ) +
    facet_wrap(~ind, ncol = cols)+
    theme_for_CMA()+
    labs(fill = "Proportion of accessible\n opportunities",
         title = munis_df_2019[abrev_muni == sigla_munii]$name_muni) +
    theme(plot.title = element_text(hjust = 0.5))+
    ggsn::scalebar(st_transform(acess, 3857), dist = dist_scale, dist_unit = "km", st.dist = 0.03,
             transform = FALSE, model = "WGS84", st.size = 2.5, height=0.01,
             facet.var = 'ind',
             border.size = 0.3,
             facet.lev = '45 Minutes')
  
  
  
  # save 
  ggsave(plot = plot3, 
         filename = sprintf("../publicacoes/2020_access_inequalities_paper/figures/fig3/fig3-%s_CMA_TT_1545.png", sigla_munii), 
         device  = "png",
         dpi = 300, width = width, height = height, units = "cm")
}


# 4) CMA Trabalho/Escola TP 60 ---------------------

fazer_plot_4 <- function(sigla_munii, cols = 2, width = 14, height = 11) {
  
  # abrir acess
  acess <-  acess_final %>% filter(sigla_muni == sigla_munii) %>%
    # read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_munii)) %>% 
    filter(modo == "tp") %>%
    select(sigla_muni, CMATT60, CMAEF60) %>%
    gather(ind, valor, CMATT60:CMAEF60)
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_munii))
  
  # fazer grafico
  acess <- acess %>%
    mutate(ind = factor(ind, 
                        levels = c("CMATT60", "CMAEF60"), 
                        labels = c("Work", "Elementary Education")))
  
  # determine scale size
  dist_scale <- ifelse(sigla_munii %in% c("man"), 16,
                       ifelse(sigla_munii %in% c("man", "cgr", "bsb"), 12,
                              ifelse(sigla_munii %in% c("spo", "rio", "man", "bsb", "cgr"), 8, 
                                     ifelse(sigla_munii %in% c("goi"), 6,
                                            ifelse(sigla_munii %in% c("nat"), 3, 4)))))
  
  # fazer plots
  plot4 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.5)+
    viridis::scale_fill_viridis(option = "B", labels = scales::percent
                                # , limits = c(0, 0.9)
                                # , breaks = c(0.001, 0.45, 0.9)
                                # , labels = c(0, "45", "90%")
    )+
    facet_wrap(~ind, ncol = cols)+
    theme_for_CMA()+
    labs(fill = "Proportion of accessible\n opportunities",
         title = munis_df_2019[abrev_muni == sigla_munii]$name_muni) +
    theme(plot.title = element_text(hjust = 0.5))+
    ggsn::scalebar(st_transform(acess, 3857), dist = dist_scale, dist_unit = "km", st.dist = 0.03,
             transform = FALSE, model = "WGS84",  st.size = 2.5, height=0.01,
             facet.var = 'ind',
             border.size = 0.3,
             facet.lev = 'Elementary Education')
  
  
  
  # save 
  ggsave(plot = plot4, 
         filename = sprintf("../publicacoes/2020_access_inequalities_paper/figures/fig4/fig4-%s_CMA_TTEF_60.png", sigla_munii), 
         device  = "png",
         dpi = 300, width = width, height = height, units = "cm")
  
}


# Aplicar funcoes -----------

purrr::walk(munis_df_2019[modo == "todos"]$abrev_muni, fazer_plot_1, height = 12)
purrr::walk(munis_df_2019$abrev_muni, fazer_plot_2, height = 12)
purrr::walk(munis_df_2019$abrev_muni, fazer_plot_3, height = 12)
purrr::walk(munis_df_2019[modo == "todos"]$abrev_muni, fazer_plot_4, height = 12)

# para o rio, optar por uma coluna so no plot!
fazer_plot_1('rio', cols = 1, width = 12, height = 16)
fazer_plot_1('bsb', cols = 1, width = 12, height = 18)
fazer_plot_1('man', cols = 1, width = 12, height = 18)

fazer_plot_2('rio', cols = 1, width = 12, height = 18)
fazer_plot_2('bsb', cols = 1, width = 12, height = 18)
fazer_plot_2('man', cols = 1, width = 12, height = 18)

fazer_plot_3('rio', cols = 1, width = 12, height = 18)
fazer_plot_3('bsb', cols = 1, width = 12, height = 18)
fazer_plot_3('man', cols = 1, width = 12, height = 18)

fazer_plot_4('rio', cols = 1, width = 12, height = 18)






  # 5) TMI Ponto Decil Todos EdMedia Bike  --------------------------------

# abrir o acess de todas as cidades e juntar

# seleciona so bike
acess_bike <- setDT(acess_final)[ modo == "bicicleta" ]

# Se nenhuma atividade acessivel (TMIEM==Inf), entao imputar TMIEM de 90 min.
acess_bike[, TMIEM := fifelse(TMIEM==Inf, 90, TMIEM)]

# tempo medio ponderapo pela populacao de cada hexagono
df4 <- acess_bike[, .(Total = weighted.mean(x = TMIEM[which(P001>0)], w = P001[which(P001>0)], na.rm=T),
                      Negra = weighted.mean(TMIEM[which(P003>0)], w = P003[which(P003>0)], na.rm=T),
                      Branca = weighted.mean(TMIEM[which(P002>0)], w = P002[which(P002>0)], na.rm=T),
                      Q1 = weighted.mean(TMIEM[which(R002==1)], w = P001[which(R002==1)], na.rm=T),
                      Q5 = weighted.mean(TMIEM[which(R002==5)], w = P001[which(R002==5)], na.rm=T)), by=sigla_muni]



# ajeitar nome das cidade
df4 <- df4 %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni))



df4 %>%
  ggplot() + 
  geom_dumbbell(aes(x = Q5, xend = Q1, y = forcats::fct_reorder(sigla_muni, Q1)), 
                size=3, color="gray80", alpha=.8, colour_x = "steelblue4", colour_xend = "springgreen4") +
  geom_point(aes(x = Total, y = sigla_muni), color = "black", size = 2)+
  scale_color_manual(values=c('#f0a150', '#f48020', '#f0750f'), 
                     name="", 
                     labels=c('Poorest Q1', 'Mean', 'Richest Q5')) +
  scale_x_continuous(name="", limits = c(0, 24),
                     breaks = c(0, 5, 10, 15, 20),
                     labels = c(0, 5,  10, 15,"20 minutes")) +
  geom_text(data = filter(df4, sigla_muni == "Goiania"),
            aes(x = Q1, y = sigla_muni),
            label = "Poorest Q1", fontface = "bold",
            color = "springgreen4",
            hjust = -0.5) +
  geom_text(data = filter(df4, sigla_muni == "Goiania"),
            aes(x = Q5, y = sigla_muni),
            label = "Richest Q5", fontface = "bold",
            color = "steelblue4",
            hjust = 1.5) +
  geom_text(data = filter(df4, sigla_muni == "Goiania"),
            aes(x = Total, y = sigla_muni),
            label = "Total", fontface = "bold",
            color = "black",
            vjust = -1) +
  expand_limits(y = 21)+
  theme_ipsum(grid= "X") +
  labs(x = "", y = "", title = "")

# TME bike
# tme walk



ggsave(file="../figures/td_en/fig5_TMIEM_bike_renda90median.png", dpi = 300, width = 16, height = 16, units = "cm")
beep()







# 6) CMA Boxplot Decil cam/poa/goi Trabalho TP 30 ------------------------------

acess_walk <- acess_final %>%
  # pegar so TP
  filter(modo == "caminhada")



baseplot2 <- theme_minimal() +
  theme( 
    axis.text.y  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
    , legend.position = "bottom"
    , axis.text.x = element_blank()
    
  )

acess_walk %>% 
  filter(R003 >0) %>%
  # filtrar so as cidades de tamanho semelhante
  filter(sigla_muni %in% c("cam", "poa", "goi")) %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(R003), y = CMATT30, weight=P001, color = factor(R003)),
               # size = 1, 
               outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
  # facet_grid(threshold_name ~ ., scales = "free_y") +
  facet_wrap(~sigla_muni) +
  scale_colour_brewer(palette = "RdBu", labels=c('D1 Poorest', paste0('D', c(2:9)), 'D10 Richest'), name='Income decile') +
  scale_y_percent() +
  hrbrthemes::theme_ipsum(grid = "Y") +
  labs(x = "",
       y = "Proportion of accesible\n oportunities") + 
  guides(color=guide_legend(nrow=1)) +
  theme( 
    axis.text.y  = element_text(face="bold")
    ,panel.grid.minor = element_blank()
    ,strip.text = element_text(size = 11, face ="bold")
    ,legend.text = element_text(size = 11)
    , legend.position = "bottom"
    , axis.text.x = element_blank()
    
  )


ggsave(file="../figures/td_en/fig6-boxplot_CMA_rendadecil_walk_TQ_30.png", dpi = 300, width = 25, height = 15, units = "cm")
beep()






# 7) CMA Palma Renda Todos Trabalho  Walk 30  --------------------------------

# calcular acessibilidade media do 90 percentil e 40 percentil de renda
# usar caminhada pico CMATT30

acess_palma <- acess_final %>%
  st_set_geometry(NULL) %>%
  filter(modo == "bicicleta" & pico == 1) %>%
  select(sigla_muni, R003, P001, CMATT30) %>%
  # pegar so decis 4 e 9
  filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
  # definir ricos e pobres
  mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
  group_by(sigla_muni, classe) %>%
  summarise(acess_media = weighted.mean(CMATT30, P001)) %>%
  ungroup() %>%
  spread(classe, acess_media) %>%
  # calcular palma ratio
  group_by(sigla_muni) %>%
  mutate(palma_ratio = rico/pobre) %>%
  ungroup()

# visualizar
acess_palma %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  mutate(sigla_muni = fct_reorder(sigla_muni, palma_ratio)) %>%
  ggplot()+
  geom_col(aes(y = palma_ratio, x = sigla_muni))+
  geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
  scale_y_continuous(breaks = c(0, 1, 3, 6, 9))+
  coord_flip()+
  theme_ipsum(grid = "X")+
  labs(x = "", y = "Palma Ratio")


ggsave(file="../figures/td_en/fig7-palma_ratio_CMA_TQ_walk_30.png", dpi = 300, width = 16.5, height = 15, units = "cm")




# 8) CMA Palma Cor SaudeAlta TP 60  --------------------------------

# calcular acessibilidade media do 90 percentil e 40 percentil de renda
# usar TP pico CMASA60

acess_palma_2 <- acess_final %>%
  st_set_geometry(NULL) %>%
  filter(modo == "tp" & pico == 1) %>%
  select(sigla_muni, R003, P001, P003, P002, CMASA60) %>%
  group_by(sigla_muni) %>%
  summarise(acess_brancos = weighted.mean(CMASA60, P002),
            acess_negros = weighted.mean(CMASA60, P003)) %>%
  # calcular palma ratio
  mutate(palma_ratio = acess_brancos/acess_negros) %>%
  ungroup()

# visualizar
acess_palma_2 %>%
  filter(sigla_muni %in% munis_df[modo == "todos"]$abrev_muni) %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  mutate(sigla_muni = fct_reorder(sigla_muni, palma_ratio)) %>%
  ggplot()+
  geom_col(aes(y = palma_ratio, x = sigla_muni))+
  geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
  scale_y_continuous(breaks = seq(0, 3, .5))+
  coord_flip()+
  theme_ipsum(grid = "X")+
  labs(x = "", y = "Ratio Whites/People of Color")


ggsave(file="../figures/td_en/fig8-palma_ratio_cor_CMA_SA_TP_60.png", dpi = 300, width = 16.5, height = 15, units = "cm")


acess_palma_3 <- acess_final %>%
  filter(modo == "caminhada") %>%
  select(sigla_muni, R003, P001, P003, P002, CMASA60) %>%
  group_by(sigla_muni) %>%
  summarise(acess_brancos = weighted.mean(CMASA60, P002),
            acess_negros = weighted.mean(CMASA60, P003)) %>%
  # calcular palma ratio
  mutate(palma_ratio = acess_brancos/acess_negros) %>%
  ungroup()

# visualizar
acess_palma_3 %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
  mutate(sigla_muni = fct_reorder(sigla_muni, palma_ratio)) %>%
  ggplot()+
  geom_col(aes(y = palma_ratio, x = sigla_muni))+
  geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
  scale_y_continuous(breaks = seq(0, 3, .5))+
  coord_flip()+
  theme_ipsum(grid = "X")+
  labs(x = "", y = "Ratio Whites/People of Color")

ggsave(file="../figures/td_en/fig8-palma_ratio_cor_CMA_SA_walk_60.png", dpi = 300, width = 16.5, height = 15, units = "cm")
