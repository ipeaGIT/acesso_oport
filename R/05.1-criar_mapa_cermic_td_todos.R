# carregar bibliotecas ----------------
source('./R/fun/setup.R')
source("R/fun/crop_ggmap.R")

library(ggalt)
library(hrbrthemes)
library(ggnewscale) # install.packages("ggnewscale")



# MAPAS:

# 1) TMI SaudeMedia/Alta TP
# 2) TMI Ed Infantil Walk
# 3) CMA Trabalho Bike 15/45
# 4) CMA Trabalho/Escola TP 60

###### A. temas para mapas ---------------------------

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
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 6)
      # legend.key.width=unit(0.5,"cm")
      
    )
}


# criar diretorio
# dir.create("../figures/td_todas")


# 1) Mapa TMI saude de media e alta - PT  ---------------------------------------

fazer_plot_1 <- function(sigla_muni, cols = 2) {
  
  # abrir acess
  acess <- read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_muni))
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_muni))
  
  
  # tirar so pt e pico e colocar em format long
  acess_pt_pico <- acess %>%
    filter(mode == "transit" & pico == 1) %>%
    # tirar so saude medio e alta
    select(TMISM, TMISA) %>%
    gather(ind, valor, TMISM:TMISA)
  
  # fazer grafico
  acess_pt_pico <- acess_pt_pico %>%
    mutate(valor = ifelse(valor > 30, 30, valor)) %>%
    mutate(ind = factor(ind, 
                        levels = c("TMISM", "TMISA"), 
                        labels = c("Saúde Média Complexidade", "Saúde Alta Complexidade")))
  
  plot1 <- ggplot() + 
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 0.5) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    geom_sf(dat = st_transform(acess_pt_pico, 3857), aes(fill = valor), color = NA, alpha=.7)  +
    viridis::scale_fill_viridis( direction = -1
                                 # , breaks = c(0, 10, 20, 30, 40)
                                 # , labels = c(0, 10, 20, 30, "+40 min")
                                 ) +
    labs(fill = "Tempo até a oportunidade\n mais próxima",
         title = munis_df[abrev_muni == sigla_muni]$name_muni)+
    facet_wrap(~ind, ncol = cols)+
    theme_for_TMI()+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # save map
  ggsave(plot1, 
         file= sprintf("../figures/td_todas/ceramic/fig1-%s_TMI_SM_TP.png", sigla_muni), 
         dpi = 300, width = 14, height = 10, units = "cm")
}





# 2) TMI Ed Infantil Walk -------------------------

fazer_plot_2 <- function(sigla_muni, cols = 2) {
  
  # abrir acess
  acess <- read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_muni))
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_muni))
  
  # tirar so pt e pico e colocar em format long
  acess_pt_pico <- acess %>%
    filter(mode == "walk" & pico == 1) %>%
    # tirar so educacao
    select(TMIEI, TMIEF) %>%
    gather(ind, valor, TMIEI:TMIEF)
  
  # fazer grafico
  acess_pt_pico <- acess_pt_pico %>%
    mutate(valor = ifelse(valor > 30, 30, valor)) %>%
    mutate(ind = factor(ind, 
                        levels = c("TMIEI", "TMIEF"), 
                        labels = c("Educação Infantil", "Educação Fundamental")))
  
  plot2 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = .5) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    geom_sf(data = st_transform(acess_pt_pico, 3857), aes(fill = valor), color = NA, alpha=.7)  +
    viridis::scale_fill_viridis( direction = -1
                                 # , breaks = c(0, 15, 30)
                                 # , labels = c(0, 15,"+30 min")
                                 ) +
    labs(fill = "Tempo até a oportunidade\n mais próxima",
         title = munis_df[abrev_muni == sigla_muni]$name_muni)+
    facet_wrap(~ind, ncol = cols)+
    theme_for_TMI()+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  
  # save map
  ggsave(plot2, 
         file= sprintf("../figures/td_todas/ceramic/fig2-%s_TMI_EI_walk.png", sigla_muni),
         dpi = 300, width = 14, height = 10, units = "cm")
}



# 3) CMA Trabalho Bike 15/45 ----------------------------------


fazer_plot_3 <- function(sigla_muni, cols = 2) {

  # abrir acess
  acess <- read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_muni)) %>%
  filter(mode == "bike") %>%
    select(city, CMATQ15, CMATQ45) %>%
    gather(ind, valor, CMATQ15:CMATQ45)
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_muni))
  
  # ajustar levels
  acess <- acess %>%
    mutate(ind = factor(ind, 
                        levels = c("CMATQ15", "CMATQ45"), 
                        labels = c("15 Minutos", "45 Minutos")))
  
  
  # fazer plots
  plot3 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = .5) +
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
    labs(fill = "Porcentagem de oportunidades\n acessíveis",
         title = munis_df[abrev_muni == sigla_muni]$name_muni) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  ggsave(plot3, 
         file= sprintf("../figures/td_todas/ceramic/fig3-%s_CMA_TQ_1545.png", sigla_muni), 
         dpi = 300, width = 14, height = 10, units = "cm")
}


# 4) CMA Trabalho/Escola TP 60 ---------------------

fazer_plot_4 <- function(sigla_muni, cols = 2) {
  
  # abrir acess
  acess <- read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_muni)) %>% 
    filter(mode == "transit") %>%
    select(city, CMATQ60, CMAEF60) %>%
    gather(ind, valor, CMATQ60:CMAEF60)
  
  # abrir tiles
  map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_muni))
  
  # fazer grafico
  acess <- acess %>%
    mutate(ind = factor(ind, 
                        levels = c("CMATQ60", "CMAEF60"), 
                        labels = c("Trabalho", "Educação Fundamental")))
  
  # fazer plots
  plot4 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = .5) +
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
    labs(fill = "Porcentagem de oportunidades\n acessíveis",
         title = munis_df[abrev_muni == sigla_muni]$name_muni) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  ggsave(plot4, 
         file= sprintf("../figures/td_todas/ceramic/fig4-%s_CMA_TQEF_60.png", sigla_muni),
         dpi = 300, width = 14, height = 10, units = "cm")
  
}


# 5) Aplicar funcoes -----------

purrr::walk(munis_df[modo == "todos"]$abrev_muni, fazer_plot_1)
purrr::walk(munis_df$abrev_muni, fazer_plot_2)
purrr::walk(munis_df$abrev_muni, fazer_plot_3)
purrr::walk(munis_df[modo == "todos"]$abrev_muni, fazer_plot_4)

# para o rio, optar por uma coluna so no plot!
fazer_plot_1('rio', cols = 1)
fazer_plot_2('rio', cols = 1)
fazer_plot_3('rio', cols = 1)
fazer_plot_4('rio', cols = 1)


