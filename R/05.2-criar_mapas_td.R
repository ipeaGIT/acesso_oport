# carregar bibliotecas ----------------
source('./R/fun/setup.R')

library(ggalt)
library(hrbrthemes)
library(ggnewscale) # install.packages("ggnewscale")



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


fazer_figuras_td <- function(ano = 2019) {
  
  ###### A. Carrega dados ---------------------------
  
  if (ano %in% c(2019, 2020)) {
    
    # abrir dados
    acess_final <- read_rds(sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_20200116.rds", ano, ano))
    
  }
  
  # Select the corerspondent munis_df
  munis_df <- get(sprintf("munis_df_%s", ano))
  
  
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
  ggsave(temp_map1, file=sprintf("../figures/td/%s/mapa1_cidades do projeto_%s.pdf", ano, ano), dpi = 300, width = 16.5, height = 16.5, units = "cm")
  beepr::beep()
  
  
  
  # criar diretorio
  # dir.create("../figures/td_todas")
  
  
  # 1) Mapa TMI saude de media e alta - PT  ---------------------------------------
  
  fazer_mapa_2 <- function(sigla_munii, cols = 2, width = 14, height = 10) {
    
    # abrir acess
    # acess <- read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_muni))
    acess <- acess_final %>% filter(sigla_muni == sigla_munii)
    
    # abrir tiles
    map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_munii))
    
    
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
                          labels = c("Saúde Média Complexidade", "Saúde Alta Complexidade")))
    
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
      labs(fill = "Tempo até a oportunidade\n mais próxima",
           title = subset(munis_df, abrev_muni == sigla_munii)$name_muni)+
      facet_wrap(~ind, ncol = cols)+
      theme_for_TMI()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # create dir
    suppressWarnings(dir.create("../figures/td/2020/mapa2_tempo de viagem por transp-publico ate estab saude mais proximo"))
    
    # save map
    ggsave(plot1, 
           file= sprintf("../figures/td/%s/mapa2_tempo de viagem por transp-publico ate estab saude mais proximo/mapa2_%s_TMI_SM_TP_%s.svg", ano, sigla_munii, ano), 
           dpi = 300, width = width, height = height, units = "cm")
  }
  
  
  
  
  
  # 2) TMI Ed Infantil Walk -------------------------
  
  fazer_mapa_3 <- function(sigla_munii, cols = 2, width = 14, height = 10) {
    
    # abrir acess
    acess <- acess_final %>% filter(sigla_muni == sigla_munii)
    
    # abrir tiles
    map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_munii))
    
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
                          labels = c("Educação Infantil", "Educação Fundamental")))
    
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
      labs(fill = "Tempo até a oportunidade\n mais próxima",
           title = subset(munis_df, abrev_muni == sigla_munii)$name_muni)+
      facet_wrap(~ind, ncol = cols)+
      theme_for_TMI()+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    # create dir
    suppressWarnings(dir.create("../figures/td/2020/mapa3_tempo de viagem a pe ate a escola mais proxima"))
    
    
    # save map
    ggsave(plot2, 
           file= sprintf("../figures/td/%s/mapa3_tempo de viagem a pe ate a escola mais proxima/mapa3-%s_TMI_EI_walk_%s.svg", ano, sigla_munii, ano),
           dpi = 300, width = width, height = height, units = "cm")
  }
  
  
  
  # 3) CMA Trabalho Bike 15/45 ----------------------------------
  
  
  fazer_mapa_4 <- function(sigla_munii, cols = 2, width = 14, height = 10) {
    
    # abrir acess
    acess <- acess_final %>% filter(sigla_muni == sigla_munii) %>%
      # read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_munii)) %>%
      filter(modo == "bicicleta") %>%
      select(sigla_muni, CMATT15, CMATT45) %>%
      gather(ind, valor, CMATT15:CMATT45)
    
    # abrir tiles
    map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_munii))
    
    # ajustar levels
    acess <- acess %>%
      mutate(ind = factor(ind, 
                          levels = c("CMATT15", "CMATT45"), 
                          labels = c("15 Minutos", "45 Minutos")))
    
    
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
      labs(fill = "Porcentagem de oportunidades\n acessíveis",
           title = subset(munis_df, abrev_muni == sigla_munii)$name_muni) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # create dir
    suppressWarnings(dir.create("../figures/td/2020/mapa4_proporcao de empregos acessiveis por bicicleta em 15 e 30 min"))
    
    
    ggsave(plot3, 
           file= sprintf("../figures/td/%s/mapa4_proporcao de empregos acessiveis por bicicleta em 15 e 30 min/mapa4-%s_CMA_TT_1545_%s.svg", ano, sigla_munii, ano), 
           dpi = 300, width = width, height = height, units = "cm")
  }
  
  
  # 4) CMA Trabalho/Escola TP 60 ---------------------
  
  fazer_mapa_5 <- function(sigla_munii, cols = 2, width = 14, height = 10) {
    
    # abrir acess
    acess <-  acess_final %>% filter(sigla_muni == sigla_munii) %>%
      # read_rds(sprintf("../data/output_access/acess_%s_2019.rds", sigla_munii)) %>% 
      filter(modo == "tp") %>%
      select(sigla_muni, CMATT60, CMAEF60) %>%
      gather(ind, valor, CMATT60:CMAEF60)
    
    # abrir tiles
    map_tiles <- read_rds(sprintf("../data/map_tiles_crop/ceramic/map_tile_crop_ceramic_%s.rds", sigla_munii))
    
    # fazer grafico
    acess <- acess %>%
      mutate(ind = factor(ind, 
                          levels = c("CMATT60", "CMAEF60"), 
                          labels = c("Trabalho", "Educação Fundamental")))
    
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
      labs(fill = "Porcentagem de oportunidades\n acessíveis",
           title = subset(munis_df, abrev_muni == sigla_munii)$name_muni) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # create dir
    suppressWarnings(dir.create("../figures/td/2020/mapas5_proporcao de empregos e escolas-med acessiveis por transp-pubico em ate 60 min"))
    
    # save plot
    ggsave(plot4, 
           file= sprintf("../figures/td/%s/mapas5_proporcao de empregos e escolas-med acessiveis por transp-pubico em ate 60 min/mapa_5-%s_CMA_TTEF_60_%s.svg", ano, sigla_munii, ano),
           dpi = 300, width = width, height = height, units = "cm")
    
  }
  
  
  # Aplicar funcoes -----------
  future::plan(future::multiprocess)
  
  furrr::future_map(munis_df[modo == "todos"]$abrev_muni, fazer_mapa_2, height = 13, .progress = F)
  furrr::future_map(munis_df$abrev_muni, fazer_mapa_3, height = 13, .progress = F)
  furrr::future_map(munis_df$abrev_muni, fazer_mapa_4, height = 13, .progress = F)
  furrr::future_map(munis_df[modo == "todos"]$abrev_muni, fazer_mapa_5, height = 13, .progress = F)
  
  # para o rio, optar por uma coluna so no plot!
  fazer_mapa_2('rio', cols = 1, width = 12, height = 18)
  fazer_mapa_3('rio', cols = 1, width = 12, height = 18)
  fazer_mapa_4('rio', cols = 1, width = 12, height = 18)
  fazer_mapa_5('rio', cols = 1, width = 12, height = 18)
  
  
  
  
  
  
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
                       labels=c('Pobres Q1', 'Média', 'Ricos Q5')) +
    scale_x_continuous(name="", limits = c(0, 24),
                       breaks = c(0, 5, 10, 15, 20),
                       labels = c(0, 5,  10, 15,"20 minutos")) +
    geom_text(data = filter(df4, sigla_muni == "Goiania"),
              aes(x = Q1, y = sigla_muni),
              label = "Pobres Q1", fontface = "bold",
              color = "springgreen4",
              hjust = -0.5) +
    geom_text(data = filter(df4, sigla_muni == "Goiania"),
              aes(x = Q5, y = sigla_muni),
              label = "Ricos Q5", fontface = "bold",
              color = "steelblue4",
              hjust = 1.5) +
    geom_text(data = filter(df4, sigla_muni == "Goiania"),
              aes(x = Total, y = sigla_muni),
              label = "Total", fontface = "bold",
              color = "black",
              vjust = -1) +
    expand_limits(y = 21)+
    theme_ipsum(grid= "X", base_family = "Helvetica") +
    labs(x = "", y = "", title = "")
  
  # TME bike
  # tme walk
  
  
  
  ggsave(file= sprintf("../figures/td/%s/graf1_tempo medio de bicicleta ate escola-med mais proxima_%s.svg", ano, ano), dpi = 300, width = 16, height = 16, units = "cm")
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
    scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
    scale_y_percent() +
    hrbrthemes::theme_ipsum(grid = "Y", base_family = "Helvetica") +
    labs(x = "",
         y = "Porcentagem de oportunidades acessíveis") + 
    guides(color=guide_legend(nrow=1)) +
    theme( 
      axis.text.y  = element_text(face="bold")
      ,panel.grid.minor = element_blank()
      ,strip.text = element_text(size = 11, face ="bold")
      ,legend.text = element_text(size = 11)
      , legend.position = "bottom"
      , axis.text.x = element_blank()
      
    )
  
  
  ggsave(file=sprintf("../figures/td/%s/graf2_distribuicao da proporcao de empregos acessiveis a pe em ate 30min por renda_POA, GOI, CAM_%s.svg", ano, ano),  dpi = 300, width = 25, height = 15, units = "cm")
  beep()
  
  
  
  
  
  
  # 7) CMA Palma Renda Todos Trabalho  Walk 30  --------------------------------
  
  # calcular acessibilidade media do 90 percentil e 40 percentil de renda
  # usar caminhada pico CMATT30
  
  temp_palma <- copy(acess_final)
  temp_palma$geometry <- NULL
  
  acess_palma <- temp_palma %>%
    filter(modo == "bicicleta" & pico == 1) %>%
    select(sigla_muni, R003, P001, CMATT30) %>%
    # pegar so decis 4 e 9
    filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
    # definir ricos e pobres
    mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
    group_by(sigla_muni, classe) %>%
    summarise(acess_media = weighted.mean(CMATT30, P001)) %>%
    ungroup() %>%
    group_by(sigla_muni) %>%
    spread(classe, acess_media) %>%
    # calcular palma ratio
    group_by(sigla_muni) %>%
    mutate(palma_ratio = rico/pobre) %>%
    ungroup()
  
  head(acess_palma)
  summary(acess_palma$P001)
  
  # visualizar
  acess_palma %>%
    mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
    mutate(sigla_muni = fct_reorder(sigla_muni, palma_ratio)) %>%
    ggplot()+
    geom_col(aes(y = palma_ratio, x = sigla_muni)) +
    geom_text(aes(y = palma_ratio, x = sigla_muni, label = round(palma_ratio,1)), size = 3, position = position_stack(vjust = 0.88), color='gray99') +
    geom_hline(yintercept = 1, color = "grey90", linetype = "dashed") +
    scale_y_continuous(breaks = c(0, 1, 3, 6, 9))+
    coord_flip()+
    theme_ipsum(grid = "X", base_family = "Helvetica")+
    labs(x = "", y = "Palma Ratio")
  
  
  ggsave(file=sprintf("../figures/td/%s/graf3_razao de palma_empregos acessiveis a pe em menos d 30min_ pobres e ricos_%s.svg", ano, ano),  dpi = 300, width = 16.5, height = 15, units = "cm")
  
  
  
  
  # 8) CMA Palma Cor SaudeAlta TP 60  --------------------------------
  
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
    geom_text(aes(y = palma_ratio, x = sigla_muni, label = round(palma_ratio,1)), size = 3, position = position_stack(vjust = 0.95), color='gray99') +
    geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
    scale_y_continuous(breaks = seq(0, 3, .5))+
    coord_flip()+
    theme_ipsum(grid = "X", base_family = "Helvetica")+
    labs(x = "", y = "Razão Brancos/Negros")
  
  ggsave(file=sprintf("../figures/td/%s/graf4_razao de palma_saude-alta acessiveis a pe em menos d 60min_ brancos e negros_%s.svg", ano, ano), dpi = 300, width = 16.5, height = 15, units = "cm")
  
  
  
  # calcular acessibilidade media do 90 percentil e 40 percentil de renda
  # usar TP pico CMASA60
  
  acess_palma_2 <- acess_final %>%
    filter(modo == "tp" & pico == 1) %>%
    select(sigla_muni, R003, P001, P003, P002, CMASA60) %>%
    group_by(sigla_muni) %>%
    summarise(acess_brancos = weighted.mean(CMASA60, P002),
              acess_negros = weighted.mean(CMASA60, P003)) %>%
    # calcular palma ratio
    mutate(palma_ratio = acess_brancos/acess_negros) %>%
    ungroup()
  
  head(acess_palma_2)
  
  # visualizar
  acess_palma_2 %>%
    mutate(sigla_muni = factor(sigla_muni, levels = munis_df$abrev_muni, labels = munis_df$name_muni)) %>%
    mutate(sigla_muni = fct_reorder(sigla_muni, palma_ratio)) %>%
    ggplot()+
    geom_col(aes(y = palma_ratio, x = sigla_muni))+
    geom_text(aes(y = palma_ratio, x = sigla_muni, label = round(palma_ratio,1)), size = 3, position = position_stack(vjust = 0.95), color='gray99') +
    geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
    scale_y_continuous(breaks = seq(0, 3, .5))+
    coord_flip()+
    theme_ipsum(grid = "X", base_family = "Helvetica")+
    labs(x = "", y = "Razão Brancos/Negros")
  
  
  ggsave(file=sprintf("../figures/td/%s/graf5_razao de palma_saude-alta acessiveis em transp-publico em menos d 60min_ brancos e negros_%s.svg", ano, ano), dpi = 300, width = 16.5, height = 15, units = "cm")
  
  
}


# aplicar
fazer_figuras_td(ano = 2019)
fazer_figuras_td(ano = 2020)

