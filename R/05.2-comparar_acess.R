# carregar bibliotecas
source('./R/fun/setup.R')

library(patchwork)

# comparar acessibilidad entre os anos

# sigla_muni <- "for"
# sigla_muni <- "bho"
# sigla_muni <- "rio"
# sigla_muni <- "spo"

compare_acess_un <- function(sigla_muni) {
  
  # abrir ttamtrix
  ttmatrix_files <- sprintf("E:/data/ttmatrix_fixed/%s/ttmatrix_fixed_%s_%s.rds",
                            c(2017, 2018, 2019), c(2017, 2018, 2019), sigla_muni)
  # ttmatrix_files <- sprintf("E:/data/output_ttmatrix/%s/r5/ttmatrix_%s_%s_r5.csv", 
  #                           c(2017, 2018, 2019), c(2017, 2018, 2019), sigla_muni)
  
  ttmatrix <- lapply(ttmatrix_files, read_rds) %>% rbindlist()
  # ttmatrix <- lapply(ttmatrix_files, fread) %>% rbindlist()
  
  # 
  # ttmatrix1 <- full_join(ttmatrix[[1]] %>% select(-ano), 
  #                        ttmatrix[[2]] %>% select(-ano, -city), 
  #                        by = c("origin", "destination", "mode", "pico"), suffix = c("_2017", "_2018"))
  # 
  # ttmatrix1 <- full_join(ttmatrix1, 
  #                        ttmatrix[[3]] %>% rename(travel_time_2019 = travel_time) %>% select(-ano, -city), 
  #                        by = c("origin", "destination", "mode", "pico"), suffix = c("_2018", "_2019"))
  # 
  # ttmatrix <- ttmatrix1
  # rm(ttmatrix1)
  
  # pegar so transporte publico
  ttmatrix <- ttmatrix[mode == "transit" & pico == 1]
  
  # calcular acess
  ttmatrix[, opp := 1]
  acess <- ttmatrix[, .(acess = sum(opp[which(travel_time <= 60)], na.rm = TRUE)),
                    by = .(origin, ano)]
  # acess <- ttmatrix[travel_time < 60]
  # acess <- acess[, .(acess = sum(opp, na.rm = TRUE)),
  #                by = .(origin, ano)]
  mapview(acess_sf, zcol = "acess")
  
  if (sigla_muni == "rio") {
    
    acess_wide <- acess %>%
      tidyr::pivot_wider(names_from = "ano", values_from = "acess", names_prefix = "acess_") %>%
      # calcular dif
      mutate(dif1 = acess_2019 - acess_2018)
    
    
  } else {
    
    
    acess_wide <- acess %>%
      tidyr::pivot_wider(names_from = "ano", values_from = "acess", names_prefix = "acess_") %>%
      # calcular dif
      mutate(dif1 = acess_2018 - acess_2017,
             dif2 = acess_2019 - acess_2018,
             dif3 = acess_2019 - acess_2017)
    
    
  }
  
  # acess_wide %>% filter(is.na(acess_2018) & !is.na(acess_2019))
  # acess_wide %>% filter(!is.na(acess_2017) & is.na(acess_2018))
  
  # map
  # bring hex
  hex <- rbind(
    read_rds(sprintf("../../data/acesso_oport/hex_municipio/2017/hex_%s_09_2017.rds", sigla_muni)) %>% mutate(ano = 2017),
    read_rds(sprintf("../../data/acesso_oport/hex_municipio/2018/hex_%s_09_2018.rds", sigla_muni)) %>% mutate(ano = 2018),
    read_rds(sprintf("../../data/acesso_oport/hex_municipio/2019/hex_%s_09_2019.rds", sigla_muni)) %>% mutate(ano = 2019)
  ) %>% distinct(id_hex, geometry)
  
  
  acess_sf <- left_join(acess_wide, hex,
                        by = c("origin" = "id_hex")) %>%
    st_sf(crs = 4326)
  
  
  limits_dif <- max(abs(c(acess_wide$dif1, acess_wide$dif2)))
  
  if (sigla_muni == "rio") {
    
    
    map1 <- ggplot()+
      geom_sf(data = acess_sf, aes(fill = dif1), color = NA)+
      scale_fill_distiller(palette = "RdBu", 
                           # limits = limits_dif*c(-1,1),
                           limits = 500*c(-1,1),
                           direction = 1)+
      theme_void()+
      labs(title = "Diferenca entre 2018 e 2019",
           subtitle = "Em vermelho: acess piorou em 2019")+
      theme(legend.position = 'bottom')
    
    boxplot1 <- ggplot()+
      geom_boxplot(data = acess_sf, aes(x = 1, y = dif1), lwd = 0.3, outlier.size = 0.5)+
      geom_hline(yintercept = 0)+
      scale_y_continuous(limits = limits_dif * c(-1,1))+
      theme_ipsum(grid = "X")+
      theme(axis.text.y = element_blank())+
      coord_flip()
    
    
    plot <- (map1 / boxplot1) + plot_layout(heights = c(4,1))
    
    
  } else {
    
    
    map1 <- ggplot()+
      geom_sf(data = acess_sf, aes(fill = dif1), color = NA)+
      scale_fill_distiller(palette = "RdBu", 
                           # limits = limits_dif*c(-1,1),
                           limits = 500*c(-1,1),
                           direction = 1)+
      theme_void()+
      labs(title = "Diferenca entre 2017 e 2018",
           subtitle = "Em vermelho: acess piorou em 2018")+
      theme(legend.position = 'bottom')
    
    boxplot1 <- ggplot()+
      geom_boxplot(data = acess_sf, aes(x = 1, y = dif1), lwd = 0.3, outlier.size = 0.5)+
      scale_y_continuous(limits = limits_dif * c(-1,1))+
      geom_hline(yintercept = 0)+
      theme_ipsum(grid = "X")+
      theme(axis.text.y = element_blank())+
      coord_flip()
    
    
    map2 <- ggplot()+
      geom_sf(data = acess_sf, aes(fill = dif2), color = NA)+
      scale_fill_distiller(palette = "RdBu", 
                           # limits = limits_dif*c(-1,1),
                           limits = 500*c(-1,1),
                           direction = 1)+
      theme_void()+
      labs(title = "Diferenca entre 2018 e 2019",
           subtitle = "Em vermelho: acess piorou em 2019")+
      theme(legend.position = 'bottom')
    
    boxplot2 <- ggplot()+
      geom_boxplot(data = acess_sf, aes(x = 1, y = dif2), lwd = 0.3, outlier.size = 0.5)+
      geom_hline(yintercept = 0)+
      scale_y_continuous(limits = limits_dif * c(-1,1))+
      theme_ipsum(grid = "X")+
      theme(axis.text.y = element_blank())+
      coord_flip()
    
    map3 <- ggplot()+
      geom_sf(data = acess_sf, aes(fill = dif3), color = NA)+
      scale_fill_distiller(palette = "RdBu", 
                           # limits = limits_dif*c(-1,1),
                           limits = 500*c(-1,1),
                           direction = 1)+
      theme_void()+
      labs(title = "Diferenca entre 2017 e 2019",
           subtitle = "Em vermelho: acess piorou em 2019")
    
    boxplot3 <- ggplot()+
      geom_boxplot(data = acess_sf, aes(x = 1, y = dif3))+
      scale_y_continuous(limits = limits_dif * c(-1,1))+
      theme_ipsum(grid = "X")+
      theme(axis.text.y = element_blank())+
      coord_flip()
    
    
    
    
    # maps <- (map1 | map2) + plot_layout(guides = "collect") &
    #   theme(legend.position = 'bottom')
    # 
    # graphs <- (boxplot1 | boxplot2)
    
    plot1 <- (map1 / boxplot1) + plot_layout(heights = c(4,1))
    plot2 <- (map2 / boxplot2) + plot_layout(heights = c(4,1))
    
    plot <- cowplot::plot_grid(plot1, plot2, nrow = 1)
  }
  
  
  # save
  ggsave(filename = sprintf("testes/compare_acess_unico/compare_acess_unico_%s.png", sigla_muni),
         plot = plot,
         width = 22,
         height = 18,
         units = "cm")
  
}


compare_acess_un("for")
compare_acess_un("rio")
compare_acess_un("cur")
compare_acess_un("poa")
compare_acess_un("bho")
compare_acess_un("spo")
compare_acess_un("cam")


# compare access jobs/schools/hospitals/cras ------------------------------


# sigla_muni <- "for"
# sigla_muni <- "bho"
compare_acess <- function(sigla_muni) {
  
  # open acess
  files <- dir("../../data/acesso_oport/output_access", pattern = sigla_muni, full.names = TRUE, recursive = TRUE)
  files <- files[grepl(pattern = "2017|2018|2019/", x = files)]
  
  acess <- lapply(files, read_rds) %>% rbindlist() %>% st_sf(crs = 4326)
  
  # make maps CMATT60, CMATET60, CMAST60, CMACT60 - bus and car - for each year
  acess <- acess %>%
    filter(mode %in% c("transit", "car") & pico == 1) %>%
    select(origin, ano, city, mode, pico, CMATT60, CMAET60, CMAST60, CMACT60)
  
  # to wide
  acess_wide <- acess %>% 
    pivot_longer(cols = CMATT60:CMACT60, "ind", "value")
  
  # map 1
  map_tt <- ggplot()+
    geom_sf(data = acess, aes(fill = CMATT60), color = NA)+
    facet_grid(mode ~ ano)+
    scale_fill_viridis_c(option = "inferno")+
    theme_void()
  
  # map 2
  map_et <- ggplot()+
    geom_sf(data = acess, aes(fill = CMAET60), color = NA)+
    facet_grid(mode ~ ano)+
    scale_fill_viridis_c(option = "inferno")+
    theme_void()
  
  # map 3
  map_st <- ggplot()+
    geom_sf(data = acess, aes(fill = CMAST60), color = NA)+
    facet_grid(mode ~ ano)+
    scale_fill_viridis_c(option = "inferno")+
    theme_void()
  
  # map 4
  map_ct <- ggplot()+
    geom_sf(data = acess, aes(fill = CMACT60), color = NA)+
    facet_grid(mode ~ ano)+
    scale_fill_viridis_c(option = "inferno")+
    theme_void()
  
  # save
  ggsave(plot = map_tt, 
         filename = sprintf("testes/compare_acess/compare_acess/compare_acess_CMATT60_%s.png", sigla_muni),
         width = 16,
         height = 14,
         units = "cm")
  
  ggsave(plot = map_et, 
         filename = sprintf("testes/compare_acess/compare_acess/compare_acess_CMAET60_%s.png", sigla_muni),
         width = 16,
         height = 14,
         units = "cm")
  
  ggsave(plot = map_st, 
         filename = sprintf("testes/compare_acess/compare_acess/compare_acess_CMAST60_%s.png", sigla_muni),
         width = 16,
         height = 14,
         units = "cm")
  ggsave(plot = map_ct, 
         filename = sprintf("testes/compare_acess/compare_acess/compare_acess_CMACT60_%s.png", sigla_muni),
         width = 16,
         height = 14,
         units = "cm")
  
}

compare_acess("for")
compare_acess("bho")
compare_acess("poa")
compare_acess("rec")
compare_acess("cam")
compare_acess("goi")