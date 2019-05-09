# from https://stackoverflow.com/questions/37664728/create-a-map-of-spatial-clusters-lisa-in-r

municipio <- "rio_08"

criar_lisamaps <- function(municipio, cols = 2) {
  
  dir <- sprintf("../data/hex_agregados/hex_agregado_%s.rds", municipio)
  
  # load data
  access_for <- read_rds(dir) %>%
    ungroup() %>%
    filter(pop_total > 0) %>%
    filter(!is.na(escolas_total),
           !is.na(saude_total))
  
  # create  Queens contiguity matrix
  spatmatrix <- poly2nb(access_for)
  
  # alguma zona que nao tenha nenhum vizinho?
  nenhum_vizinho <- which(map_dbl(spatmatrix, sum) == 0)
  
  if (length(nenhum_vizinho) > 0) {
    
    # tirar essa zona que nao tem vizinho
    access_for <- access_for %>%
      slice(-nenhum_vizinho)
    
    # criar novamento matrix de vizinhanca
    spatmatrix <- poly2nb(access_for)
    
  }
  
  # create a neighbours list with spatial weights
  listw <- nb2listw(spatmatrix)
  
  # calculate the local moran of the distribution of pop total
  lmoran_pop <- localmoran(access_for$pop_total, listw)
  # lmoran_ <- localmoran(access_for$saude_total, listw)
  
  # padronize the variable and save it to a new column
  access_for$s_pop <- scale(access_for$pop_total)  %>% as.vector()
  # access_for$s_saude <- scale(access_for$saude_total)  %>% as.vector()
  
  # create a spatially lagged variable and save it to a new column
  access_for$lag_s_pop <- lag.listw(listw, access_for$s_pop)
  # access_for$lag_s_saude <- lag.listw(listw, access_for$s_saude)
  
  # # moran sccaterplot, in basic graphics (with identification of influential observations)
  # x <- access_for$s_pop
  # y <- access_for$lag_s_escolas %>% as.vector()
  # xx <- tibble(x, y)
  # 
  # moran.plot(x, listw)
  # 
  # 
  # 
  # # moran sccaterplot, in ggplot 
  # # (without identification of influential observations - which is possible but requires more effort)
  # ggplot(xx, aes(x, y)) + 
  #   geom_point() + 
  #   geom_smooth(method = 'lm', se = F) + 
  #   geom_hline(yintercept = 0, linetype = 'dashed') + 
  #   geom_vline(xintercept = 0, linetype = 'dashed') 
  
  # create a new variable identifying the moran plot quadrant for each observation, dismissing the non-significant ones
  access_for$quad_sig_escolas <- NA
  access_for$quad_sig_saude <- NA
  
  # PARA POP ------------------------------------------
  # high-high quadrant
  access_for[(access_for$s_pop >= 0 & 
                access_for$lag_s_pop >= 0), "quad_sig_escolas"] <- "high-high"
  # low-low quadrant
  access_for[(access_for$s_pop <= 0 & 
                access_for$lag_s_pop <= 0), "quad_sig_escolas"] <- "low-low"
  # high-low quadrant
  access_for[(access_for$s_pop >= 0 & 
                access_for$lag_s_pop <= 0), "quad_sig_escolas"] <- "high-low"
  # low-high quadrant
  access_for[(access_for$s_pop <= 0 
              & access_for$lag_s_pop >= 0), "quad_sig_escolas"] <- "low-high"
  # non-significant observations
  access_for[(lmoran_escolas[, 5] > 0.05), "quad_sig_escolas"] <- "not signif."  
  
  # PARA POP significantes --------------------------------------------
  # high-high quadrant
  access_for[(access_for$s_pop >= 0 & 
                access_for$lag_s_pop >= 0) & 
               (lmoran_pop[, 5] <= 0.05), "quad_sig_pop"] <- "high-high"
  # low-low quadrant
  access_for[(access_for$s_pop <= 0 & 
                access_for$lag_s_pop <= 0) & 
               (lmoran_pop[, 5] <= 0.05), "quad_sig_pop"] <- "low-low"
  # high-low quadrant
  access_for[(access_for$s_pop >= 0 & 
                access_for$lag_s_pop <= 0) & 
               (lmoran_pop[, 5] <= 0.05), "quad_sig_pop"] <- "high-low"
  # low-high quadrant
  access_for[(access_for$s_pop <= 0 
              & access_for$lag_s_pop >= 0) & 
               (lmoran_pop[, 5] <= 0.05), "quad_sig_pop"] <- "low-high"
  # non-significant observations
  access_for[(lmoran_pop[, 5] > 0.05), "quad_sig_pop"] <- "not signif."  
  
  access_for$quad_sig_escolas <- as.factor(access_for$quad_sig_escolas)
  access_for$quad_sig_saude <- as.factor(access_for$quad_sig_saude)
  
  # # plotting the map
  # df <- fortify(access_for, region="id")
  # df <- left_join(df, access_for)
  
    access_for %>% 
      select(id_hex, quad_sig_pop) %>%
      ggplot()+
      geom_sf(aes(fill = quad_sig_pop), color = "black", size = 0.05)+
      # ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
      # geom_polygon(color = "white", size = .05)  + 
      # coord_equal() + 
      theme_bw()+
    # theme(legend.position = "none") +
    scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "not signif" = "grey85"))+
    labs(title = "Clusters de populacao)")
  
  
}

# criar_lisamaps("for")
