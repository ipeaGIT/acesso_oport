# from https://stackoverflow.com/questions/37664728/create-a-map-of-spatial-clusters-lisa-in-r

criar_lisamaps <- function(municipio) {
  
  dir <- sprintf("../data/output_access/access_%s.rds", municipio)
  
  # load data
  access_for <- read_rds(dir) %>%
    filter(!is.na(escolas_total),
           !is.na(saude_total))
  
  # create  Queens contiguity matrix
  spatmatrix <- poly2nb(access_for)
  
  # create a neighbours list with spatial weights
  listw <- nb2listw(spatmatrix)
  
  # calculate the local moran of the distribution of white p
  lmoran_escolas <- localmoran(access_for$escolas_total, listw)
  lmoran_saude <- localmoran(access_for$saude_total, listw)
  
  # padronize the variable and save it to a new column
  access_for$s_escolas <- scale(access_for$escolas_total)  %>% as.vector()
  access_for$s_saude <- scale(access_for$saude_total)  %>% as.vector()
  
  # create a spatially lagged variable and save it to a new column
  access_for$lag_s_escolas <- lag.listw(listw, access_for$s_escolas)
  access_for$lag_s_saude <- lag.listw(listw, access_for$s_saude)
  
  # # moran sccaterplot, in basic graphics (with identification of influential observations)
  # x <- access_for$s_escolas
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
  
  # PARA ESCOLAS ------------------------------------------
  # high-high quadrant
  access_for[(access_for$s_escolas >= 0 & 
                access_for$lag_s_escolas >= 0) & 
               (lmoran_escolas[, 5] <= 0.05), "quad_sig_escolas"] <- "high-high"
  # low-low quadrant
  access_for[(access_for$s_escolas <= 0 & 
                access_for$lag_s_escolas <= 0) & 
               (lmoran_escolas[, 5] <= 0.05), "quad_sig_escolas"] <- "low-low"
  # high-low quadrant
  access_for[(access_for$s_escolas >= 0 & 
                access_for$lag_s_escolas <= 0) & 
               (lmoran_escolas[, 5] <= 0.05), "quad_sig_escolas"] <- "high-low"
  # low-high quadrant
  access_for[(access_for$s_escolas <= 0 
              & access_for$lag_s_escolas >= 0) & 
               (lmoran_escolas[, 5] <= 0.05), "quad_sig_escolas"] <- "low-high"
  # non-significant observations
  access_for[(lmoran_escolas[, 5] > 0.05), "quad_sig_escolas"] <- "not signif."  
  
  # PARA SAUDE --------------------------------------------
  # high-high quadrant
  access_for[(access_for$s_saude >= 0 & 
                access_for$lag_s_saude >= 0) & 
               (lmoran_saude[, 5] <= 0.05), "quad_sig_saude"] <- "high-high"
  # low-low quadrant
  access_for[(access_for$s_saude <= 0 & 
                access_for$lag_s_saude <= 0) & 
               (lmoran_saude[, 5] <= 0.05), "quad_sig_saude"] <- "low-low"
  # high-low quadrant
  access_for[(access_for$s_saude >= 0 & 
                access_for$lag_s_saude <= 0) & 
               (lmoran_saude[, 5] <= 0.05), "quad_sig_saude"] <- "high-low"
  # low-high quadrant
  access_for[(access_for$s_saude <= 0 
              & access_for$lag_s_saude >= 0) & 
               (lmoran_saude[, 5] <= 0.05), "quad_sig_saude"] <- "low-high"
  # non-significant observations
  access_for[(lmoran_saude[, 5] > 0.05), "quad_sig_saude"] <- "not signif."  
  
  access_for$quad_sig_escolas <- as.factor(access_for$quad_sig_escolas)
  access_for$quad_sig_saude <- as.factor(access_for$quad_sig_saude)
  
  # # plotting the map
  # df <- fortify(access_for, region="id")
  # df <- left_join(df, access_for)
  
  access_for %>% 
    select(id_hex, quad_sig_escolas) %>%
    ggplot()+
    geom_sf(aes(fill = quad_sig_escolas), color = "black", size = 0.05)+
    # ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
    # geom_polygon(color = "white", size = .05)  + 
    # coord_equal() + 
    theme_bw()+
    theme(legend.position = "none") +
    scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "not signif." = "grey95"))+
  access_for %>% 
    select(id_hex, quad_sig_saude) %>%
    ggplot()+
    geom_sf(aes(fill = quad_sig_saude), color = "black", size = 0.05)+
    # ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
    # geom_polygon(color = "white", size = .05)  + 
    # coord_equal() + 
    theme_bw()+
    theme(legend.position = "bottom")+
    scale_fill_manual(values = c("high-high" = "red", "low-low" = "blue", "not signif." = "grey95"))+
    plot_layout(ncol = 2)
  

  }

criar_lisamaps("for")
