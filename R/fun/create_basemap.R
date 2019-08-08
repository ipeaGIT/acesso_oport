# cidade <- "rio"
# cidade <- "for"
# cidade <- "bel"
cidade <- "sao"

# posicao_legenda = c(0.95, 0.13)
# tamanho_escala = 10

create_citymap <- function(cidade, 
                           posicao_legenda = c(0.95, 0.1),
                           tamanho_escala = 10,
                           location_escala = "bottomleft",
                           location_arrow = "topright",
                           width_out = 16
                           
                           
                           ) {
  
  # Arquivo com a correspondecia dos municipios e nome
  muni_code <- data.table(
    cidade1 = c("for", "bel", "rio", "sao", "cur", "por"),
    uf = c("ce", "mg", "rj", "sp", "pr", "rs"),
    code_muni = c(2304400, 3106200, 3304557, 3550308, 4106902, 4314902),
    aeroportos = list("FOR", "CNF", c("SDU", "GIG"), c("GRU", "CGH"), "BFH", "POA")
  )
  
  muni_code_ok <- muni_code %>% filter(cidade1 %in% cidade)
  
  # Abrir linhas de alta/media capacidade
  linhas_hm <- read_rds("../data/linhas_HMcapacidade/linhas_HMcapacidade.rds") %>%
    dplyr::filter(cidade1 %in% cidade) %>%
    mutate(Modo = ifelse(Modo == "BRT Básico", "BRT", Modo)) %>%
    count(Modo)
  
  # iconv(linhas_hm$Modo, to = "US-ASCII")
  
  # Abrir as ruas
  if (cidade == "sao") {
    streets <- read_rds("../data-raw/uber_speed/SAO_uber_speed/osm_network_sao.rds") %>%
      filter(highway %in% c("primary", "secundary", "residential"))
    
  } else {
    streets_path <- dir(sprintf("../otp/graphs/%s", cidade), full.names = TRUE, pattern = ".pbf$")
    streets <- st_read(streets_path, layer = "lines") %>%
      # Selecionar so primarias e secundarias
      filter(highway %in% c("primary", "secundary", "residential")) %>%
      filter(osm_id %nin% 43042156)
  }
  
  # Abrir limites do municipio
  uf <- muni_code %>% filter(cidade1 == cidade) %>% mutate(uf = as.character(uf)) %>% .$uf
  muni <- read_rds(sprintf("../data/municipios/municipios_%s.rds", uf)) %>%
    filter(CD_GEOCODM == muni_code_ok$code_muni)
  
  # pegar bb do muni
  bb_muni <- sf::st_bbox(muni)
  
  # pegar estado
  estado <- geobr::read_state(code_state = toupper(uf))
  # estado <- read_rds("../data-raw/23UF.rds")
  
  # aeroport0s
  # extrair aeroporto
  aero_muni <- muni_code %>% filter(cidade1 == cidade) %>% .$aeroportos %>% .[[1]]
  airports <- fread("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat") %>%
    select(V5, V7, V8) %>%
    filter(V5 %in% aero_muni) %>%
    mutate(image = "figure/airplane-4-xxl.png")
  
  # # elevation
  # elevation <- raster(sprintf("../otp/graphs/%s/elevation_%s.tif", cidade, cidade))
  # plot(elevation)
  # 
  # elevation <- as(elevation, "SpatialPixelsDataFrame")
  # 
  # # elevation_df <- as.data.frame(elevation, xy = TRUE)
  # elevation_df <- as.data.frame(elevation)
  # 
  # head(elevation_df)
    
  
  # muni <- geobr::read_municipality(code_muni=2304400, year=2017)
  # geobr::read_municipality(code_muni=1200179, year=2010)
  # # mapview(streets)
  # 
  
  theme_opts <- function(base_size) {
    
    theme_void(base_family = "Roboto Condensed") %+replace%
      
      theme(
        # panel.background = element_blank(),
        plot.background = element_rect(fill="#e6e8ed"),
        panel.border=element_blank(),
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major = element_line(color = "grey50", linetype = "dashed", 
                                        size = 0.2),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        # axis.text =element_text(size=7),
        # text = element_text(size=9),
        strip.background = element_rect(colour = "white", fill = "white", size = 11), #muda estilo de facet_grid boxes
        strip.text.x = element_text(size = 11, face ="bold"),
        # Legends  
        legend.position=posicao_legenda, # c("da esquerda", "de baixo")
        legend.direction='vertical',
        legend.box='vertical',
        legend.title = element_blank(),
        legend.text = element_text(size=6),
        legend.key.size = unit(0.5,"line")
        # legend.key = element_rect(color = "transparent")
        #legend.key.size = unit(5, "cm"),
        #plot.title = element_text(size=22)
        
      )
  }
  
  
  # theme_opts <- list(theme(
  #   # panel.background = element_blank(),
  #   # plot.background = element_rect(fill="#e6e8ed"),
  #   panel.border=element_blank(),
  #   panel.background = element_rect(fill = "gray99", colour = NA),
  #   axis.line = element_blank(),
  #   axis.ticks = element_blank(),
  #   # axis.text =element_text(size=7),
  #   text = element_text(size=9),
  #   strip.background = element_rect(colour = "white", fill = "white", size = 11), #muda estilo de facet_grid boxes
  #   strip.text.x = element_text(size = 11, face ="bold"),
  #   # Legends  
  #   legend.position=c(0.115, 0.1), # horz vert
  #   legend.direction='horizontal',
  #   legend.box='horizontal',
  #   legend.title=element_text(size=8),
  #   legend.text=element_text(size=8)),
  #   #legend.key.size = unit(5, "cm"),
  #   #plot.title = element_text(size=22)
  #   guides(fill = guide_colorbar(title.position = "top"))
  #   
  #   # panel.grid = element_blank(), 
  #   # line = element_blank(), 
  #   # rect = element_blank()
  #   # text = element_blank()
  #   # coord_sf(xlim = c(-43.828854, -43.117908),ylim = c(-23.091709, -22.760493)) # coord_cartesian Coordinates Zoom
  # )
  
  
  # # teste ceramic
  # library(ceramic)
  # Sys.setenv(MAPBOX_API_KEY = "pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
  # 
  # 
  # for_tile <- cc_location(muni, type = "streets-v9")
  # 
  # raster::plotRGB(for_tile)
  # 
  # library(raster)
  # 
  # teste <- projectRaster(for_tile,
  #                        crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
  #                        method = "bilinear")
  # 
  # 
  # raster::plotRGB(teste)
  # 
  # for_tile_df <- as.data.frame(teste, xy = TRUE)
  # 
  # names(for_tile_df) <- c("x", "y", "red", "green", "blue")
  # for_tile_df <- filter(for_tile_df, !is.na(red))
  # for_tile_df$hex <- rgb(for_tile_df$red, for_tile_df$green, for_tile_df$blue, maxColorValue = 255)
  # # 
  # ggplot()+
  #   geom_raster(data = for_tile_df, aes(x, y, fill = hex))+
  #   geom_sf(data = muni, fill="transparent", color="black", size = 2) +
  #   # coord_equal()+ 
  #   scale_fill_identity()
  
  scalebar <- ggsn::scalebar(muni, 
                             dist = tamanho_escala,
                             location = location_escala,
                             dist_unit = "km", 
                             st.size= 2,
                             height=0.01, 
                             model = 'WGS84',
                             transform = TRUE,
                             border.size = 0.4)
  
  # arrow <- ggsn::north(muni,
  #                      location = location_arrow)
  
  ggplot() +
    # geom_raster(data = elevation_df, aes(x = x, y = y, alpha = elevation_for))+
    # scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    geom_sf(data = estado, fill = "white", size = 0.3)+
    geom_sf(data= streets, fill="white", color="gray85", size = 0.2) +
    geom_sf(data= muni, fill="transparent", color = gray(.5), size = 0.6) +
    # scalebar +
    geom_sf(data = linhas_hm, aes(fill = as.factor(Modo), color = as.factor(Modo)), size = 0.5)+
    scale_fill_brewer(palette = "Set2", name = "Modo")+
    scale_color_brewer(palette = "Set2", name = "Modo")+
    guides(color = "none",
           fill = guide_legend(override.aes = list(color = "transparent"))) +
    ggimage::geom_image(data=airports, aes(x=V8,y=V7, image=image), size=.04, by="height")+
    # scale_size_identity()+
    coord_sf(xlim = c(bb_muni[1], bb_muni[3]), ylim = c(bb_muni[2], bb_muni[4]))+
    # coord_sf(crs = st_crs(muni), datum = NA) +
    theme_opts()+
    # scale_shape(10)+
    scalebar
    # arrow
  
  # salvar output
  path_out <- sprintf("figure/basemap/%s_basemap.png", cidade)
  
  ggsave(filename = path_out, 
         dpi = 300, 
         units = "cm", 
         height = 9, 
         width = width_out)
}


create_citymap("for", tamanho_escala = 5)

create_citymap("bel", tamanho_escala = 5, 
               posicao_legenda = c(0.93, 0.15),
               location_escala = "bottomright")

create_citymap("rio", tamanho_escala = 10, 
               posicao_legenda = c(0.95, 0.13),
               location_arrow = "topleft")

create_citymap("por", tamanho_escala = 5, 
               posicao_legenda = c(0.95, 0.13),
               location_arrow = "topright")

create_citymap("cur", tamanho_escala = 5, 
               posicao_legenda = c(0.93, 0.13),
               location_escala = "bottomright",
               location_arrow = "topleft")

create_citymap("sao", tamanho_escala = 10, 
               posicao_legenda = c(0.93, 0.16),
               location_escala = "bottomright",
               location_arrow = "topleft",
               width_out = 13)
