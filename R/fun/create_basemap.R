cidade <- "for"

create_citymap <- function(cidade) {
  
  # Arquivo com a correspondecia dos municipios e nome
  muni_code <- data.frame(
    cidade1 = c("for", "bel", "rio", "sao", "cur", "por"),
    code_muni = c(2304400)
  )
  
  muni_code_ok <- muni_code %>% filter(cidade1 %in% cidade)
  
  # Abrir linhas de alta/media capacidade
  linhas_hm <- read_rds("../data/linhas_HMcapacidade/linhas_HMcapacidade.rds") %>%
    mutate(city = substr(Cidade, 1, 3) %>% tolower()) %>%
    filter(city %in% cidade)
  
  # Abrir as ruas
  streets_path <- dir(sprintf("../otp/graphs/%s", cidade), full.names = TRUE, pattern = ".pbf$")
  streets <- st_read(streets_path, layer = "lines") %>%
    # Selecionar so primarias e secundarias
    filter(highway %in% c("primary", "secundary"))
  
  # Abrir limites do municipio
  muni <- geobr::read_municipality(code_muni=2304400, year=2017)
  
  # mapview(streets)
  
  
  
  
  theme_opts <- list(theme(# panel.background = element_blank(),
    plot.background = element_rect(fill="#e6e8ed"),
    panel.border=element_blank(),
    panel.background = element_rect(fill = "gray99", colour = NA),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text =element_text(size=7),
    text = element_text(size=9),
    strip.background = element_rect(colour = "white", fill = "white", size = 11), #muda estilo de facet_grid boxes
    strip.text.x = element_text(size = 11, face ="bold"),
    # Legends  
    legend.position=c(0.115, 0.1), # horz vert
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.title=element_text(size=8),
    legend.text=element_text(size=8)),
    #legend.key.size = unit(5, "cm"),
    #plot.title = element_text(size=22)
    guides(fill = guide_colorbar(title.position = "top"))
    
    # panel.grid = element_blank(), 
    # line = element_blank(), 
    # rect = element_blank()
    # text = element_blank()
    # coord_sf(xlim = c(-43.828854, -43.117908),ylim = c(-23.091709, -22.760493)) # coord_cartesian Coordinates Zoom
  )
  
  # scalebar <- ggsn::scalebar(muni, dist = 10, st.size=2, height=0.001, dd2km = TRUE, model = 'WGS84')
  
  ggplot() +
    geom_sf(data= muni, fill="white", color="gray30", size = 1) +
    # geom_image(data=airports, aes(x=long,y=lat, image=image), size=.025) + 
    # scalebar +
    geom_sf(data= streets, fill="white", color="gray70") +
    geom_sf(data = linhas_hm, size=0.7, color="#2340e7") +
    # coord_sf(crs = st_crs(muni), datum = NA) +
    ggthemes::theme_map() +
    theme_opts 
  
}

create_citymap("for")
