~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.7 Leitura e filtro de elevacao/topografia
  
## info:
#' Os dados brutos de elevação são retirados do [Earth Explorer](https://earthexplorer.usgs.gov/). 
#' Lá, é necessário especificar a região e data que se quer extrair os dados de elevação. Na 
#' aba de _Select Your Data Set(s)_, seleciona-se ``Digital Elevation`` -> ``SRTM``. SRTM (_Shuttle Radar 
#' Topography Mission_) é um esforço de pesquisa internacional que obtém dados de elevação numa precisão de 
#' 30 metros. Os dados de elevação do SRTM são divididos por quadrículo de 1 grau de latidude e 1 longitude, 
#' então é necessário cortar os municípios desejados dessa área.
#' A função ``crop_save_raster`` foi criada para tratar e salvar os dados de elevação, e requer dois 
#' argumentos: ``municipio``, que é a sigla (três primeiras letras) do município desejado, e ``bb``, que 
#' é o _bounding box_ do município (pares de coordenadas que delimitam a área do município). Esse argumento 
#' pode ser extraído do [Bounding Box Tool](https://boundingbox.klokantech.com/), onde na aba de busca é pesquisada 
#' e selecionada a cidade em questão. Por fim, na parte inferior esquerda, é selecionada a opção ``CSV RAW`` na 
#' aba _Copy & Paste_, e as coordenadas são inseridas na função como um vetor.



  
## ----elevacao------------------------------------------------------------

# FUNCAO ------------------------------------------------------------------

crop_save_raster <- function(municipio, bb) {
  
  dir <- sprintf("../data-raw/elevation/%s", municipio)
  
  files <- dir(dir, full.names = T)
  
  if (length(files) == 1) {
    
    elev_img_bind <- raster::raster(files)
    
  } else {
    
    elev_img <- map(files, raster)
    elev_img_bind <- do.call(raster::merge, elev_img)
    
    
  }
  
  bb1 <- c(bb[1], bb[3], bb[2], bb[4])
  
  e <- as(extent(bb1), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  elev_img_fim <- crop(elev_img_bind, e)
  
  # salvar
  output <- sprintf("../data/elevation/elevation_%s.tif", municipio)
  
  writeRaster(elev_img_fim, output, format="GTiff", overwrite=TRUE)
  
}

# crop_save_raster("for", bb = c(-38.63656796,-3.88812428,-38.40154132,-3.69197903))
# crop_save_raster("bel", bb = c(-44.06329161,-20.0594646,-43.85721992,-19.77654377))
# crop_save_raster("rio", bb = c(-43.79625205,-23.08270518,-43.09908114,-22.74608786))

# # TESTES ------------------------------------------------------------------
# 
# elev_img_fim <- raster("../data/elevation/elevation_for.tif")
# 
# # testar
# # agora vai
# elev_matrix <- matrix(
#   raster::extract(elev_img_fim, raster::extent(elev_img_fim), buffer = 1000), 
#   nrow = ncol(elev_img_fim), ncol = nrow(elev_img_fim)
# )
# 
# elev_matrix %>%
#   sphere_shade(texture = "desert") %>%
#   add_water(detect_water(elev_matrix), color = "imhof4") %>%
#   # add_shadow(raymat, max_darken = 0.5) %>%
#   # add_shadow(ambmat, max_darken = 0.5) %>%
#   plot_map()
# 
# # plot 3d
# elev_matrix %>%
#   sphere_shade(texture = "imhof4") %>%
#   add_water(detect_water(elev_matrix), color="desert") %>%
#   # add_shadow(raymat) %>%
#   # add_shadow(ambmat,0.5) %>%
#   plot_3d(elev_matrix,zscale=30,fov=60,theta=45,zoom=0.75,phi=45, windowsize = c(1000,800))
# 
# render_snapshot()

crop_save_raster("for", bb = c(-38.63656796,-3.88812428,-38.40154132,-3.69197903))
crop_save_raster("bel", bb = c(-44.06329161,-20.0594646,-43.85721992,-19.77654377))
crop_save_raster("rio", bb = c(-43.79625205,-23.08270518,-43.09908114,-22.74608786))


#' 
