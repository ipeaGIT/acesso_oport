#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.7 Leitura e filtro de elevacao/topografia

# ATENCAO: dados de topografia ja cortados para os municipios foram baixados manualmente
# e estao na pasta data/topografia3
  
## info:
#' Os dados brutos de elevação são retirados manualmente do [Earth Explorer](https://earthexplorer.usgs.gov/). 
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


# carregar bibliotecas
source('./R/fun/setup.R')

# sigla_muni <- "slz"

crop_save_raster <- function(ano, munis = "all") {
  
  
  # Select the corerspondent munis_df
  munis_df <- get(sprintf("munis_df_%s", ano))
  
  # Criar pasta para salvar arquivos
  dir.create(sprintf("../data/topografia/%s", ano), recursive = TRUE)
  
  # Funcao para gerar arquivo raster com a topografia do municipio  ------------------------------------------------------------------
  crop_save_raster_muni <- function(sigla_muni) {
    
    
    my_merge <- function(...) {
      
      x <- raster::merge(..., tolerance = 10)
    }
    
    # listar arquivos .tiff na pasta
    dir <- sprintf("../data-raw/topografia/%s/%s", ano, sigla_muni)
    files <- dir(dir, full.names = T, pattern = ".tif$")
    
    # leitura de arquivo raster
    if (length(files) == 1) {
      elev_img_bind <- raster::raster(files)
      
    } else {
      elev_img <- purrr::map(files, raster::raster)
      
      # atencao: tem que reprojetar os raster para um origin comum com o uso da funcao raster::resample
      # onde o segundo raster da funcao eh o raster de referencia
      # nesse caso sempre pegar o primeiro raster como referencia para o resample
      
      # elev_img_corrigido <- c(elev_img[[1]], map(elev_img[c(-1)], raster::resample, elev_img[[1]]))
      
      # elev_img_bind <- do.call(raster::merge, elev_img_corrigido)
      elev_img_bind <- do.call(my_merge, elev_img)
    }
    
    # carrega shape do municipio
    muni_sf <- readr::read_rds(sprintf("../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla_muni, ano))
    
    # Cria bounding box do municipio
    bb <- sf::st_bbox(muni_sf)
    bb1 <- c(bb[1], bb[3], bb[2], bb[4])
    
    # Converte para spatial polygon para fazer o crop
    e <- as(extent(bb1), 'SpatialPolygons')
    crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
    e <- spTransform(e, CRS(proj4string(elev_img_bind)))
    
    # crop do municipio com raster de topografia
    elev_img_fim <- raster::crop(elev_img_bind, e)
    
    # salvar
    output <- sprintf("../data/topografia/%s/topografia_%s_%s.tif", ano, sigla_muni, ano)
    raster::writeRaster(elev_img_fim, output, format="GTiff", overwrite=TRUE)
  }
  
  #### Aplicando funcao em paralelo para salvar grades de hexagonos ---------------------------------------------------------------
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  future::plan(future::multiprocess)
  future.apply::future_lapply(X=x, FUN=crop_save_raster_muni, future.packages=c('sf', 'raster'))
  
}
