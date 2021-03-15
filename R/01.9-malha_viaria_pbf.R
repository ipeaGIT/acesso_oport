# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Download de dados de malha de ruas do OSM em formato .pbf (que sera utilizado no OpenTripPlanner)

#' Esse script extrai a malha viária de cada cidade, de acordo com a bounding box
#' do arquivo raster de topografia. 
#' 
#' Primeiro, é necessário baixar o arquivo PBF do Brasil inteiro e salvar na
#' pasta 'data-raw/malha_viaria/ano/br'. O PBF pode ser baixado manualmente do
#' site download.geofabrik.de/


# carregar bibliotecas
source('./R/fun/setup.R')
library("raster")

# muni <- "poa"
# year <- 2020

extrai_malha_viaria <- function(muni, year) {
  ## encontrar bounding box da cidade, a partir do grid de topografia
  topo_file <- sprintf("../../data/acesso_oport/topografia3/%s/topografia3_%s.tif", muni, muni)
  topo_raster <- raster::raster(topo_file)
  
  muni_bbox <- raster::extent(topo_raster)
  
  #' construir paths com a localização do Osmosis e dos arquivos PBF de 
  #' origem e destino
  
  osmosis_path <- sprintf("../../data-raw/malha_viaria/osmosis/bin/osmosis.bat")
  
  # PBF do Brasil inteiro
  br_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest.osm.pbf", year)
  
  # PBF do município
  muni_pbf <- sprintf("../../data-raw/malha_viaria/%s/%s/%s_%s.osm.pbf", year, muni, muni, year)
  
  
  # Cria pasta do município, caso não exista
  muni_path <- sprintf("../../data-raw/malha_viaria/%s/%s", year, muni)
  if (!dir.exists(muni_path)) {
    dir.create(path = muni_path)
  }
  
  # Constrói linha de comando para executar o Osmosis
  osmosis_cmd <- sprintf("%s --read-pbf %s --bounding-box left=%s bottom=%s right=%s top=%s --write-pbf %s",
                         osmosis_path, br_pbf, 
                         muni_bbox@xmin, muni_bbox@ymin, muni_bbox@xmax, muni_bbox@ymax,
                         muni_pbf)
  
  # Chama o Osmosis
  shell(osmosis_cmd, translate = TRUE)
}

purrr::walk(munis_df$abrev_muni, extrai_malha_viaria, year = 2020)
system.time(extrai_malha_viaria(muni = "poa", year = 2020))


