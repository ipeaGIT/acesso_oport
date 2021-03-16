# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Download de dados de malha de ruas do OSM em formato .pbf (que sera utilizado no OpenTripPlanner)

#' Esse script extrai a malha viária de cada cidade, de acordo com a bounding box
#' do arquivo raster de topografia. 
#' 
#' Instruções sobre como fazer isso foram retiradas de 
#' https://docs.analysis.conveyal.com/prepare-inputs#cropping
#' 
#' Primeiro, é necessário baixar o arquivo PBF do Brasil inteiro e salvar na
#' pasta 'data-raw/malha_viaria/ano/br'. O PBF pode ser baixado manualmente do
#' site download.geofabrik.de/


# carregar bibliotecas
source('./R/fun/setup.R')
library("raster")




# checar e filtrar brazil-latest.osm.pbf ----------------------------------

# year <- 2020

filtrar_malha_viaria_br <- function(year) {
 
  #' construir paths com a localização do Osmosis e dos arquivos PBF
  osmosis_path <- sprintf("../../data-raw/malha_viaria/osmosis/bin/osmosis.bat")
  
  # PBF do Brasil inteiro
  br_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest.osm.pbf", year)
  
  # PBF do Brasil inteiro, filtrado (somente malha viária, sem polígonos)
  br_filtered_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest-filtered.osm.pbf", year)
  
  
  # testar se o arquivo PBF do Brasil existe
  if (!file.exists(br_pbf)) {
    stop(sprintf("Malha viária não encontrada. Faça o download em download.geofabrik.de e salve o arquivo em %s", br_pbf))
  }
  

  # testar se o arquivo filtrado já existe, para não repetir o processo desnecessariamente  
  if (file.exists(br_filtered_pbf)) {
    stop(sprintf("Malha viária filtrada já existe. Para gerar uma nova, remova o arquivo %s antes.", br_filtered_pbf))
  }
  
  # Constrói linha de comando para executar o Osmosis
  osmosis_cmd <- sprintf(
    paste("%s --read-pbf %s",
          "    --tf accept-ways highway=* public_transport=platform railway=platform park_ride=*",
          "--tf accept-relations type=restriction",
          "--used-node",
          "--write-pbf %s"),
          osmosis_path, br_pbf, br_filtered_pbf)
  
  # Chama o Osmosis
  shell(osmosis_cmd, translate = TRUE)  
  
  
}

filtrar_malha_viaria_br(year = 2020)


# função para extrair a malha viária do município -------------------------


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
  br_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest-filtered.osm.pbf", year)
  
  # PBF do município
  muni_pbf <- sprintf("../../data-raw/malha_viaria/%s/%s/%s_%s.osm.pbf", year, muni, muni, year)
  
  
  # Cria pasta do município, caso não exista
  muni_path <- sprintf("../../data-raw/malha_viaria/%s/%s", year, muni)
  if (!dir.exists(muni_path)) {
    dir.create(path = muni_path, recursive = TRUE)
  }
  
  # Constrói linha de comando para executar o Osmosis
  osmosis_cmd <- sprintf("%s --read-pbf %s --bounding-box left=%s bottom=%s right=%s top=%s --write-pbf %s",
                         osmosis_path, br_pbf, 
                         muni_bbox@xmin, muni_bbox@ymin, muni_bbox@xmax, muni_bbox@ymax,
                         muni_pbf)
  
  # Chama o Osmosis
  tictoc::tic(msg = muni)
  shell(osmosis_cmd, translate = TRUE)
  tictoc::toc()
}

purrr::walk(munis_df$abrev_muni, extrai_malha_viaria, year = 2020)
system.time(extrai_malha_viaria(muni = "poa", year = 2020))



