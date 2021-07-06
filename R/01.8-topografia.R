#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Leitura e filtro de elevacao/topografia

#' info:
#' Os dados de topografia são provenientes da missão SRTM (_Shuttle Radar 
#' Topography Mission_), que é um esforço de pesquisa internacional que obtém 
#' dados de elevação numa precisão de 30 metros. Os dados de elevação do SRTM são 
#' divididos por quadrículo de 1 grau de latidude e 1 longitude, então é 
#' necessário:
#'   1 - identificar os quadrículos que cobrem a cidade escolhida;
#'   2 - baixar o raster correspondente a cada quadrículos em uma pasta temporária;
#'   3 - unir os grids em um grid único, usando a função raster::mosaic();
#'   4 - recortar do raster a área correspondente ao bounding box do município;
#'   5 - salvar o raster do município na pasta correspondente.
#'   
#' Para ter acesso a esses dados, é necessário criar um login no site
#' https://urs.earthdata.nasa.gov, e informar os dados de usuário e senha quando
#' for rodar esse script.


# carregar bibliotecas
source('./R/fun/setup.R')
library(httr)
library(raster)

message("Inform username and password to access https://urs.earthdata.nasa.gov ")
username <- readline("Give the username : ")
password <- readline("Give the password : ")

# username <- "user"
# password <- "pass"

# ano <- 2020
# sigla_muni <- "man"
# sigla_muni="for"

download_srtm <- function(sigla_muni) {
  # read municipality boundary
  muni_sf <- readr::read_rds(sprintf("../../data-raw/municipios/2020/municipio_%s_2020.rds", sigla_muni))
  
  # muni_sf %>% mapview()
  
  # extract bounding box
  bbox <- st_bbox(muni_sf)
  bbox <- as.integer(bbox) - 1
  
  # identify which tiles are needed to cover the whole study area
  lons <- seq(floor(bbox[1]), ceiling(bbox[3]), by = 1)
  lats <- seq(floor(bbox[2]), ceiling(bbox[4]), by = 1)
  tiles <- expand.grid(lat = lats, lon = lons) %>%
    mutate(hx = if_else(lon < 0, "W", "E"),
           hy = if_else(lat < 0, "S", "N"))
  tile = sprintf("%s%02d%s%03d", tiles$hy, abs(tiles$lat), tiles$hx, abs(tiles$lon))
  
  # build the url's for each tile
  urls <- paste0("https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/",
                tile, ".SRTMGL1.hgt.zip")

  # download zip files and extract raster tiles
  outputdir <- tempdir()
  zipfiles <- paste0(outputdir, "\\", tile, ".hgt.zip")
  rstfiles <- paste0(outputdir, "\\", tile, ".hgt")
  
  walk2(urls, zipfiles, function(url, filename) {
    httr::GET(url = url, 
              authenticate(username, password),
              write_disk(path =filename, overwrite = TRUE),
              progress())
  })
  
  walk(zipfiles, unzip, exdir = outputdir)
  
  # read all raster tiles, merge them together, and then crop to the study area's bounding box
  rst <- map(rstfiles, raster)
  if (length(rst) == 1) {
    rst_layer <- rst[[1]]
  } else {
    rst_layer <- do.call(raster::mosaic, args = c(rst, fun = mean))
  }
  rst_layer_crop <- raster::crop(rst_layer, st_bbox(muni_sf))

  # save processed raster to the municipality folder
  dir.create(paste0("../../data/acesso_oport/topografia/", sigla_muni, "/"), recursive = TRUE)
  raster::writeRaster(rst_layer_crop, 
                      paste0("../../data/acesso_oport/topografia/", sigla_muni, "/topografia_", sigla_muni, ".tif"),
                      overwrite = TRUE)
}

# download_srtm("poa")
# download_srtm("bel")
walk(munis_list$munis_df$abrev_muni, download_srtm)

