Sys.setenv(TZ='UTC') # Fuso horario local

# carregar bibliotecas
library(raster)
library(ggplot2)      # visualizacao de dados
library(ggthemes)     # temas para visualizacao de dados
library(sf)           # leitura e manipulacao de dados espaciais
library(data.table)   # manipulacao de dados
# library(read.dbc)     # leitura de bases relacionais em Microsoft Access
library(geobr)        # dados espaciais do brasil
library(pbapply)      # progress bar
library(readr)        # rapida leitura de dados 
library(tidyr)        # manipulacao de dados
library(stringr)      # operacoes em strings
library(lubridate)    # dados em data/horario
library(fasttime)     # rapido processamento deddados em data/horario
library(mapview)      # visualizacao interativa dos dados
library(RColorBrewer) # paleta de cores
library(extrafont)    # fontes de texto
# library(bit.64)       # lidar com numeros ee 64bits
library(knitr)
library(furrr)
library(purrr)
library(forcats)
library(future.apply) # Aplicar funcoes em paralelo
# library(h3jsr) # H3 grade hexagonal
library(dplyr)
library(hrbrthemes)
library(beepr)
library(patchwork)
# library(Hmisc) # calcular quantis ponderados
# library(osmdata) # Download de dados do OpenStreeteMaps (OSM)
library(opentripplanner) # Usar OTP de dentro do R: https://github.com/ITSLeeds/opentripplanner
library(ggmap) # geocoding




# Cria data.frame com municipios do projeto

# mudanças na geração do script em python

munis_df <- data.table( code_muni= c(2304400, 3550308, 3304557, 4106902, 4314902, 3106200,
                                     5300108, 2927408, 1302603, 2611606, 5208707, 1501402,
                                     3518800, 3509502, 2111300, 3304904, 2704302, 3301702,
                                     5002704, 2408102, 2211001),
                        abrev_muni=c('for', 'spo', 'rio', 'cur', 'poa', 'bho', 
                                     'bsb', 'sal', 'man', 'rec', 'goi', 'bel',
                                     'gua', 'cam', 'slz', 'sgo', 'mac', 'duq',
                                     'cgr', 'nat', 'ter'),
                        name_muni=c('Fortaleza', 'Sao Paulo', 'Rio de Janeiro', 'Curitiba', 'Porto Alegre', 'Belo Horizonte',
                                    'Brasilia', 'Salvador', 'Manaus', 'Recife', 'Goiania', 'Belem',
                                    'Guarulhos', 'Campinas', 'Sao Luis', 'Sao Goncalo', 'Maceio', 'Duque de Caxias',
                                    'Campo Grande', 'Natal', 'Teresina'),
                        abrev_estado=c('CE', 'SP', 'RJ', 'PR', 'RS', 'MG',
                                      'DF', 'BA', 'AM', 'PE', 'GO', 'PA',
                                      'SP', 'SP', 'MA', 'RJ', 'AL', 'RJ',
                                      'MS', 'RN', 'PI'),
                        modo = c('todos', 'todos', 'todos', 'todos', 'todos', 'todos',
                                 'ativo', 'ativo', 'ativo', 'todos', 'ativo', 'ativo',
                                 'ativo', 'ativo', 'ativo', 'ativo', 'ativo', 'ativo',
                                 'ativo', 'ativo', 'todos'))


# library(hrbrthemes)
# library(leaflet.minicharts)
#extrafont::loadfonts(device="win")

options(scipen=10000)

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)


#library(htmltools)
#library(htmlwidgets)

#library(tmap)

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)


to_spatial <- function(df1, coordenada = c("lon", "lat")) {
  x <- st_as_sf(df1, coords = coordenada, crs = 4326)
}


rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}



#' Transforme um objeto `sf` de pontos para coordenadas lon e lat
#' 
#' `sfc_as_cols` retorna um data.frame com colunas de latitude e longitude a partir de um objeto `sf`
#' 
#' Adaptado de https://github.com/r-spatial/sf/issues/231
#' 
#' @param x Um objeto `sf` com pontos do tipo 
#' @param names Um vetor com as colunas desejadas de output de longitude e latitute
sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}

