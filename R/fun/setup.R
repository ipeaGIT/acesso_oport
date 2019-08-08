
Sys.setenv(TZ='UTC') 

library(sp)
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(fasttime)
# library(mapview)
#library(ggmap) #função geocode() pra extrair as coordenadas dos endereços
library(sf) #pra importar os dados espaciais e tal
library(data.table)
library(knitr)
library(readr)
library(tidyr)
# library(hrbrthemes)
library(stringr)
# library(leaflet.minicharts)
library(purrr)
library(lubridate)
library(mapview)
library(RColorBrewer)
library(furrr)
library(extrafont)
library(read.dbc)

#extrafont::loadfonts(device="win")

options(scipen=10000)

`%nin%` = Negate(`%in%`)


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

