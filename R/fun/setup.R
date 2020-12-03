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
library(h3jsr) # h3 hex remotes::install_github("obrl-soil/h3jsr")
library(bit64) # viz large numbers
library(quantreg)



# Cria data.frame com municipios do projeto

# mudanças na geração do script em python



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

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)




munis_df <- tribble(
  ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado, ~modo_2017, ~modo_2018, ~modo_2019, ~modo_2020,
  2304400,    "for",       "Fortaleza",       "CE",          "todos",    "todos",    "todos",    "todos",
  3550308,    "spo",       "Sao Paulo",       "SP",          "todos",    "todos",    "todos",    "todos",
  3304557,    "rio",       "Rio de Janeiro",  "RJ",          "ativo",    "todos",    "todos",    "todos",
  4106902,    "cur",       "Curitiba",        "PR",          "todos",    "todos",    "todos",    "todos",
  4314902,    "poa",       "Porto Alegre",    "RS",          "todos",    "todos",    "todos",    "todos",
  3106200,    "bho",       "Belo Horizonte",  "MG",          "todos",    "todos",    "todos",    "todos",
  5300108,    "bsb",       "Brasilia",        "DF",          "ativo",    "ativo",    "ativo",    "ativo",
  2927408,    "sal",       "Salvador",        "BA",          "ativo",    "ativo",    "todos",    "todos",
  1302603,    "man",       "Manaus",          "AM",          "ativo",    "ativo",    "ativo",    "ativo",
  2611606,    "rec",       "Recife",          "PE",          "ativo",    "ativo",    "todos",    "todos",
  5208707,    "goi",       "Goiania",         "GO",          "ativo",    "ativo",    "todos",    "ativo",
  1501402,    "bel",       "Belem",           "PA",          "ativo",    "ativo",    "ativo",    "ativo",
  3518800,    "gua",       "Guarulhos",       "SP",          "ativo",    "ativo",    "ativo",    "ativo",
  3509502,    "cam",       "Campinas",        "SP",          "todos",    "todos",    "todos",    "todos",
  2111300,    "slz",       "Sao Luis",        "MA",          "ativo",    "ativo",    "ativo",    "ativo",
  3304904,    "sgo",       "Sao Goncalo",     "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  2704302,    "mac",       "Maceio",          "AL",          "ativo",    "ativo",    "ativo",    "ativo",
  3301702,    "duq",       "Duque de Caxias", "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  5002704,    "cgr",       "Campo Grande",    "MS",          "ativo",    "ativo",    "ativo",    "ativo",
  2408102,    "nat",       "Natal",           "RN",          "ativo",    "ativo",    "ativo",    "ativo"
  ) %>% setDT()











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



# Function to switch between git remotes in a project

change_remotes <- function(remote, branch = "master") {
  
  command <- sprintf("git branch --set-upstream-to=%s/%s", remote, branch)
  
  shell(command)
  
  
}


options(scipen = 99999)



# ggplot themes -------------------------------------------------------------------------------

theme_aop_map <- function(base_size, ...) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      # plot.title = element_text(hjust = 0, vjust = 4),
      ...
      
      
    )
}
