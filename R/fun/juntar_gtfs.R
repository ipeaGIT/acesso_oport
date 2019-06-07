library(readr)
library(purrr)
library(dplyr)
library(stringr)


routes_00 <- read_delim("gtfs_rio_00_20171218/routes.txt", delim = ",")
routes_01 <- read_delim("gtfs_rio_01_20170418/routes.txt", delim = ",")


que <- rbind(routes_00, routes_01)

length(unique(que$route_id))

# OK!



# funcao para juntar dois gtfs --------------------------------------------

# gtfs_folders: um vetor com as pastas de gtfs a serem unidas
juntar_gtfs <- function(gtfs_folders) {
  
  # a <- length(gtfs_folder)
  
  get_files <- function(folder) {
    
    files <- list.files(folder, full.names = T)
    
  }
  
  vai <- gtfs_folders %>%
    map(get_files)
  
  
  
  
  
  
  
}


# testes ------------------------------------------------------------------

opa <- list.files("gtfs_teste", full.names = T)

rep(opa, each = 2)

ai <- opa %>%
  map(get_files)

ai %>%
  map(str_extract, "[[:lower:]]+.txt") %>%
  unlist() %>%
  sort() %>%
  map2_chr(repopa, paste0)
