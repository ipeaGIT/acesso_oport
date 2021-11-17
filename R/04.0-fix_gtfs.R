
# load -----
options(java.parameters = '-Xmx120G')
library(r5r) # devtools::install_github("ipeaGIT/r5r", subdir = "r-package")

# carregar bibliotecas
#source('./R/fun/setup.R')
library(data.table)
library(gtfsio)
library(magrittr)
library(mapview)
library(sf)
library(sfheaders)
library(dplyr)
library(readr)
library(ggplot2)
source("./R/fun/selecionar_data_gtfs.R")
source('./R/fun/setup.R')



# copy new gtfs to r5r folders

# sigla_muni1 <- "for"
# ano1 <- 2017
copy_gtfs <- function(sigla_muni1, ano1) {
  
  # define gtfs name for each city
  gtfs_files <- fread("../../r5/gtfs_files.csv")
  gtfs_files <- gtfs_files[ano == ano1 & sigla_muni == sigla_muni1]
  
  # copy gtfs raw ------------------
  dir.create(sprintf("../../otp/graphs/teste_joao/%s/%s_raw", ano1, sigla_muni1))
  # copy
  purrr::walk(gtfs_files$gtfs_path_raw,
              file.copy,
              to = sprintf("../../otp/graphs/teste_joao/%s/%s_raw", ano1, sigla_muni1),
              overwrite = TRUE)
  
  
  # copy gtfs updated ---------------------
  dir.create(sprintf("../../otp/graphs/teste_joao/%s/%s_updated", ano1, sigla_muni1))
  # copy
  purrr::walk(gtfs_files$gtfs_path_updated,
              file.copy,
              to = sprintf("../../otp/graphs/teste_joao/%s/%s_updated", ano1, sigla_muni1),
              overwrite = TRUE)
}


walk(munis_list$munis_modo[ano_modo == 2017 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2017)
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2018)
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2019)


# copy other r5 files
copy_r5_files <- function(sigla_muni1, ano1){
  
  # malha viaria
  malha_viaria_dir <- sprintf("../../data-raw/malha_viaria/2020/%s/%s_2020.osm.pbf", sigla_muni1, sigla_muni1)
  
  file.copy(from = malha_viaria_dir,
            to = sprintf("../../otp/graphs/teste_joao/%s/%s_raw/", ano1, sigla_muni1))
  
  file.copy(from = malha_viaria_dir,
            to = sprintf("../../otp/graphs/teste_joao/%s/%s_updated/", ano1, sigla_muni1))
  
  # topografia
  topografia_dir <- sprintf("../../data-raw/topografia/%s/topografia3_%s.tif", sigla_muni1, sigla_muni1)
  
  file.copy(from = topografia_dir,
            to = sprintf("../../otp/graphs/teste_joao/%s/%s_raw/", ano1, sigla_muni1))
  
  file.copy(from = topografia_dir,
            to = sprintf("../../otp/graphs/teste_joao/%s/%s_updated/", ano1, sigla_muni1))
  
}


walk(munis_list$munis_modo[ano_modo == 2017 & modo == "todos"]$abrev_muni, copy_r5_files, ano1 = 2017)
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, copy_r5_files, ano1 = 2018)
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, copy_r5_files, ano1 = 2019)


# sigla_muni = "rec"; ano <- 2019

construir_graph_muni <- function(sigla_muni, ano) { # sigla_muni = "for"; ano <- 2017
  
  
  path <- sprintf("../../otp/graphs/teste_joao/%s/%s_raw", ano, sigla_muni)
  r5r::setup_r5(data_path = path, use_elevation = TRUE, overwrite = TRUE)
  path <- sprintf("../../otp/graphs/teste_joao/%s/%s_updated", ano, sigla_muni)
  r5r::setup_r5(data_path = path, use_elevation = TRUE, overwrite = TRUE)
  
}



walk(munis_list$munis_modo[ano_modo == 2017 & modo == "todos"]$abrev_muni, construir_graph_muni, ano = 2017)
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, construir_graph_muni, ano = 2018)
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, construir_graph_muni, ano = 2019)


construir_graph_muni("for", ano = 2017)
construir_graph_muni("rec", 2019)
construir_graph_muni("spo")
construir_graph_muni("poa")
construir_graph_muni("cam")
construir_graph_muni("goi")
construir_graph_muni("sjc")
construir_graph_muni("rec")


# calcular acessibilidade 1

# sigla_muni <- "bho"; ano <- 2019
# sigla_muni <- "cur"; ano <- 2018
# sigla_muni <- "for"; ano <- 2018
# sigla_muni <- "spo"; ano <- 2019
# sigla_muni <- "rec"; ano <- 2019

calcular_access <- function(ano, sigla_muni, time_window1 = 30) {
  
  
  message(sprintf("working with city of '%s'",sigla_muni))
  
  
  points <- fread(sprintf("../../otp/points/2019/points_%s_09_2019.csv", sigla_muni)) %>% 
    dplyr::select(id = id_hex, lat = Y, lon = X) %>%
    dplyr::mutate(opportunities = 1)
  
  
  mode <- c("WALK","TRANSIT") # , "TRANSIT"
  max_walk_dist <- 1000   # meters
  max_trip_duration <- 180 # minutes
  # pick date
  # select date
  if (sigla_muni == "spo" & ano == 2017) {
    
    date <- "2018-05-01"
    
  } else if (sigla_muni == "bho" & ano == 2019) {
    
    
    date <- "2019-09-20"
    
  } else {
  
  
    
    date <- selecionar_data_gtfs_teste(sigla_muni, ano)
    
  }
  departure <- paste0(date, " 06:00:00")
  
  
  
  departure_datetime <- as.POSIXct(departure, format = "%Y-%m-%d %H:%M:%S")
  
  
  # raw ------
  
  r5r_core <- r5r::setup_r5(data_path = sprintf("../../otp/graphs/teste_joao/%s/%s_raw",
                                                ano, sigla_muni))
  
  # acess_raw <- r5r::accessibility(r5r_core,
  #                                 origins = points,
  #                                 destinations = points,
  #                                 mode = mode,
  #                                 departure_datetime = departure_datetime,
  #                                 time_window = time_window1,
  #                                 cutoffs = 60,
  #                                 max_walk_dist = max_walk_dist
  # )
  
  ttmatrix_raw <- r5r::travel_time_matrix(r5r_core,
                                          origins = points,
                                          destinations = points,
                                          mode = mode,
                                          departure_datetime = departure_datetime,
                                          time_window = 120,
                                          max_walk_dist = max_walk_dist
  )
  
  acess1 <- ttmatrix_raw %>%
    filter(travel_time <= 60) %>%
    count(fromId)
  
  # updated speed------
  r5r_core <- r5r::setup_r5(data_path = sprintf("../../otp/graphs/teste_joao/%s/%s_updated",
                                                ano, sigla_muni))
  
  # acess_updated <- r5r::accessibility(r5r_core,
  #                                     origins = points,
  #                                     destinations = points,
  #                                     mode = mode,
  #                                     departure_datetime = departure_datetime,
  #                                     time_window = time_window1,
  #                                     cutoffs = 60,
  #                                     max_walk_dist = max_walk_dist)
  
  
  ttmatrix_updated <- r5r::travel_time_matrix(r5r_core,
                                          origins = points,
                                          destinations = points,
                                          mode = mode,
                                          departure_datetime = departure_datetime,
                                          time_window = 120,
                                          max_walk_dist = max_walk_dist)
  
  
  acess2 <- ttmatrix_updated %>%
    filter(travel_time <= 60) %>%
    count(fromId)
  
  # acess <- left_join(acess_raw, acess_updated,
  #                    by = "from_id",
  #                    suffix = c("_raw", "_updated"))
  
  ttmatrix <- full_join(ttmatrix_raw, ttmatrix_updated,
                     by = c("fromId", "toId"),
                     suffix = c("_raw", "_updated"))
  
  # summary(acess$dif_perc)
  # oi <- ttmatrix %>% mutate(dif = travel_time_raw/travel_time_updated)
  
  # summary(oi$dif)
  
  hex <- readr::read_rds(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds", sigla_muni)) %>%
    dplyr::select(id_hex)
  
  
  acess_sf <- left_join(acess, hex, by = c("from_id" = "id_hex")) %>% st_sf() %>%
    mutate(ano = ano)
  
  readr::write_rds(acess_sf,sprintf("testes/teste_updated_speed_gtfs/acess_%s_%s.rds",
                                    ano, sigla_muni))
  
  
}



walk(c(2017, 2018, 2019), calcular_access, "for")
walk(c(2017, 2018, 2019), calcular_access, "cur")
walk(c(2017, 2018, 2019), calcular_access, "spo")
walk(c(2017, 2018, 2019), calcular_access, "poa")
walk(c(2017, 2018, 2019), calcular_access, "bho")
walk(c(2018, 2019), calcular_access, "rio")
walk(c(2019), calcular_access, "rec")
walk(c(2019), calcular_access, "goi")


# plot--------


# ano <- 2019; sigla_muni <- "for"
# ano <- 2019; sigla_muni <- "poa"
# ano <- 2019; sigla_muni <- "cur"
# ano <- 2019; sigla_muni <- "rio"
# ano <- 2019; sigla_muni <- "cam"
# ano <- 2019; sigla_muni <- "goi"
# ano <- 2019; sigla_muni <- "spo"

fazer_plot <- function(ano, sigla_muni) {
  
  # paths <- dir("testes/teste_updated_speed_gtfs/", 
  #              pattern = sprintf("acess_\\d{4}_%s", sigla_muni),
  #              full.names = TRUE)
  # 
  # acess_city <- lapply(paths, readr::read_rds) %>% rbindlist() %>% st_sf()
  
  acess_city <- readr::read_rds(sprintf("testes/teste_updated_speed_gtfs/acess_%s_%s.rds", 
                                        ano, sigla_muni))
  
  acess_city <- acess_city %>% 
    # mutate(dif_perc := accessibility_updated / accessibility_raw) %>%
    mutate(dif_perc := log(accessibility_updated / accessibility_raw))
  
  
  max <- max(abs(acess_city$dif_perc), na.rm = TRUE)
  max <- ifelse(sigla_muni == "spo", 0.4, max)
  
  # summary(acess_city$dif_perc)
  # boxplot(acess_city$dif_perc)
  
  # ncol_facet <- ifelse(sigla_muni == "rio", 1, 2)
  
  
  map <- ggplot()+
    geom_sf(data = acess_city, aes(fill = dif_perc), color = NA)+
    # scale_fill_gradient2(low = scales::muted("red"),mid = "white"
    #                      ,high = scales::muted("blue"),midpoint = 1)+
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1)*max, direction = 1)+
    labs(subtitle = "Em vermelho: acessibilidade diminuiu com atualizacao das velocidades")+
    theme_void()+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(0.1, "cm"),
          title = element_text(size = 10))
  
  
  boxplot <- ggplot()+
    geom_boxplot(data = acess_city, aes(x = 1, y = dif_perc))+
    labs(x = "", y = "")+
    theme_bw()
  
  
  library(patchwork)
  plot <- map + boxplot + plot_layout(widths = c(4, 1))
  
  # save it
  ggsave(filename = sprintf("testes/teste_updated_speed_gtfs/plots/plot_%s_%s.png", ano, sigla_muni),
         plot = plot,
         width = 16,
         height = 10,
         units = "cm")
  
}


walk(c(2017, 2018, 2019), fazer_plot, "for")
walk(c(2017, 2018, 2019), fazer_plot, "cur")
walk(c(2017, 2018, 2019), fazer_plot, "spo")
walk(c(2017, 2018, 2019), fazer_plot, "poa")
walk(c(2017, 2018, 2019), fazer_plot, "cam")
walk(c(2019), fazer_plot, "bho")
walk(c(2018, 2019),       fazer_plot, "rio")
walk(c(2019),             fazer_plot, "rec")
walk(c(2019),             fazer_plot, "goi")
