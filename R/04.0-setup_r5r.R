
# load packages -----------------------------------------------------------
source('./R/fun/setup.R')


# copy new gtfs to r5r folders -------------------------

# create folders
walk(sprintf("../../r5/network/2017/%s", munis_list$munis_df$abrev_muni), dir.create)
walk(sprintf("../../r5/network/2018/%s", munis_list$munis_df$abrev_muni), dir.create)
walk(sprintf("../../r5/network/2019/%s", munis_list$munis_df$abrev_muni), dir.create)

# sigla_muni1 <- "for"; ano1 <- 2017
copy_gtfs <- function(sigla_muni1, ano1) {
  
  # define gtfs name for each city
  gtfs_files <- fread("../../r5/gtfs_files.csv")
  gtfs_files <- gtfs_files[ano == ano1 & sigla_muni == sigla_muni1]
  
  # copy gtfs updated ---------------------
  dir.create(sprintf("../../r5/network/%s/%s", ano1, sigla_muni1))
  # copy
  purrr::walk(gtfs_files$gtfs_path_updated,
              file.copy,
              to = sprintf("../../r5/network/%s/%s", ano1, sigla_muni1),
              overwrite = TRUE)
}


walk(munis_list$munis_modo[ano_modo == 2017 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2017)
walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2018)
walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2019)





# copy other r5 files ----------------------------
copy_r5_files <- function(sigla_muni1, ano1){
  
  # malha viaria
  malha_viaria_dir <- sprintf("../../data-raw/malha_viaria/2020/%s/%s_2020.osm.pbf", sigla_muni1, sigla_muni1)
  
  file.copy(from = malha_viaria_dir,
            to = sprintf("../../r5/network/%s/%s", ano1, sigla_muni1))
  
  # topografia
  topografia_dir <- sprintf("../../data-raw/topografia/%s/topografia3_%s.tif", sigla_muni1, sigla_muni1)
  
  file.copy(from = topografia_dir,
            to = sprintf("../../r5/network/%s/%s", ano1, sigla_muni1))
  
}


walk(munis_list$munis_modo[ano_modo == 2017]$abrev_muni, copy_r5_files, ano1 = 2017)
walk(munis_list$munis_modo[ano_modo == 2018]$abrev_muni, copy_r5_files, ano1 = 2018)
walk(munis_list$munis_modo[ano_modo == 2019]$abrev_muni, copy_r5_files, ano1 = 2019)

