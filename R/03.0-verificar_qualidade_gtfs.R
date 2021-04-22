# Esse script cria e aplica um funcao que roda o feedvalidator para o GTFS
# de cada cidade e ano
# Os GTFS devem estar na pasta ../../data-raw/gtfs/cidade/ano

# carregar bibliotecas
source('./R/fun/setup.R')


# FUNCAO!!!!!!!! ----------------------------------------------------------

# cidade <- "sal"; ano <- 2019

run_validator <- function(cidade, ano,  wait = TRUE) {
  
  gtfs_files <- dir(sprintf("../../data-raw/gtfs/%s/%s", cidade, ano), pattern = "*.zip$")
  
  validator_gtfs <- function(file, city) {
    
    dir_output <- sprintf("../%s/%s/validator_%s.html", city, ano, file)
    
    dir_gtfs <- sprintf("../%s/%s/%s", city, ano, file)
    
    command <- sprintf("cd ../../data-raw/gtfs/feedvalidator && feedvalidator -o %s %s", dir_output, dir_gtfs)
    
    shell(command, wait = wait)
    
  }
  
  purrr::walk(gtfs_files, validator_gtfs, cidade)
  
  
}

run_validator("sal", 2019)
run_validator("cam", 2017)
run_validator("cam", 2018)
run_validator("bho", 2019)
run_validator("cur", 2019)
run_validator("poa", 2019)

run_validator("for", 2021)
