
# # FEED VALIDATOR ----------------------------------------------------------
# 
# cidade <- "rio"
# 
# gtfs_files <- dir(paste0("../data-raw/gtfs/", cidade), pattern = "*.zip")[1]
# 
# dir_output <- paste0("../", cidade, "/validator_", cidade, ".html")
# 
# dir_gtfs <- sprintf("../%s/%s", cidade, gtfs_files)
# 
# command <- sprintf("cd ../data-raw/gtfs/feedvalidator && feedvalidator -o %s %s", dir_output, dir_gtfs)
# 
# 
# # command <- "L: && cd /Proj_acess_oport/data-raw/gtfs/feedvalidator && 
# # feedvalidator ../for/gtfs_for_00_201810.zip"
# 
# # command <- "cd ../data-raw/gtfs/feedvalidator && feedvalidator -o ../for/validator_for.html ../for/gtfs_for_00_201810.zip"
# 
# shell(command)


# FUNCAO!!!!!!!! ----------------------------------------------------------


run_validator <- function(cidade) {
  
  gtfs_files <- dir(paste0("../data-raw/gtfs/", cidade), pattern = "*.zip")[1]
  
  dir_output <- paste0("../", cidade, "/validator_", cidade, ".html")
  
  dir_gtfs <- sprintf("../%s/%s", cidade, gtfs_files)
  
  command <- sprintf("cd ../data-raw/gtfs/feedvalidator && feedvalidator -o %s %s", dir_output, dir_gtfs)
  
  shell(command)
  
}

# # aplicar
# 
# run_validator("for")



