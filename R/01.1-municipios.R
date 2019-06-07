## ----municipios----------------------------------------------------------

# ajeitar os municipios


arquivos <- dir("../data-raw/municipios", full.names = T, pattern = "_municipios.zip", recursive = T)


out_dir <- paste0("../data-raw/municipios/", str_sub(arquivos, -17, -16))

walk2(arquivos, out_dir, ~unzip(zipfile = .x, exdir = .y))

# # criar pastas
# walk(str_sub(arquivos, -17, -16), ~dir.create(paste0("../data/municipios/", .)))

# nome dos arquivos .shp para abrir
arquivos_shp <- dir("../data-raw/municipios", full.names = T, pattern = "*.shp", recursive = T)

# # arquivo com output
# out_dir_data <- paste0("../data/municipios/", str_sub(arquivos, -17, -16))

# funcao

shp_to_rds <- function(shp) {
  
  shp_files <- st_read(shp, crs = 4326, options = "ENCODING=WINDOWS-1252")
  
  uf <- gsub(".+/(\\D{2})/.+", "\\1", shp)
  
  out_dir <- paste0("../data/municipios/municipios_", uf, ".rds")
  
  write_rds(shp_files, out_dir)
  
  
}


walk(arquivos_shp, shp_to_rds)


#' 
