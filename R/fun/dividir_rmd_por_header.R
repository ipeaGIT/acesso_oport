
# adaptado de https://stackoverflow.com/questions/35855837/with-knitr-preserve-chunk-options-when-purling-chunks-into-separate-files
# Essa funcao divide um arquivo .Rmd em diversos arquivos .R de acordo com o header numero 2 (##)

input_file <- "04_matriz.Rmd"
input_file <- "03_otp.Rmd"




dividir_rmd_por_header <- function(input_file) {
  
  purled <- knitr::purl(input_file, documentation = 2)    # purl original file; save name to variable
  
  # Extrair o capitulo
  capitulo <- substr(input_file, 1, 2)
  
  lines <- readLines(purled)    # read purled file into a character vector of lines
  # Deletar texto
  lines <- lines[-grep("#' ([[:alpha:]]|-){1,}", lines)]
  starts <- grep("#' #{2} ", lines)    # use grep to find header row indices
  stops <- c(starts[-1] - 1L, length(lines))   # end row indices
  # extract chunk names from headers
  names <- sub("^#' ## (.*)", '\\1', lines[starts])
  # ajeitar os nomes
  names <- janitor::make_clean_names(names)
  # names <- ifelse(names == lines[starts], '', paste0('_', names)) # clean if no chunk name
  # make nice file names with chunk number and name (if exists)
  file_names <- paste0("R/", capitulo, ".", seq_along(starts), "-", names, '.R')
  for(chunk in seq_along(starts)){    # loop over header rows
    # save the lines in the chunk to a file
    writeLines(lines[starts[chunk]:stops[chunk]], con = file_names[chunk])
  }
  unlink(purled)    # delete purled file of entire document
  
}




# list all rmd files in folder
files_rdm <- list.files(pattern = "0*.Rmd")

# apply function
lapply(files_rdm, FUN=dividir_rmd_por_header)
