# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Calcula matriz de tempo de viagem

  
# carregar bibliotecas
source('./R/fun/setup.R')

# ## ----tabela_parametros_otp-----------------------------------------------
# 
# tibble::tribble(
#   ~Parâmetro,                ~Valor,
#   "MaxTimeSec",                "7200",
#   "maxWalkDistance", "Ilimitado (default)",
#   "walkSpeed",     "3 mph (default)",
#   "bikeSpeed",    "11 mph (default)",
#   "walkReluctance",         "2 (default)"
# ) %>%
#   kable() %>%
#   # column_spec(3, width = "3cm") %>%
#   kable_styling(bootstrap_options = "striped", full_width = F)
# 



#### 1. Funcao pra rodar scripts em python no OTP e calcular matriz de tempo de viagem

gerar_tt_matrix <- function(sigla_muni) {
  
  # sigla_muni <- "for"

  # lista scripts em python daquela cidade
  python_scripts <- dir("../otp/py", pattern = sprintf("otp_%s", sigla_muni))
  
  # Funcao para chamar OTP
  chama_otp <- function(x){
    comando <- sprintf("cd ../otp && java -jar programs/jython.jar -Dpython.path=programs/otp-1.4.0-shaded.jar py/%s", x)
    shell(comando)
  }
  
  # Aplica funcao
   lapply(X=python_scripts, FUN=chama_otp)
  
}

# start 17:49
# Aplica funcao para todas as cidades
lapply(X=munis_df$abrev_muni, gerar_tt_matrix)

#' 
#'   
#'   
#'   # colar os arquivos
#'   
#'   # pegar os arquivos
#'   files <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
#'                pattern = "^ttmatrix_\\w{3}_pt",
#'                full.names = TRUE)
#'   
#'   # extrair os horarios
#'   horarios <- str_extract(files, "\\d{1,2}-\\d{1,2}") %>% unique()
#'   
#'   # funcao para abrir e juntar os arquivos de cada horario
#'   
#'   # horarios1 <- horarios[1]
#'   
#'   abrir_e_juntar <- function(horarios1) {
#'     
#'     files_ok <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
#'                     pattern = sprintf("^ttmatrix_\\w{3}_pt_%s", horarios1),
#'                     full.names = TRUE)
#'     
#'     # abrir, juntar e salvar arquivos
#'     path_out <- sprintf("../data/output_ttmatrix/%s/ttmatrix_%s_%s.csv", sigla_muni, sigla_muni, horarios1)
#'     
#'     furrr::future_map(files_ok, fread) %>%
#'       rbindlist() %>%
#'       fwrite(path_out)
#'     
#'     # remove files?
#'     walk(files_ok, file.remove)
#'   }
#'   
#'   # aplicar funcao
#'   plan(multiprocess)
#'   invisible(furrr::future_map(horarios, abrir_e_juntar))
#'   
#'   
#' }
#' 
#' 
#' #' 
