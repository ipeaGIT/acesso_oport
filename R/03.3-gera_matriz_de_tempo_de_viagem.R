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



#### 1. Funcao pra rodar scripts em python no OTP e calcular matriz de tempo de viagem -----------------------

gerar_tt_matrix <- function(sigla_muni) {
  
  # sigla_muni <- "cur"
  
  # status message
  message('Woking on city ', sigla_muni, '\n')
  
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

# Aplicar funcao para somente as cidades de modo ativo
munis_ativo <- subset(munis_df, modo == "ativo")$abrev_muni
lapply(X=munis_ativo, gerar_tt_matrix)



a <- munis_df$abrev_muni
a <- a[-2]
lapply(X=a, gerar_tt_matrix)

#### 2. Agrega matriz do OTP num arquivo por cidade -----------------------


# funcao para juntar arquivos por sigla_muni

juntar_output_OTP <- function(sigla_muni, ano){
  
  # sigla_muni <- 'bel'; ano <- 2019
  
  # status message
  message("Working on city ", sigla_muni, "\n")
  
  ### Public Transport
  # pegar os arquivos
  files <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni),
               pattern = "^ttmatrix_\\w{3}_pt",
               full.names = TRUE)
  #    files <- files[c(1:2, 22, 24,45)]
  # files <- files[c(1:20)]
  
  # abrir, juntar e salvar arquivos
  path_out <- sprintf("E:/data/output_ttmatrix/%s/ttmatrix_%s_%s_%s.csv", sigla_muni, ano, sigla_muni,'pt')
  
  # ler, empilhar e salvar arquivos
  plan(multiprocess)
  # furrr::future_map(files, data.table::fread) %>%
  furrr::future_map(files, fread, .progress = TRUE) %>%
    rbindlist() %>%
    fwrite(path_out)
  
  
  ### Walking and Cycling
  # pegar os arquivos
  files <- dir(sprintf("../data/output_ttmatrix/%s", sigla_muni), 
               pattern = "^ttmatrix_\\w{3}_walk|^ttmatrix_\\w{3}_bike",
               full.names = TRUE)
  
  # abrir, juntar e salvar arquivos
  path_out <- sprintf("E:/data/output_ttmatrix/%s/ttmatrix_%s_%s_%s.csv", sigla_muni, ano, sigla_muni,'ativo')
  
  # ler, empilhar e salvar arquivos
  # furrr::future_map(files, data.table::fread, nThread=getDTthreads()) %>%
  lapply(files, data.table::fread, nThread=getDTthreads()) %>%
    rbindlist() %>%
    fwrite(path_out) 
  
  #  # remove files?
  #  walk(files, file.remove)
}

# Aplicar funcao
pbapply::pblapply(munis_df$abrev_muni, FUN=juntar_output_OTP, ano=2019)
juntar_output_OTP("sao", 2019)
juntar_output_OTP("por", 2019)

# plan(multiprocess)
# invisible(furrr::future_map(horarios, abrir_e_juntar))
