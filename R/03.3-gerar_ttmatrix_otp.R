# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Calcula matriz de tempo de viagem


# carregar bibliotecas
source('./R/fun/setup.R')


#### 1) Funcao pra rodar scripts em python no OTP e calcular matriz de tempo de viagem -----------------------
gerar_tt_matrix_muni <- function(sigla_muni, ano) {
  
  # sigla_muni <- "for"; ano <- 2017
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano)
  
  # criar diretorio
  dir.create(sprintf("../../data/acesso_oport/output_ttmatrix/%s/%s", ano, sigla_muni))
  
  # lista scripts em python daquela cidade
  python_scripts <- dir(sprintf("../../otp/py/%s/%s", ano, sigla_muni), pattern = sprintf("otp_%s", sigla_muni))
  
  # Funcao para chamar OTP
  chama_otp <- function(x){
    comando <- sprintf("cd ../../otp && java8 -jar programs/jython.jar -Dpython.path=programs/otp-1.4.0-shaded.jar py/%s/%s/%s", ano, sigla_muni, x)
    shell(comando)
  }
  
  # Aplica funcao
  lapply(X=python_scripts, FUN=chama_otp)
  
}

# Aplica funcao para todas as cidades
walk(munis_df$abrev_muni, gerar_tt_matrix_muni, ano = 2017)
walk(munis_df$abrev_muni, gerar_tt_matrix_muni, ano = 2018)
walk(munis_df$abrev_muni, gerar_tt_matrix_muni, ano = 2019)

# tempo para as 20 cidades, 17 horarios de partida
# 97635.94 sec elapsed








#### 2) Agrega matriz do OTP num arquivo por cidade -----------------------

# funcao para juntar arquivos por sigla_muni

juntar_output_OTP_muni <- function(sigla_muni, ano = 2019){
  
  # sigla_muni <- 'for'; ano <- 2017
  
  # status message
  message("Working on city ", sigla_muni, ' at year ', ano, "\n")
  
  ### Public Transport
  # pegar os arquivos
  files <- dir(sprintf("../../data/acesso_oport/output_ttmatrix/%s/%s", ano, sigla_muni),
               pattern = "^ttmatrix_\\w{3}_pt",
               full.names = TRUE)
  #    files <- files[c(1:2, 22, 24,45)]
  # files <- files[c(1:20)]
  
  # abrir, juntar e salvar arquivos
  if(!dir.exists(sprintf("E:/data/output_ttmatrix/%s/otp/%s/", ano, sigla_muni))) {
    
    dir.create(sprintf("E:/data/output_ttmatrix/%s/otp/%s/", ano, sigla_muni))
    
  }
  
  path_out <- sprintf("E:/data/output_ttmatrix/%s/otp/%s/ttmatrix_%s_%s_%s.csv", ano, sigla_muni, sigla_muni,'pt', ano)
  
  # ler, empilhar e salvar arquivos
  plan(multiprocess)
  # furrr::future_map(files, data.table::fread) %>%
  furrr::future_map(files, fread, .progress = TRUE) %>%
    rbindlist() %>%
    fwrite(path_out)
  
  
  ### Walking and Cycling
  # pegar os arquivos
  files <- dir(sprintf("../../data/acesso_oport/output_ttmatrix/%s/%s", ano, sigla_muni), 
               pattern = "^ttmatrix_\\w{3}_walk|^ttmatrix_\\w{3}_bike",
               full.names = TRUE)
  
  # abrir, juntar e salvar arquivos
  path_out <- sprintf("E:/data/output_ttmatrix/%s/otp/%s/ttmatrix_%s_%s_%s.csv", ano, sigla_muni, sigla_muni, 'ativo',ano)
  
  # ler, empilhar e salvar arquivos
  # furrr::future_map(files, data.table::fread, nThread=getDTthreads()) %>%
  lapply(files, data.table::fread, nThread=getDTthreads()) %>%
    rbindlist() %>%
    fwrite(path_out) 
  
  #  # remove files?
  #  walk(files, file.remove)
}

# plan(multiprocess)
# invisible(furrr::future_map(horarios, abrir_e_juntar))
