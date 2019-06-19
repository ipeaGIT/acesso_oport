#' ## Matriz de tempo de viagem
#' 
#' 
#' 
## ----tabela_parametros_otp-----------------------------------------------

tibble::tribble(
  ~Parâmetro,                ~Valor,
  "MaxTimeSec",                "7200",
  "maxWalkDistance", "Ilimitado (default)",
  "walkSpeed",     "3 mph (default)",
  "bikeSpeed",    "11 mph (default)",
  "walkReluctance",         "2 (default)"
) %>%
  kable() %>%
  # column_spec(3, width = "3cm") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


#' 
#' 
## ----criar_pontos_allres-------------------------------------------------

# criar pontos
points_allres("for")
# criar pontos
points_allres("bel")
# criar pontos
points_allres("rio")
# criar pontos
points_allres("cur")
# criar pontos
points_allres("por")
# criar pontos
points_allres("sao")


#' 
#' ### Matriz para Fortaleza
#' 
#' 
## ----aplicar_otp_for-----------------------------------------------------

# Selecionar o dia
dia <- selecionar_data_gtfs("for")

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("for", data = dia, res = "08", from = 7, until = 8, every = 30)

# aplicar otp para todos os modos
aplicar_otp("for", data = dia, res = "08", all_modes = TRUE)

# Elapsed time was 649.622 seconds


#' 
#' 
#' 
## ----aplicar_otp_for_09--------------------------------------------------

# Selecionar o dia
dia <- selecionar_data_gtfs("for")

# criar arquivo python
criar_script_python("for", dia, "09")

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("for", data = dia, res = "09", from = 7, until = 8, every = 30)

# aplicar otp para todos os modos
aplicar_otp("for", data = dia, res = "09", all_modes = TRUE)

# Elapsed time was 3113.68 seconds


#' 
#' ### Matriz para Belo Horizonte
#' 
#' 
## ----matriz bel----------------------------------------------------------

# Selecionar o dia
dia <- selecionar_data_gtfs("bel")

# criar arquivo python
criar_script_python("bel", dia, "08")

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("bel", data = dia, res = "08", from = 7, until = 8, every = 30)

# aplicar otp para todos os modos
aplicar_otp("bel", data = dia, res = "08", all_modes = TRUE)

# Elapsed time was 1453.82 seconds


#' 
#' 
#' ### Matriz para o Rio de Janeiro
#' 
#' 
## ----matriz rio----------------------------------------------------------

# Selecionar o dia
dia <- selecionar_data_gtfs("rio")

# criar arquivo python
criar_script_python("rio", dia, "08")

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("rio", data = dia, res = "08", from = 8, until = 9, every = 30)

# aplicar otp para todos os modos
aplicar_otp("rio", data = dia, res = "08", all_modes = TRUE)

# Elapsed time was 1874.02 seconds


#' 
#' 
#' ### Matriz para Curitiba
#' 
#' 
## ----aplicar_otp_cur-----------------------------------------------------

# Selecionar o dia
dia <- selecionar_data_gtfs("cur")

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, a cada 30 minutos, e para todos os modos
criar_script_python_paral_modes("cur", data = dia, res = "08", from = 7, until = 8, every = 30)

# aplicar otp
aplicar_otp("cur", dia, "08", all_modes = TRUE)

# Elapsed time was 407.342 seconds


#' 
#' 
#' ### Matriz para Porto Alegre
#' 
#' 
## ----aplicar_otp_por-----------------------------------------------------

# Selecionar o dia
dia <- selecionar_data_gtfs("por")

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, a cada 30 minutos, e para todos os modos
criar_script_python_paral_modes("por", data = dia, res = "08", from = 7, until = 8, every = 30)

# aplicar otp
aplicar_otp("por", dia, "08", all_modes = TRUE)

# Elapsed time was 584.123 seconds


#' 
#' 
#' ### Matriz para São Paulo
#' 
#' 
#' 
## ----aplicar_otp_sao-----------------------------------------------------

# Selecionar o dia
dia <- "2019-05-15"

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, a cada 30 minutos, e para todos os modos
criar_script_python_paral_modes("sao", data = dia, res = "08", from = 8, until = 9, every = 30)

# aplicar otp
aplicar_otp("sao", dia, "08", all_modes = TRUE)

# Elapsed time was 8098.53 seconds


#' 
