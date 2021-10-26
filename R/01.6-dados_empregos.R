#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.6 Limpeza e Geocode dados da RAIS

# carregar bibliotecas
source('./R/fun/setup.R')

# carregar funcoes
purrr::walk(dir('./R/fun/empregos', full.names = TRUE), source)

# Aplicar funcoes para o ano de 2017 -------------

# 0) Save raw data with columns we use in the municipalities of the project
rais_filter_raw_data(2017)

# 0.1) Filter raw workers data
rais_filter_pessoas(2017)

# 1) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2017)

# 2) Tratar os outliers 
rais_treat_outliers(2017)

# 3) Realizar e trazer geocode dos estabelecimentos
# realizar geocode
# rais_clean_estabs_raw(2017)
# rais_export_data_to_galileo(2017)

## Run Galileo to geocode the data before moving on to the next functions

# rais_check_new_estabs(2017)
# rais_gmaps_geocode(2017, run_gmaps = FALSE)
# trazer geocode
rais_bring_geocode(2017)


# 5) Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
geocode_filter(ano = 2017, "rais")



# Aplicar funcoes para o ano de 2018 -------------

# 0) Save raw data with columns we use
rais_filter_raw_data(2018)

# 0.1) Filter raw trabalhadores data
rais_filter_pessoas(2018)

# 1) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2018)

# 2) Tratar os outliers 
rais_treat_outliers(2018)

# 3) Realizar e trazer geocode dos estabelecimentos
# realizar geocode
rais_clean_estabs_raw(2018)
rais_export_data_to_galileo(2018)

## Run Galileo to geocode the data before moving on to the next functions

rais_check_new_estabs(2018)
rais_gmaps_geocode(2018, run_gmaps = FALSE)
# trazer geocode
rais_bring_geocode(2018)

# 5) Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
geocode_filter(ano = 2018, "rais")







# Aplicar funcoes para o ano de 2019 -------------

# 0) Save raw data with columns we use
rais_filter_raw_data(2019)

# 0.1) Filter raw trabalhadores data
rais_filter_pessoas(2019)

# 1) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2019)

# 2) Tratar os outliers 
rais_treat_outliers(2019)

# 3) Realizar e trazer geocode dos estabelecimentos
# realizar geocode
rais_clean_estabs_raw(2019)
rais_export_data_to_galileo(2019)

## Run Galileo to geocode the data before moving on to the next functions

rais_check_new_estabs(2019)
rais_gmaps_geocode(2019, run_gmaps = FALSE)
# trazer geocode
rais_bring_geocode(2019)


# 5) Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
geocode_filter(ano = 2019, "rais")


