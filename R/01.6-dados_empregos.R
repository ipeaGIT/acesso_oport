#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.6 Limpeza e Geocode dados da RAIS
# Esse script aplica funcoes que foram definidas na pasta R/fun/empregos p/ todos os anos de processamento
# Sao as funcoes principais:
# 0) Save raw data with columns we use in the municipalities of the project
# 1) Filter raw workers data - only private jobs
# 2) Categorizar trabalhadores por grau de instrucao
# 3) Tratar os outliers 
# 4) Trazer o geocode dos estabelecimentos e aprimorar a qualidade quando necessario
# 5) Trazer geocode p/ a base original (da etapa 2)
# 6) Selecionar somente as obsservacoes com boa qualidade de geocode

# carregar bibliotecas
source('./R/fun/setup.R')

# carregar funcoes
purrr::walk(dir('./R/fun/empregos', full.names = TRUE), source)




# Aplicar funcoes para o ano de 2017 -------------

# 0) Save raw data with columns we use in the municipalities of the project
rais_filter_raw_data(2017)

# 1) Filter raw workers data - only private jobs
rais_filter_pessoas(2017)

# 2) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2017)

# 3) Tratar os outliers 
rais_treat_outliers(2017)

# 4) Aprimorar o geocode dos estabelecimentos que veio do streetmap
# primeiro, limpar a base completa geocoded que veio do streetmap
rais_clean_estabs_raw(2017)
# segundo, aprimorar o geocde utilizando o geocode do gmaps
rais_gmaps_geocode(2017, run_gmaps = FALSE)

# 5) Trazer geocode p/ a base original (da etapa 2)
rais_bring_geocode(2017)

# 6) Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
geocode_filter(ano = 2017, "rais")




# Aplicar funcoes para o ano de 2018 -------------

# 0) Save raw data with columns we use in the municipalities of the project
rais_filter_raw_data(2018)

# 1) Filter raw workers data - only private jobs
rais_filter_pessoas(2018)

# 2) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2018)

# 3) Tratar os outliers 
rais_treat_outliers(2018)

# 4) Aprimorar o geocode dos estabelecimentos que veio do streetmap
# primeiro, limpar a base completa geocoded que veio do streetmap
rais_clean_estabs_raw(2018)
# segundo, aprimorar o geocde utilizando o geocode do gmaps
rais_gmaps_geocode(2018, run_gmaps = FALSE)

# 5) Trazer geocode p/ a base original (da etapa 2)
rais_bring_geocode(2018)

# 6) Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
geocode_filter(ano = 2018, "rais")



# Aplicar funcoes para o ano de 2019 -------------

# 0) Save raw data with columns we use in the municipalities of the project
rais_filter_raw_data(2019)

# 1) Filter raw workers data - only private jobs
rais_filter_pessoas(2019)

# 2) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2019)

# 3) Tratar os outliers 
rais_treat_outliers(2019)

# 4) Aprimorar o geocode dos estabelecimentos que veio do streetmap
# primeiro, limpar a base completa geocoded que veio do streetmap
rais_clean_estabs_raw(2019)
# segundo, aprimorar o geocde utilizando o geocode do gmaps
rais_gmaps_geocode(2019, run_gmaps = FALSE)

# 5) Trazer geocode p/ a base original (da etapa 2)
rais_bring_geocode(2019)

# 6) Selecionar somente as obsservacoes com boa qualidade de geocode
source("R/fun/filter_geocode.R")
geocode_filter(ano = 2019, "rais")






