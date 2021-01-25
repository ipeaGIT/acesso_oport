#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Geocode dados do censo escolar

# carregar bibliotecas
source('./R/fun/setup.R')

# carregar funcoes
purrr::walk(dir('./R/fun/empregos', full.names = TRUE), source)

# Aplicar funcoes para o ano de 2017 -------------

# 1) Filter raw trabalhadores data
rais_filter_raw_data(2017)

# 2) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2017)

# 4) Tratar os outliers 
rais_treat_outliers(2017)

# 5) Realizar e trazer geocode dos estabelecimentos
# realizar geocode
rais_clean_estabs_raw(2017)
rais_export_data_to_galileo(2017)
rais_gmaps_geocode(2017, run_gmaps = FALSE)
# trazer geocode
rais_bring_geocode(2017)

# 6) Trazer informacoes de funcionarios de escolas publicas do censo escolar 
rais_bring_schools(2017)




# Aplicar funcoes para o ano de 2018 -------------

# 1) Filter raw trabalhadores data
rais_filter_raw_data(2018)

# 2) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2018)

# 4) Tratar os outliers 
rais_treat_outliers(2018)

# 5) Realizar e trazer geocode dos estabelecimentos
# realizar geocode
rais_clean_estabs_raw(2018)
rais_export_data_to_galileo(2018)
rais_gmaps_geocode(2018, run_gmaps = FALSE)
# trazer geocode
rais_bring_geocode(2018)

# 6) Trazer informacoes de funcionarios de escolas publicas do censo escolar 
rais_bring_schools(2018)



# Aplicar funcoes para o ano de 2019 -------------

# 1) Filter raw trabalhadores data
rais_filter_raw_data(2019)

# 2) Categorizar trabalhadores por grau de instrucao
rais_categorize_inst(2019)

# 4) Tratar os outliers 
rais_treat_outliers(2019)

# 5) Realizar e trazer geocode dos estabelecimentos
# realizar geocode
rais_clean_estabs_raw(2019)
rais_export_data_to_galileo(2019)
rais_gmaps_geocode(2019, run_gmaps = FALSE)
# trazer geocode
rais_bring_geocode(2019)

# 6) Trazer informacoes de funcionarios de escolas publicas do censo escolar 
rais_bring_schools(2019)




