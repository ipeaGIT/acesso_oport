



# This function filter only the estabs (either jobs, schools or hospitals) that
# have a a good geocododing quality
# We consider good quality:
# - From galileo: "4 Estrelas" and "3 Estrelas"
# - From gmaps (https://developers.google.com/maps/documentation/geocoding/overview#Types):
# 'airport'
# 'amusement_park'
# 'bus_station'
# 'establishment'
# 'intersection'
# 'neighborhood'
# 'political'
# 'post_box'
# 'street_number'
# 'premise'
# 'subpremise'
# 'town_square'
# 'postal_code'
# 'route': only when cep from the input matches the cep from the output


# ano <- 2018; atividade <- 'educacao'

geocode_filter <- function(ano, atividade) {
  
  # determinar path dos dados
  path_in <- case_when(
    atividade == 'rais' ~ sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_censoEscolar.rds", ano, ano),
    atividade == 'educacao' ~ sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_geocoded.rds", ano, ano),
    atividade == 'saude' ~ sprintf("../../data/acesso_oport/saude/%s/saude_%s_geocoded.rds", ano, ano),
    
  )
  
  # 1) abrir dados ---------
  data <- read_rds(path_in) %>%
    # identificar a variabel que repreesnta a identificacao do estabelecimento
    rename(id = 1)
  
  # table(data$PrecisionDepth, useNA = 'always')
  

  
  # 2.1) Identificar os route que tem o mesmo cep
  data1 <- data %>%
    filter(PrecisionDepth %in% c('route')) %>%
    # geocode_engine == 'gmaps_prob2') %>%
    # select columns
    # select(id_estab, geocode_engine, PrecisionDepth, SearchedAddress, MatchedAddress) %>%
    # extract cep
    mutate(cep_searched = str_extract(SearchedAddress, "(\\d{5}-\\d{3})|(\\d{8})"),
           cep_matched = str_extract(MatchedAddress, "\\d{5}-\\d{3}")) %>%
    # delete '-'
    mutate(across(starts_with("cep"), ~str_replace(.x, "-", ""))) %>%
    # mutate(across(starts_with("cep"), ~str_sub(.x, 1, 7))) %>%
    # check if they are the same
    mutate(igual = ifelse(cep_searched == cep_matched, TRUE, FALSE)) %>%
    # filter only the same
    filter(igual)
  
  
  # 2) Filtrar todos os precision dept e somente os estabs de route com match de cep
  data <- data[(PrecisionDepth %in% c('3 Estrelas', '4 Estrelas', 'airport', 'amusement_park',
                                      'bus_station', 'establishment', 'intersection', 'neighborhood', 
                                      'political', 'post_box', 'street_number', 'premise', 'subpremise',
                                      'town_square', 'postal_code', 'inep')) |
                 (id %in% data1$id)]
  
  
  # 3) Salvar --------------------------------------------------------------
  
  path_out <- sprintf("../../data/acesso_oport/%s/%s/%s_%s_geocoded_filter.rds", 
                      atividade, ano, atividade, ano)
  write_rds(data, path_out)
  
  
  
}