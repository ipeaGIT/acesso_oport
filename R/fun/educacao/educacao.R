


#' A funcao 'educacao_filter':
#' 1) Lê os dados do censo escolar (que foram baixadas do site do INEP, base de escolas) e faz filtros selecionando


educacao_filter <- function(ano, download = FALSE) {
  
  
  # a <- fread("../../data-raw/censo_escolar/2017/MATRICULA_CO.CSV", nrow = 10)
  
  # 1) Abrir e juntar dados de matriculas ------------------------------------
  matriculas <- lapply(list.files(sprintf("../../data-raw/censo_escolar/%s", ano), 
                                  pattern = "MATRICULA", full.names = TRUE),
                       fread, select = c("CO_ENTIDADE", "TP_DEPENDENCIA", "TP_ETAPA_ENSINO", 
                                         "IN_REGULAR", "IN_PROFISSIONALIZANTE")) %>%
    rbindlist()
  
  # selecionar somente matriculas regulares
  matriculas <- matriculas[IN_REGULAR == 1 | IN_PROFISSIONALIZANTE == 1]
  # selecionar somente matriculas em escolas publicas
  matriculas <- matriculas[TP_DEPENDENCIA %in% c(1, 2, 3)]
  
  # count(matriculas, TP_ETAPA_ENSINO)
  
  # categorias a serem escolhidas e re-categorizadas
  # checar arquivo Dicionário de Dados da Educaç╞o Básica 2017.excel na mesma pasta
  matriculas[,
             mat_tipo := fcase(
               TP_ETAPA_ENSINO == 1 , "mat_infantil"   , # - Educação Infantil - Creche
               TP_ETAPA_ENSINO == 2 , "mat_infantil"   , # - Educação Infantil - Pré-escola
               TP_ETAPA_ENSINO == 4 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 1ª Série
               TP_ETAPA_ENSINO == 5 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 2ª Série
               TP_ETAPA_ENSINO == 6 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 3ª Série
               TP_ETAPA_ENSINO == 7 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 4ª Série
               TP_ETAPA_ENSINO == 8 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 5ª Série
               TP_ETAPA_ENSINO == 9 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 6ª Série
               TP_ETAPA_ENSINO == 10, "mat_fundamental", # - Ensino Fundamental de 8 anos - 7ª Série
               TP_ETAPA_ENSINO == 11, "mat_fundamental", # - Ensino Fundamental de 8 anos - 8ª Série
               TP_ETAPA_ENSINO == 14, "mat_fundamental", # - Ensino Fundamental de 9 anos - 1º Ano
               TP_ETAPA_ENSINO == 15, "mat_fundamental", # - Ensino Fundamental de 9 anos - 2º Ano
               TP_ETAPA_ENSINO == 16, "mat_fundamental", # - Ensino Fundamental de 9 anos - 3º Ano
               TP_ETAPA_ENSINO == 17, "mat_fundamental", # - Ensino Fundamental de 9 anos - 4º Ano
               TP_ETAPA_ENSINO == 18, "mat_fundamental", # - Ensino Fundamental de 9 anos - 5º Ano
               TP_ETAPA_ENSINO == 19, "mat_fundamental", # - Ensino Fundamental de 9 anos - 6º Ano
               TP_ETAPA_ENSINO == 20, "mat_fundamental", # - Ensino Fundamental de 9 anos - 7º Ano
               TP_ETAPA_ENSINO == 21, "mat_fundamental", # - Ensino Fundamental de 9 anos - 8º Ano
               TP_ETAPA_ENSINO == 41, "mat_fundamental", # - Ensino Fundamental de 9 anos - 9º Ano
               TP_ETAPA_ENSINO == 25, "mat_medio"      , # - Ensino Médio - 1ª Série
               TP_ETAPA_ENSINO == 26, "mat_medio"      , # - Ensino Médio - 2ª Série
               TP_ETAPA_ENSINO == 27, "mat_medio"      , # - Ensino Médio - 3ª Série
               TP_ETAPA_ENSINO == 28, "mat_medio"      , # - Ensino Médio - 4ª Série
               TP_ETAPA_ENSINO == 29, "mat_medio"      , # - Ensino Médio - Não Seriada
               TP_ETAPA_ENSINO == 30, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série
               TP_ETAPA_ENSINO == 31, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série
               TP_ETAPA_ENSINO == 32, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série
               TP_ETAPA_ENSINO == 33, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série
               TP_ETAPA_ENSINO == 34, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada
               TP_ETAPA_ENSINO == 35, "mat_medio"      , # - Ensino Médio - Normal/Magistério 1ª Série
               TP_ETAPA_ENSINO == 36, "mat_medio"      , # - Ensino Médio - Normal/Magistério 2ª Série
               TP_ETAPA_ENSINO == 37, "mat_medio"      , # - Ensino Médio - Normal/Magistério 3ª Série
               TP_ETAPA_ENSINO == 38, "mat_medio"      , # - Ensino Médio - Normal/Magistério 4ª Série
               TP_ETAPA_ENSINO == 39, "mat_medio"      , # - Curso Técnico - Concomitante
               TP_ETAPA_ENSINO == 40, "mat_medio"      , # - Curso Técnico - Subsequente
               TP_ETAPA_ENSINO == 68, NA_character_    , # - Curso FIC Concomitante
               TP_ETAPA_ENSINO == 65, NA_character_    , # - EJA - Ensino Fundamental - Projovem Urbano
               TP_ETAPA_ENSINO == 67, NA_character_    , # - Curso FIC integrado na modalidade EJA  - Nível Médio
               TP_ETAPA_ENSINO == 69, NA_character_    , # - EJA - Ensino Fundamental -  Anos iniciais
               TP_ETAPA_ENSINO == 70, NA_character_    , # - EJA - Ensino Fundamental -  Anos finais
               TP_ETAPA_ENSINO == 71, NA_character_    , # - EJA - Ensino Médio
               TP_ETAPA_ENSINO == 72, NA_character_    , # - EJA - Ensino Fundamental  - Anos iniciais e Anos finais4
               TP_ETAPA_ENSINO == 73, NA_character_    , # - Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)
               TP_ETAPA_ENSINO == 74, NA_character_    # - Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)
             )]
  
  # table(matriculas$mat_tipo, useNA = "always")
  
  # tirar NAs
  matriculas <- matriculas[!is.na(mat_tipo)]
  # agrupar por escola e tipo de matricula
  matriculas_group <- matriculas[, .(.N), by = .(CO_ENTIDADE, mat_tipo)]
  
  # transformar para formato largo
  matriculas_group_wide <- tidyr::pivot_wider(matriculas_group,
                                              names_from = mat_tipo,
                                              values_from = N,
                                              values_fill = 0)
  
  
  # 2) Ler escolas do ano do censo escolar ---------------------------------------------------------
  
  # processo manual:
  # 1) baixar dados para os anos desse link: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
  # 2) dezipar o arquivo completamente
  # 3) copiar o arquivo de ANEXOS/dicionario, ESCOLAS, e as MATRICULAS de todas as regioes
  # 4) copiar para a pasta do ano no data-raw
  
  # essa funcao esta desativada porque os dados estao sendo baixados manualmente
  # deu problema na funcao de zip_list que sempre dava crash no R
  # if (download) {
  #   
  #   # os dados do censo escolar sao baixados daqui https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
  #   # os dados tambem podem ser baixados automaticamente para cada ano (arquivo .zip)
  #   # definir url
  #   url <- ifelse(ano == 2017, sprintf("https://download.inep.gov.br/microdados/micro_censo_escolar_%s.zip", ano),
  #                 sprintf("https://download.inep.gov.br/microdados/microdados_educacao_basica_%s.zip", ano))
  #   curl::curl_download(url,
  #                       destfile = sprintf("../../data-raw/censo_escolar/%s/censo-escolar_%s.zip", ano, ano),
  #                       quiet = FALSE)
  #   
  #   # extrar os arquivos dentro de cada zip
  #   zip_list <- zip_list(sprintf("https://download.inep.gov.br/microdados/micro_censo_escolar_%s.zip", ano))
  #   
  #   # so vamos extrair dos arquivos: a base de escolas e a base com a qtde de matriculas
  #   zip::unzip(zipfile = sprintf("https://download.inep.gov.br/microdados/micro_censo_escolar_%s.zip", ano), 
  #              files = "",
  #              exdir = sprintf("../../data-raw/censo_escolar/%s", ano))
  # }
  
  # colunas de interesse: 
  colunas <- c(c("CO_ENTIDADE", "NO_ENTIDADE", "CO_MUNICIPIO",
                 # "IN_COMUM_CRECHE", "IN_COMUM_PRE", 
                 # "IN_COMUM_FUND_AI", "IN_COMUM_FUND_AF", 
                 # "IN_COMUM_MEDIO_MEDIO", "IN_COMUM_MEDIO_NORMAL",
                 # "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE", 
                 # "IN_COMUM_MEDIO_INTEGRADO", "IN_PROFISSIONALIZANTE",
                 # "IN_ESP_EXCLUSIVA_FUND_AI", "IN_ESP_EXCLUSIVA_FUND_AF",
                 # "IN_ESP_EXCLUSIVA_MEDIO_MEDIO", "IN_ESP_EXCLUSIVA_MEDIO_INTEGR",
                 # "IN_ESP_EXCLUSIVA_MEDIO_NORMAL","IN_COMUM_EJA_MEDIO","IN_COMUM_EJA_PROF",
                 # "IN_ESP_EXCLUSIVA_EJA_MEDIO","IN_ESP_EXCLUSIVA_EJA_PROF","IN_COMUM_PROF",
                 # "IN_ESP_EXCLUSIVA_PROF","IN_COMUM_EJA_FUND","IN_ESP_EXCLUSIVA_EJA_FUND",
                 "IN_LOCAL_FUNC_UNID_PRISIONAL", "IN_LOCAL_FUNC_PRISIONAL_SOCIO", # escolas prisionais
                 "IN_REGULAR", "IN_PROFISSIONALIZANTE", "IN_EJA",
                 "TP_DEPENDENCIA", "TP_SITUACAO_FUNCIONAMENTO"), 
               ifelse(ano == 2017, "NU_FUNCIONARIOS", "QT_FUNCIONARIOS"))
  
  escolas <- fread(sprintf("../../data-raw/censo_escolar/%s/ESCOLAS.CSV", ano),
                   select = colunas)
  # rename funcionarios variable
  if (ano != 2019) {
    
    colnames(escolas)[ncol(escolas)] <- "NU_FUNCIONARIOS"
    
  }
  
  # format columns
  escolas <- janitor::clean_names(escolas)
  
  # filter municipalties
  muni_list <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist()
  escolas_munis <- escolas[co_municipio %in% muni_list]
  
  # only public
  escolas_munis <- escolas_munis[tp_dependencia %in% c(1, 2, 3)]
  
  # only active
  escolas_munis <- escolas_munis[tp_situacao_funcionamento == 1]
  
  # selecionar somente escola com ensino regular
  escolas_munis <- escolas_munis[in_regular == 1 | in_profissionalizante == 1]
  
  # Identifica codigo das escolas priosionais
  escolas_prisionais <- subset(escolas_munis, in_local_func_unid_prisional ==1 | in_local_func_prisional_socio ==1)$co_entidade
  
  # remove escolas prisionais
  escolas_fim <- subset(escolas_munis, co_entidade %nin% escolas_prisionais)
  escolas_fim$in_local_func_unid_prisional <- NULL
  escolas_fim$in_local_func_prisional_socio <- NULL
  

  # 3) trazer matriculas -------------------------------------------------------

  # usando inner_join para manter apenas escolas com matriculas que nao sejam EJA
  escolas_fim_mat <- inner_join(
    escolas_fim, 
    matriculas_group_wide,
    by = c("co_entidade" = "CO_ENTIDADE"),
    sort = FALSE
  )
  
  sum(is.na(escolas_fim_mat$mat_infatil))
  sum(is.na(escolas_fim_mat$mat_fundamental))
  sum(is.na(escolas_fim_mat$mat_medio))
  
  
  # 4) Selecionar variaveis e salvar ---------------
  # after 2019, variable 'nu_funcionarios' was descontinued
  if (ano %in% c(2017, 2018)) {
    
    escolas_fim_mat <- escolas_fim_mat %>%
      select(co_entidade, code_muni = co_municipio, no_entidade, 
             mat_infantil, mat_fundamental, mat_medio, 
             nu_funcionarios
             )
    
    
  } else {
    
    
    escolas_fim_mat <- escolas_fim_mat %>%
      select(co_entidade, code_muni = co_municipio, no_entidade, 
             mat_infantil, mat_fundamental, mat_medio
             )
    
  }
  
  message("Total de matriculas nivel mat_infantil: ", sum(escolas_fim_mat$mat_infantil, na.rm = TRUE))
  message("Total de matriculas nivel mat_fundamental: ", sum(escolas_fim_mat$mat_fundamental, na.rm = TRUE))
  message("Total de matriculas nivel mat_medio: ", sum(escolas_fim_mat$mat_medio, na.rm = TRUE))
  
  # 4) salvar ---------------------------
  write_rds(escolas_fim_mat, sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter.rds", ano, ano), compress = 'gz')
  
  
}







#' A funcao 'educacao_geocode' faz o geocode de escolas com coordenadas problematicas e tem como output
#' a base final do ano ja com as coordenadas corrigidas e pronta
#' Etapas:
#' 1) Abre as escolas do ano que foram filtradas na etapa anterior
#' 2) Separa somente as escolas para geocode que nao forem geocoded no ano anterior (isso so serve a partir de 2018)
#' 3) Identifica escolas com lat/long problematicos a partir dos quatro criterios estabelecidos
#' 4) Faz geocode das escolas problematicas usando google maps. Se a opcao 'run_gmaps = FALSE', vai fazer uso
#' dos dados que ja foram rodados no gmaps antes
#' 5) Recodifica escolas que estao fora da cidade: algumas escolas persistem em ficar fora da cidade, o q pode indicar
#' que esses estabs estao com a localizacao correta so que foram registrados numa cidade da RM
#' 6) Traz as coordenadas corrigidas para a base do novo ano
#' 7) Traz as coordenadas do ano anterior para a base do novo ano




educacao_geocode <- function(ano, run_gmaps = FALSE) {
  
  
  # 1) Trazer as coordenadas da escolas fornecidas pelo INEP -------------------------------------------
  escolas_coords <- fread("../../data-raw/censo_escolar/coordenadas_geo/escolas_inep_2020.csv",
                          encoding = "UTF-8")
  # abrir escolas filter
  escolas <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter.rds", ano, ano))
  
  # reformat columns
  escolas_coords <- janitor::clean_names(escolas_coords)
  escolas_coords <- escolas_coords %>% 
    select(co_entidade = codigo_inep, endereco, lon = longitude, lat = latitude)
  
  
  # table(nchar(escolas$co_entidade))
  # table(nchar(escolas_coords$co_entidade))
  
  # 3) Join das escolas do censo escolar com as coords fornecidas pelo INEP ------------------------
  # join to create escolas geo
  escolas_geo <- merge(
    escolas,
    escolas_coords,
    by = "co_entidade",
    all.x = TRUE
  ) %>% mutate(co_entidade = as.character(co_entidade)) %>% setDT()
  
  
  # 2) Separar somente as escolas para geocode que nao forem geocoded no ano anterior ----------
  
  if (ano != 2017) {
    
    escolas_previous_geo <- read_rds(sprintf("../../data/acesso_oport/educacao/%i/educacao_%i_geocoded.rds", ano - 1, ano - 1)) %>%
      select(co_entidade, SearchedAddress,  PrecisionDepth, MatchedAddress, geocode_engine, lon, lat, year_geocode)
    
    escolas_geo_togeo <- escolas_geo %>% filter(co_entidade %nin% escolas_previous_geo$co_entidade)
    
    
    
  } else (escolas_geo_togeo <- escolas_geo)
  
  
  
  
  # 3) Identificar escolas com lat/long problematicos ----------------------
  
  # A - poucos digitos
  # B - fora dos limites do municipio
  # C - com coordenadas NA
  # D - mais de 5 estabelecimentos com coordenadas iguais
  
  
  # qual o nivel de precisao das coordenadas que deve ser aceito?
  # 0.01 = 1113.2 m
  # 0.001 = 111.32 m
  # 0.0001 = 11.132 m
  # 0.00001 = 1.1132 m
  
  
  # A) Numero de digitos de lat/long apos ponto
  
  # Acho que esse aqui funciona melhor
  setDT(escolas_geo_togeo)[, ndigitos_lat := nchar( gsub("^.*\\.","", lat))    ]
  setDT(escolas_geo_togeo)[, ndigitos_lon := nchar( gsub("^.*\\.","", lon))    ]
  setDT(escolas_geo_togeo)[, ndigitos := pmin(ndigitos_lon, ndigitos_lat) , by= co_entidade ]
  A_estbs_pouco_digito <- escolas_geo_togeo[ ndigitos <=2,]
  
  
  # B) fora dos limites do municipio
  
  # carrega shapes
  shps <- purrr::map_dfr(dir("../../data-raw/municipios/2019", recursive = TRUE, full.names = TRUE), read_rds) %>% 
    # as_tibble() %>%
    st_sf(crs = 4326)
  
  # convert para sf
  censoescolar_df_coords_fixed_df <- escolas_geo_togeo[!(is.na(lat))] %>% 
    st_as_sf( coords = c('lon', 'lat'), crs = 4326)
  
  temp_intersect <- sf::st_join(censoescolar_df_coords_fixed_df, shps)
  
  # escolas que cairam fora de algum municipio
  B_muni_fora <- subset(temp_intersect, is.na(code_state))
  
  
  
  
  # C) Lat lon NA (feito mais a frente)
  
  
  
  # D) mais de 5 estabelecimentos com coordenadas iguais
  
  # junta lat lon
  escolas_geo_togeo$latlon <- paste0(escolas_geo_togeo$lat, '/', escolas_geo_togeo$lon)
  
  # freq de lat lon repetido
  tab_latlon <- escolas_geo_togeo %>% count(latlon, sort = T)
  latlon_problema <- subset(tab_latlon, n >3 & latlon != "NA/NA")
  
  
  
  # juntar todos municipios com erro de lat/lon
  munis_problemaA <- subset(escolas_geo_togeo, co_entidade %in% A_estbs_pouco_digito$co_entidade )
  munis_problemaB <- subset(escolas_geo_togeo, co_entidade %in% B_muni_fora$co_entidade ) 
  munis_problemaC <- escolas_geo_togeo[ is.na(lat), ]
  munis_problemaD <- subset(escolas_geo_togeo, latlon %in% latlon_problema$latlon) 
  
  munis_problema <- rbind(munis_problemaA, munis_problemaB, munis_problemaC, munis_problemaD) 
  munis_problema <- dplyr::distinct(munis_problema, co_entidade, .keep_all=T) # remove duplicates, 1072 obs
  nrow(munis_problema)
  
  # 4) Fazer geocode das escolas problematicas usando google maps -----------------------------------------
  
  # select only address that are not NA
  munis_problema_enderecos <- munis_problema %>% filter(!is.na(endereco))
  
  # lista de enderecos com problema
  enderecos <- munis_problema_enderecos$endereco
  
  # registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1[3])
  
  if (run_gmaps) {
    
    a <- menu(choices = c("Yes", "No"), title = sprintf("Are  you sure you wanna send %s address to Google Maps?", length(enderecos)))
    
    if (a == 1) {
      
      message("Running gmaps, this may take a while")
      
      # geocode
      coordenadas_google1 <- ggmap::geocode(enderecos, output = "all")
      
      # identify list names as co_entidade
      names(coordenadas_google1) <- munis_problema_enderecos$co_entidade
      
      # save
      write_rds(coordenadas_google1, sprintf("../../data/acesso_oport/educacao/%s/geocode/escolas_geocode_%s_output_google1.rds", ano, ano), compress = 'gz')
      
      
    }
    
    
  } else {
    
    coordenadas_google1 <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/geocode/escolas_geocode_%s_output_google1.rds", ano, ano))
  }
  
  
  
  # build df with result from ggmap::geocode
  
  create_dt <- function(x) { # x <- coordenadas_google1[1]
    
    precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0, 
                               x[["results"]][[1]][["address_components"]], 
                               NA)
    
    # check length from precision depth
    precision_depth <- ifelse(is.na(precision_depth0), NA,
                              ifelse(length(precision_depth0[[1]]$types) > 0,
                                     precision_depth0[[1]]$types[[1]], 
                                     NA))
    
    a <- data.table(
      MatchedAddress = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
      # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
      PrecisionDepth = precision_depth,
      lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
      lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
    )
    
  }
  
  
  estabs_problema_geocoded <- lapply(coordenadas_google1, create_dt)
  # estabs_problema_geocoded <- lapply(coordenadas_google1, possibly(create_dt, otherwise = "erro"))
  
  # rbind as data.table
  estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "co_entidade",
                                           use.names = TRUE)
  
  # unique(estabs_problema_geocoded_dt$id_estab) %>% length()
  
  # MAKE SURE WE ARE ONLY TREATING PROBLEMATIC SCHOOLS
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[co_entidade %in% munis_problema$co_entidade]
  
  # identify searched address
  searchedaddress <- filter(munis_problema_enderecos, co_entidade %in% names(coordenadas_google1)) %>%
    mutate(SearchedAddress = endereco) %>% select(co_entidade, SearchedAddress) %>%
    distinct(co_entidade, .keep_all = TRUE) %>% mutate(co_entidade = as.character(co_entidade))
  
  estabs_problema_geocoded_dt <- left_join(estabs_problema_geocoded_dt, searchedaddress, by = "co_entidade") %>% setDT()
  
  # identify problem
  estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
  
  # identify quality
  estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  # keep schools that did not have address info
  if(length(munis_problema %>% filter(is.na(endereco)) %>% .$co_entidade) > 0) {
    without_address <- data.table(
      co_entidade = munis_problema %>% filter(is.na(endereco)) %>% .$co_entidade,
      MatchedAddress = NA,
      SearchedAddress = NA,
      PrecisionDepth = "without_address",
      geocode_engine = "without_address",
      lon = NA,
      lat = NA
    )
    
    estabs_problema_geocoded_dt <- rbind(estabs_problema_geocoded_dt,
                                         without_address, fill=T)
    
  }
  
  
  
  # 5) Recodificar escolas que estao fora da cidade ---------------------------
  
  # ainda ha escolas mal georreferenciadas!
  # identificar essas escolas e separa-las
  # convert para sf
  escolas_google_mal_geo <- estabs_problema_geocoded_dt %>%
    filter(!is.na(lat)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_buffer(0.0008)) %>%
    # escolas que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(code_state)) %>%
    select(co_entidade, SearchedAddress)
  
  
  # mapview(escolas_google_mal_geo)
  
  # identify these addresses as outside the city
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[co_entidade %in% escolas_google_mal_geo$co_entidade,
                                                             PrecisionDepth := 'address_outside_city']
  
  # identify year
  estabs_problema_geocoded_dt[, year_geocode := ano]
  
  # table(estabs_problema_geocoded_dt$PrecisionDepth, useNA = 'always')
  
  
  # bring to the original dataset
  # # bring CEP's fixed to the original fixed dataset problems 1 and 2
  # rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
  #                           c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
  #                             list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]
  
  # identify columns 
  escolas_geo_togeo[, co_entidade := as.character(co_entidade)]
  escolas_geo_togeo[, MatchedAddress := endereco]
  escolas_geo_togeo[, SearchedAddress := endereco]
  escolas_geo_togeo[, PrecisionDepth := "inep"]
  escolas_geo_togeo[, geocode_engine := "inep"]
  
  # Replace problematic INEP coordinates and classifications with results from Google
  escolas_geo_togeo[estabs_problema_geocoded_dt, on = "co_entidade",
                    c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine", "year_geocode") := 
                      list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine, i.year_geocode)]
  
  # table(escolas_geo$PrecisionDepth, useNA = 'always')
  # table(escolas_geo$geocode_engine, useNA = 'always')
  
  
  # 6) Trazer coordenadas corrigidas para as novas escolas --------------------------
  escolas_geo[escolas_geo_togeo, on = "co_entidade",
              c('lon', 'lat', 
                'MatchedAddress', 'SearchedAddress', 'PrecisionDepth', 'geocode_engine', 'year_geocode') := 
                list(i.lon, i.lat,
                     i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.geocode_engine, i.year_geocode)]
  
  
  
  # 7) Trazer coordenadas do ano anterior para as novas escolas --------------------------
  if (ano != "2017") {
    
    escolas_geo[escolas_previous_geo, on = "co_entidade",
                c('lon', 'lat',
                  'MatchedAddress', 'SearchedAddress', 'PrecisionDepth', 'geocode_engine', 'year_geocode') := 
                  list(i.lon, i.lat,
                       i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.geocode_engine, i.year_geocode)]
    
  }
  
  
  # table(escolas_geo$PrecisionDepth, useNA = 'always')
  # table(escolas_geo$year_geocode, useNA = 'always')
  
  # 8) Salvar ---------------------
  write_rds(escolas_geo,
            sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_geocoded.rds", ano, ano), compress = 'gz')
  
}





