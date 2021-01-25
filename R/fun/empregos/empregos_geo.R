# carregar bibliotecas
# source('./R/fun/setup.R')


#' Essa funcao faz um limpezada da base raw dos estabelecimentos
#' Etapas:
#' 1) Abrir os dados e selecionar as colunas
#' 2) Filtrar somente empresas com vinculos ativos
#' 3) Trazer o nome dos municipios
#' 4) salvar

rais_clean_estabs_raw <- function(ano) {
  
  # 1) Dados dos estabelecimentos
  # 1.1) Primeiro, ler somente um subset dos dados da RAIS para selecionar colunas
  # de interesse
  colnames <- fread(sprintf("../../data-raw/rais/%s/rais_estabs_raw_%s.csv", ano, ano),
                    nrows = 100,
                    # select = c("id_estab", "qt_vinc_ativos", 'nat_jur2018',  "logradouro", "bairro", "codemun", "uf", "cep"),
                    colClasses = "character") %>% colnames()
  
  # select columns
  columns <- c("id_estab", "qt_vinc_ativos", colnames[colnames %like% "nat_jur"], "logradouro", "bairro", "codemun", "uf", "cep")
  
  # 1.2) Abrir dados e selecionar as colunas
  rais_estabs_raw <- fread(sprintf("../../data-raw/rais/%s/rais_estabs_raw_%s.csv", ano, ano),
                           select = columns,
                           colClasses = "character")
  # 1.3) Renomear columns
  colnames(rais_estabs_raw) <- c("id_estab", "qt_vinc_ativos", "nat_jur", "logradouro", "bairro", "codemun", "uf", "cep")
  
  
  # 2) Filtrar somente empresas com vinculos ativos
  rais_estabs_raw_0 <- rais_estabs_raw[as.numeric(qt_vinc_ativos) > 0]
  
  message("Total number of active estabs: ", unique(rais_estabs_raw_0$id_estab) %>% length())
  
  # todo os estabs para 14 characetrs
  rais_estabs_raw_0[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # 3) Trazer o nome do municipio
  muni_lookup <- geobr::lookup_muni(code_muni = "all")
  muni_lookup <- muni_lookup %>%
    select(codemun = code_muni, name_muni, abrev_state) %>%
    mutate(codemun = substr(codemun, 1, 6))
  
  rais_estabs_raw_0 <- rais_estabs_raw_0 %>%
    left_join(muni_lookup, by = "codemun") 
  
  # 4) Selecionar colunas e salvar
  rais_estabs_raw_0 %>%
    # fix uf and codemun
    select(id_estab, qt_vinc_ativos, logradouro, bairro, codemun, name_muni, uf = abrev_state, cep) %>% 
    # save it
    fwrite(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano))
  
}



#' A funcao `rais_export_data_to_galileo` exporta os dados a serem georreferenciados
#' pelo galileo
#' Etapas:
#' 1) Abrir rais filtrada
#' 2) Se o ano nao for 2017, selecionar somente os estabelecimentos que sao
#' novos ou que mudaram de endereço e um ano para o outro
#' 3) Salvar




rais_export_data_to_galileo <- function(ano) {
  
  # 1) Abrir rais filtrada
  rais_filter <- fread(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano),
                       colClasses = "character",
                       select = c("id_estab", "logradouro", "bairro", "codemun", "name_muni", "uf", "cep"),
                       encoding = "UTF-8")
  
  
  # 2) Se o ano nao for 2017, selecionar somente os estabelecimentos que sao
  # novos ou que mudaram de endereço e um ano para o outro
  if (ano != 2017) {
    
    # 2.1) Abrir RAIS final do ano anterior
    rais_etapa8 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido_geocoded_censoEscolar.rds", ano-1, ano-1))
    # Selecionar variaveis
    rais_etapa8 <- rais_etapa8 %>% select(id_estab, logradouro, cep, lon, lat) %>% setDT()
    # Pad to 14 chars
    rais_etapa8[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
    
    # 2.2) Selecionar somente os estabs que nao foram georef antes
    rais_geocode_new <- rais_filter[id_estab %nin% rais_etapa8$id_estab]
    # identify type
    rais_geocode_new[, type_input_galileo := paste0("new_estab_", ano)]
    
    message("Total of new estabs compared to previous year: ", nrow(rais_geocode_new))
    
    # 2.3) Selecionar somente os estabs que mudaram o cep em relacao ao ano anterior
    rais_geocode_cep <- left_join(rais_filter, select(rais_etapa8, id_estab, logradouro, cep),
                                  by = "id_estab",
                                  suffix = c("_anterior", "_atual")) %>%
      mutate(cep_anterior = str_pad(cep_anterior, side = "left", width = 8, pad = 0)) %>%
      mutate(cep_atual = str_pad(cep_atual, side = "left", width = 8, pad = 0)) %>%
      mutate(cep_anterior = substr(cep_anterior, 1, 6)) %>%
      mutate(cep_atual = substr(cep_atual, 1, 6)) %>%
      mutate(cep_igual = ifelse(cep_anterior == cep_atual, "sim", "nao")) %>%
      filter(cep_igual == "nao") %>%
      rename(cep = cep_atual, logradouro = logradouro_atual) %>%
      select(-cep_anterior, -logradouro_anterior, -cep_igual) %>%
      # identify type
      mutate(type_input_galileo = paste0("cep_changed_", ano))
    
    
    message("Total of estabs that changed CEP: ", nrow(rais_geocode_cep))
    
    # 2.4) Juntar os novos estabs a serem georef
    rais_filter_geocode <- rbind(rais_geocode_new, rais_geocode_cep)
    
    message("Total of to be geocoded at galileo: ", nrow(rais_filter_geocode))
    
    # 2.5) Selecionar colunas e salvar input para galileo
    rais_filter_geocode %>%
      write_delim(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_input_galileo.csv", ano, ano), delim = ";")
    
    message("Input to galileo saved at: ", sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_input_galileo.csv", ano, ano))
    
  } else if (ano == 2017) {
    
    rais_filter_geocode <- rais_filter %>%
      mutate(type_input_galileo = paste0("rais_", "2017"))
    
    message("Total of to be geocoded at galileo: ", nrow(rais_filter_geocode))
    
    # 2.5) Selecionar colunas e salvar input para galileo
    rais_filter_geocode %>%
      write_delim(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_input_galileo.csv", ano, ano), delim = ";")
    
    message("Input to galileo saved at: ", sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_input_galileo.csv", ano, ano))
    
  }
  
  
  
}




#' A funcao `rais_gmaps_geocode` roda o geocode para os estabelecimentos problematicos
#' e atualiza a base que foi georef pelo galileo, por fim a atualiazando a base final 
#' do ano
#' Etapas:
#' 1) Abrir output do galileo
#' 2) Corrigir coordenadas
#' 3) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao
#' 1 e 2 estrelas
#' 4) Rodar Google API  somente para estabs 3 estrelas que estao em rodovias
#' 5) Juntar datasets dos problemas 1 e 2
#' 6) Rodar gmaps com o CEP somente para empresa que ficaram fora das respectivas
#' cidades
#' 7) Fazer a identificacao de quando o estab ficou fora da cidade
#' 8) Trazer os estabs que foram georef so com o CEP para o dataset original
#' dos problemas 1 e 2
#' 9) Check if there are missing address
#' 10) Substituir as coordenadas problematicas que estao na base do galileo
#' pelas novas coordenadas que foram corrigidas pelo gmaps
#' 11) Trazer as novas coordenadas para a base completa do ano e completar com
#' os estabs que foram georef no ano anterior



rais_gmaps_geocode <- function(ano, run_gmaps = FALSE) {
  
  
  
  # 1) Abrir output do galileo ------------
  rais_galileo_output <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_output_galileo.csv", ano, ano),
                               colClasses = 'character')
  
  
  # table(rais_galileo_output$PrecisionDepth, useNA = 'always')
  # table(rais_galileo_output$type_input_galileo, useNA = 'always')
  
  # 2) Corrigir coordenadas --------------
  rais_galileo_output <- rais_galileo_output %>% rename(lat=Latitude , lon=Longitude)
  
  # 2.1) Substituir , por  .)
  rais_galileo_output[, ':='(lon = str_replace(lon, ",", "."),
                             lat = str_replace(lat, ",", "."))]
  # 2.2) Trazer para numerico
  rais_galileo_output[, ':='(lon = as.numeric(lon),
                             lat = as.numeric(lat))]
  
  
  
  # 3) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao ------
  # 1 e 2 estrelas
  
  # 3.1) Selecionar estabs com baixa precisao do Galileo 1 e 2 estrelas
  estabs_problema <- rais_galileo_output[ PrecisionDepth %in% c('1 Estrela', '2 Estrelas'), ]
  
  
  message("Total of estabs to go to gmaps problem 1: ", unique(estabs_problema$id_estab) %>% length())
  
  
  # 3.2) Listar esses enderecos com problema
  enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
  
  # 3.3) Registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1[3])
  
  # 3.4) Rodar o geocode do gmaps
  
  if (run_gmaps) {
    
    message("Running gmaps, this may take a while")
    
    coordenadas_google1_1 <- lapply(X=enderecos[1:5000], ggmap::geocode, output = "all")
    coordenadas_google1_2 <- lapply(X=enderecos[5001:10000], ggmap::geocode, output = "all")
    coordenadas_google1_3 <- lapply(X=enderecos[10001:length(enderecos)], ggmap::geocode, output = "all")
    
    # join them all together
    coordenadas_google1 <- c(coordenadas_google1_1, coordenadas_google1_2, coordenadas_google1_3)
    
    # identify list names as id_estab
    names(coordenadas_google1) <- estabs_problema$id_estab
    
    # save
    write_rds(coordenadas_google1, 
              sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google1.rds", ano, ano))
    
  } else {
    
    coordenadas_google1 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google1.rds", ano, ano))
  }
  
  
  # function to create data.frame from gmaps output
  create_dt <- function(x) {
    
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
  
  # 3.5) Rodar funcao que transforma todos os estabs georef em data.table
  estabs_problema_geocoded <- lapply(coordenadas_google1, create_dt)
  
  # 3.6) Rbind as data.table
  estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "id_estab",
                                           use.names = TRUE)
  
  # unique(estabs_problema_geocoded_dt$id_estab) %>% length()
  
  
  # 3.7) MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[id_estab %in% estabs_problema$id_estab]
  
  # 3.8) Identificar a informacao de searchedaddress
  searchedaddress <- filter(estabs_problema, id_estab %in% names(coordenadas_google1)) %>%
    mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
    distinct(id_estab, .keep_all = TRUE)
  estabs_problema_geocoded_dt <- left_join(estabs_problema_geocoded_dt, searchedaddress, by = "id_estab") %>% setDT()
  
  # 3.9) Identificar o tipo de problema
  estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
  
  # 3.10) Identificar qualidade quando o endereco nao foi encontrado
  estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  # 4) Rodar gmaps somente para estabs 3 estrelas que estao em rodovias --------
  # Estabelecimentos com 3 estrelas que estao localizados em rodovias tendem a ter uma qualidade ruim
  # de georreferenciamento!
  # Essa etapa busca identificar todos os 3 estrelas que sejam em rodovias, e separa-os para aplicar
  # o geocoding do google!
  
  
  # 4.1) Abrir base de rodovias 
  rodovias <- st_read("../../data-raw/rodovias/2014/2014/rodovia_2014.shp") %>%
    # Excluir geometria (desnecessario)
    st_set_geometry(NULL) %>%
    mutate(nome = as.character(DescSeg)) %>%
    # Extrair o nome da rodovia na forma BR-116
    mutate(nome = str_extract(nome, "[[:upper:]]{2}-\\d{3}")) %>%
    # Pegar rodovias unicas
    distinct(nome) %>%
    # Tirar NAs
    filter(!is.na(nome)) %>%
    # Separar por uf da rodovia e numero da rodovia
    separate(nome, c("uf", "numero"), sep = "-", remove = FALSE) %>%
    # Tipo 1 possivel: BR-116;   # Tipo 2 possivel: BR 116
    mutate(tipo1 = nome, tipo2 = paste0(uf, " ", numero))
  
  
  # 4.2) Pegar so tres estrelas da rais
  estabs_problema_3estrelas <- rais_galileo_output[PrecisionDepth == "3 Estrelas"]
  
  # 4.3) Extrair as rodovias de cada tipo possivel, e depois juntar
  # criar coluna com rodovias
  estabs_problema_3estrelas[, rodovia := str_extract(logradouro, "( |^|,)[[:upper:]]{2}(-| )\\d{3}( |$|,)")]
  # tirar virgulas
  estabs_problema_3estrelas[, rodovia := str_replace(rodovia, ",", "")]
  # tirar espacos em branco
  estabs_problema_3estrelas[, rodovia := trimws(rodovia, "both")]
  
  # extrair somente os que sao rodovia
  rais_rodovias_tipo1 <- estabs_problema_3estrelas[rodovia %in% rodovias$tipo1]
  rais_rodovias_tipo2 <- estabs_problema_3estrelas[rodovia %in% rodovias$tipo2]
  rais_rodovias_tipo3 <- estabs_problema_3estrelas[logradouro %like% "(RODOVIA )|(ROD )|(ROD. )(RODOVIARIO )"]
  # deletar os duplicados do tipo 1 e 2 no tipo 3
  rais_rodovias_tipo3 <- rais_rodovias_tipo3[id_estab %nin% c(rais_rodovias_tipo1$id_estab, rais_rodovias_tipo2$id_estab)]
  
  # 4.4) Juntar todas as rodovias
  rais_rodovias <- rbind(rais_rodovias_tipo1, rais_rodovias_tipo2, rais_rodovias_tipo3)
  
  message("Total estabs to go to gmaps problem 2 (rodovias): ", nrow(rais_rodovias))
  
  # 4.5) Salvar os estabs de rodovia problematicos
  fwrite(rais_rodovias, sprintf("../../data/acesso_oport/rais/%s/geocode/geocode_google2_rodovias.csv", ano))
  rais_rodovias <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/geocode_google2_rodovias.csv", ano),
                         colClasses = "character")
  
  # 4.5) Listas enderecosde rodovia problematicos
  enderecos_rodovias <- rais_rodovias %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
  
  # 4.6) Rodar o geocode do gmaps para rodovias
  
  if(run_gmaps) {
    
    # geocode
    coordenadas_google_rodovias <- lapply(X=enderecos_rodovias, ggmap::geocode, output = "all")
    
    # identify list names as id_estab
    names(coordenadas_google_rodovias) <- rais_rodovias$id_estab 
    
    # save
    write_rds(coordenadas_google_rodovias, sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google2.rds", ano, ano))
    
  } else {
    
    coordenadas_google_rodovias <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google2.rds", ano, ano))
    
  }
  
  # 4.7) Rodar funcao que transforma todos os estabs georef em data.table 
  rodovias_problema_geocoded <- lapply(coordenadas_google_rodovias, create_dt)
  
  
  # 4.8) Rbind as data.table
  rodovias_problema_geocoded_dt <- rbindlist(rodovias_problema_geocoded, idcol = "id_estab",
                                             use.names = TRUE)
  
  
  # 4.9) MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  rodovias_problema_geocoded_dt <- rodovias_problema_geocoded_dt[id_estab %in% rais_rodovias$id_estab]
  
  # 4.10) Identificar a informacao de searchedaddress
  searchedaddress_rodovias <- filter(rais_rodovias, id_estab %in% names(coordenadas_google_rodovias)) %>%
    mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
    distinct(id_estab, .keep_all = TRUE)
  rodovias_problema_geocoded_dt <- left_join(rodovias_problema_geocoded_dt, searchedaddress_rodovias, by = "id_estab") %>% setDT()
  
  # 4.11) Identificar tipo de problema
  rodovias_problema_geocoded_dt[, geocode_engine := 'gmaps_prob2']
  
  # 4.12) Identificar qualidade quando o endereco nao foi encontrado
  rodovias_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  # 5) Juntar datasets dos problemas 1 e 2 ---------------------
  rais_problema1e2_geocoded <- rbind(estabs_problema_geocoded_dt, rodovias_problema_geocoded_dt,
                                     fill = TRUE)
  
  # 5.1) Salvar dataset dos problemas 1 e 2
  write_rds(rais_problema1e2_geocoded, sprintf("../../data/acesso_oport/rais/%s/geocode/rais_problema1e2_geocoded.rds", ano))
  rais_problema1e2_geocoded <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_problema1e2_geocoded.rds", ano))
  
  
  
  # 6) Rodar gmaps com o CEP somente para empresa que ficaram fora das respectivas ------
  # cidades
  # identifica-las atraves do shape dos municipios e refazer geocode do google considerando so CEP
  
  # 6.1) Carregar shape dos munis
  shps <- purrr::map_dfr(munis_df$code_muni, geobr::read_municipality) %>% as_tibble() %>% st_sf()
  
  
  # 6.2) Identificar os estabs que estao fora dos municipios
  rais_google_mal_geo <- rais_problema1e2_geocoded %>%
    filter(!is.na(lat)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_set_crs(4326)) %>%
    # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(name_muni)) %>%
    # Selecionar so o estab
    select(id_estab) %>%
    # Trazer demais informacoes
    left_join(select(rais_galileo_output, id_estab, name_muni, uf, cep), by = "id_estab") %>%
    filter(cep != ".")
  
  
  # 6.3) LIstar CEPs para geocode do gmaps
  somente_ceps <- paste0("CEP ", rais_google_mal_geo$cep, " - ", rais_google_mal_geo$name_muni, ", ", rais_google_mal_geo$uf)
  
  
  message("Total of estabs to go to gmaps prob CEP: ", length(somente_ceps))
  
  # 6.4) Rodar gmaps para CEPS
  if (run_gmaps) {
    
    # consulta google api
    coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode, output = 'all')
    
    # identify list names as id_estab
    names(coordenadas_google_cep) <- rais_google_mal_geo$id_estab
    
    # save it
    write_rds(coordenadas_google_cep, sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google3.rds", ano, ano))
    
  } else {
    
    coordenadas_google_cep <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google3.rds", ano, ano))
    
  }
  
  # 6.5) Rodar funcao que transforma todos os estabs georef em data.table 
  cep_problema_geocoded <- lapply(coordenadas_google_cep, create_dt)
  
  # 6.6) Rbind as data.table
  cep_problema_geocoded_dt <- rbindlist(cep_problema_geocoded, idcol = "id_estab",
                                        fill = TRUE, use.names = TRUE)
  
  
  # 6.7) MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  cep_problema_geocoded_dt <- cep_problema_geocoded_dt[id_estab %in% rais_google_mal_geo$id_estab]
  
  # 6.8) Identificar a informacao de searchedaddress
  searchedaddress_cep <- filter(rais_google_mal_geo, id_estab %in% names(coordenadas_google_cep)) %>%
    mutate(SearchedAddress = paste0("CEP ", cep, " - ", name_muni, ", ", uf)) %>% 
    select(id_estab, SearchedAddress) %>%
    distinct(id_estab, .keep_all = TRUE)
  cep_problema_geocoded_dt <- left_join(cep_problema_geocoded_dt, searchedaddress_cep, by = "id_estab") %>% setDT()
  
  # 6.9) Identificar o tipo de problema
  cep_problema_geocoded_dt[, geocode_engine := 'gmaps_prob3']
  
  # 6.10) Identificar qualidade quando o endereco nao foi encontrado
  cep_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  # 7) Fazer a identificacao de quando o estab ficou fora da cidade -------
  
  # 7.1) check again with therey wihtin cities
  cep_problema_again <- cep_problema_geocoded_dt %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_set_crs(4326)) %>%
    # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(name_muni)) %>%
    sfc_as_cols() %>%
    select(id_estab)
  
  # 7.2) Fix these in the final dataset of ceps
  cep_problema_geocoded_dt_fixed <- setDT(cep_problema_geocoded_dt)[id_estab %in% cep_problema_again$id_estab, 
                                                                    ':='(PrecisionDepth = "address_outside_city",
                                                                         lon = NA,
                                                                         lat = NA)]
  
  
  # 8) Trazer os estabs que foram georef so com o CEP para o dataset original -------
  # dos problemas 1 e 2
  rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
                            c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                              list(i.MatchedAddress, i.SearchedAddress,  i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]
  
  # table(rais_problema1e2_geocoded$PrecisionDepth, useNA = 'always')
  # table(rais_problema1e2_geocoded$geocode_engine, useNA = 'always')
  # filter(rais_problema1e2_geocoded, is.na(MatchedAddress)) %>% View()
  
  
  # 9) Como aconteceram varias mudancas nas etapas de processamento da RAIS e o -----
  # geocode completo do gmaps so pode ser rodado uma vez, muitas vezes alguns
  # estabs acabam escapando e nao sao georef
  
  # 9.1) Check if there are missing address
  if (nrow(rais_problema1e2_geocoded[is.na(PrecisionDepth)]) > 0) {
    
    # 9.2) Selecionar os missings
    estabs_missing <- rais_problema1e2_geocoded[is.na(PrecisionDepth)]
    
    # 9.3) Prepara os missing para geocode gmaps
    enderecos_estabs_missing <- rais_galileo_output %>%
      filter(id_estab %in% estabs_missing$id_estab) %>%
      mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
    
    # 9.3) Geocode gmaps
    coordenadas_google_missing <- lapply(X=enderecos_estabs_missing, ggmap::geocode, output = "all")
    
    # 9.4) identify list names as id_estab
    names(coordenadas_google_missing) <- estabs_missing$id_estab 
    
    # 9.5) create dt 
    estabs_missing_geocoded <- lapply(coordenadas_google_missing, create_dt)
    
    # 9.6) rbind as data.table
    estabs_missing_geocoded_dt <- rbindlist(estabs_missing_geocoded, idcol = "id_estab",
                                            use.names = TRUE)
    
    # 9.7) identify searchedaddress
    estabs_missing_geocoded_dt$SearchedAddress <- enderecos_estabs_missing
    
    # 9.8) Identify quality when no address was found
    estabs_missing_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
    estabs_missing_geocoded_dt[is.na(PrecisionDepth), ':='(lon = NA,
                                                           lat = NA,
                                                           PrecisionDepth = "address_not_found")]
    
    # 9.10) Join to the dataset
    rais_problema1e2_geocoded[estabs_missing_geocoded_dt, on = "id_estab",
                              c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat") := 
                                list(i.MatchedAddress, i.SearchedAddress,  i.PrecisionDepth, i.lon, i.lat)]
    
    
  }
  
  
  
  
  # 10) Substituir as coordenadas problematicas que estao na base do galileo ----
  # pelas novas coordenadas que foram corrigidas pelo gmaps
  message("Joining gmaps dataset to the oringinal galileo dataset ... ")
  rais_galileo_output_fixed <- data.table::copy(rais_galileo_output)
  
  # 10.1) Identificar geocode engine (essa info vai acabar sendo sunstituida quando
  # necessario)
  rais_galileo_output_fixed[, geocode_engine := "galileo"]
  # 10.2) Fazer a substituicao
  rais_galileo_output_fixed[rais_problema1e2_geocoded, on = "id_estab",
                            c("MatchedAddress", "PrecisionDepth", "lon", "lat", "SearchedAddress", "geocode_engine") :=
                              list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.SearchedAddress, i.geocode_engine)]
  
  
  
  # table(rais_galileo_output_fixed$PrecisionDepth, useNA = 'always')
  # table(rais_galileo_output_fixed$geocode_engine, useNA = 'always')
  # table(rais_galileo_output_fixed$type_input_galileo, useNA = 'always')
  
  
  # 10.3) Garantir que os enderecos sejam unicos
  rais_galileo_output_fixed <- distinct(rais_galileo_output_fixed, id_estab, .keep_all = TRUE)
  
  # 10.4) Salvar
  write_rds(rais_galileo_output_fixed, 
            sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode_final.rds", ano, ano))
  
  
  # 11) Trazer as novas coordenadas para a base completa do ano e completar com ----
  # os estabs que foram georef no ano anterior
  # O que foi geoferenciado no total (galileo + gmaps) precisa
  # ser inserido na base bruta do ano, ja que o georef foi feito
  # somente para os novos enderecos e para os que mudaram de cep
  # Para o ano de 2017 essa operacao nao precisa ser feita
  
  # 11.1) Abrir os enderecos georef galileo + gmaps
  rais_galileo_output_fixed <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode_final.rds", ano, ano))
  
  # Se ano o ano for 2017, essa base ja eh a base final pois todos os enderecos
  # foram georef
  if (ano == 2017) {
    
    rais_filter_geocode_end <- rais_galileo_output_fixed
    
  } else {
    
    # 11.2) Abrir a rais original (sem ser georef) do ano
    rais_filter <- fread(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano),
                         colClasses = "character",
                         encoding = "UTF-8")
    # pad everyone to 14 characters
    setDT(rais_galileo_output_fixed)[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
    rais_filter[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
    
    # table(nchar(rais_galileo_output_fixed$id_estab))
    # table(nchar(rais_2018_filter$id_estab))
    
    # 11.3) Grantir que os enderecos sao unicos
    rais_filter <- rais_filter %>% distinct(id_estab, .keep_all = TRUE)
    rais_galileo_output_fixed <- rais_galileo_output_fixed %>% distinct(id_estab, .keep_all = TRUE)
    
    # 11.4) Trazer entao os estabs geocoded (galileo + gmaps) para a base original
    
    message("Joining the geocoded dataset to the completed dataset ... ")
    
    rais_filter_geocode_end <- merge(
      setDT(rais_filter),
      setDT(rais_galileo_output_fixed)[, .(id_estab, PrecisionDepth, SearchedAddress, MatchedAddress,
                                           lon, lat, type_input_galileo, geocode_engine)],
      by = "id_estab",
      all.x = TRUE,
      sort = FALSE
    )
    
    # 11.5) Trazer entao os estabs geocoded do ano anterir
    
    # 11.5.1) Get previous geocode
    rais_etapa8 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido_geocoded_censoEscolar.rds", ano-1, ano-1))
    rais_etapa8 <- rais_etapa8 %>% select(id_estab, lon, lat, 
                                          SearchedAddress, MatchedAddress,
                                          PrecisionDepth, type_input_galileo, geocode_engine)
    # 11.5.2) Filter only estabs that we will keep from the previous yer
    rais_etapa8 <- setDT(rais_etapa8)[id_estab %nin% rais_galileo_output_fixed$id_estab]
    # 11.5.3) Delete the ones that came from inep (we will add them later)
    rais_etapa8 <- rais_etapa8[type_input_galileo %nin% "inep"]
    
    # table(rais_etapa8$type_input_galileo, useNA = 'always')
    # table(rais_etapa8$PrecisionDepth)
    # table(rais_etapa8$geocode_engine)
    # table(nchar(rais_etapa8$id_estab))
    
    # 11.5.4) Make sure estabs are unique
    rais_etapa8 <- rais_etapa8 %>% distinct(id_estab, .keep_all = TRUE)
    
    message("Joining the completed year geocoded dataset to the previous geocoded year ... ")
    
    # 11.5.5) Merge with the previous results
    rais_filter_geocode_end[rais_etapa8, on = "id_estab",
                            c('PrecisionDepth', 'SearchedAddress', 'MatchedAddress','lon', 'lat', 'type_input_galileo', 'geocode_engine') :=
                              list(i.PrecisionDepth, i.SearchedAddress, i.MatchedAddress, i.lon, i.lat, i.type_input_galileo, i.geocode_engine)]
    
    # table(rais_filter_geocode_end$PrecisionDepth, useNA = 'always')
    # table(rais_filter_geocode_end$geocode_engine, useNA = 'always')
    # table(rais_filter_geocode_end$type_input_galileo, useNA = 'always')
    # colnames(rais_2018_filter_geocode_end)
    # summary(as.numeric(rais_2018_filter_geocode_end$qt_vinc_ativos))
    
    
  }
  
  # 12) Salvar! ---------
  write_rds(rais_filter_geocode_end, sprintf("../../data/acesso_oport/rais/%s/rais_%s_estabs_geocode_final.rds", ano, ano))
  
}





