# carregar bibliotecas
# source('./R/fun/setup.R')


# CLEAN UP RAIS ESTAB --------------------------------------------------------------

rais_clean_estabs_raw <- function(ano) {
  
  # get colnames
  colnames <- fread(sprintf("../../data-raw/rais/%s/rais_estabs_raw_%s.csv", ano, ano),
                    nrows = 100,
                    # select = c("id_estab", "qt_vinc_ativos", 'nat_jur2018',  "logradouro", "bairro", "codemun", "uf", "cep"),
                    colClasses = "character") %>% colnames()
  
  # select columns
  columns <- c("id_estab", "qt_vinc_ativos", colnames[colnames %like% "nat_jur"], "logradouro", "bairro", "codemun", "uf", "cep")
  
  # open rais and select columns
  rais_estabs_raw <- fread(sprintf("../../data-raw/rais/%s/rais_estabs_raw_%s.csv", ano, ano),
                           select = columns,
                           colClasses = "character")
  # rename columns
  colnames(rais_estabs_raw) <- c("id_estab", "qt_vinc_ativos", "nat_jur", "logradouro", "bairro", "codemun", "uf", "cep")
  
  
  # somente empresas com vinc ativo
  rais_estabs_raw_0 <- rais_estabs_raw[as.numeric(qt_vinc_ativos) > 0]
  
  message("Total number of active estabs: ", unique(rais_estabs_raw_0$id_estab) %>% length())
  
  muni_lookup <- geobr::lookup_muni(code_muni = "all")
  muni_lookup <- muni_lookup %>%
    select(codemun = code_muni, name_muni, abrev_state) %>%
    mutate(codemun = substr(codemun, 1, 6))
  
  rais_estabs_raw_0[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  
  # select and save
  rais_estabs_raw_0 %>%
    # fix uf and codemun
    left_join(muni_lookup, by = "codemun") %>%
    select(id_estab, qt_vinc_ativos, logradouro, bairro, codemun, name_muni, uf = abrev_state, cep) %>% 
    # save it in rds b
    fwrite(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano))
  
}






rais_export_data_to_galileo <- function(ano) {
  
  # get previous geocode
  if (ano != 2017) {
    
    # get new rais filter
    rais_filter <- fread(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano),
                         colClasses = "character",
                         select = c("id_estab", "logradouro", "bairro", "codemun", "name_muni", "uf", "cep"),
                         encoding = "UTF-8")
    
    
    # table(nchar(rais_filter$id_estab))
    
    year_previous <- ano -  1
    rais_etapa8 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido_geocoded_censoEscolar.rds", ano, ano))
    rais_etapa8 <- rais_etapa8 %>% select(id_estab, logradouro, cep, lon, lat) %>% setDT()
    rais_etapa8[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
    
    # filter only estabs that were not geocoded before
    rais_geocode_new <- rais_filter[id_estab %nin% rais_etapa8$id_estab]
    # identify type
    rais_geocode_new[, type_input_galileo := paste0("new_estab_", ano)]
    
    message("Total of new estabs compared to previous year: ", nrow(rais_geocode_new))
    
    # get eestabbss that changed cep from ano and preious year
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
    
    rais_filter_geocode <- rbind(rais_geocode_new, rais_geocode_cep)
    
    message("Total of to be geocoded at galileo: ", nrow(rais_filter_geocode))
    
    # select columns and create input to galileo
    rais_filter_geocode %>%
      write_delim(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_input_galileo.csv", ano, ano), delim = ";")
    
    message("Input to galileo saved at: ", sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_input_galileo.csv", ano, ano))
  }
  
  
  
}



# RUN GMAPS GEOCODE ---------------------------------------------------------------------------

rais_gmaps_geocode <- function(ano, run_gmaps = FALSE) {
  
  if (ano == 2017) {
    
    # leitura da RAIS geocodificada ( output do galileo )
    rais_galileo_output <- fread("../../data-raw/rais/2017/rais_2017_georef.csv"
                      , select = c("id_estab", "codemun", "qt_vinc_ativos",
                                   "latitude", 'longitude', 'precisiondepth', 'matchedaddress',
                                   'logradouro', 'cep', 'uf', 'BA_Nome_do_municipio')
                      , colClasses='character'
                      # , nrows = 10
    )
    
    # rename vars
    colnames(rais_galileo_output) <- c("id_estab", "codemun", "qt_vinc_ativos",
                                       "Latitude", 'Longitude', 'PrecisionDepth', 'MatchedAddress',
                                       'logradouro', 'cep', 'uf', 'name_muni')
    
    # somente empresas com vinc ativo
    rais_galileo_output <- rais_galileo_output[as.numeric(qt_vinc_ativos) > 0]
    
    rais_galileo_output <- rais_galileo_output[codemun %in% substr(munis_df$code_muni, 1, 6) ]
    
  } else {
    
    
    # open galileo output
    rais_galileo_output <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_output_galileo.csv", ano, ano),
                                 colClasses = 'character')
    
  }
  
  # table(rais_galileo_output$PrecisionDepth, useNA = 'always')
  
  # recode lat lon column names
  rais_galileo_output <- rais_galileo_output %>% rename(lat=Latitude , lon=Longitude)
  
  # corrigir coordenadas ( substitui , por  .)
  rais_galileo_output[, ':='(lon = str_replace(lon, ",", "."),
                             lat = str_replace(lat, ",", "."))]
  
  rais_galileo_output[, ':='(lon = as.numeric(lon),
                             lat = as.numeric(lat))]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##### 5) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao  -------------------
  # Os estabelecimentos que o Galileo encontrou com 1, 2 ou 3 estrelas serao jogados para o google api
  
  ######################### GOOGLE 1 , somente 1 e 2 estrelas, todos, endereco completo 
  
  # Estabs com baixa precisao do Galileo 1 e 2 estrelas
  estabs_problema <- rais_galileo_output[ PrecisionDepth %in% c('1 Estrela', '2 Estrelas'), ]
  
  
  message("Total of estabs to go to gmaps problem 1: ", unique(estabs_problema$id_estab) %>% length())
  
  
  # lista de enderecos com problema
  enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
  
  # registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1[2])
  
  # geocode
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
  
  # run function
  estabs_problema_geocoded <- lapply(coordenadas_google1, create_dt)
  
  # rbind as data.table
  estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "id_estab",
                                           use.names = TRUE)
  
  # unique(estabs_problema_geocoded_dt$id_estab) %>% length()
  
  
  # MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[id_estab %in% estabs_problema$id_estab]
  
  # identify searchedaddress
  searchedaddress <- filter(estabs_problema, id_estab %in% names(coordenadas_google1)) %>%
    mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
    distinct(id_estab, .keep_all = TRUE)
  estabs_problema_geocoded_dt <- left_join(estabs_problema_geocoded_dt, searchedaddress, by = "id_estab") %>% setDT()
  # identify problem
  estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
  # identify quality
  estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  ######################### GOOGLE 2 , somente 3 estrelas, rodovias, endereco completo 
  # Estabelecimentos com 3 estrelas que estao localizados em rodovias tendem a ter uma qualidade ruim
  # de georreferenciamento!
  # Essa etapa busca identificar todos os 3 estrelas que sejam em rodovias, e separa-os para aplicar
  # o geocoding do google!
  
  
  # abrir base de rodovias rodovias
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
  
  
  # pegar so tres estrelas da rais
  estabs_problema_3estrelas <- rais_galileo_output[PrecisionDepth == "3 Estrelas"]
  
  # extrair as rodovias de cada tipo possivel, e depois juntar
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
  # juntar todas as rodovias
  rais_rodovias <- rbind(rais_rodovias_tipo1, rais_rodovias_tipo2, rais_rodovias_tipo3)
  
  message("Total estabs to go to gmaps problem 2 (rodovias): ", nrow(rais_rodovias))
  
  fwrite(rais_rodovias, sprintf("../../data/acesso_oport/rais/%s/geocode/geocode_google2_rodovias.csv", ano))
  rais_rodovias <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/geocode_google2_rodovias.csv", ano),
                         colClasses = "character")
  
  # lista de enderecos com problema
  enderecos_rodovias <- rais_rodovias %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
  
  # registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1[2])
  
  
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
  
  # create dt 
  rodovias_problema_geocoded <- lapply(coordenadas_google_rodovias, create_dt)
  
  
  # rbind as data.table
  rodovias_problema_geocoded_dt <- rbindlist(rodovias_problema_geocoded, idcol = "id_estab",
                                             use.names = TRUE)
  
  
  # MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  rodovias_problema_geocoded_dt <- rodovias_problema_geocoded_dt[id_estab %in% rais_rodovias$id_estab]
  
  # identify searchedaddress
  searchedaddress_rodovias <- filter(rais_rodovias, id_estab %in% names(coordenadas_google_rodovias)) %>%
    mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
    distinct(id_estab, .keep_all = TRUE)
  rodovias_problema_geocoded_dt <- left_join(rodovias_problema_geocoded_dt, searchedaddress_rodovias, by = "id_estab") %>% setDT()
  
  
  # identify problem
  rodovias_problema_geocoded_dt[, geocode_engine := 'gmaps_prob2']
  # identify quality
  rodovias_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  # join datasets from problemas 1 and 2 ----------------------------------------------
  rais_problema1e2_geocoded <- rbind(estabs_problema_geocoded_dt, rodovias_problema_geocoded_dt)
  
  write_rds(rais_problema1e2_geocoded, sprintf("../../data/acesso_oport/rais/%s/geocode/rais_problema1e2_geocoded.rds", ano))
  rais_problema1e2_geocoded <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_problema1e2_geocoded.rds", ano))
  
  
  
  ######################### GOOGLE 3, so ceps
  
  # ainda ha empresas mal georreferenciadas!
  # identificar esses empresas e separa-los
  # muitas empresas foram georreferenciadas fora dos municipios
  # identifica-las atraves do shape dos municipios e refazer geocode do google considerando so CEP
  
  # carrega shape dos munis
  shps <- purrr::map_dfr(munis_df$code_muni, geobr::read_municipality) %>% as_tibble() %>% st_sf()
  
  
  
  rais_google_mal_geo <- rais_problema1e2_geocoded %>%
    filter(!is.na(lat)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_set_crs(4326)) %>%
    # sf::st_join(shps %>% st_set_crs(4326) %>% st_buffer(0.0008)) %>%
    # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(name_muni)) %>%
    # pegar so o estab
    select(id_estab) %>%
    left_join(select(rais_galileo_output, id_estab, name_muni, uf, cep), by = "id_estab") %>%
    filter(cep != ".")
  
  
  # Retorna somente os ceps dos que deram errado para jogar no google API
  somente_ceps <- paste0("CEP ", rais_google_mal_geo$cep, " - ", rais_google_mal_geo$name_muni, ", ", rais_google_mal_geo$uf)
  
  
  message("Total of estabs to go to gmaps prob CEP: ", length(somente_ceps))
  
  
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
  
  # create list with dt
  cep_problema_geocoded <- lapply(coordenadas_google_cep, create_dt)
  
  # rbind as data.table
  cep_problema_geocoded_dt <- rbindlist(cep_problema_geocoded, idcol = "id_estab",
                                        fill = TRUE, use.names = TRUE)
  
  
  # MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  cep_problema_geocoded_dt <- cep_problema_geocoded_dt[id_estab %in% rais_google_mal_geo$id_estab]
  
  # identify searchedaddress
  searchedaddress_cep <- filter(rais_google_mal_geo, id_estab %in% names(coordenadas_google_cep)) %>%
    mutate(SearchedAddress = paste0("CEP ", cep, " - ", name_muni, ", ", uf)) %>% 
    select(id_estab, SearchedAddress) %>%
    distinct(id_estab, .keep_all = TRUE)
  cep_problema_geocoded_dt <- left_join(cep_problema_geocoded_dt, searchedaddress_cep, by = "id_estab") %>% setDT()
  # identify problem
  cep_problema_geocoded_dt[, geocode_engine := 'gmaps_prob3']
  # identify quality
  cep_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  # check again with therey wihtin cities
  cep_problema_again <- cep_problema_geocoded_dt %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_set_crs(4326)) %>%
    # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(name_muni)) %>%
    sfc_as_cols() %>%
    select(id_estab)
  
  # fix these in the final dataset of ceps
  cep_problema_geocoded_dt_fixed <- setDT(cep_problema_geocoded_dt)[id_estab %in% cep_problema_again$id_estab, 
                                                                    ':='(PrecisionDepth = "address_outside_city",
                                                                         lon = NA,
                                                                         lat = NA)]
  
  
  # bring CEP's fixed to the original fixed dataset problems 1 and 2
  rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
                            c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                              list(i.MatchedAddress, i.SearchedAddress,  i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]
  
  # table(rais_problema1e2_geocoded$PrecisionDepth, useNA = 'always')
  # table(rais_problema1e2_geocoded$geocode_engine, useNA = 'always')
  # filter(rais_problema1e2_geocoded, is.na(MatchedAddress)) %>% View()
  
  
  # join the dataset with the fixed coordinates to the original output from galileo
  message("Joining gmaps dataset to the oringinal galileo dataset ... ")
  rais_galileo_output_fixed <- data.table::copy(rais_galileo_output)
  # identify geocode engine
  rais_galileo_output_fixed[, geocode_engine := "galileo"]
  # join!
  rais_galileo_output_fixed[rais_problema1e2_geocoded, on = "id_estab",
                            c("MatchedAddress", "PrecisionDepth", "lon", "lat", "SearchedAddress", "geocode_engine") :=
                              list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.SearchedAddress, i.geocode_engine)]
  
  
  # table(rais_galileo_output_fixed$PrecisionDepth, useNA = 'always')
  # table(rais_galileo_output_fixed$geocode_engine, useNA = 'always')
  # table(rais_galileo_output_fixed$type_input_galileo, useNA = 'always')
  
  
  # unique addresses
  # rais_galileo_output_fixed %>% add_count(id_estab) %>% filter(n>=2) %>% View()
  rais_galileo_output_fixed <- distinct(rais_galileo_output_fixed, id_estab, .keep_all = TRUE)
  
  # save it
  write_rds(rais_galileo_output_fixed, 
            sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode_final.rds", ano, ano))
  
  
  
  
  # merge with the complete base -----------------------------------------------------------
  # rais_galileo_output_fixed <- read_rds("../../data/acesso_oport/rais/2018/geocode/rais_2018_estabs_geocode_final.rds")
  
  if (ano == 2017) {
    
    rais_galileo_output_fixed <- rais_galileo_output_fixed %>%
      mutate(type_input_galileo = "rais_2017")
    
  }
  
  
  # open original rais from the year
  rais_filter <- fread(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano),
                            colClasses = "character",
                            encoding = "UTF-8") 
  
  # pad everyone to 14 characters
  setDT(rais_galileo_output_fixed)[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  rais_filter[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # table(nchar(rais_galileo_output_fixed$id_estab))
  # table(nchar(rais_2018_filter$id_estab))
  
  # make sure address are unique
  rais_filter <- rais_filter %>% distinct(id_estab, .keep_all = TRUE)
  rais_galileo_output_fixed <- rais_galileo_output_fixed %>% distinct(id_estab, .keep_all = TRUE)
  
  
  message("Joining the geocoded dataset to the completed dataset ... ")
  
  rais_filter_geocode_end <- merge(
    setDT(rais_filter),
    setDT(rais_galileo_output_fixed)[, .(id_estab, PrecisionDepth, SearchedAddress, MatchedAddress, 
                                         lon, lat, type_input_galileo, geocode_engine)],
    by = "id_estab",
    all.x = TRUE,
    sort = FALSE
    
  )
  
  
  # table(rais_filter_geocode_end$type_input_galileo, useNA = 'always')
  # table(rais_filter_geocode_end$PrecisionDepth, useNA = 'always')
  
  
  if (ano != 2017) {
    
    # get previous geocode
    rais_etapa8 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/check/rais_%s_check1.rds", ano, ano))
    rais_etapa8 <- rais_etapa8 %>% select(id_estab, lon, lat, 
                                          SearchedAddress, MatchedAddress,
                                          PrecisionDepth, type_input_galileo, geocode_engine)
    # filter only estabs that we will keep from 2017
    rais_etapa8 <- setDT(rais_etapa8)[id_estab %nin% rais_galileo_output_fixed$id_estab]
    # delete the ones that came from inep (we will add them later)
    rais_etapa8 <- rais_etapa8[type_input_galileo %nin% "inep"]
    
    # table(rais_etapa8$type_input_galileo)
    # table(rais_etapa8$PrecisionDepth)
    # table(rais_etapa8$geocode_engine)
    # table(nchar(rais_etapa8$id_estab))
    
    # make sure estabs are unique
    rais_etapa8 <- rais_etapa8 %>% distinct(id_estab, .keep_all = TRUE)
    
    message("Joining the completed year geocoded dataset to the previous geocoded year ... ")
    
    # merge with the previous results
    rais_filter_geocode_end[rais_etapa8, on = "id_estab",
                            c('PrecisionDepth', 'SearchedAddress', 'MatchedAddress','lon', 'lat', 'type_input_galileo', 'geocode_engine') :=
                              list(i.PrecisionDepth, i.SearchedAddress, i.MatchedAddress, i.lon, i.lat, i.type_input_galileo, i.geocode_engine)]
    
    # table(rais_2018_filter_geocode_end$PrecisionDepth, useNA = 'always')
    # table(rais_2018_filter_geocode_end$geocode_engine, useNA = 'always')
    # table(rais_2018_filter_geocode_end$type_input_galileo, useNA = 'always')
    # colnames(rais_2018_filter_geocode_end)
    # summary(as.numeric(rais_2018_filter_geocode_end$qt_vinc_ativos))
    
  } 
  
  
  
  write_rds(rais_filter_geocode_end, sprintf("../../data/acesso_oport/rais/%s/rais_%s_estabs_geocode_final.rds", ano, ano))
  
}





