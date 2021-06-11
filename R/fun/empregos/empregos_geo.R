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
  
  # 3) Filtro 2: deletar todas as intituicoes com Natureza Juridica 'publica' 
      # (ver ../data-raw/rais/ManualRAIS2018.pdf) pagina 19
      # todos que comecam com o numeral 1 sao de administracao publica
      
      # 3.1) Identificar natureza de adm publica
      rais_estabs_raw_0[, adm_pub := ifelse( substr(nat_jur, 1, 1)==1, 1, 0)]
      
      # 3.2) Filtrar apenas adm publica
      rais_filtro_1 <- rais_estabs_raw_0[ adm_pub != 1 ]
      
      # quantos vinculos de natureza juridica publica a gente perde?
      # nrow(rais_filtro_2) / nrow(rais_filtro_1)
      
      # 3.3) Deletar todas as empresas publicas (a entidade 2011 eh empresa publica)
      rais_filtro_2 <- rais_filtro_1[nat_jur != "2011"]
      
      message("Total number of private active estabs: ", unique(rais_filtro_2$id_estab) %>% length())
      
  # todo os estabs para 14 characetrs
  rais_filtro_2[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # 4) Trazer o nome do municipio
  muni_lookup <- geobr::lookup_muni(code_muni = "all")
  muni_lookup <- muni_lookup %>%
    select(codemun = code_muni, name_muni, abrev_state) %>%
    mutate(codemun = substr(codemun, 1, 6))
  
  rais_filtro_2 <- rais_filtro_2 %>%
    left_join(muni_lookup, by = "codemun") 
  
  # 5) Selecionar colunas e salvar
  rais_filtro_2 %>%
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
#' novos ou que mudaram de endereço de um ano para o outro
#' 3) Salvar

rais_export_data_to_galileo <- function(ano) {
  
  # 1) Abrir rais filtrada
  rais_filter <- fread(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_filter.csv", ano, ano),
                       colClasses = "character",
                       select = c("id_estab", "logradouro", "bairro", "codemun", "name_muni", "uf", "cep"),
                       encoding = "UTF-8")
  
  
  # todo os CEPS para 8 digitos characetrs
  rais_filter[, cep := str_pad(cep, width = 8, pad = 0)]
  
  # correcao de logradouro, retira '999999'
  rais_filter[ , logradouro := str_replace(string = logradouro,
                                          pattern = ', 999999$',
                                          replacement = '')]
  rais_filter[ , logradouro := str_replace(string = logradouro,
                                           pattern = ' 999999$',
                                           replacement = '')]
  
  rais_filter[ , logradouro := str_replace(string = logradouro,
                                            pattern = ', 99999$',
                                            replacement = '')]
  rais_filter[ , logradouro := str_replace(string = logradouro,
                                           pattern = ' 99999$',
                                           replacement = '')]
  
  rais_filter[ , logradouro := str_replace(string = logradouro,
                                           pattern = ', 9999$',
                                           replacement = '')]
  rais_filter[ , logradouro := str_replace(string = logradouro,
                                           pattern = ' 9999$',
                                           replacement = '')]

# 2) Se o ano nao for 2017, selecionar somente os estabelecimentos que sao
  # novos ou que mudaram de endereço e um ano para o outro
  if (ano != 2017) {
    
    # 2.1) Abrir RAIS final do ano anterior
    rais_anterior <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode_completo.rds", 
                                    ano-1, ano-1))
    # Selecionar variaveis
    rais_anterior <- rais_anterior %>% select(id_estab, logradouro, cep, lon, lat) %>% setDT()
    # Pad to 14 chars
    rais_anterior[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
    
    # 2.2) Selecionar somente os estabs que nao foram georef antes
    rais_geocode_new <- rais_filter[id_estab %nin% rais_anterior$id_estab]
    
    # identify if estab is new or whether it comes from previous year
    rais_geocode_new[, type_input_galileo := paste0("new_estab_", ano)]
    
    message("Total of new estabs compared to previous year: ", nrow(rais_geocode_new))
    
    # 2.3) Selecionar somente os estabs que mudaram o cep em relacao ao ano anterior
    # Pad to 8 chars
    rais_anterior[, cep := str_pad(cep, width = 8, pad = 0)]
    rais_geocode_new[, cep := str_pad(cep, width = 8, pad = 0)]
    rais_filter[, cep := str_pad(cep, width = 8, pad = 0)]
    
    # identifica empresas que foram geocode no ano anterior, mas que mudaram de endereco no ano atual
    rais_geocode_cep <- left_join(rais_filter, select(rais_anterior, id_estab, logradouro, cep),
                                  by = "id_estab",
                                  suffix = c("_anterior", "_atual")) %>%
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
    rais_filter_geocode <- unique(rais_filter_geocode) %>% setDT()
    message("Total of to be geocoded at galileo: ", nrow(rais_filter_geocode))
    
    # criar logradouro
    rais_filter_geocode[, logradouro := paste0(logradouro, " - ", bairro)]
    
    # 2.5) Selecionar colunas e salvar input para galileo
    rais_filter_geocode %>%
      write_delim(sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_input_galileo.csv", ano, ano), delim = ";")
    
    message("Input to galileo saved at: ", sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_input_galileo.csv", ano, ano))
    
  } else if (ano == 2017) {
    
    rais_filter_geocode <- rais_filter %>%
      mutate(type_input_galileo = paste0("rais_", "2017")) %>%
      # criar logradouro
      mutate(logradouro = paste0(logradouro, " - ", bairro))
    
    message("Total of to be geocoded at galileo: ", nrow(rais_filter_geocode))
    
    # 2.5) Selecionar colunas e salvar input para galileo
    rais_filter_geocode %>%
      write_delim(sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_input_galileo.csv", ano, ano), delim = ";")
    
    message("Input to galileo saved at: ", sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_input_galileo.csv", ano, ano))
    
  }
  
}



#' This function gets the Galileo output, check if all estabs are within the galileo input
#' Eventually, new cities may be added to the project and we need to run their geocode
#' after all the main geocode was already done
#' If all estabs have been processed, then TRUE, it moves on
#' If FALSE, it creates a new input to galileo with the new coords
#' Then it binds the new output from galileo to the old output

rais_check_new_estabs <- function(ano) {
  
  # open galileo input
  rais_galileo_input <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_input_galileo.csv", ano, ano),
                              colClasses = 'character')
  # rbind(data.frame(id_estab = "aaaaa"), fill = TRUE)
  
  
  # open galileo output
  rais_galileo_output <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_output_galileo.csv", ano, ano),
                               colClasses = 'character', fill = TRUE)
  rais_galileo_output[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # test if there is any estab behind
  if (length(setdiff(rais_galileo_input$id_estab, rais_galileo_output$id_estab)) > 0) {
    
    # get the new estabs
    rais_galileo_new_input <- rais_galileo_input[id_estab %nin% rais_galileo_output$id_estab]
    
    message("You have ", nrow(rais_galileo_new_input), " new estabs to geocode")
    
    # export them
    write_delim(rais_galileo_new_input, sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_new_input_galileo.csv", ano, ano), delim = ";")
    
    
    # mywait()
    
    invisible(readline(prompt="\nPlease run the new input in Galileo and save the output\nPress [enter] to continue"))
    
    rais_galileo_new_output <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_new_output_galileo.csv", ano, ano),
                                     colClasses = 'character')
    
    rais_galileo_new_output[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
    
    # bind to the old galileo output
    rais_galileo_total_output <- rbind(rais_galileo_output, rais_galileo_new_output) %>% setDT()
    
    # save it
    fwrite(rais_galileo_total_output, 
           sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_output_galileo.csv", ano, ano),
           sep = ",")
    
    
    
    
  } else {message("All Estabs are geocoded!")}
  
  
}


#' A funcao `rais_gmaps_geocode` roda o geocode para os estabelecimentos problematicos
#' e atualiza a base que foi georef pelo galileo, por fim a atualiazando a base final 
#' do ano
#' Etapas:
#' 1) Abrir output do galileo
#' 2) Reformatar coordenadas
#' 3) Problema 1: Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao
#' 1 e 2 estrelas
#' 4) Problema 2: Rodar Google API  somente para estabs 3 estrelas que estao em rodovias
#' 5) Juntar datasets dos problemas 1 e 2
#' 6) Problema 3: Rodar gmaps com o CEP somente para empresa que ficaram fora das respectivas
#' cidades
#' 7) Problema 4: Fazer a identificacao de quando o estab continua fora da cidade
    #' 8) Trazer os estabs que foram georef so com o CEP para o dataset original
    #' dos problemas 1 e 2
    #' 9) Check if there are missing address
    #' 10) Substituir as coordenadas problematicas que estao na base do galileo
    #' pelas novas coordenadas que foram corrigidas pelo gmaps
    #' 11) Trazer as novas coordenadas para a base completa do ano e completar com
    #' os estabs que foram georef no ano anterior

rais_gmaps_geocode <- function(ano, run_gmaps = FALSE) {
  
  
  # 1) Abrir output do galileo ------------
  rais_galileo_output <- fread(sprintf("../../data/acesso_oport/rais/%s/geocode/galileo/rais_%s_output_galileo.csv", ano, ano),
                               colClasses = 'character',
                               fill = TRUE)
  
  # head(rais_galileo_output)
  
  # pad 14 digits of estabe id
  rais_galileo_output[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # table(rais_galileo_output$PrecisionDepth, useNA = 'always')
  # table(rais_galileo_output$type_input_galileo, useNA = 'always')
  
  
  # 2) Formatar coordenadas --------------
  rais_galileo_output <- rais_galileo_output %>% rename(lat=Latitude , lon=Longitude)
  
  # 2.1) Substituir , por  .)
  rais_galileo_output[, ':='(lon = str_replace(lon, ",", "."),
                             lat = str_replace(lat, ",", "."))]
  # 2.2) Trazer para numerico
  rais_galileo_output[, ':='(lon = as.numeric(lon),
                             lat = as.numeric(lat))]
  
  
  # 3) Para estabs com mais um georef, selecionar oq tem melhor precisao
  # 3.1) Identificar precisao como numerico
  # table(rais_galileo_output$PrecisionDepth)
  rais_galileo_output[, precision_depth1 := substr(PrecisionDepth, 1, 1)]
  # table(rais_galileo_output$precision_depth1)
  
  # 3.2) Selecionar os com melhor precisao
  rais_galileo_output <- rais_galileo_output %>%
    group_by(id_estab) %>%
    slice(which.max(precision_depth1)) %>%
    setDT()
  
  # apagar precision_depth1
  rais_galileo_output <- rais_galileo_output %>% select(-precision_depth1)

  
  # 3) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao ------
  # Problema 1: output do galileo com 1 e 2 estrelas
  
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
              sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google1.rds", ano, ano), compress = 'gz')
    
  } else {
    
    # check if there any difference between the estabs that were saved and the estabs that were
    # supposed to be geocoded
    coordenadas_google1 <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google1.rds", ano, ano))
    
    names(coordenadas_google1) <- str_pad(names(coordenadas_google1), width = 14, pad = 0)
    
    if (length(setdiff(estabs_problema$id_estab, names(coordenadas_google1))) > 0) {
      
      invisible(
        readline
        (prompt=sprintf("\nThere are %i new estabs to geocode in gmaps1, press [enter] to continue",  
                        length(setdiff(estabs_problema$id_estab, names(coordenadas_google1)))))
      )
      
      # new estab to geocode in gmaps1
      estabs_problema_new <- estabs_problema[ id_estab %nin% names(coordenadas_google1)]
      
      # list address
      enderecos_new <- estabs_problema_new %>% 
        mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
      
      # send to gmaps
      coordenadas_google1_new <- lapply(X=enderecos_new, ggmap::geocode, output = "all")
      
      # identify list names as id_estab
      names(coordenadas_google1_new) <- estabs_problema_new$id_estab
      
      # bind to the old geocoded estabs by gmaps1
      coordenadas_google1 <- c(coordenadas_google1, coordenadas_google1_new)
      
      # save it
      write_rds(coordenadas_google1, 
                sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google1.rds", ano, ano), compress = 'gz')
      
      
    } else {message("All Estabs are geocoded!")}
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
  
  
  # 3.7) MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs (estrelas 1 e 2)
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[id_estab %in% estabs_problema$id_estab]
  
  # 3.8) Identificar a informacao de searchedaddress
  searchedaddress <- filter(estabs_problema, id_estab %in% names(coordenadas_google1)) %>%
                        mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% 
                        select(id_estab, SearchedAddress) %>%
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
    mutate(nome = str_extract(nome, "[[:upper:]]{2,3}-\\d{3}")) %>%
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
  
  
  
  
  ##### Identificar muitos stabs com as mesmas coordenadas ---------------------------------
  
  #! adicionar checagem se Galileo retorna muitos endereços com mesmas coordenadas
  same_lat <- estabs_problema_3estrelas[, same_coords := paste(lat,'-',lon)] %>%
    count(name_muni, same_coords, sort = T) %>% 
    subset(., n >1)
  # ponto de corte = 14, percentil 95%
  #quantile(same_lat$n, probs=seq(0,1,.05))
  same_lat <- subset(same_lat, n  >14)
  
  
  # definir esses estabelecimentos
  rais_same_coords <- subset(estabs_problema_3estrelas, same_coords %in% same_lat$same_coords)
  
  # rais_same_coords[, a := str_replace(string = logradouro, "(^.+),\\s?(\\d+)\\s-\\s.+$", "\\1")]
  # rais_same_coords[, b := str_replace(string = logradouro, "(^.+),\\s?(\\d+)\\s-\\s.+$", "\\2")]
  # rais_same_coords %>% arrange(lon) %>% select(logradouro, a, b) %>% View()
  
  # unique(rais_same_coords$logradouro) %>% length()
  
  # juntar tudo na rais rodovias
  rais_rodovias_samecoords <- estabs_problema_3estrelas[id_estab %in% c(rais_rodovias$id_estab, rais_same_coords$id_estab)]
  head(rais_rodovias_samecoords)

  
  # 4.5) Listas enderecos de rodovia problematicos
  enderecos_rodovias <- rais_rodovias_samecoords %>% mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
  
  # 4.6) Rodar o geocode do gmaps para rodovias
  
  if(run_gmaps) {
    
    # geocode
    coordenadas_google_rodovias <- lapply(X=enderecos_rodovias, ggmap::geocode, output = "all")
    
    # identify list names as id_estab
    names(coordenadas_google_rodovias) <- rais_rodovias_samecoords$id_estab 
    
    # save
    write_rds(coordenadas_google_rodovias, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google2.rds", ano, ano), compress = 'gz')
    
  } else {
    
    # check if there is any difference between the estabs that were saved and the estabs that were
    # supposed to be geocoded
    coordenadas_google_rodovias <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google2.rds", ano, ano))
    
    names(coordenadas_google_rodovias) <- str_pad(names(coordenadas_google_rodovias), width = 14, pad = 0)
    
    if (length(setdiff(rais_rodovias_samecoords$id_estab, names(coordenadas_google_rodovias))) > 0) {
      
      invisible(
        readline
        (prompt=sprintf("\nThere are %i new estabs to geocode in gmaps2, press [enter] to continue",  
                        length(setdiff(rais_rodovias_samecoords$id_estab, names(coordenadas_google_rodovias)))))
      )
      
      # new estab to geocode in gmaps1
      rais_rodovias_samecoords_new <- rais_rodovias_samecoords[ id_estab %nin% names(coordenadas_google_rodovias)]
      
      # list address
      enderecos_rodovias_new <- rais_rodovias_samecoords_new %>% 
        mutate(fim = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% .$fim
      
      # send to gmaps
      coordenadas_google_rodovias_new <- lapply(X=enderecos_rodovias_new, ggmap::geocode, output = "all")
      
      # identify list names as id_estab
      names(coordenadas_google_rodovias_new) <- rais_rodovias_samecoords_new$id_estab
      
      # bind to the old geocoded estabs by gmaps1
      coordenadas_google_rodovias <- c(coordenadas_google_rodovias, coordenadas_google_rodovias_new)
      
      # save it
      write_rds(coordenadas_google_rodovias, 
                sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google2.rds", ano, ano), compress = 'gz')
      
    }
  }
  
  
 
  # 4.7) Rodar funcao que transforma todos os estabs georef em data.table 
  rodovias_problema_geocoded <- lapply(coordenadas_google_rodovias, create_dt)
  
  
  # 4.8) Rbind as data.table
  rodovias_problema_geocoded_dt <- rbindlist(rodovias_problema_geocoded, idcol = "id_estab",
                                             use.names = TRUE)
  
  
  # 4.9) MAKE SURE WE ARE ONLY TREATING PROBLEMATIC estabs
  rodovias_problema_geocoded_dt <- rodovias_problema_geocoded_dt[id_estab %in% rais_rodovias_samecoords$id_estab]
  head(rodovias_problema_geocoded_dt)
  
  # 4.10) Identificar a informacao de searchedaddress que foi pesquisa no google maps
  searchedaddress_rodovias <- filter(rais_rodovias_samecoords, id_estab %in% names(coordenadas_google_rodovias)) %>%
                                mutate(SearchedAddress = paste0(logradouro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% select(id_estab, SearchedAddress) %>%
                                distinct(id_estab, .keep_all = TRUE)
  rodovias_problema_geocoded_dt <- left_join(rodovias_problema_geocoded_dt, searchedaddress_rodovias, by = "id_estab") %>% setDT()
  head(rodovias_problema_geocoded_dt)
  
  # 4.11) Identificar tipo de problema
  rodovias_problema_geocoded_dt[, geocode_engine := 'gmaps_prob2']
  
  # 4.12) Identificar qualidade quando o endereco nao foi encontrado
  rodovias_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  
  # 5) Juntar datasets dos problemas 1 e 2 ---------------------
  rais_problema1e2_geocoded <- rbind(estabs_problema_geocoded_dt, rodovias_problema_geocoded_dt,
                                     fill = TRUE)
  
  # 5.1) Salvar dataset dos problemas 1 e 2
  write_rds(rais_problema1e2_geocoded, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_problema1e2_geocoded.rds", ano), compress = 'gz')
  rais_problema1e2_geocoded <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_problema1e2_geocoded.rds", ano))
  head(rais_problema1e2_geocoded)
  
  
  
  # 6) Rodar gmaps com o CEP somente para empresa que ficaram fora das respectivas ------
  # cidades
  # identifica-las atraves do shape dos municipios e refazer geocode do google considerando so CEP
  
  code_munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist()
  
  # 6.1) Carregar shape dos munis
  shps <- purrr::map_dfr(code_munis, geobr::read_municipality) %>% as_tibble() %>% st_sf()
  
  
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
  
  
  # 6.3) Listar CEPs para geocode do gmaps
  somente_ceps <- paste0("CEP ", rais_google_mal_geo$cep, ", ", rais_google_mal_geo$uf)
  
  
  message("Total of estabs to go to gmaps prob CEP: ", length(somente_ceps))
  
  # 6.4) Rodar gmaps para CEPS
  if (run_gmaps) {
    
    # consulta google api
    coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode, output = 'all')
    
    # identify list names as id_estab
    names(coordenadas_google_cep) <- rais_google_mal_geo$id_estab
    
    # save it
    write_rds(coordenadas_google_cep, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google3.rds", ano, ano), compress = 'gz')
    
  } else {
    
    # check if there any difference between the estabs that were saved and the estabs that were
    # supposed to be geocoded
    coordenadas_google_cep <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google3.rds", ano, ano))
    
    names(coordenadas_google_cep) <- str_pad(names(coordenadas_google_cep), width = 14, pad = 0)
    
    if (length(setdiff(rais_google_mal_geo$id_estab, names(coordenadas_google_cep))) > 0) {
      
      invisible(
        readline
        (prompt=sprintf("\nThere are %i new estabs to geocode in gmaps cep, press [enter] to continue",  
                        length(setdiff(rais_google_mal_geo$id_estab, names(coordenadas_google_cep)))))
      )
      
      # new estab to geocode in gmaps1
      rais_google_mal_geo_new <- setDT(rais_google_mal_geo)[id_estab %nin% names(coordenadas_google_cep)]
      
      # list address
      somente_ceps_new <- rais_google_mal_geo_new %>% 
        mutate(fim = paste0("CEP ", cep, " - ", name_muni, ", ", uf)) %>%
        .$fim
      
      # send to gmaps
      coordenadas_google_cep_new <- lapply(X=somente_ceps_new, ggmap::geocode, output = "all")
      
      # identify list names as id_estab
      names(coordenadas_google_cep_new) <- rais_google_mal_geo_new$id_estab
      
      # bind to the old geocoded estabs by gmaps1
      coordenadas_google_cep <- c(coordenadas_google_cep, coordenadas_google_cep_new)
      
      # save it
      write_rds(coordenadas_google_cep, 
                sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google3.rds", ano, ano), compress = 'gz')
      
    }
    
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
            sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode.rds", ano, ano), compress = 'gz')
  
  
  # 11) Trazer as novas coordenadas para a base completa do ano e completar com ----
  # os estabs que foram georef no ano anterior
  # O que foi geoferenciado no total (galileo + gmaps) precisa
  # ser inserido na base bruta do ano, ja que o georef foi feito
  # somente para os novos enderecos e para os que mudaram de cep
  # Para o ano de 2017 essa operacao nao precisa ser feita
  
  # 11.1) Abrir os enderecos georef galileo + gmaps no ano anterior
  rais_galileo_output_fixed <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode.rds", ano, ano))
  
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
    # aqui inclu apenas os estabs que foram geocoded nesse ano
    message("Joining the geocoded dataset to the completed dataset ... ")
    
    rais_filter_geocode_end <- merge(
      setDT(rais_filter),
      setDT(rais_galileo_output_fixed)[, .(id_estab, PrecisionDepth, SearchedAddress, MatchedAddress,
                                           lon, lat, type_input_galileo, geocode_engine)],
      by = "id_estab",
      all.x = TRUE,
      sort = FALSE
    )
    
    # 11.5) Trazer entao os estabs geocoded do ano anterior
    
    # 11.5.1) Get previous geocode
    rais_ano_anterior <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode_completo.rds", ano-1, ano-1))
    rais_ano_anterior <- rais_ano_anterior %>% select(id_estab, lon, lat, 
                                          SearchedAddress, MatchedAddress,
                                          PrecisionDepth, type_input_galileo, geocode_engine)
    
    # 11.5.2) Filter only estabs that we will keep from the previous year (not present in the following year)
    rais_ano_anterior <- setDT(rais_ano_anterior)[id_estab %nin% rais_galileo_output_fixed$id_estab]
    
    # table(rais_ano_anterior$type_input_galileo, useNA = 'always')
    # table(rais_ano_anterior$PrecisionDepth, useNA = 'always')
    # table(rais_ano_anterior$geocode_engine)
    # table(nchar(rais_ano_anterior$id_estab))
    
    # 11.5.4) Make sure estabs are unique
    rais_ano_anterior <- rais_ano_anterior %>% distinct(id_estab, .keep_all = TRUE)
    
    message("Joining the completed year geocoded dataset to the previous geocoded year ... ")
    
    # 11.5.5) Merge with the previous results
    rais_filter_geocode_end[rais_ano_anterior, on = "id_estab",
                            c('PrecisionDepth', 'SearchedAddress', 'MatchedAddress','lon', 'lat', 'type_input_galileo', 'geocode_engine') :=
                              list(i.PrecisionDepth, i.SearchedAddress, i.MatchedAddress, i.lon, i.lat, i.type_input_galileo, i.geocode_engine)]
    
    # table(rais_filter_geocode_end$PrecisionDepth, useNA = 'always')
    # table(rais_filter_geocode_end$geocode_engine, useNA = 'always')
    # table(rais_filter_geocode_end$type_input_galileo, useNA = 'always')
    # colnames(rais_2018_filter_geocode_end)
    # summary(as.numeric(rais_2018_filter_geocode_end$qt_vinc_ativos))
    
    
  }
  
  # 12) Salvar! ---------
  write_rds(rais_filter_geocode_end, 
            sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_estabs_geocode_completo.rds", ano, ano), compress = 'gz')
  
}





