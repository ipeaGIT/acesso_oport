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
  
  # 1.2) Abrir dados e selecionar as colunas
  rais_estabs_raw <- fread(sprintf("../../data/geocode/rais/%s/rais_%s_raw_geocoded.csv", ano, ano),
                           # select = columns,
                           colClasses = "character",
                           encoding = "UTF-8")
  
  
  
  # 2) Filtrar somente empresas com vinculos ativos
  rais_estabs_raw_0 <- rais_estabs_raw[as.numeric(qt_vinc_ativos) > 0]
  
  # filtrar somente nossos municipios
  rais_estabs_raw_0 <- rais_estabs_raw_0[codemun %in% substr(munis_list$munis_df$code_muni, 1, 6)]
  
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
  
  # 5) Selecionar colunas e salvar
  rais_filtro_2 %>%
    # fix uf and codemun
    select(id_estab, qt_vinc_ativos, logradouro, bairro, codemun, name_muni, uf, cep,
           lon, lat,
           Addr_type, Score, Status, type_year_input) %>% 
    # save it
    fwrite(sprintf("../../data/acesso_oport/rais/%s/rais_%s_filter_geocoded.csv", ano, ano))
}





#' A funcao `rais_gmaps_geocode` roda o geocode para os estabelecimentos problematicos
#' e atualiza a base que foi georef pelo streetmap, por fim a atualiazando a base final 
#' do ano
#' Etapas:
#' 1) Abrir output do streetmap
#' 2) Reformatar coordenadas
#' 3) Problema 1: Rodar Google API p/ estabelecimentos q streetmap encontrou com baixa precisao
#' 1 e 2 estrelas
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
  
  
  # 1) Abrir geocoded filter ------------
  rais_geocoded_filter <- fread(sprintf("../../data/acesso_oport/rais/%s/rais_%s_filter_geocoded.csv", ano, ano),
                                encoding = "UTF-8")
  
  # head(rais_galileo_output)
  
  # pad 14 digits of estabe id
  rais_geocoded_filter[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # table(rais_galileo_output$PrecisionDepth, useNA = 'always')
  # table(rais_galileo_output$type_input_galileo, useNA = 'always')
  
  # extrair somente o que eh novo para cada ano
  if (ano != 2017) {
    
    # identificar tipos
    tipos <- c(paste0("new_estab_", ano),
               paste0("cep_changed_", ano))
    
    rais_geocoded_filter <- rais_geocoded_filter[type_year_input %in% tipos]
    
  }
  
  
  # 3) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao ------
  # Manter todos resultados ‘point address’
  # ‘Street address (ext) e street name’, manter score  >= 0.90
  # Vai para google maps
  # Tied e Todas outras observações vão para Google maps
  # ‘Street address (ext) e street name’, manter score  < 0.90
  
  # 3.1) Selecionar estabs com baixa precisao
  rais_geocoded_filter[, gmaps := fifelse(Status %in% c("T", "U"), TRUE,
                                          fifelse(Addr_type == "PointAddress", FALSE,
                                                  fifelse(Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 90, FALSE, TRUE)))]
  
  estabs_problema <- rais_geocoded_filter[gmaps == TRUE]
  
  # # 3.2) Selecionar muitos stabs com as mesmas coordenadas 
  # 
  # #! adicionar checagem se streetmap retorna muitos endereços com mesmas coordenadas
  # same_lat <- rais_geocoded_filter[, same_coords := paste0(lat,'-',lon)]
  # same_lat <- rais_geocoded_filter[, N := .N, by = same_coords]
  # same_lat <- same_lat[N  > 14]
  # 
  # # definir esses estabelecimentos
  # rais_same_coords <- subset(estabs_problema_3estrelas, same_coords %in% same_lat$same_coords)
  
  
  message("Total of estabs to go to gmaps problem 1: ", unique(estabs_problema$id_estab) %>% length())
  
  
  # 3.2) Listar esses enderecos com problema
  enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", bairro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% pull(fim)
  
  # 3.3) Registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = TRUE)
  
  # # 3.4) Rodar o geocode do gmaps
  # seqlast <- function (from, to, by) 
  # {
  #   vec <- do.call(what = seq, args = list(from, to, by))
  #   if ( tail(vec, 1) != to ) {
  #     return(c(vec, to))
  #   } else {
  #     return(vec)
  #   }
  # }
  
  # bounds <- seqlast(1, nrow(enderecos), by = 20000)
  # bounds <- Map(c, bounds[-length(bounds)], bounds[-1])
  # 
  # # create list with arguments
  # run_gmaps_list_args <- list(
  #   up = up,
  #   down = down,
  #   key1 = my_api[length(up)]
  # )
  # 
  # run_gmaps <- function(bounds1, key1) {
  #   
  #   enderecos_go <- enderecos[bounds1[[1]]:bounds1[[2]]]
  #   enderecos_go1 <- enderecos_go$fim
  #   register_google(key = key1)
  #   
  #   # o ideal eh fazer a cada 10 mil, mas as vezes tenho menos de 10000
  # 
  #   coordenadas_google1_1 <- lapply(X=enderecos_go1[1:10000], ggmap::geocode, output = "all")
  #   coordenadas_google1_2 <- lapply(X=enderecos_go1[10001:length(enderecos_go1)], ggmap::geocode, output = "all")
  # 
  #   # join them all together
  #   coordenadas_google1 <- c(coordenadas_google1_1, coordenadas_google1_2)
  #   # identify list names as id_estab
  #   names(coordenadas_google1) <- enderecos_go$id_estab
  #   # save
  #   write_rds(coordenadas_google1,
  #             sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps/rais_geocode_%s_output_google_%s_%s.rds", ano, ano, up, down), compress = 'gz')
  #   
  #   
  # }
  
  
  if (run_gmaps) {
    
    message("Running gmaps, this may take a while")
    
    register_google(key = my_api$key[1]) # rafa
    coordenadas_google_1 <- lapply(X=enderecos[1:10000], ggmap::geocode, output = "all")
    names(coordenadas_google_1) <- estabs_problema[1:10000]$id_estab
    write_rds(coordenadas_google_1, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp1.rds", ano))
    
    coordenadas_google_2 <- lapply(X=enderecos[10001:20000], ggmap::geocode, output = "all")
    names(coordenadas_google_2) <- estabs_problema[10001:20000]$id_estab
    write_rds(coordenadas_google_2, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp2.rds", ano))
    
    coordenadas_google_3 <- lapply(X=enderecos[20001:30000], ggmap::geocode, output = "all")
    names(coordenadas_google_3) <- estabs_problema[20001:30000]$id_estab
    write_rds(coordenadas_google_3, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp3.rds", ano))
    
    register_google(key = my_api$key[2]) # kaue
    coordenadas_google_4 <- lapply(X=enderecos[30001:40000], ggmap::geocode, output = "all")
    names(coordenadas_google_4) <- estabs_problema[30001:40000]$id_estab
    write_rds(coordenadas_google_4, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp4.rds", ano))
    
    coordenadas_google_5 <- lapply(X=enderecos[40001:50000], ggmap::geocode, output = "all")
    names(coordenadas_google_5) <- estabs_problema[40001:50000]$id_estab
    write_rds(coordenadas_google_5, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp5.rds", ano))
    
    coordenadas_google_6 <- lapply(X=enderecos[50001:60000], ggmap::geocode, output = "all")
    names(coordenadas_google_6) <- estabs_problema[50001:60000]$id_estab
    write_rds(coordenadas_google_6, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp6.rds", ano))
    
    register_google(key = my_api$key[3]) # diego
    coordenadas_google_7 <- lapply(X=enderecos[60001:70000], ggmap::geocode, output = "all")
    names(coordenadas_google_7) <- estabs_problema[60001:70000]$id_estab
    write_rds(coordenadas_google_7, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp7.rds", ano))
    
    coordenadas_google_8 <- lapply(X=enderecos[70001:80000], ggmap::geocode, output = "all")
    names(coordenadas_google_8) <- estabs_problema[70001:80000]$id_estab
    write_rds(coordenadas_google_8, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp8.rds", ano))
    
    coordenadas_google_9 <- lapply(X=enderecos[80001:90000], ggmap::geocode, output = "all")
    names(coordenadas_google_9) <- estabs_problema[80001:90000]$id_estab
    write_rds(coordenadas_google_9, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp9.rds", ano))
    
    register_google(key = my_api$key[4]) # marcus
    coordenadas_google_10 <- lapply(X=enderecos[90001:100000], ggmap::geocode, output = "all")
    names(coordenadas_google_10) <- estabs_problema[90001:100000]$id_estab
    write_rds(coordenadas_google_10, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp10.rds", ano))
    
    coordenadas_google_11 <- lapply(X=enderecos[100001:length(enderecos)], ggmap::geocode, output = "all")
    names(coordenadas_google_11) <- estabs_problema[100001:length(enderecos)]$id_estab
    write_rds(coordenadas_google_11, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp11.rds", ano))
    
    # join them all together
    coordenadas_google <- c(coordenadas_google_1, coordenadas_google_2, coordenadas_google_3, coordenadas_google_4, coordenadas_google_5,
                             coordenadas_google_6, coordenadas_google_7, coordenadas_google_8, coordenadas_google_9, coordenadas_google_10,
                             coordenadas_google_11)
    # identify list names as id_estab
    names(coordenadas_google) <- estabs_problema$id_estab
    # save
    write_rds(coordenadas_google, 
              sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google.rds", ano, ano), compress = 'gz')
    
  } else {
    
    # check if there any difference between the estabs that were saved and the estabs that were
    # supposed to be geocoded
    coordenadas_google <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_geocode_%s_output_google.rds", ano, ano))
    
    names(coordenadas_google) <- str_pad(names(coordenadas_google), width = 14, pad = 0)
    
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





