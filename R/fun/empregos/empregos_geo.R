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
  rais_estabs_raw_0 <- rais_estabs_raw_0[codemun %in% substr(munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist(), 1, 6)]
  
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
  
  # garantir que os cnpjs sao unicos
  rais_filtro_2 <- rais_filtro_2 %>% distinct(id_estab, .keep_all = TRUE)
  
  # 5) Selecionar colunas e salvar
  rais_filtro_2 %>%
    # fix uf and codemun
    select(id_estab, qt_vinc_ativos, logradouro, bairro, codemun, name_muni, uf, cep,
           lon, lat,
           Addr_type, Score, Status, matched_address, type_year_input) %>% 
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
  
  # identify engine
  rais_geocoded_filter[, geocode_engine := "streetmap"]
  
  
  # trazer entao as coordenadas do gmaps do ano anterior
  if (ano != 2017) {
    
    rais_anterior <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_filter_geocoded_gmaps.rds", ano-1, ano-1))
    # identificar estabs a serem atualizados
    estabs_to_update <- rais_geocoded_filter[type_year_input %nin% c(paste0("new_estab_", ano), paste0("cep_changed_", ano))]$id_estab
    # filtra entao esses do ano anteior
    rais_anterior <- rais_anterior[id_estab %in% estabs_to_update]
    # juncao
    # table(rais_geocoded_filter$Addr_type, useNA = 'always')
    # table(rais_geocoded_filter$geocode_engine, useNA = 'always')
    # table(rais_anterior$geocode_engine, useNA = 'always')
    rais_geocoded_filter[rais_anterior, on = "id_estab",
                         c("Addr_type", "matched_address", "lon", "lat", "geocode_engine") :=
                           list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine)]
    
  }
  
  
  # table(rais_geocoded_filter$geocode_engine, useNA = 'always')
  # table(rais_geocoded_filter$type_year_input, useNA = 'always')
  
  # extrair somente o que eh novo para cada ano
  if (ano != 2017) {
    
    # identificar tipos
    tipos <- c(paste0("new_estab_", ano),
               paste0("cep_changed_", ano))
    
    rais_geocoded_filter_new <- rais_geocoded_filter[type_year_input %in% tipos]
    
  } else rais_geocoded_filter_new <- rais_geocoded_filter 
  
  
  # 3) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao ------
  # Manter todos resultados ‘point address’
  # ‘Street address (ext) e street name’, manter score  >= 0.90
  # Vai para google maps
  # Tied e Todas outras observações vão para Google maps
  # ‘Street address (ext) e street name’, manter score  < 0.90
  
  # 3.1) Selecionar estabs com baixa precisao
  rais_geocoded_filter_new[, gmaps := fifelse(Status %in% c("T", "U"), TRUE,
                                              fifelse(Addr_type == "PointAddress", FALSE,
                                                      fifelse(codemun %like% "530010" & Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 75, FALSE,
                                                              fifelse(Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 90, FALSE, TRUE))))]
  
  # StreetAddress, StreetAddressExt e StreetName que tiverem nota >=75
  
  estabs_problema <- rais_geocoded_filter_new[gmaps == TRUE]
  
  # # 3.2) Selecionar muitos stabs com as mesmas coordenadas 
  
  
  message("Total of estabs to go to gmaps: ", unique(estabs_problema$id_estab) %>% length())
  
  
  # 3.2) Listar esses enderecos com problema
  enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", bairro, " - ", name_muni, ", ", uf, " - CEP ", cep)) %>% pull(fim)
  
  # 3.3) Registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = TRUE)
  
  
  
  if (run_gmaps) {
    
    message("Running gmaps, this may take a while")
    
    if (ano == 2017) {
      
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
      
    } else {
      
      register_google(key = my_api$key[6]) # joao_parga
      coordenadas_google_1 <- lapply(X=enderecos[1:10000], ggmap::geocode, output = "all")
      names(coordenadas_google_1) <- estabs_problema[1:10000]$id_estab
      write_rds(coordenadas_google_1, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp1.rds", ano))
      
      coordenadas_google_2 <- lapply(X=enderecos[10001:20000], ggmap::geocode, output = "all")
      names(coordenadas_google_2) <- estabs_problema[10001:20000]$id_estab
      write_rds(coordenadas_google_2, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp2.rds", ano))
      
      coordenadas_google_3 <- lapply(X=enderecos[20001:length(enderecos)], ggmap::geocode, output = "all")
      names(coordenadas_google_3) <- estabs_problema[20001:length(enderecos)]$id_estab
      write_rds(coordenadas_google_3, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp3.rds", ano))
      
      # join them all together
      coordenadas_google <- c(coordenadas_google_1, coordenadas_google_2, coordenadas_google_3)
      
      
      
      
    }
    
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
    
    if (length(setdiff(estabs_problema$id_estab, names(coordenadas_google))) > 0) {
      
      invisible(
        readline
        (prompt=sprintf("\nThere are %i new estabs to geocode in gmaps1, press [enter] to continue",  
                        length(setdiff(estabs_problema$id_estab, names(coordenadas_google)))))
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
      matched_address = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
      # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
      Addr_type = precision_depth,
      lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
      lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
    )
    
  }
  
  
  # 3.5) Rodar funcao que transforma todos os estabs georef em data.table
  estabs_problema_geocoded <- lapply(coordenadas_google, create_dt)
  
  # 3.6) Rbind as data.table
  estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "id_estab",
                                           use.names = TRUE)
  
  # make sure we only using the correct subset
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[id_estab %in% estabs_problema$id_estab]
  
  # 3.9) Identificar o tipo de problema
  estabs_problema_geocoded_dt[, geocode_engine := 'gmaps']
  
  # 3.10) Identificar qualidade quando o endereco nao foi encontrado
  estabs_problema_geocoded_dt[is.na(lon), ':='(Addr_type = "address_not_found")]
  
  
  
  # 10) Substituir as coordenadas problematicas que estao na base do streetmap ----
  # pelas novas coordenadas que foram corrigidas pelo gmaps
  
  # table(rais_geocoded_filter_new$Addr_type, useNA = 'always')
  # table(rais_geocoded_filter_new$geocode_engine, useNA = 'always')
  # table(rais_geocoded_filter_new$type_year_input, useNA = 'always')
  
  # primeiro trazer as coordenadas do gmaps para as novas coordenadas
  rais_geocoded_filter_new[estabs_problema_geocoded_dt, on = "id_estab",
                           c("Addr_type", "matched_address", "lon", "lat", "geocode_engine") :=
                             list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine)]
  
  
  # depois, trazer essas para as coordendas completas do ano
  # 10.2) Fazer a substituicao
  rais_geocoded_filter[rais_geocoded_filter_new, on = "id_estab",
                       c("Addr_type", "matched_address", "lon", "lat", "geocode_engine") :=
                         list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine)]
  
  
  
  # table(rais_geocoded_filter$Addr_type, useNA = 'always')
  # table(rais_geocoded_filter$geocode_engine, useNA = 'always')
  # table(rais_geocoded_filter$type_year_input, useNA = 'always')
  
  
  # nrow(rais_geocoded_filter)
  # nrow(distinct(rais_geocoded_filter, id_estab))
  
  # 10.4) Salvar
  write_rds(rais_geocoded_filter, 
            sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_filter_geocoded_gmaps.rds", ano, ano), compress = 'gz')
  
  
}





