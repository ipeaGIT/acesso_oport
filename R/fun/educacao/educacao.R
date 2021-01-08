


educacao_geocode <- function(ano, run_gmaps = FALSE) {
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1) Ler das escolas do ano -----------------------------------------------------------------------------
  
  escolas <- fread(sprintf("../../data-raw/censo_escolar/%s/censo-escolar_escolas_%s.CSV", ano, ano))
  
  # filter municipalties
  escolas <- janitor::clean_names(escolas)
  escolas <- escolas[co_municipio %in% munis_df$code_muni]
  
  # only public
  escolas <- escolas[tp_dependencia %in% c(1, 2, 3)]
  # only active
  escolas <- escolas[tp_situacao_funcionamento == 1]
  
  # abrir as coordenadas
  escolas_coords <- fread("../../data-raw/censo_escolar/escolas_inep_2020.csv",
                          encoding = "UTF-8")
  # filter municipalties
  escolas_coords <- janitor::clean_names(escolas_coords)
  escolas_coords[, muni_uf := tolower(paste0(municipio, "-", uf))]
  escolas_coords[, muni_uf := stringi::stri_trans_general(str = muni_uf, 
                                                          id = "Latin-ASCII")]
  escolas_coords <- escolas_coords %>% 
    select(co_entidade = codigo_inep, endereco, lon = longitude, lat = latitude)
  
  
  # join to create escolas geo
  escolas_geo <- merge(
    escolas,
    escolas_coords,
    by = "co_entidade",
    all.x = TRUE
  )
  
  
  # table(nchar(escolas_geo$co_entidade))
  
  
  # get coordinatess from the previous year
  ano_previous <- ano - 1
  
  if (ano != 2017) {
    
    escolas_previous_geo <- read_rds(sprintf("../../data/acesso_oport/censo_escolar/%s/educacao_inep_final_%s.rds", ano_previous, ano_previous)) %>%
      select(co_entidade, SearchedAddress,  PrecisionDepth, MatchedAddress, geocode_engine, lon, lat) %>%
      filter(geocode_engine == "gmaps_prob1") %>%
      filter(!is.na(lon)) %>% setDT()
    
    # table(nchar(escolas_previous_geo$co_entidade))
    
    
    
    
    escolas_geo[, co_entidade := as.character(co_entidade)]
    escolas_geo[escolas_previous_geo, on = "co_entidade", 
                c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                  list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]
  }
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. Identificar CENSO ESCOLAR com lat/long problematicos
  
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
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Checar isso aqui e nos scripts de saude e educacao
  
  # usao em 2019
  #  setDT(escolas_filt)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  
  # Acho que esse aqui funciona melhor
  setDT(escolas_geo)[, ndigitos := nchar( gsub("^.*\\.","", lat) )]
  A_estbs_pouco_digito <- escolas_geo[ ndigitos <=2,]
  
  
  # B) fora dos limites do municipio
  
  # carrega shapes
  shps <- purrr::map_dfr(dir("../../data-raw/municipios/2019", recursive = TRUE, full.names = TRUE), read_rds) %>% 
    as_tibble() %>% 
    st_sf(crs = 4326)
  
  # convert para sf
  censoescolar_df_coords_fixed_df <- escolas_geo[!(is.na(lat))] %>% 
    st_as_sf( coords = c('lon', 'lat'), crs = 4326)
  
  temp_intersect <- sf::st_join(censoescolar_df_coords_fixed_df, shps)
  
  # escolas que cairam fora de algum municipio
  B_muni_fora <- subset(temp_intersect, is.na(name_muni))
  
  
  # C) Lat lon NA (feito mais a frente)
  
  
  
  # D) mais de 5 estabelecimentos com coordenadas iguais
  
  # junta lat lon
  escolas_geo$latlon <- paste0(escolas_geo$lat, '/', escolas_geo$lon)
  
  # freq de lat lon repetido
  tab_latlon <- escolas_geo %>% count(latlon, sort = T)
  latlon_problema <- subset(tab_latlon, n >3 & latlon != "NA/NA")
  
  
  
  # juntar todos municipios com erro de lat/lon
  munis_problemaA <- subset(escolas_geo, co_entidade %in% A_estbs_pouco_digito$co_entidade ) # 10 obs
  munis_problemaB <- subset(escolas_geo, co_entidade %in% B_muni_fora$co_entidade ) # 11 obs
  munis_problemaC <- escolas_geo[ is.na(lat), ] # 683 obs
  munis_problemaD <- subset(escolas_geo, latlon %in% latlon_problema$latlon) # 20 obs
  
  munis_problema <- rbind(munis_problemaA, munis_problemaB, munis_problemaC, munis_problemaD) # 1072 obs
  munis_problema <- dplyr::distinct(munis_problema, co_entidade, .keep_all=T) # remove duplicates, 1072 obs
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. Usando google maps -----------------------------------------
  
  # select only address that are not NA
  munis_problema_enderecos <- munis_problema %>% filter(!is.na(endereco))
  
  # lista de enderecos com problema
  enderecos <- munis_problema_enderecos$endereco # 724 obs
  
  # registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1[4])
  
  if (run_gmaps) {
    
    a <- menu(choices = c("Yes", "No"), title = sprintf("Are  you sure you wanna send %s address to Google Maps?", length(enderecos)))
    
    if (a == 1) {
      
      message("Running gmaps, this may take a while")
      
      # geocode
      coordenadas_google1 <- ggmap::geocode(enderecos, output = "all")
      
      # identify list names as co_entidade
      names(coordenadas_google1) <- munis_problema_enderecos$co_entidade
      
      # save
      write_rds(coordenadas_google1, sprintf("../../data/acesso_oport/censo_escolar/%s/geocode/escolas_geocode_%s_output_google1.rds", ano, ano))
    
      
      }
    
  
  } else {
    
    coordenadas_google1 <- read_rds(sprintf("../../data/acesso_oport/censo_escolar/%s/geocode/escolas_geocode_%s_output_google1.rds", ano, ano))
  }
  
  
  
  # build df with result from ggmap::geocode
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
  
  
  estabs_problema_geocoded <- lapply(coordenadas_google1, create_dt)
  # estabs_problema_geocoded <- lapply(coordenadas_google1, possibly(create_dt, otherwise = "erro"))
  
  # rbind as data.table
  estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "co_entidade",
                                           use.names = TRUE)
  
  # unique(estabs_problema_geocoded_dt$id_estab) %>% length()
  
  # identify searchedaddress
  searchedaddress <- filter(munis_problema_enderecos, co_entidade %in% names(coordenadas_google1)) %>%
    mutate(SearchedAddress = endereco) %>% select(co_entidade, SearchedAddress) %>%
    distinct(co_entidade, .keep_all = TRUE)
  estabs_problema_geocoded_dt <- left_join(estabs_problema_geocoded_dt, searchedaddress, by = "co_entidade") %>% setDT()
  
  # identify problem
  estabs_problema_geocoded_dt[, geocode_engine := 'gmaps_prob1']
  
  # identify quality
  estabs_problema_geocoded_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]
  
  # bring adress that didnt have an address
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
                                       without_address)
  
  
  
  #### GOOGLE 2, so ceps ------------------------
  
  # ainda ha escolas mal georreferenciadas!
  # identificar essas escolas e separa-las
  # convert para sf
  escolas_google_mal_geo <- estabs_problema_geocoded_dt %>%
    filter(!is.na(lat)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_buffer(0.0008)) %>%
    # escolas que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(name_muni)) %>%
    select(co_entidade, SearchedAddress)
  
  # mapview(escolas_google_mal_geo)
  
  
  # identify these address as outside the city
  estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[co_entidade %in% escolas_google_mal_geo$co_entidade,
                                                             PrecisionDepth := 'address_outside_city']
  
  # table(estabs_problema_geocoded_dt$PrecisionDepth, useNA = 'always')
  
  
  # bring to the original dataset
  # # bring CEP's fixed to the original fixed dataset problems 1 and 2
  # rais_problema1e2_geocoded[cep_problema_geocoded_dt_fixed, on = "id_estab",
  #                           c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
  #                             list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]
  
  # identify columns 
  escolas_geo[, co_entidade := as.character(co_entidade)]
  escolas_geo[, MatchedAddress := endereco]
  escolas_geo[, SearchedAddress := endereco]
  escolas_geo[, PrecisionDepth := "inep"]
  escolas_geo[, geocode_engine := "inep"]
  
  escolas_geo[estabs_problema_geocoded_dt, on = "co_entidade",
              c("MatchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
                list(i.MatchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]  
  
  # table(escolas_geo$PrecisionDepth, useNA = 'always')
  # table(escolas_geo$geocode_engine, useNA = 'always')
  
  # saveit
  write_rds(escolas_geo,
            sprintf("../../data/acesso_oport/censo_escolar/%s/censo-escolar_escolas_geocoded_%s.rds", ano, ano))
  
}


