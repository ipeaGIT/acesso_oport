#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Leitura e filtro de dados do censo escolar

## info:
# dados originais fornecidos pelo INEP


# carregar bibliotecas
source('./R/fun/setup.R')

# ATENCAO ####################################################################
# ESSE SCRIPT SO FUNCIONA PARA O ANO DE 2019!

# Determinar o ano
ano <- 2019

# Select the corerspondent munis_df
munis_df <- get(sprintf("munis_df_%s", ano))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Ler dados do INEP -----------------------------------------------------------------------------

escolas <- fread("../data-raw/censo_escolar/2019/ESCOLAS_APLI_CATALOGO_ABR2019.csv")
nrow(escolas) # 226251 obs 






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2) Limpar dados do INEP ---------------------------------

# filtrar so dos nossos municipios, escolas publicas
escolas_filt <- escolas %>%
  filter(CO_MUNICIPIO %in% munis_df$code_muni) %>%
  filter(CATEGORIA_ADMINISTRATIVA == "Pública") %>%
  rename(lon = LONGITUDE, lat = LATITUDE)
nrow(escolas_filt) # 12861 obs


# excluir escolas paralisadas
escolas_filt <- escolas_filt %>% filter(RESTRICAO_ATENDIMENTO != "ESCOLA PARALISADA")
nrow(escolas_filt) # 12259 obs



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
  setDT(escolas_filt)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  A_estbs_pouco_digito <- escolas_filt[ ndigitos <=2,]


# B) fora dos limites do municipio
  
  # carrega shapes
  shps <- purrr::map_dfr(dir("../data-raw/municipios/2019", recursive = TRUE, full.names = TRUE), read_rds) %>% 
    as_tibble() %>% 
    st_sf(crs = 4326)
  
  # convert para sf
  censoescolar2019_df_coords_fixed_df <- escolas_filt[!(is.na(lat))] %>% 
    st_as_sf( coords = c('lon', 'lat'), crs = 4326)
  
  temp_intersect <- sf::st_join(censoescolar2019_df_coords_fixed_df, shps)

  # escolas que cairam fora de algum municipio
  B_muni_fora <- subset(temp_intersect, is.na(name_muni))
  
  
# C) Lat lon NA (feito mais a frente)
  
  
  
# D) mais de 5 estabelecimentos com coordenadas iguais
  
  # junta lat lon
  escolas_filt$latlon <- paste0(escolas_filt$lat, '/', escolas_filt$lon)
  
  # freq de lat lon repetido
  tab_latlon <- escolas_filt %>% count(latlon, sort = T)
  latlon_problema <- subset(tab_latlon, n >3 & latlon != "NA/NA")
    


# juntar todos municipios com erro de lat/lon
  munis_problemaA <- subset(escolas_filt, CO_ENTIDADE %in% A_estbs_pouco_digito$CO_ENTIDADE ) # 14 obs
  munis_problemaB <- subset(escolas_filt, CO_ENTIDADE %in% B_muni_fora$CO_ENTIDADE ) # 9 obs
  munis_problemaC <- escolas_filt[ is.na(lat), ] # 1013 obs
  munis_problemaD <- subset(escolas_filt, latlon %in% latlon_problema$latlon) # 36 obs
  
  munis_problema <- rbind(munis_problemaA, munis_problemaB, munis_problemaC, munis_problemaD) # 1072 obs
  munis_problema <- dplyr::distinct(munis_problema, CO_ENTIDADE, .keep_all=T) # remove duplicates, 1072 obs


# ajeitar enderecos para o galileo
# quebrar a string de endereco nos pontos
# colunas: logradouro, cidade, bairro, cep, uf
teste <- munis_problema %>%
  separate(ENDERECO, c("logradouro", "bairro", "cep"), "\\.") %>%
  # tirar espacos em branco
  mutate(bairro = trimws(bairro, which = "both")) %>%
  # se a coluna do bairro for vazia, significa que o endereco nao tem bairro. transpor essa coluna
  # para a coluna do cep
  mutate(cep = ifelse(grepl("^\\d{5}.*", bairro), bairro, cep)) %>%
  # e apagar os bairros com cep
  mutate(bairro = ifelse(grepl("^\\d{5}.*", bairro), "", bairro)) %>%
  # agora separar so o cep
  mutate(cep1 = str_extract(cep, "\\d{5}-\\d{3}")) %>%
  # extrair cidade e uf
  mutate(cidade_uf = gsub("(\\d{5}-\\d{3}) (.*)$", "\\2", cep)) %>%
  # tirar espacos em branco
  mutate(cidade_uf = trimws(cidade_uf, "both")) %>%
  # separar cidade e uf
  separate(cidade_uf, c("cidade", "uf"), "-",  remove = TRUE) %>%
  mutate(cidade = trimws(cidade, "both")) %>%
  mutate(uf = trimws(uf, "both")) %>%
  # selecionar colunas
  select(CO_ENTIDADE, rua = logradouro, cidade, bairro, cep = cep1, uf)

# salvar input para o galileo
write_delim(teste, "../data-raw/censo_escolar/2019/escolas_2019_input_galileo.csv", delim = ";")  
  
# Para o Galileo: 1072 obs


### RODAR GALILEO--------------
# depois de rodar o galileo...

# abrir output do galileo
educacao_output_galileo <- fread("../data-raw/censo_escolar/2019/Òescolas_2019_output_galileo.csv") %>%
  # filtrar somente os maiores que 2 estrelas
  filter(PrecisionDepth %in% c("4 Estrelas")) %>%
  # substituir virgula por ponto
  mutate(Latitude = str_replace(Latitude, ",", "\\.")) %>%
  mutate(Longitude = str_replace(Longitude, ",", "\\.")) %>%
  # selecionar colunas
  select(CO_ENTIDADE, lat = Latitude, lon = Longitude) %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))

# juntar com a base anterior completa para atualizar lat e lon q veio do Galileo
  setDT(escolas_filt)[educacao_output_galileo, on='CO_ENTIDADE', c('lat', 'lon') := list(i.lat, i.lon)]
  summary(escolas_filt$lon) # 505 NA's, mais nos valores de menos que 2 estrelas do galileo

# para esses, sera utilizado o geocode do google maps 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Recupera a info lat/long que falta usando google maps -----------------------------------------

#### GOOGLE 1, endereco completo ------------------------
  
  # A) Escolas com lat/long de baixa precisao (1 ou 2 digitos apos casa decimal)
  setDT(escolas_filt)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  lat_impreciso <- subset(escolas_filt, ndigitos <=2)$CO_ENTIDADE

  # continuam imprecisos
  A_escolas_lat_impreciso <- subset(escolas_filt, CO_ENTIDADE %in% lat_impreciso)


  # B) Saude com lat/long missing  
  CO_ENTIDADE_lat_missing <- subset(escolas_filt, is.na(lat))$CO_ENTIDADE
  B_escolas_problema <- subset(escolas_filt, CO_ENTIDADE %in% CO_ENTIDADE_lat_missing)

  # C) Saude com 1, 2 e 3 estrelas do galileo
  escolas_galileo_baixo <- fread("../data-raw/censo_escolar/escolas_2019_output_galileo.csv") %>%
    # selecionar so os 1, 2 e 3 estrelas
    filter(PrecisionDepth %in% c("1 Estrela", "2 Estrelas", "3 Estrelas")) %>%
    .$CO_ENTIDADE
  C_escolas_galileo_baixo <- subset(escolas_filt, CO_ENTIDADE %in% escolas_galileo_baixo)
  
  
  # lista de enderecom com problema
  escolas_problema_gmaps <- rbind(A_escolas_lat_impreciso, B_escolas_problema, C_escolas_galileo_baixo) %>%
    distinct(CO_ENTIDADE, .keep_all = TRU8E)
  nrow(escolas_problema_gmaps) # 526 obs
  
  

# registrar Google API Key
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)


# lista de enderecos com problema
# cnes_problema[, endereco := paste0(CEP,", ", MUNICÍPIO) ]
enderecos <- escolas_problema_gmaps$ENDERECO # total de 660 escolas


# geocode
coordenadas_google <- lapply(X=enderecos, ggmap::geocode) %>% rbindlist()


# Link escolas com lat lon do geocode
escolas_lat_missing_geocoded <- cbind(escolas_problema_gmaps %>% select(-lon, -lat), coordenadas_google)


summary(escolas_lat_missing_geocoded$lat) # Google nao encontrou 3 casos

# atualiza lat lon a partir de google geocode
escolas_filt[, lat := as.numeric(lat)][, lon := as.numeric(lon)]
setDT(escolas_filt)[escolas_lat_missing_geocoded, on='CO_ENTIDADE', c('lat', 'lon') := list(i.lat, i.lon) ]

summary(escolas_filt$lat)


subset(escolas_filt, !is.na(lat)) %>%
  to_spatial() %>%
  mapview()



#### GOOGLE 2, so ceps ------------------------

# ainda ha escolas mal georreferenciadas!
# identificar essas escolas e separa-las
# convert para sf
escolas_google_mal_geo <- escolas_filt %>%
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  sf::st_join(shps) %>%
  # escolas que cairam fora de algum municipio, a serem georreferenciadas na unha
  filter(is.na(name_muni)) %>%
  select(CO_ENTIDADE, ENDERECO)

mapview(escolas_google_mal_geo)

# Retorna somente os ceps dos que deram errado para jogar no google API
somente_ceps <- gsub("(^.*)(\\d{5}-\\d{3}.*$)", "\\2", escolas_google_mal_geo$ENDERECO) # total de 34 escolas

# consulta google api
coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode) %>% rbindlist()


# atualiza lat lon a partir de google geocode
escolas_google_bom_geo <- cbind(as.data.frame(escolas_google_mal_geo), coordenadas_google_cep)
setDT(escolas_filt)[escolas_google_bom_geo, on='CO_ENTIDADE', c('lat', 'lon') := list(i.lat, i.lon) ]

subset(escolas_filt, !is.na(lat)) %>%
  to_spatial() %>%
  mapview()




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. trazer escolas do censo escolar 2018 ----------------------------------------------------------
  
  # O Censo escolar traz dado codificada da etapa de ensino. Para as informacoes missing, a gente usa
  # a info de etapa de ensino informada no dado do INEP geo

# colunas de interesse: 
colunas <- c("CO_ENTIDADE", "NO_ENTIDADE",
             "IN_COMUM_CRECHE", "IN_COMUM_PRE", 
             "IN_COMUM_FUND_AI", "IN_COMUM_FUND_AF", 
             "IN_COMUM_MEDIO_MEDIO", "IN_COMUM_MEDIO_NORMAL",
             "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE", 
             "IN_COMUM_MEDIO_INTEGRADO", "IN_PROFISSIONALIZANTE",
             "IN_ESP_EXCLUSIVA_FUND_AI", "IN_ESP_EXCLUSIVA_FUND_AF",
             "IN_ESP_EXCLUSIVA_MEDIO_MEDIO", "IN_ESP_EXCLUSIVA_MEDIO_INTEGR",
             "IN_ESP_EXCLUSIVA_MEDIO_NORMAL","IN_COMUM_EJA_MEDIO","IN_COMUM_EJA_PROF",
             "IN_ESP_EXCLUSIVA_EJA_MEDIO","IN_ESP_EXCLUSIVA_EJA_PROF","IN_COMUM_PROF",
             "IN_ESP_EXCLUSIVA_PROF","IN_COMUM_EJA_FUND","IN_ESP_EXCLUSIVA_EJA_FUND",
             "IN_LOCAL_FUNC_UNID_PRISIONAL", "IN_LOCAL_FUNC_PRISIONAL_SOCIO", # escolas prisionais
             "QT_FUNCIONARIOS")            





# abrir
escolas_censo <- fread("../data-raw/censo_escolar/censo_escolar_escolas_2018.CSV", select=colunas ) %>%
  # selecionar escolas
  filter(CO_ENTIDADE %in% escolas_filt$CO_ENTIDADE) %>%
  # identificar o tipo de ensino em cada escola
  mutate(mat_infantil = ifelse(IN_COMUM_CRECHE == 1 | 
                                 IN_COMUM_PRE == 1 |
                               IN_ESP_EXCLUSIVA_CRECHE == 1 |
                               IN_ESP_EXCLUSIVA_PRE ==1, 1, 0)) %>%
  
  mutate(mat_fundamental = ifelse(IN_COMUM_FUND_AI == 1 | 
                                    IN_COMUM_FUND_AF == 1 |
                                    IN_ESP_EXCLUSIVA_FUND_AI ==1 |
                                    IN_ESP_EXCLUSIVA_FUND_AF ==1 |
                                    IN_COMUM_EJA_FUND ==1 |
                                    IN_ESP_EXCLUSIVA_EJA_FUND ==1, 1, 0)) %>%
  mutate(mat_medio = ifelse(IN_COMUM_MEDIO_MEDIO == 1 |
                              IN_COMUM_MEDIO_NORMAL == 1 |
                              IN_COMUM_MEDIO_INTEGRADO ==1 |
                              IN_PROFISSIONALIZANTE ==1 |
                              IN_ESP_EXCLUSIVA_MEDIO_MEDIO ==1 |
                              IN_ESP_EXCLUSIVA_MEDIO_INTEGR ==1 |
                              IN_ESP_EXCLUSIVA_MEDIO_NORMAL ==1 |
                              IN_COMUM_EJA_MEDIO ==1 |
                              IN_COMUM_EJA_PROF ==1 |
                              IN_ESP_EXCLUSIVA_EJA_MEDIO ==1 |
                              IN_ESP_EXCLUSIVA_EJA_PROF ==1 |
                              IN_COMUM_PROF ==1 |
                              IN_ESP_EXCLUSIVA_PROF ==1, 1, 0)) %>%
  # Selecionar variaveis
  select(CO_ENTIDADE, NO_ENTIDADE, mat_infantil, mat_fundamental, mat_medio, IN_LOCAL_FUNC_UNID_PRISIONAL, IN_LOCAL_FUNC_PRISIONAL_SOCIO, QT_FUNCIONARIOS)


# Identifica escolas priosionais
  escolas_prisionais <- subset(escolas_censo, IN_LOCAL_FUNC_UNID_PRISIONAL ==1 | IN_LOCAL_FUNC_PRISIONAL_SOCIO ==1)$CO_ENTIDADE



# juntar com a base nova
escolas_etapa <- escolas_filt %>%
  left_join(escolas_censo, by = c("CO_ENTIDADE"))


# remove escolas prisionais
  escolas_etapa_fim <- subset(escolas_etapa, CO_ENTIDADE %nin% escolas_prisionais)
  escolas_etapa_fim$IN_LOCAL_FUNC_UNID_PRISIONAL <- NULL
  escolas_etapa_fim$IN_LOCAL_FUNC_PRISIONAL_SOCIO <- NULL




# codifica etapa de ensino pela string
setDT(escolas_etapa_fim)[, mat_infantil := ifelse(is.na(mat_infantil) & OFERTA_ETAPA_MODALIDADE %like% 'Creche|Pré-escola', 1, 
                                              mat_infantil)]
setDT(escolas_etapa_fim)[, mat_fundamental := ifelse(is.na(mat_fundamental) & OFERTA_ETAPA_MODALIDADE %like% 'Ensino Fundamental', 1, 
                                                 mat_fundamental)]
setDT(escolas_etapa_fim)[, mat_medio := ifelse(is.na(mat_medio) & OFERTA_ETAPA_MODALIDADE %like% "Ensino Médio|nível Médio|Curso Profissional|Curso Técnico", 1, 
                                           mat_medio)]
  


table(escolas_etapa_fim$mat_infantil, useNA = "always")
table(escolas_etapa_fim$mat_fundamental, useNA = "always")
table(escolas_etapa_fim$mat_medio, useNA = "always")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. salvar ----------------------------------------------------------------------------------------

# salvar
  write_rds(escolas_etapa_fim, "../data/censo_escolar/2019/educacao_inep_2019.rds")


