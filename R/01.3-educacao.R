#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Leitura e filtro de dados do censo escolar

## info:
# dados originais fornecidos pelo INEP


# carregar bibliotecas
source('./R/fun/setup.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Ler dados do INEP novos ------------------------------------------------------------------

escolas <- fread("../data-raw/censo_escolar/ESCOLAS_APLI_CATALOGO_ABR2019.csv")


# filtrar so dos nossos municipios
escolas_filt <- escolas %>%
  filter(CO_MUNICIPIO %in% munis_df$code_muni) %>%
  filter(CATEGORIA_ADMINISTRATIVA == "Pública")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Corrigir Lat Long  ---------------------------------

### Fix lat long info


# trazer a quantidade de digitos para o cnes19
escolas_df_digitos <- escolas_filt %>%
  rename(code_muni = IBGE) %>%
  # selecionar so os municipios do projeto
  filter(code_muni %in% substr(munis_df$code_muni, 1, 6)) %>%
  mutate(code_muni = as.character(code_muni)) %>%
  left_join(munis, by = "code_muni")

# criar dataframe com as coordenadas ajeitadas
cnes19_df_coords_fixed <- cnes19_df_digitos %>%
  # primeiro, tirar tudo que for ponto ou virgula
  mutate(lon = gsub("(\\.|,)", "", LONGITUDE),
         lat = gsub("(\\.|,)", "", LATITUDE)) %>%
  # tirar sinal de negativo
  mutate(lon = gsub("-", "", lon),
         lat = gsub("-", "", lat)) %>%
  # o ponto na longitude vai ser sempre depois do segundo numerico, e vai ser sempre negativo
  mutate(lon = sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lon)) %>%
  # o ponto na latitude vai depender do nchar
  mutate(lat = ifelse(lat_digits == 1, sub("(^\\d{1})(\\d+)", "-\\1\\.\\2", lat),
                      sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lat))) %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))

# # teste
# cnes19_df_coords_fixed %>%
#   filter(!is.na(lon)) %>%
#   to_spatial() %>%
#   mapview()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Checar dados de Lat Long e corrigir erros com Galileo ------------------------------------------------------------------

# ver missings
escolas_filt %>%
  filter(CATEGORIA_ADMINISTRATIVA == "Pública") %>%
  mutate(loc_missing = ifelse(is.na(LATITUDE), "sim", "nao")) %>%
  group_by(NO_MUNICIPIO, loc_missing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(NO_MUNICIPIO) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n/sum) %>%
  # View()
  ggplot()+
  geom_col(aes(x = NO_MUNICIPIO, y = perc, fill = loc_missing))+
  scale_y_percent()


ggsave("figure/diagnostico_escolas.png")



# pegar missings e ajeitar enderecos para o galileu
escolas_filt_miss <- escolas_filt %>%
  filter(is.na(LATITUDE)) %>%
  # pegar so o codigo e o endereco
  select(CO_ENTIDADE, ENDERECO)

# quebrar a string de endereco nos pontos
# colunas: logradouro, cidade, bairro, cep, uf
teste <- escolas_filt_miss %>%
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
write_delim(teste, "escolas_2019_input_galileo.csv", delim = ";")  
  















### RODAR GALILEO--------------
# depois de rodar o galileo...

# abrir output do galileo
output_galileo <- fread("../data-raw/censo_escolar/escolas_2019_output_galileo.csv") %>%
  # filtrar somente os maiores que 2 estrelas
  filter(PrecisionDepth %nin% c("1 Estrela", "2 Estrelas")) %>%
  # substituir virgula por ponto
  mutate(Latitude = str_replace(Latitude, ",", "\\.")) %>%
  mutate(Longitude = str_replace(Longitude, ",", "\\.")) %>%
  # selecionar colunas
  select(CO_ENTIDADE, Latitude, Longitude)

# juntar com a base original
escolas_galileo <- escolas_filt %>%
  left_join(output_galileo, by = c("CO_ENTIDADE")) %>%
  mutate(LATITUDE = ifelse(is.na(LATITUDE), Latitude, LATITUDE)) %>%
  mutate(LONGITUDE = ifelse(is.na(LONGITUDE), Longitude, LONGITUDE)) %>%
  # selecionar colunas
  select(CO_MUNICIPIO, CO_ENTIDADE, lon = LONGITUDE, lat = LATITUDE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. trazer escolas do censo escolar 2018 ------------------------------------------------------------------
  # O Censo escolar traz dado codificada da etapa de ensino. Para as informacoes missing, a gente usa
  # a info de etapa de ensino informada no dado do INEP geo

# colunas de interesse: 
colunas <- c("CO_ENTIDADE", 
             "IN_COMUM_CRECHE", "IN_COMUM_PRE", 
             "IN_COMUM_FUND_AI", "IN_COMUM_FUND_AF", 
             "IN_COMUM_MEDIO_MEDIO", "IN_COMUM_MEDIO_NORMAL")

# abrir
escolas_censo <- fread("../data-raw/censo_escolar/censo_escolar_escolas_2018.CSV") %>%
  select(!!colunas) %>%
  # selecionar escolas
  filter(CO_ENTIDADE %in% escolas_galileo$CO_ENTIDADE) %>%
  # identificar o tipo de ensino em cada escola
  mutate(mat_infantil = ifelse(IN_COMUM_CRECHE == 1 | IN_COMUM_PRE == 1, 1, 0)) %>%
  mutate(mat_fundamental = ifelse(IN_COMUM_FUND_AI == 1 | IN_COMUM_FUND_AF == 1, 1, 0)) %>%
  mutate(mat_medio = ifelse(IN_COMUM_MEDIO_MEDIO == 1 | IN_COMUM_MEDIO_NORMAL == 1, 1, 0)) %>%
  # Selecionar variaveis
  select(CO_ENTIDADE, mat_infantil, mat_fundamental, mat_medio)

# 631 escolas sem informacao alguma
sum( is.na(escolas_censo$mat_infantil) )

# juntar com a base nova
escolas_etapa <- escolas_galileo %>%
  left_join(escolas_censo, by = c("CO_ENTIDADE")) # %>%
  # filter(!is.na(lon)) %>%
  # filter(!is.na(mat_infantil))

# Quantas escolas estao com dados missing (lat/long e nivel de ensino) por municipio
setDT(escolas_etapa)[, .( total= .N,
                          lat_miss = sum(is.na(lat)),
                          lat_miss_p = 100*sum(is.na(lat) /.N),
                          ensino_miss = sum(is.na(mat_infantil)),
                          ensino_miss_p = 100*sum(is.na(mat_infantil))/.N), by=CO_MUNICIPIO ][order(-lat_miss_p)]



table(escolas_etapa$mat_infantil)
table(escolas_etapa$mat_fundamental)
table(escolas_etapa$mat_medio)

# Recupera informacao de etapa de ensino do censo escolar informado no dado enviado pelo INEP geo
  escolas_etapa <- left_join(escolas_etapa, select(escolas, CO_ENTIDADE, OFERTA_ETAPA_MODALIDADE), by='CO_ENTIDADE')

  # tests
  # subset(test,mat_infantil !=1 &  mat_fundamental !=1 & mat_medio ==1 )$OFERTA_ETAPA_MODALIDADE %>% table %>% View()
  
# codifica etapa de ensino pela string
  setDT(escolas_etapa)[, mat_infantil := ifelse(is.na(mat_infantil) & OFERTA_ETAPA_MODALIDADE %like% 'Creche|Pré-escola', 1, 0)]
  setDT(escolas_etapa)[, mat_fundamental := ifelse(is.na(mat_fundamental) & OFERTA_ETAPA_MODALIDADE %like% 'Ensino Fundamental', 1, 0)]
  setDT(escolas_etapa)[, mat_medio := ifelse(is.na(mat_medio) & OFERTA_ETAPA_MODALIDADE %like% "Ensino Médio|nível Médio|Curso Profissional|Curso Técnico", 1, 0)]
  
  
 
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Recupera a info lat/long que falta usando google maps ------------------------------------------------------------------
  
# Escolas com lat/long de baixa precisa (1 ou 2 digitos apos casa decimal)
  setDT(escolas_etapa)[, ndigitos := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  lat_impreciso <- subset(escolas_etapa, ndigitos <=2)$CO_ENTIDADE
  escolas_lat_impreciso <- subset(escolas, CO_ENTIDADE %in% lat_impreciso)
  

# Escolas com lat/long missing  
  CO_ENTIDADE_lat_missing <- subset(escolas_etapa, is.na(lat))$CO_ENTIDADE
  escolas_lat_missing <- subset(escolas, CO_ENTIDADE %in% CO_ENTIDADE_lat_missing)

# escolas problema
  escolas_problema <- rbind(escolas_lat_impreciso, escolas_lat_missing)
  
# lista de enderecom com problema
  enderecos <- escolas_problema$ENDERECO

# registar Google API Key
  register_google(key = "")

# geocode
  coordenadas_google <- lapply(X=enderecos, ggmap::geocode) %>% rbindlist()
  summary(escolas_lat_missing_geocoded$lat) # Google nao encontrou 3 casos
  
# Link escolas com lat lon do geocode
  escolas_lat_missing_geocoded <- cbind(escolas_lat_missing, coordenadas_google)


# atualiza lat lon a partir de google geocode
  escolas_etapa[, lat := as.numeric(lat)][, lon := as.numeric(lon)]
  setDT(escolas_etapa)[escolas_lat_missing_geocoded, on='CO_ENTIDADE', c('lat', 'lon') := list(i.lat, i.lon) ]
  

summary(escolas$lat)
summary(escolas_lat_missing_geocoded$lat)


subset(escolas_etapa, !is.na(lat)) %>%
  to_spatial() %>%
  mapview()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. trazer escolas do censo escolar 2018 ------------------------------------------------------------------

# salvar
escolas_final %>%
  write_rds("../data/censo_escolar/educacao_inep_2019.rds")


