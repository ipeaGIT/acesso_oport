# carregar bibliotecas
source('./R/fun/setup.R')

# dados de educacao novos

escolas <- fread("../data-raw/censo_escolar/ESCOLAS_APLI_CATALOGO_ABR2019.csv")


# filtrar so dos nossos municipios
escolas_filt <- escolas %>%
  filter(CO_MUNICIPIO %in% munis_df$code_muni) %>%
  filter(CATEGORIA_ADMINISTRATIVA == "Pública")


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



# pegar missiings e ajeitar enderecos para o galileu
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
escolas_filt_fim <- escolas_filt %>%
  left_join(output_galileo, by = c("CO_ENTIDADE")) %>%
  mutate(LATITUDE = ifelse(is.na(LATITUDE), Latitude, LATITUDE)) %>%
  mutate(LONGITUDE = ifelse(is.na(LONGITUDE), Longitude, LONGITUDE)) %>%
  # selecionar colunas
  select(CO_MUNICIPIO, CO_ENTIDADE, lon = LONGITUDE, lat = LATITUDE)





# ##################################################
# trazer escolas do censo escolar 2018 ----------------------------------------
# ##################################################


# colunas de interesse: 
colunas <- c("CO_ENTIDADE", 
             "IN_COMUM_CRECHE", "IN_COMUM_PRE", 
             "IN_COMUM_FUND_AI", "IN_COMUM_FUND_AF", 
             "IN_COMUM_MEDIO_MEDIO", "IN_COMUM_MEDIO_NORMAL")

# abrir
escolas_censo <- fread("../data-raw/censo_escolar/censo_escolar_escolas_2018.CSV") %>%
  select(!!colunas) %>%
  # selecionar escolas
  filter(CO_ENTIDADE %in% escolas_filt$CO_ENTIDADE) %>%
  # identificar o tipo de ensino em cada escola
  mutate(mat_infantil = ifelse(IN_COMUM_CRECHE == 1 | IN_COMUM_PRE == 1, 1, 0)) %>%
  mutate(mat_fundamental = ifelse(IN_COMUM_FUND_AI == 1 | IN_COMUM_FUND_AF == 1, 1, 0)) %>%
  mutate(mat_medio = ifelse(IN_COMUM_MEDIO_MEDIO == 1 | IN_COMUM_MEDIO_NORMAL == 1, 1, 0)) %>%
  # Selecionar variaveis
  select(CO_ENTIDADE, mat_infantil, mat_fundamental, mat_medio)
  

# juntar com a base nova
escolas_final <- escolas_filt_fim %>%
  left_join(escolas_censo, by = c("CO_ENTIDADE")) %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(mat_infantil))

# salvar
escolas_final %>%
  write_rds("../data/censo_escolar/educacao_inep_2019.rds")
