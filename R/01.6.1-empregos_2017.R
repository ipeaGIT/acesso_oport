#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.6 Leitura e limpeza de dados da RAIS




# carregar bibliotecas
source('./R/fun/setup.R')




### 0) get raw data ------------------------------------

#### companies data
# read raw data
estabs2017 <- fread('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab2017.csv' 
                    #, nrows = 5
                    , colClasses='character')

# subset municipalities
estabs2017_mun <- estabs2017[ codemun %in% substring(munis_df$code_muni, 1,6)]

# subset columns
estabs2017_mun <- estabs2017_mun[, 1:33]

# save
fwrite(estabs2017_mun, '../../data-raw/rais/2017/rais_estabs_raw_2017.csv')


rm(estabs2017_mun, estabs2017)
gc(reset = T)

#### workers data
# read raw data
trabal2017 <- fread('//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil2017.csv'
                    # , nrows = 5
                    , select = c("id_estab", "grau_instr","emp_31dez", 'clas_cnae20', 'uf', 'codemun', 'nat_jur2016')
                    , colClasses='character')


# subset municipalities
nrow(trabal2017)
trabal2017 <- trabal2017[ codemun %in% substring(munis_df$code_muni, 1,6)]
nrow(trabal2017)


# save
fwrite(trabal2017, '../../data-raw/rais/2017/rais_trabal_raw_2017.csv')


##### 1) Filtrar Rais-2017 pessoas-------------------------------------------------------

# Leitura dos dados da RAIS pessoas com colunas que vamos usar
rais_trabs <- data.table::fread("../../data-raw/rais/2017/brasil2017.csv"
                                , select = c("id_estab", "grau_instr", "emp_31dez", 'clas_cnae20', 'uf', 'codemun', 'nat_jur2016')
                                , colClasses='character'
                                # , nrows = 1000
)

unique(rais_trabs$id_estab) %>% length() # 3845034 estabs

# Filtro 0: selecionar so municipios do projeto
gc(reset=T)
rais_filtro_0 <- rais_trabs[codemun %in% substr(munis_df_2019$code_muni, 1, 6) ]

unique(rais_filtro_0$id_estab) %>% length() # 1023954 estabs


# Filtro 1: selecionar so vinculos ativos
rais_filtro_1 <- rais_filtro_0[emp_31dez == 1]


unique(rais_filtro_1$id_estab) %>% length()


# Filtro 2 deletar todas as intituicoes com Natureza Juridica 'publica' (ver ../data-raw/rais/ManualRAIS2018.pdf) pagina 19
# todos que comecam com o numeral 1 sao de administracao publica


# identifica natureza de adm publica
rais_filtro_1[, adm_pub := ifelse( substr(nat_jur2016, 1, 1)==1, 1, 0) ]

# fica apenas com adm publica
rais_filtro_2 <- rais_filtro_1[ adm_pub != 1 ]

# quantos vinculos de natureza juridica publica a gente perde? 23.7%
nrow(rais_filtro_2) / nrow(rais_filtro_1)


# # identify top employers da adm publica
# top_employers <- rais_filtro_1[, .(qtd_vinc_ativos = .N), by=.(id_estab, codemun, adm_pub) ][order(-qtd_vinc_ativos)] 
# top_employers <- top_employers[ adm_pub== 1 ]
# 
# # adiciona nome social e endereco
# top_employers[rais_estabs, on = 'id_estab', c('razao_social', 'logradouro', 'cep') := list(i.razao_social, i.logradouro, i.cep)]
# 
# 
# # total de muni por cnpj raiz
# cnpj_by_mun <- top_employers[, .(quant_mun = length(unique(codemun)),
#                                  qtd_vinc_ativos = sum(qtd_vinc_ativos)), by = .(cnpj_raiz=substring(id_estab , 1, 8) 
#                                                                                  # , logradouro
#                                  ) ]
# 
# 
#   
#   top_employers_fim <- top_employers %>%
#     mutate(cnpj_raiz = substring(id_estab , 1, 8)) %>%
#     left_join(., cnpj_by_mun %>% select(cnpj_raiz, quant_mun), by='cnpj_raiz' ) %>%
#     arrange(desc(qtd_vinc_ativos)) %>%
#     setDT()
# 
#   
#   
# # linha de corte OU cnpj raiz um unico municipio
#   
# a <-   top_employers_fim[ !(quant_mun == 1 & qtd_vinc_ativos > 3000)]
# sum(a$qtd_vinc_ativos) / sum(top_employers_fim$qtd_vinc_ativos) 
# 
# # instituicoes com grande quantidade de vinculos e atuacao descentralizada e que delcara todos vinculos num unico endereco
# 
# # corte bruto retira 80% dos empregos publicos
# top200 <- head(top_employers, n=200)
# sum(top200$qtd_vinc_ativos) / sum(top_employers$qtd_vinc_ativos)



# Filtro 3 deletar todas as empresas publicas (a entidade 2011 eh empresa publica)
rais_filtro_3 <- rais_filtro_2[nat_jur2016 != "2011"]


# Salvar em formato rds para formato rapido
write_rds(rais_filtro_3, "../../data/acesso_oport/rais/2017/rais_2017_ind_filtrada.rds")




####### 2) Categorizar trabalhadores por grau de instrucao  ----------------------------------------------------------------------------------

# Abrir RAIS  em formato rapido rds
rais_trabs <- read_rds("../../data/acesso_oport/rais/2017/rais_2017_ind_filtrada.rds")



# Categorizar trabalhadores por grau de instrucao
rais_cats <- rais_trabs[, instrucao := fifelse(grau_instr %in% c(1:6), "baixo",                                    # menor do que ensino medio (inclui ensino medio incompleto)
                                               fifelse(grau_instr %in% c(7, 8), "medio",                           # ensino medio
                                                       fifelse(grau_instr %in% c(9, 10, 11), "alto", grau_instr)))] # ensino superior


# Calcula quantidade de vinculo por grau de instrucao em cada estabelecimento
rais_fim <- rais_trabs[, .(vinculos = .N), by = .(id_estab, clas_cnae20, instrucao)]

# soma quantidade total de empregados em cada empresa
rais_fim <- rais_fim[, total := sum(vinculos), by = .(id_estab, clas_cnae20)]
head(rais_fim)

# Reshape da base de formato long para wide
rais_fim_wide <- tidyr::spread(rais_fim, instrucao, vinculos, fill = 0)
head(rais_fim_wide)

# Salvar agregado do numero de trabalhadores por empresa
write_rds(rais_fim_wide, "../../data/acesso_oport/rais/rais_2017_vin_instrucao.rds")






### 3) Limpar outliers (empresas que ainda declara muitos trabalhadores na mesma sede) -----------------------------------------------------

# Abrir
rais <- read_rds("../../data/acesso_oport/rais/rais_2017_vin_instrucao.rds")


# 1) Primeiro faz correcao do total de vinculos ouliers por CNAE

# Extrair o cnae do setor
rais <- setDT(rais)[, cnae.setor := substr(clas_cnae20, 1, 2)]

# Extrair os setores desejados
# 35 eletricidade, gas e agua quente OK
# 36 captacao, tratamento e distribuicao de agua OK
# 49 transporte terrestre OK
# 51 transporte aereo OK
# 82 servicoes prestados principalmente a empresas
# 38 limpeza urbana, esgoto e atividades relacionadas (COLETA, TRATAMENTO E DISPOSIÇÃO DE RESÍDUOS; RECUPERAÇÃO DE MATERIAIS 2.0)
# 78 SELEÇÃO, AGENCIAMENTO E LOCAÇÃO DE MÃO-DE-OBRA
# 80 ATIVIDADES DE VIGILÂNCIA, SEGURANÇA E INVESTIGAÇÃO
# 41 CONSTRUÇÃO DE EDIFÍCIOS
# 42 OBRAS DE INFRA-ESTRUTURA
# 43 SERVIÇOS ESPECIALIZADOS PARA CONSTRUÇÃO
# 64 ATIVIDADES DE SERVIÇOS FINANCEIROS
# 81 SERVIÇOS PARA EDIFÍCIOS E ATIVIDADES PAISAGÍSTICAS

# ? 65 seguradoras
# ? 56.2 Serviços de catering, bufê e outros serviços de comida preparada

cnaes_problema <- c("35","36","49","51","82","38", "78", "80", "41", "42", "43", "64", "81")

rais.problema <- setDT(rais)[cnae.setor %in% cnaes_problema]


# Funcoes para identificar ouliet
# valor no percentil 90
quanti<-function(x){quantile(x,probs = 0.9)}
# quantidade acima do percentil 90
quant<-function(x){sum(x>=quantile(x,probs = 0.9))}
# interquantile range s?? dos acima do percentil 90
IQa<-function(x){IQR(x[x>=quantile(x,probs = 0.90)])}
IQb<-function(x){3}
#O valor do percentil 90 somado a 3 vezes o valor do interquantile range
IQe<-function(x){quantile(x,probs = 0.90)+IQR(x[x>=quantile(x,probs = 0.90)])*3}
#quantidade de casos acima desse threshold
IQf<-function(x){sum(x>quantile(x,probs = 0.90)+IQR(x[x>=quantile(x,probs = 0.90)])*3)}

# Aplica funcoes a por setor
iqa<-aggregate(rais.problema$total, by=list(rais.problema$cnae.setor), IQa)
q<-aggregate(rais.problema$total, by=list(rais.problema$cnae.setor), quanti)
qq<-aggregate(rais.problema$total, by=list(rais.problema$cnae.setor), quant)
iqb<-aggregate(rais.problema$total, by=list(rais.problema$cnae.setor), IQb)
iqe<-aggregate(rais.problema$total, by=list(rais.problema$cnae.setor), IQe)
iqf<-aggregate(rais.problema$total, by=list(rais.problema$cnae.setor), IQf)


# rais.problema<-data.table(rais.problema)
rais.problema[,p90:=quantile(total,0.90),by=cnae.setor]
geral<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2])
names(geral)<-c("cnae.setor","quantil","freq","desviointerq","fator","corte","outliers")

rais.problema2 <- merge(rais.problema, setDT(geral), 
                        all.x = TRUE)

rais.problema2$diff<-rep(0,nrow(rais.problema2))

rais.problema2$diff[rais.problema2$total>=rais.problema2$corte] <- 
  rais.problema2$total[rais.problema2$total>=rais.problema2$corte] - rais.problema2$corte[rais.problema2$total>=rais.problema2$corte]

dif<-aggregate(rais.problema2$diff, by=list(rais.problema2$cnae.setor), sum)

geral2<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2],dif[,2])

names(geral2) <- c("cnae.setor","q","freq","desviointerq","fator","corte","outlier","perda")

# criando nova variavel com valores de outliers corrigidos
rais$total_corrigido <- rais$total

#zerando empregos de administracao publica
rais$total_corrigido <- ifelse(rais$cnae.setor=="84",0,rais$total_corrigido)

#tabela com valor de corte por setor
geral3 <- geral2[,c(1,6)]

#colocando esse valor de corte na base
rais <- merge(rais, geral3, 
              by="cnae.setor",
              all.x = TRUE)

#substituindo valores maiores que o corte pelo valor de corte
rais$total_corrigido <- ifelse(rais$cnae.setor %in% cnaes_problema & rais$total_corrigido>rais$corte,rais$corte,rais$total_corrigido)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Corrigir quantidade de empregos por grau de escolaridade


# 2) Aplica fatores de correcao para cada nivel de escolaridade em cada estabelecimento

# Calcular a proporcao que cada um dos vinculos por escolaridade representa dos vinculos totais
rais[,  ":="(prop_baixo = baixo/total,
             prop_medio = medio/total,
             prop_alto = alto/total)]

# Substituir a quantidade de vinculos baixo, medio e alto somente se o estabelecimento recebeu corte
rais[, ":="(alto = ifelse(total_corrigido < total, 
                          round(prop_alto * total_corrigido, 0), alto),
            medio = ifelse(total_corrigido < total,
                           round(prop_medio * total_corrigido, 0), medio),
            baixo = ifelse(total_corrigido < total, 
                           round(prop_baixo * total_corrigido, 0), baixo))]

# Drop colunas que nao vamos usar
rais[, c('corte', 'prop_baixo', 'prop_medio',  'prop_alto') := NULL]



# Salvar Rais de estabelecimentos com numero de vinculos corrigos e quant de pessoas por escolaridade
write_rds(rais, "../../data/acesso_oport/rais/2017/rais_2017_corrigido.rds")



# 4) TRAZER GEOCODE DOS ESTABELECIMENTOS -----------------------------------------------------

rais_estabs <- read_rds("../../data/acesso_oport/rais/2017/rais_2017_corrigido.rds")
rais_estabs_geocode <- read_rds("../../data/acesso_oport/rais/2017/rais_2017_estabs_geocode_final.rds")
rais_estabs_geocode <- select(rais_estabs_geocode, -qt_vinc_ativos)

# pad everyone to 14 characters
rais_estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
rais_estabs_geocode[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

unique(rais_estabs$id_estab) %>% length()
unique(rais_estabs_geocode$id_estab) %>% length()


# join them!
rais_estabs_geocode_end <- merge(
  rais_estabs,
  rais_estabs_geocode,
  by = "id_estab",
  sort = FALSE,
  all.x = TRUE
)

table(rais_estabs_geocode_end$PrecisionDepth, useNA = 'always')
table(rais_estabs_geocode_end$geocode_engine, useNA = 'always')
table(rais_estabs_geocode_end$type_input_galileo, useNA = 'always')


filter(rais_estabs_geocode_end, is.na(geocode_engine)) %>% View()

# save it
write_rds(rais_estabs_geocode_end, "../../data/acesso_oport/rais/2017/rais_estabs_2017_geocoded_all.rds")



# 6) Trazer informacoes de funcionarios de escolas publicas do censo escolar --------------------------
# (a partir do script 01.3-educacao)


# abri rais corrigida
rais <- read_rds("../../data/acesso_oport/rais/2017/rais_estabs_2017_geocoded_all.rds")


table(rais$PrecisionDepth, useNA = 'always')
table(rais$geocode_engine, useNA = 'always')
table(rais$type_input_galileo, useNA = 'always')


# abrir censo escolar geo
escolas <- read_rds("../../data/acesso_oport/censo_escolar/2018/educacao_inep_2018.rds") %>%
  # Deletar escolas q nao foram localizadas
  dplyr::filter(!is.na(lat)) %>%
  # Selecionar variaveis
  dplyr::select(id_estab = CO_ENTIDADE, codemun = CO_MUNICIPIO, lon, lat, total_corrigido = QT_FUNCIONARIOS)


# pegar distribuicao de nivel des escolaridade da cidade
rais_prop_escol <- rais %>%
  group_by(codemun) %>%
  summarise(prop_alto = sum(alto)/sum(total_corrigido),
            prop_medio = sum(medio)/sum(total_corrigido),
            prop_baixo = sum(baixo)/sum(total_corrigido))

# trazer proporcoes para as escolas
escolas_prop <- escolas %>%
  # ajeitar codemun
  mutate(codemun = substr(codemun, 1, 6)) %>%
  left_join(rais_prop_escol, by = "codemun") %>%
  # multiplicar totais por composicao
  mutate(alto = round(prop_alto * total_corrigido),
         medio = round(prop_medio * total_corrigido),
         baixo = round(prop_baixo * total_corrigido)) %>%
  select(id_estab, codemun, lon, lat, alto, medio, baixo, total_corrigido)

setDT(escolas_prop)[, geocode_engine := "galileo"]
escolas_prop[, PrecisionDepth := "4 Estrelas"]
escolas_prop[, type_input_galileo := "inep"]

# juntar rais com escolas proporcionais
rais2 <- rbind(rais, escolas_prop, fill = T)



table(rais2$PrecisionDepth, useNA = 'always')
table(rais2$geocode_engine, useNA = 'always')
table(rais2$type_input_galileo, useNA = 'always')


# Salvar
write_rds(rais2, "../../data/acesso_oport/rais/2017/rais_2017_corrigido_geocoded_censoEscolar.rds")




# ##### Eliminar manualmente estabelecimentos indentificados como problematicos
# # identificacao desses estabs no script 2.4 com funcao 'diagnost_hex_us_prop'
# 
# rais <- readr::read_rds("../data/rais/rais_2017_corrigido_latlon_censoEscolar.rds")






# view
subset(output_google_api1_rais, !is.na(lat)) %>%
  to_spatial() %>%
  mapview()







# 7) Corrigir a posteriori falhas no geocode do Galileo --------------------------

# abrir rais
rais <- readr::read_rds("../../data/acesso_oport/rais/2017/rais_2017_corrigido_geocoded_censoEscolar.rds")



# lista dos hexagonos problematicos

hex_probs <- data.frame(
  hexs = c("89a8c0cea23ffff", # goi - Ruas com nome de numero (e.g. "RUA T50, 71", ou "RUA 05, 715")
           "8980088739bffff", # slz, - Avenida dos Holandeses (erro Galileo - mesmo afirmando que encontrou 4 estrelas)
           "898008829a7ffff", # slz, - AVENIDA JOAO PESSOA, 363, São Luís - State of Maranhão
           "898008829afffff", # slz, - AV ENG EMILIANO MACIEIRA
           "898008829abffff", # slz, - AV ENG EMILIANO MACIEIRA de novo
           "89800880d5bffff", # slz, - Av. São Luís Rei de França
           "89800882a3bffff", # slz, AV JERONIMO DE ALBUQUERQUE
           "89800882a0fffff", # slz, AV JERONIMO DE ALBUQUERQUE de novo
           "89800882a53ffff", # slz, - Ruas com nome de numero (e.g. "RUA T50, 71", ou "RUA 05, 715")
           "89a8a2a6413ffff" # sgo, - Avenida Eugênio Borges (rodovia?)
  ),
  sigla_muni = c("goi", "slz","slz", "slz", "slz", "slz", "slz", "slz", "slz", "sgo")
  
)


# ler hex agregados e juntar
hex <- lapply(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds", unique(hex_probs$sigla_muni)), read_rds) %>%
  rbindlist() %>%
  st_sf() %>%
  select(id_hex) %>%
  # filtra hex ids problematicos
  filter(id_hex %in% hex_probs$hexs)

# Qual o codigo dos municipio em questao?
cod_mun_ok <- munis_df_2019[abrev_muni %in% unique(hex_probs$sigla_muni)]$code_muni

# Carrega somente os dados da rais estabes nestes municipios
base <- rais %>%
  filter(!is.na(lon)) %>%
  filter(codemun %in% substr(cod_mun_ok, 1, 6)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(codemun, id_estab)

# Intersecao do hex ids problema com base de uso do solo
fim <- st_join(base, hex) %>%
  filter(!is.na(id_hex))

# Extrair os estabs 'problematicos' concentrados em hexagonos
oi <- fim %>% st_set_geometry(NULL) %>% distinct(id_estab) %>% .$id_estab

# Filtrar estabs
rais_prob <- rais %>%
  filter(id_estab %in% oi)

# lista de enderecos com problema
enderecos_etapa7 <- rais_prob %>% 
  mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% 
  .$fim


# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[4])

# geocode
coordenadas_google_etapa7 <- lapply(X=enderecos_etapa7, ggmap::geocode, output = 'all')

# identify list names as id_estab
names(coordenadas_google_etapa7) <- rais_prob$id_estab 

# save
write_rds(coordenadas_google_etapa7, "../../data/acesso_oport/rais/2017/geocode/rais_2017_output_google_manyhex.rds")


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

# create dt 
enderecos_google_etapa7 <- lapply(coordenadas_google_etapa7, create_dt)


# rbind as data.table
enderecos_google_etapa7_dt <- rbindlist(enderecos_google_etapa7, idcol = "id_estab",
                                        use.names = TRUE)

# identify searchedaddress
enderecos_google_etapa7_dt[, SearchedAddress := enderecos_etapa7]
# identify problem
enderecos_google_etapa7_dt[, geocode_engine := 'toomany_hex']
# identify quality
enderecos_google_etapa7_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]



# bring tot he original dataset
rais[enderecos_google_etapa7_dt, on = "id_estab",
     c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
       list(i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]


table(rais$PrecisionDepth, useNA = 'always')
table(rais$geocode_engine, useNA = 'always')
table(rais$type_input_galileo, useNA = 'always')


# Salvar
write_rds(rais, "../../data/acesso_oport/rais/2017/rais_2017_etapa7.rds")






# 8) Excluir CNPJ's problematicos --------------------------

# Esse CPNJ foram identificados a partir da inspecao de hexagonos em cada cidade com uma quantidade
# maior que 3000 empregos

# Abrir rais da etapa 7
rais <- read_rds("../../data/acesso_oport/rais/2017/rais_2017_etapa7.rds")

# Trazer CNPJs problematicos
# Lista de CNPJS problematicos
cnpjs_prob <- c("01437408000198", # for - Nordeste Cidadania
                "02137309000153", # spo - Icomon Tecnologia
                "17178195000167", # bho - Sociedade Mineira de Cultura
                "03873484000171", # goi - EMPREZA SERVICE CENTER - Locação de mão-de-obra temporária (EM RECUPERACAO JUDICIAL)
                "67945071000138", # cam - Sapore S/A , 56.2 Serviços de catering, bufê e outros serviços de comida preparada
                "03254082000512", # slz - INSTITUTO ACQUA, com varias unidades em outras cidades
                "00801512000157") # duq - AGILE CORP 56.2 Serviços de catering, bufê e outros serviços de comida preparada

# Excluir esses CNPJs da rais
rais_etapa8 <- rais[id_estab %nin% cnpjs_prob]

# Salvar RAIS etapa 8 
write_rds(rais_etapa8, "../../data/acesso_oport/rais/2017/rais_2017_etapa8.rds")




# 8) Corrigir a posteriori hex que apresentaram grande diferença pro ano anterior --------------------------
# Nesse caso, aqui sao os hex que apresentaram um valor muito maior (>1000 vinculos) pra 2017 do que para 2018

# abrir rais
rais <- readr::read_rds("../../data/acesso_oport/rais/2017/rais_2017_etapa8.rds")

# function by city
compare_jobs_distribution <- function(sigla_munii) {
  
  # open hex files
  hex_jobs_2017 <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2017/hex_agregado_%s_09_2017.rds",
                                    sigla_munii)) %>%
    mutate(ano_jobs = 2017)
  
  hex_jobs_2018 <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2018/hex_agregado_%s_09_2018.rds",
                                    sigla_munii)) %>%
    mutate(ano_jobs = 2018)
  
  hex_jobs <- rbind(hex_jobs_2017, hex_jobs_2018)
  hex_jobs <- select(hex_jobs, id_hex, sigla_muni, empregos_total, ano_jobs, geometry) %>% setDT()
  
  hex_jobs_wide <- pivot_wider(hex_jobs, names_from = ano_jobs, values_from = empregos_total,
                               names_prefix = "jobs_")
  
  # compare!
  hex_jobs_wide <- hex_jobs_wide %>%
    mutate(dif1_abs = jobs_2018 - jobs_2017) %>%
    mutate(dif1_log = log(jobs_2018/jobs_2017)) %>%
    # truncate
    mutate(dif1_abs_tc = case_when(dif1_abs < -500 ~ -500,
                                   dif1_abs > 500 ~ 500,
                                   TRUE ~ dif1_abs)) %>%
    mutate(dif1_log_tc = case_when(dif1_log < -1 ~ -1,
                                   dif1_log > 1 ~ 1,
                                   TRUE ~ dif1_log))
  
  
  hex_jobs_wide <- hex_jobs_wide %>%
    filter(!(jobs_2017 == 0 & jobs_2018 == 0))
  
  # filter hex with a diffrence of more than 1000 vinc
  hex_probs <- filter(hex_jobs_wide, dif1_abs < -1000) %>%
    select(sigla_muni, id_hex)
  
}

hex_probs9 <- lapply(munis_df_2019$abrev_muni, compare_jobs_distribution)
hex_probs9_dt <- do.call(rbind, hex_probs9) 



# ler hex agregados e juntar
hex <- lapply(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds", unique(hex_probs9_dt$sigla_muni)), read_rds) %>%
  rbindlist() %>%
  st_sf() %>%
  select(id_hex) %>%
  # filtra hex ids problematicos
  filter(id_hex %in% hex_probs9_dt$id_hex)

# Qual o codigo dos municipio em questao?
cod_mun_ok <- munis_df_2019[abrev_muni %in% unique(hex_probs9_dt$sigla_muni)]$code_muni

# Carrega somente os dados da rais estabes nestes municipios
base <- rais %>%
  filter(!is.na(lon)) %>%
  filter(codemun %in% substr(cod_mun_ok, 1, 6)) %>%
  filter(PrecisionDepth %in% c("4 Estrelas", "3 Estrelas", "street_number", "route")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  select(codemun, id_estab)

# Intersecao do hex ids problema com base de uso do solo
fim <- st_join(base, hex) %>%
  filter(!is.na(id_hex))

# Extrair os estabs 'problematicos' concentrados em hexagonos
oi <- fim %>% st_set_geometry(NULL) %>% distinct(id_estab) %>% .$id_estab

# Filtrar estabs
rais_prob <- rais %>%
  filter(id_estab %in% oi)

# lista de enderecos com problema
enderecos_etapa9 <- rais_prob %>% 
  mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% 
  .$fim


# registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[2])

# geocode
coordenadas_google_etapa9 <- lapply(X=enderecos_etapa9, ggmap::geocode, output = 'all')

# identify list names as id_estab
names(coordenadas_google_etapa9) <- rais_prob$id_estab 

# save
write_rds(coordenadas_google_etapa9, "../../data/acesso_oport/rais/2017/geocode/rais_2017_output_google_etapa9.rds")


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

# create dt 
enderecos_google_etapa9 <- lapply(coordenadas_google_etapa9, create_dt)


# rbind as data.table
enderecos_google_etapa7_dt <- rbindlist(enderecos_google_etapa7, idcol = "id_estab",
                                        use.names = TRUE)

# identify searchedaddress
enderecos_google_etapa7_dt[, SearchedAddress := enderecos_etapa7]
# identify problem
enderecos_google_etapa7_dt[, geocode_engine := 'toomany_hex']
# identify quality
enderecos_google_etapa7_dt[is.na(lon), ':='(PrecisionDepth = "address_not_found")]



# bring tot he original dataset
rais[enderecos_google_etapa7_dt, on = "id_estab",
     c("MatchedAddress", "SearchedAddress", "PrecisionDepth", "lon", "lat", "geocode_engine") := 
       list(i.MatchedAddress, i.SearchedAddress, i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]


table(rais$PrecisionDepth, useNA = 'always')
table(rais$geocode_engine, useNA = 'always')
table(rais$type_input_galileo, useNA = 'always')


# Salvar
write_rds(rais, "../../data/acesso_oport/rais/2017/rais_2017_etapa7.rds")



