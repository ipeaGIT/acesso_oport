#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.6 Leitura e limpeza de dados da RAIS




# carregar bibliotecas
source('./R/fun/setup.R')


### 0) get raw data ------------------------------------

#### companies data
  # read raw data
  estabs2019 <- fread('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab2019.csv' 
                      #, nrows = 5
                      , colClasses='character')
  
  # subset municipalities
  estabs2019_mun <- estabs2019[ codemun %in% substring(munis_df$code_muni, 1,6)]
  
  # subset columns
  estabs2019_mun <- estabs2019_mun[, 1:33]
  
  # save
  fwrite(estabs2019_mun, '../../data-raw/rais/2019/rais_estabs_raw_2019.csv')


rm(estabs2019_mun, estabs2019)
gc(reset = T)

#### workers data
# read raw data
trabal2019 <- fread('//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil2019.csv'
                    #, nrows = 5
                    , select = c("id_estab", "grau_instr","emp_31dez", 'clas_cnae20', 'uf', 'codemun', 'nat_jur2018')
                    , colClasses='character'
                    )

# subset municipalities
nrow(trabal2019)
trabal2019 <- trabal2019[ codemun %in% substring(munis_df$code_muni, 1,6)]
nrow(trabal2019)


# save
fwrite(trabal2019, '../../data-raw/rais/2019/rais_trabal_raw_2019.csv')




##### 1) Filtrar Rais-2019 pessoas-------------------------------------------------------


# Leitura dos dados da RAIS pessoas com colunas que vamos usar
rais_trabs <- data.table::fread("../../data-raw/rais/2019/rais_trabal_raw_2019.csv"
<<<<<<< HEAD
                                , select = c("id_estab", "grau_instr", "emp_31dez", 'clas_cnae20', 'uf', 'codemun', 'nat_jur2018')
                                , colClasses='character'
                                # , nrows = 1000
)

unique(rais_trabs$id_estab) %>% length() # 987175 estabs

# Filtro 0: selecionar so municipios do projeto (ja feito)
gc(reset=T)
rais_filtro_0 <- rais_trabs[codemun %in% substr(munis_df$code_muni, 1, 6) ]

unique(rais_filtro_0$id_estab) %>% length() # 987175 estabs
unique(rais_trabs$id_estab) %>% length() # 3819807 estabs

# Filtro 0: selecionar so municipios do projeto
gc(reset=T)
rais_filtro_0 <- rais_trabs[codemun %in% substr(munis_df_2019$code_muni, 1, 6) ]

unique(rais_filtro_0$id_estab) %>% length() # 1011069 estabs


# Filtro 1: selecionar so vinculos ativos
rais_filtro_1 <- rais_filtro_0[emp_31dez == 1]


unique(rais_filtro_1$id_estab) %>% length() # 902049 estabs


# Filtro 2 deletar todas as intituicoes com Natureza Juridica 'publica' (ver ../data-raw/rais/ManualRAIS2018.pdf) pagina 19
unique(rais_filtro_1$id_estab) %>% length() # 916370 estabs


# Filtro 2 deletar todas as intituicoes com Natureza Juridica 'publica' (ver ../data-raw/rais/ManualRAIS2019.pdf) pagina 19
# todos que comecam com o numeral 1 sao de administracao publica


# identifica natureza de adm publica
rais_filtro_1[, adm_pub := ifelse( substr(nat_jur2018, 1, 1)==1, 1, 0) ]

# fica apenas com adm publica
rais_filtro_2 <- rais_filtro_1[ adm_pub != 1 ]

# quantos vinculos de natureza juridica publica a gente perde? 22%
unique(rais_filtro_2$id_estab) %>% length() # 899502 estabs
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
rais_filtro_3 <- rais_filtro_2[nat_jur2018 != "2011"]


# Salvar em formato rds para formato rapido
write_rds(rais_filtro_3, "../../data/acesso_oport/rais/2019/rais_2019_ind_filtrada.rds")




####### 2) Categorizar trabalhadores por grau de instrucao  ----------------------------------------------------------------------------------

# Abrir RAIS  em formato rapido rds
rais_trabs <- read_rds("../../data/acesso_oport/rais/2019/rais_2019_ind_filtrada.rds")



# Categorizar trabalhadores por grau de instrucao
rais_cats <- rais_trabs[, instrucao := fifelse(grau_instr %in% c("01", "02", "03", "04", "05", "06"), "baixo",                                    # menor do que ensino medio (inclui ensino medio incompleto)
                                               fifelse(grau_instr %in% c("07", "08"), "medio",                           # ensino medio
                                                       fifelse(grau_instr %in% c("09", "10", "11"), "alto", grau_instr)))] # ensino superior

# Calcula quantidade de vinculo por grau de instrucao em cada estabelecimento
rais_fim <- rais_trabs[, .(vinculos = .N), by = .(id_estab, clas_cnae20, instrucao)]

# soma quantidade total de empregados em cada empresa
rais_fim <- rais_fim[, total := sum(vinculos), by = .(id_estab, clas_cnae20)]
head(rais_fim)

# Reshape da base de formato long para wide
rais_fim_wide <- tidyr::spread(rais_fim, instrucao, vinculos, fill = 0)
head(rais_fim_wide)

# Salvar agregado do numero de trabalhadores por empresa
write_rds(rais_fim_wide, "../../data/acesso_oport/rais/2019/rais_2019_vin_instrucao.rds")






### 3) Limpar outliers (empresas que ainda declara muitos trabalhadores na mesma sede) -----------------------------------------------------

# Abrir
rais <- read_rds("../../data/acesso_oport/rais/2019/rais_2019_vin_instrucao.rds")


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
              sort = FALSE,
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
write_rds(rais, "../../data/acesso_oport/rais/2019/rais_2019_corrigido.rds")







# 4) TRAZER GEOCODE DOS ESTABELECIMENTOS -----------------------------------------------------

rais_estabs <- read_rds("../../data/acesso_oport/rais/2019/rais_2019_corrigido.rds")
rais_estabs_geocode <- read_rds("../../data/acesso_oport/rais/2019/rais_2019_estabs_geocode_final.rds")
rais_estabs_geocode <- select(rais_estabs_geocode, -qt_vinc_ativos)
rais_estabs_geocode <- distinct(rais_estabs_geocode, id_estab, .keep_all = TRUE)

# pad estabs to 14 characters
setDT(rais_estabs)[, id_estab := str_pad(id_estab, width = 14, pad = 0)]

table(nchar(rais_estabs$id_estab))
table(nchar(rais_estabs_geocode$id_estab))


table(rais_estabs_geocode$PrecisionDepth, useNA = 'always')
table(rais_estabs_geocode$geocode_engine, useNA = 'always')
table(rais_estabs_geocode$type_input_galileo, useNA = 'always')

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


# save it
write_rds(rais_estabs_geocode_end, "../../data/acesso_oport/rais/2019/rais_estabs_2019_geocoded_all.rds")





# 5) Trazer informacoes de funcionarios de escolas publicas do censo escolar --------------------------
# (a partir do script 01.3-educacao)


# abri rais corrigida
rais <- read_rds("../../data/acesso_oport/rais/2019/rais_estabs_2019_geocoded_all.rds")


table(rais$PrecisionDepth, useNA = 'always')
table(rais$geocode_engine, useNA = 'always')
table(rais$type_input_galileo, useNA = 'always')


# abrir censo escolar geo
escolas <- read_rds("../../data/acesso_oport/censo_escolar/2018/educacao_inep_final_2018.rds") %>%
  # Deletar escolas q nao foram localizadas
  dplyr::filter(!is.na(lat)) %>%
  # Selecionar variaveis
  dplyr::select(id_estab = co_entidade, codemun = code_muni, lon, lat, total_corrigido = qt_funcionarios)

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
  select(id_estab, codemun, lon, lat, alto, medio, baixo, total_corrigido) %>%
  mutate(id_estab = paste0("inep_", id_estab)) %>%
  setDT()

escolas_prop[, geocode_engine := "galileo"]
escolas_prop[, PrecisionDepth := "4 Estrelas"]
escolas_prop[, type_input_galileo := "inep"]

# juntar rais com escolas proporcionais
rais2 <- rbind(rais, escolas_prop, fill = T)



table(rais2$PrecisionDepth, useNA = 'always')
table(rais2$geocode_engine, useNA = 'always')
table(rais2$type_input_galileo, useNA = 'always')


# remove duplicates
rais2 <- rais2 %>% distinct(id_estab, .keep_all = TRUE)


# trazer razao social
estabs <- fread("../../data-raw/rais/2019/rais_estabs_raw_2019.csv", select = c("id_estab", "razao_social"))
estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
estabs <- distinct(estabs, id_estab, .keep_all = TRUE)

rais2 <- merge(rais2, estabs, by = "id_estab", all.x = TRUE)


# Salvar
write_rds(rais2, "../../data/acesso_oport/rais/2019/rais_2019_corrigido_geocoded_censoEscolar.rds")






