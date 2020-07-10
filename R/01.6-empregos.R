#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.6 Leitura e limpeza de dados da RAIS




# carregar bibliotecas
source('./R/fun/setup.R')


# ATENCAO ####################################################################
# ESSE SCRIPT SO FUNCIONA PARA O ANO DE 2019!

# Determinar o ano
ano <- 2019

# Select the corerspondent munis_df
munis_df <- get(sprintf("munis_df_%s", ano))



##### 1) Filtrar Rais-2017 pessoas-------------------------------------------------------


# Leitura dos dados da RAIS estabs geocoded
rais_estabs <- fread("../data-raw/rais/2019/rais_2017_georef.csv", colClasses = 'character')


rais_estabs[ id_estab == '33892175000290'] # ipea
rais_estabs[ id_estab == '33892175000100'] # ipea


# Leitura dos dados da RAIS pessoas com colunas que vamos usar
rais_trabs <- data.table::fread("../data-raw/rais/2019/brasil2017.csv"
                                , select = c("id_estab", "grau_instr", "emp_31dez", 'clas_cnae20', 'uf', 'codemun', 'nat_jur2016')
                                , colClasses='character'
                                # , nrows = 1000
)



# Filtro 0: selecionar so municipios do projeto
gc(reset=T)
rais_filtro_0 <- rais_trabs[codemun %in% substr(munis_df$code_muni, 1, 6) ]



# Filtro 1: selecionar so vinculos ativos
rais_filtro_1 <- rais_filtro_0[emp_31dez == 1]




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
write_rds(rais_filtro_3, "../data/rais/2019/rais_2017_ind_filtrada.rds")




####### 2) Categorizar trabalhadores por grau de instrucao  ----------------------------------------------------------------------------------

# Abrir RAIS  em formato rapido rds
rais_trabs <- read_rds("../data/rais/2019/rais_2017_ind_filtrada.rds")



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
write_rds(rais_fim_wide, "../data/rais/rais_2017_vin_instrucao.rds")






### 3) Limpar outliers (empresas que ainda declara muitos trabalhadores na mesma sede) -----------------------------------------------------

# Abrir
rais <- read_rds("../data/rais/2019/rais_2017_vin_instrucao.rds")


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
write_rds(rais, "../data/rais/2019/rais_2017_corrigido.rds")



##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### 4) Adicionar dados de lat long-------------------------------------------------------

# Abrir Rais de estabelecimentos com numero de vinculos corrigos e quant de pessoas por escolaridade
rais <- read_rds("../data/rais/2019/rais_2017_corrigido.rds")


# leitura da RAIS geocodificada ( output do galileo )
rais_geo <- fread("../data-raw/rais/2019/rais_2017_georef.csv"
                  , select = c("id_estab", "codemun", "latitude", 'longitude', 'precisiondepth', 'logradouro', 'cep', 'uf', 'BA_Nome_do_municipio')
                  , colClasses='character'
                  # , nrows = 10
                  )
nrow(rais_geo) # 8186586 obs


# Filtro selecionar so municipios do projeto
gc(reset=T)
rais_geo <- rais_geo[codemun %in% substr(munis_df$code_muni, 1, 6) ]
nrow(rais_geo) # 2348816 obs

# recode lat lon column names
rais_geo <- rais_geo %>% rename(lat=latitude , lon=longitude)
head(rais_geo)


# corrigir coordenadas ( substitui , por  .)
rais_geo[, ':='(lon = str_replace(lon, ",", "."),
                lat = str_replace(lat, ",", "."))]

rais_geo[, ':='(lon = as.numeric(lon),
                lat = as.numeric(lat))]


# Merge tipo look up
rais[rais_geo, on = 'id_estab', 
     c('lat', 'lon', 'codemun', 'precisiondepth', 'logradouro', 'cep', 'uf', 'BA_Nome_do_municipio') := 
       list(i.lat, i.lon, i.codemun, i.precisiondepth, i.logradouro, i.cep, i.uf, i.BA_Nome_do_municipio)]

head(rais)


# qual a porcentagem de coordenadas NA?
# 0%
rais[is.na(lat), sum(total_corrigido)] / sum(rais$total_corrigido)


# qual a porcentagem de precisiondepth 1 ou 2 estrelas?
# 4%
rais[ precisiondepth %in% c('1 Estrela', '2 Estrelas'), sum(total_corrigido)] / sum(rais$total_corrigido)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### 5) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao  -------------------
# Os estabelecimentos que o Galileo encontrou com 1, 2 ou 3 estrelas serao jogados para o google api



######################### GOOGLE 1 , somente 1 e 2 estrelas, todos, endereco completo 

# Estabs com baixa precisao do Galileo 1 e 2 estrelas
estabs_problema <- rais[ precisiondepth %in% c('1 Estrela', '2 Estrelas'), ]
nrow(estabs_problema) # 34579 obs


# lista de enderecos com problema
enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% .$fim

# registrar Google API Key
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)

# geocode
  coordenadas_google <- lapply(X=enderecos, ggmap::geocode) %>% rbindlist()
  
  # save google API output 
  output_google_api1_rais <- cbind(estabs_problema[,'id_estab'], coordenadas_google)
  

  # view
  subset(output_google_api1_rais, !is.na(lat)) %>%
    to_spatial() %>%
    mapview()
  

######################### GOOGLE 2 , somente 3 estrelas, rodovias, endereco completo 
  # Estabelecimentos com 3 estrelas que estao localizados em rodovias tendem a ter uma qualidade ruim
  # de georreferenciamento!
  # Essa etapa busca identificar todos os 3 estrelas que sejam em rodovias, e separa-os para aplicar
  # o geocoding do google!
  
  
  # abrir base de rodovias rodovias
  rodovias <- st_read("../data-raw/rodovias/2014/2014/rodovia_2014.shp") %>%
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
  estabs_problema_3estrelas <- rais[precisiondepth == "3 Estrelas"]
  
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
  nrow(rais_rodovias) # 9620 obs
  
  # lista de enderecos com problema
  enderecos_rodovias <- rais_rodovias %>% mutate(fim = paste0(logradouro, " - ", BA_Nome_do_municipio, ", ", uf, " - CEP ", cep)) %>% .$fim
  
  # registrar Google API Key
  my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1)
  
  # geocode
  coordenadas_google_rodovias <- lapply(X=enderecos_rodovias, ggmap::geocode) %>% rbindlist()
  
  # save google API output 
  output_google_api2_rais <- cbind(rais_rodovias[,'id_estab'], coordenadas_google_rodovias)
  
  
  
######################### GOOGLE 3, so ceps
  
  # ainda ha empresas mal georreferenciadas!
  # identificar esses empresas e separa-los
  # muitas empresas foram georreferenciadas fora dos municipios
  # identifica-las atraves do shape dos municipios e refazer geocode do google considerando so CEP
  
  # carrega shape dos munis
  shps <- purrr::map_dfr(dir("../data-raw/municipios/", recursive = TRUE, full.names = TRUE), read_rds) %>% as_tibble() %>% st_sf()

rais_google_mal_geo <- output_google_api1_rais %>%
    filter(!is.na(lat)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    sf::st_join(shps %>% st_set_crs(4326)) %>%
    # empregos que cairam fora de algum municipio, a serem georreferenciadas na unha
    filter(is.na(name_muni)) %>%
    # pegar so o cep
    select(id_estab) %>%
    left_join(estabs_problema, by = "id_estab") %>%
    filter(cep != ".")
  
  
  # Retorna somente os ceps dos que deram errado para jogar no google API
  somente_ceps <- paste0("CEP ", rais_google_mal_geo$cep, " - ", rais_google_mal_geo$BA_Nome_do_municipio, ", ", rais_google_mal_geo$uf)
  # 4.420  
  
  # consulta google api
  coordenadas_google_cep <- lapply(X=somente_ceps, ggmap::geocode) %>% rbindlist()
  
  # save google API output 
  output_google_api3_rais <- cbind(as.data.frame(rais_google_mal_geo[,'id_estab']), coordenadas_google_cep)
  
  # view
  subset(output_google_api3_rais, !is.na(lat)) %>%
    to_spatial() %>%
    mapview()
  
  # registrar q a declaracao do municipio da RAIS esta errada em algumas empresas. Exemplo abaixo de est declarado
  # em SP, mas localizado em Bauro
  # subset(rais_google_mal_geo, id_estab       == 18145772001744  )
  
  
### Junta outputs 1, 2 e 3 de google API
  # bind do output1 com o output2
  output_google_api1_rais <- rbind(output_google_api1_rais, output_google_api2_rais)
  # merge tipo look up do resultado anterior com o 3, que busca substituir as coordenadas
  output_google_api1_rais[output_google_api3_rais, on='id_estab', c('lat', 'lon') := list(i.lat, i.lon) ]
  
  # save google API output 
  fwrite(output_google_api1_rais, "../data/rais/2019/output_google_api_rais.csv")
  
  
  # open google API output
  output_google_api1_rais <- fread("../data/rais/2019/output_google_api_rais.csv", colClasses = 'character')
  
  
# atualiza lat lon a partir de google geocode
  setDT(rais)[output_google_api1_rais, on='id_estab', c('lat', 'lon') := list(i.lat, i.lon) ]

# Salvar
write_rds(rais, "../data/rais/2019/rais_2017_corrigido_latlon.rds")




# 6) Trazer informacoes de funcionarios de escolas publicas do censo escolar --------------------------
# (a partir do script 01.3-educacao)


# abri rais corrigida
rais <- read_rds("../data/rais/2019/rais_2017_corrigido_latlon.rds")


# abrir censo escolar geo
escolas <- read_rds("../data/censo_escolar/2019/educacao_inep_2019.rds") %>%
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

# juntar rais com escolas proporcionais
rais2 <- rbind(rais, escolas_prop, fill = T)



# Salvar
write_rds(rais2, "../data/rais/2019/rais_2017_corrigido_latlon_censoEscolar.rds")




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
rais <- readr::read_rds("../data/rais/2019/rais_2017_corrigido_latlon_censoEscolar.rds")

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
    "89a8a2a6413ffff", # sgo, - Avenida Eugênio Borges (rodovia?)
    "8980055454bffff", # ter, - AV DEPUTADO PAULO FERRAZ
    "89800556a2bffff", # ter, - muitas ruas
    "89800554ccfffff", # ter, - muitas ruas
    "89800554eabffff", # ter, - muitas ruas
    "89800554d8bffff", # ter, - muitas ruas
    "89800554e17ffff"),# ter - muitas ruas
  

  sigla_muni = c("goi", "slz","slz", "slz", "slz", "slz", "slz", "slz", "slz", "sgo", "ter", "ter", "ter", "ter", "ter", "ter")
  
)

  
# ler hex agregados e juntar
hex <- lapply(sprintf("../data/hex_agregados/2019/hex_agregado_%s_09_2019.rds", unique(hex_probs$sigla_muni)), read_rds) %>%
  rbindlist() %>%
  st_sf() %>%
  select(id_hex) %>%
  # filtra hex ids problematicos
  filter(id_hex %in% hex_probs$hexs)

# Qual o codigo dos municipio em questao?
cod_mun_ok <- munis_df[abrev_muni %in% unique(hex_probs$sigla_muni)]$code_muni

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
my_api <- data.table::fread("../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1)

# geocode
coordenadas_google_etapa7 <- lapply(X=enderecos_etapa7, ggmap::geocode) %>% rbindlist()

output_google_etapa7_rais <- cbind(setDT(rais_prob)[,'id_estab'], coordenadas_google_etapa7)

# abrir output teste
output_google_etapa7_rais <- read_rds("../data/output_gmaps_temp_etapa7.rds")
output_google_etapa7_rais <- output_google_etapa7_rais[, .(id_estab = V1, lon, lat)]

# qual a distancia entre a localizacao do galileo e do gmaps?
rais_dif <- rais %>% 
  select(id_estab, lon, lat, BA_Nome_do_municipio) %>%
  right_join(output_google_etapa7_rais, by = "id_estab") %>%
  mutate(dist = geosphere::distHaversine(matrix(c(rais_dif$lon.x, rais_dif$lat.x), ncol = 2), matrix(c(.$lon.y, .$lat.y), ncol = 2)))

hist(rais_dif$dist[rais_dif$dist < 5000])


# atualiza lat lon a partir de google geocode
setDT(rais)[output_google_etapa7_rais, on='id_estab', c('lat', 'lon') := list(i.lat, i.lon) ]

# Salvar
write_rds(rais, "../data/rais/2019/rais_2017_etapa7.rds")


a <- geocode('AV ENGENHEIRO EMILIANO MACIEIRA, 1030 - SÃO LUÍS, Maranhão, CEP 65095603', output='more', force=T, data=T) 
b <- geocode('AV ENGENHEIRO EMILIANO MACIEIRA, 2300 - SÃO LUÍS, Maranhão, CEP 65095971', output='more', force=T, data=T) 

a$lon == b$lon

# 8) Excluir CNPJ's problematicos --------------------------

# Esse CPNJ foram identificados a partir da inspecao de hexagonos em cada cidade com uma quantidade
# maior que 3000 empregos

# Abrir rais da etapa 7
rais <- read_rds("../data/rais/2019/rais_2017_etapa7.rds")

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
write_rds(rais_etapa8, "../data/rais/2019/rais_2017_etapa8.rds")
