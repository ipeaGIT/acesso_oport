~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.6 Leitura e limpeza de dados da RAIS
  
  
  
  
# carregar bibliotecas
  source('./R/fun/setup.R')


 
#####----rais-2017 pessoas-------------------------------------------------------


# Leitura dos dados da RAIS pessoas com colunas que vamos usar
rais <- data.table::fread("../data-raw/rais/brasil2017.csv",
              select = c("id_estab", "grau_instr", "emp_31dez", 'clas_cnae10', 'uf', 'codemun'), colClasses='character'
               #,nrows = 1000
              )



# Salvar em formato rds para formato rapido
write_rds(rais, "../data/rais/rais_2017_ind.rds")


# Abrir RAIS  em formato rapido rds
rais <- read_rds("../data/rais/rais_2017_ind.rds")


# selecionar so vinculos ativos
rais <- rais[emp_31dez == 1]


# Categorizar trabalhadores por grau de instrucao
rais_cats <- rais[, instrucao := ifelse(grau_instr %in% c(1:6), "baixo",                                    # menor do que ensino medio (inclui ensino medio incompleto)
                                         ifelse(grau_instr %in% c(7, 8), "medio",                           # ensino medio
                                                ifelse(grau_instr %in% c(9, 10, 11), "alto", grau_instr)))] # ensino superior


# Calcula quantidade de vinculo por grau de instrucao em cada estabelecimento
rais_fim <- rais[, .(vinculos = .N), by = .(id_estab, clas_cnae10, instrucao)]

# soma quantidade total de empregados em cada empresa
rais_fim <- rais_fim[, total := sum(vinculos), by = .(id_estab, clas_cnae10)]
head(rais_fim)
  
# Reshape da base de formato long para wide
rais_fim_wide <- tidyr::spread(rais_fim, instrucao, vinculos, fill = 0)
head(rais_fim_wide)

# Salvar agregado do numero de trabalhadores por empresa
data.table::fwrite(rais_fim_wide, "../data/rais/rais_2017_vin_instrucao.csv")


### Limpeza de outliers -----------------------------------------------------



# Abrir
rais <- fread("../data/rais/rais_2017_vin_instrucao.csv")

# Limpeza de estabelecimentos com total de empregos outliers _ do codigo do Bruno ------------------

# Extrair o cnae do setor
rais <- setDT(rais)[, cnae.setor := substr(clas_cnae10, 1, 2)]

# Extrair os setores desejados
  # 40 eletricidade, gas e agua quente
  # 41 captacao, tratamento e distribuicao de agua
  # 60 transporte terrestre
  # 62 transporte aereo
  # 74 servicoes prestados principalmente a empresas
  # 90 limpeza urbana, esgoto e atividades relacionadas
  rais.problema <- setDT(rais)[cnae.setor %in% c("40","41","60","62","74","90")]


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

names(geral2)<-c("cnae.setor","q","freq","desviointerq","fator","corte","outlier","perda")

# criando nova variavel com valores de outliers corrigidos
rais$total_corrigido <- rais$total

#zerando empregos de administracao publica
rais$total_corrigido<-ifelse(rais$cnae.setor=="75",0,rais$total_corrigido)

#tabela com valor de corte por setor
geral3<-geral2[,c(1,6)]

#colocando esse valor de corte na base
rais <- merge(rais, geral3, 
              by="cnae.setor",
              all.x = TRUE)

#substituindo valores maiores que o corte pelo valor de corte
rais$total_corrigido<-ifelse(rais$cnae.setor %in% c("40","41","60","62","74","90") & rais$total_corrigido>rais$corte,rais$corte,rais$total_corrigido)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Corrigir quantidade de empregos por grau de escolaridade -------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



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




##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Adicionar dados de lat long -----------------------------------


# # abrir rais georef
# rais_raw <- foreign::read.dta("../data-raw/rais/rais_2017_georef.dta")
# rais1 <- setDT(rais_raw)[precisiondepth != "1 Estrela", .(id_estab,
#                                                       clas_cnae10,
#                                                       lon = longitude, lat = latitude, codemun, qt_vinc_ativos,
#                                                       cidade = BA_Nome_do_municipio)]
# write_rds(rais1, "../data-raw/rais/rais_2017_raw.rds")
# fwrite(rais1, "../data-raw/rais/rais_2017_raw.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LIMPEZA DOS DADOS DA RAIS 2017 -------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# leitura da RAIS geocodificada
rais_geo <- fread("../data-raw/rais/rais_2017_raw.csv", colClasses='character', select = c("id_estab", 'codemun', "lat", "lon"))
rais_geo[, id_estab := as.numeric(id_estab)]
head(rais_geo)

# corrigir coordenadas
rais_geo[, ':='(lon = str_replace(lon, ",", "."),
            lat = str_replace(lat, ",", "."))]

rais_geo[, ':='(lon = as.numeric(lon),
            lat = as.numeric(lat))]

# Merge tipo look up
rais[rais_geo, on = 'id_estab', c('lat', 'lon', 'codemun') := list(i.lat, i.lon, i.codemun)]
head(rais)

rais[is.na(lat), sum(total_corrigido)] / sum(rais$total_corrigido)

# Salvar
write_rds(rais, "../data/rais/rais_2017_corrigido.rds")


# Salvar base somente com municipios do projeto

# filtar
rais2 <- rais[ codemun %in% substr(munis_df$code_muni, 1, 6)]

# Salvar
write_rds(rais2, "../data/rais/rais_2017_corrigido_cidades_selecionadas2019.rds")



