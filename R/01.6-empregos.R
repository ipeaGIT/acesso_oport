#' ## Empregos
#' 
#' 
#' 
#' 
## ----rais----------------------------------------------------------------

# Abrir RAIS (formato stata)
rais_raw <- foreign::read.dta("../data-raw/rais/estab_2015_vinc_coord.dta")
# Transformar em data.table
setDT(rais_raw)
# Deletar as localizacoes com precisao de 1 estrela
rais_v1 <- rais_raw[Precison_original != "1 Estrela"]
# Ajeitar as coordenadas
rais_v1 <- rais_v1[, ':='(lon = as.numeric(gsub(",", ".", longitude)), lat = as.numeric(gsub(",", ".", latitude)))]
# Selecionar as colunas de interesse
rais_v1 <- rais_v1[, .(id_estab, qt_vinc_ativos, cod_mun = ibge_cod7, lon, lat)]
# Dropar coordenadas NA
rais_v1 <- na.omit(rais_v1, cols = c("lon", "lat"))
# # Transformar para sf
# rais_v1 <- st_as_sf(rais_v1, coords = c("lon", "lat"), crs = 4326)


# Salvar
write_rds(rais_v1, "../data/rais/rais_2015.rds")


#' 
#' 
## ----rais-2017-ind-------------------------------------------------------

# RAIS PESSOAS! -------------------------------------------------------------------------------

# rais <- fread("\\storage6\\bases\\DADOS\\RESTRITO\\RAIS")

rais <- fread("../data-raw/rais/brasil2017.csv"
              ,select = c("id_estab", "grau_instr", "emp_31dez")
              # ,nrows = 1000
              )

# Salvar
write_rds(rais, "../data/rais/rais_2017_ind.rds")

# Abrir
rais <- read_rds("../data/rais/rais_2017_ind.rds")

# selecionar so vinculos ativos
rais <- rais[emp_31dez == 1]

# fazer sumario
rais_sum <- rais[, .N, by = grau_instr]

# categorizar
rais_cats <- rais[, instrucao := ifelse(grau_instr %in% c(1:6), "baixo",
                                         ifelse(grau_instr %in% c(7, 8), "medio",
                                                ifelse(grau_instr %in% c(9, 10, 11), "alto", grau_instr)))]

rais_fim <- rais[, .(vinculos = .N), by = .(id_estab, instrucao)]

rais_fim_wide <- spread(rais_fim, instrucao, vinculos, fill = 0)

fwrite(rais_fim_wide, "../data/rais/rais_2017_vin_instrucao.csv")


#' 
#' 
## ----rais_2017_outliers--------------------------------------------------

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

# Abrir
rais <- fread("../data-raw/rais/rais_2017_raw.csv")

# Limpeza de estabelecimentos com total de empregos outliers _ do codigo do Bruno ------------------

# Extrair o cnae do setor
rais <- setDT(rais)[, cnae.setor := substr(clas_cnae10, 1, 2)]

# Extrair os setores desejados 
rais.problema <- setDT(rais)[cnae.setor %in% c("40","41","60","62","74","90")]

# Extrair somente os que tem vinculos ativos
# hist(rais.problema$qt_vinc_ativos)
# summary(rais.problema$qt_vinc_ativos)
rais.problema <- rais.problema[qt_vinc_ativos > 0]
# dessas, umas 7 mil n??o tinham v??nculos ativos

# o valor no percentil 90
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

iqa<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQa)
q<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), quanti)
qq<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), quant)
iqb<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQb)
iqe<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQe)
iqf<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQf)

# vai agregar essas informa????es na base


# rais.problema<-data.table(rais.problema)
rais.problema[,p90:=quantile(qt_vinc_ativos,0.90),by=cnae.setor]
geral<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2])
names(geral)<-c("cnae.setor","quantil","freq","desviointerq","fator","corte","outliers")

rais.problema2 <- merge(rais.problema, setDT(geral), 
                        all.x = TRUE)

rais.problema2$diff<-rep(0,nrow(rais.problema2))

rais.problema2$diff[rais.problema2$qt_vinc_ativos>=rais.problema2$corte] <- 
rais.problema2$qt_vinc_ativos[rais.problema2$qt_vinc_ativos>=rais.problema2$corte] - rais.problema2$corte[rais.problema2$qt_vinc_ativos>=rais.problema2$corte]

dif<-aggregate(rais.problema2$diff, by=list(rais.problema2$cnae.setor), sum)

geral2<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2],dif[,2])

names(geral2)<-c("cnae.setor","q","freq","desviointerq","fator","corte","outlier","perda")

# criando nova vari??vel com valores de outliers corrigidos
rais$qt_vinc_ativos2<-rais$qt_vinc_ativos

#zerando empregos de administra????o p??blica
rais$qt_vinc_ativos2<-ifelse(rais$cnae.setor=="75",0,rais$qt_vinc_ativos2)

#tabela com valor de corte por setor
geral3<-geral2[,c(1,6)]

#colocando esse valor de corte na base
rais <- merge(rais, geral3, 
              by="cnae.setor",
              all.x = TRUE)

#substituindo valores maiores que o corte pelo valor de corte
rais$qt_vinc_ativos2<-ifelse(rais$cnae.setor %in% c("40","41","60","62","74","90") & rais$qt_vinc_ativos2>rais$corte,rais$corte,rais$qt_vinc_ativos2)

# corrigir coordenadas
rais[, ':='(lon = str_replace(lon, ",", "."),
                   lat = str_replace(lat, ",", "."))]

rais[, ':='(lon = as.numeric(lon),
                   lat = as.numeric(lat))]

rais_fim <- rais[qt_vinc_ativos2 > 0]

rais_fim[, id_estab := as.character(id_estab)]

# Salvar
write_rds(rais_fim, "../data/rais/rais_2017_corrigido.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Trazer a informacao da quantidade de vinculos por escolaridade
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rais_corrig <- read_rds("../data/rais/rais_2017_corrigido.rds")
rais_escol <- fread("../data/rais/rais_2017_vin_instrucao.csv")

# Juntar as bases!
rais_fim_escol <- merge(rais_corrig, rais_escol,
                        all.x = TRUE,
                        by = "id_estab")

# Calcular a proporcao que cada um dos vinculos por escolaridade representa dos vinculos totais
rais_fim_escol[,  ":="(prop_baixo = baixo/qt_vinc_ativos,
                       prop_medio = medio/qt_vinc_ativos,
                       prop_alto = alto/qt_vinc_ativos)]

# Substituir a quantidade de vinculos baixo, medio e alto somente se o estabelecimento recebeu corte
rais_fim_escol[, ":="(alto = ifelse(qt_vinc_ativos2 < qt_vinc_ativos, 
                                    round(prop_alto * qt_vinc_ativos2, 0), alto),
                      medio = ifelse(qt_vinc_ativos2 < qt_vinc_ativos,
                                     round(prop_medio * qt_vinc_ativos2, 0), medio),
                      baixo = ifelse(qt_vinc_ativos2 < qt_vinc_ativos, 
                                     round(prop_baixo * qt_vinc_ativos2, 0), baixo))]

# Selecionar colunas de interesse
rais_fim_escol <- rais_fim_escol[, .(id_estab, codemun, baixo, medio, alto, lon, lat)]


# Salvar
write_rds(rais_fim_escol, "../data/rais/rais_2017_corrigido_escol.rds")



#' 
#' 
#' 
#' 
