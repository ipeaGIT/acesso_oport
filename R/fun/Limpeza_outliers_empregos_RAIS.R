# Script: Limpeza_outliers_empregos_RAIS.R
# Limpeza de estabelecimento com total de vínculos outliers:
# Código do bolsista Bruno, de dezembro de 2017. Caio (ASMEQ) participou da discussão.


library(foreign)
library(dplyr)
library(data.table)
library(xlsx)

setwd("L:\\# VANESSA GAPRIOTTI NADALIN #\\Georef RAIS")
rais15 = read.dta("estab_2015_vinc_coord_rms.dta")
# este banco j?? esta somente com estabelecimentos de rais n??o negativa
# e com corre??os no processo de georeferenciamento do galileo
#esta limpo de todos endere??os que n??o deu para georeferenciar.

###############################################################################################


# limpeza de estabelecimentos com total de empregos outliers _ do c??digo do Bruno

rais15$cnae.setor<-substr(rais15$clas_cnae10,1,2)
rais.problema<-rais15[rais15$cnae.setor==c("40","41","60","62","74","90"),]
#umas 57 mil tem problema
rais.problema<-rais.problema[rais.problema$qt_vinc_ativos>0,]
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


rais.problema<-data.table(rais.problema)
rais.problema[,p90:=quantile(qt_vinc_ativos,0.90),by=cnae.setor]
geral<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2])
names(geral)<-c("cnae.setor","quantil","freq","desviointerq","fator","corte","outliers")
rais.problema2<-left_join(rais.problema,geral)
rais.problema2$diff<-rep(0,nrow(rais.problema2))
rais.problema2$diff[rais.problema2$qt_vinc_ativos>=rais.problema2$corte] <- rais.problema2$qt_vinc_ativos[rais.problema2$qt_vinc_ativos>=rais.problema2$corte] - rais.problema2$corte[rais.problema2$qt_vinc_ativos>=rais.problema2$corte]
dif<-aggregate(rais.problema2$diff, by=list(rais.problema2$cnae.setor), sum)
geral2<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2],dif[,2])
names(geral2)<-c("cnae.setor","q","freq","desviointerq","fator","corte","outlier","perda")

# criando nova vari??vel com valores de outliers corrigidos
rais15$qt_vinc_ativos2<-rais15$qt_vinc_ativos

#zerando empregos de administra????o p??blica
rais15$qt_vinc_ativos2<-ifelse(rais15$cnae.setor=="75",0,rais15$qt_vinc_ativos2)

#tabela com valor de corte por setor
geral3<-geral2[,c(1,6)]

#colocando esse valor de corte na base
rais15<-left_join(rais15,geral3,by="cnae.setor")

#substituindo valores maiores que o corte pelo valor de corte
rais15$qt_vinc_ativos2<-ifelse(rais15$cnae.setor %in% c("40","41","60","62","74","90") & rais15$qt_vinc_ativos2>rais15$corte,rais15$corte,rais15$qt_vinc_ativos2)

rm(list=setdiff(ls(), "rais15"))
