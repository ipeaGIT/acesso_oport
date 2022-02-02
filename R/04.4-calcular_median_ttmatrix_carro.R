library(data.table)
library(R.utils)
library(sf)
library(dplyr)
library(matrixStats)
setDTthreads(percent = 100)

setwd("//storage6/usuarios/Proj_acess_oport/git_diego")

## calcula mediana dos ttmatrix para todos os munic?pio menos para bsb, cgr, man, rio, spo. Utilizar script seguinte para rodar bsb, cgr, man, rio, spo
cidades <- c("bel", "bho", "cam","cur","duq","for_","goi","gua","mac","nat","poa","rec","sal","sgo","slz")
pastas <- c("06AM","0615AM","0630AM","0645AM","07AM","0715AM","0730AM","0745AM","08AM","02PM","0215PM","0230PM","0245PM","03PM","0315PM","0330PM","0345PM","04PM")

for (i in (1:length(cidades))){
  for (j in (1:length(pastas))){
    print(cidades[i])
    print(pastas[j])

    OD_TI <- fread(gunzip(paste0(cidades[i],"/wkday", pastas[j],"/",pastas[j], ".csv.gzip"), temporary=T, remove = F, overwrite=T))
    
    setnames(OD_TI, "Total_Time", paste0("Total_Time_", pastas[j]))
    OD_TI[, concat := paste0(OriginName,'-', DestinationName)]
    OD_TI[,c("OriginName","DestinationName"):=NULL]
    
    if (j == 1) {
      OD_TI_temp <- OD_TI
      rm(OD_TI)
      gc()
    }
    
    if (j == 2) {
      OD_TI_concat <- merge(OD_TI_temp, OD_TI, by="concat", all=TRUE)
      rm(OD_TI)
      rm(OD_TI_temp)
      gc()
    }
    
    if (j>2){
      OD_TI_concat <- merge(OD_TI_concat, OD_TI, by="concat", all=TRUE)
      rm(OD_TI)
      gc()
    }
  }
  OD_TI_concat[, c("origin", "destination"):= tstrsplit(concat, "-", fixed=TRUE)]
  
  shp <- st_read(dsn="hexagonos.gdb", layer=cidades[i]) 
  shp <- shp%>%
    st_drop_geometry()%>%
    mutate("id_num" = rownames(shp)) %>%
    select("id_hex","id_num")%>%
    rename(origin = id_num)
  
  shp <- as.data.table(shp)  
  
  OD_TI_concat <- merge(OD_TI_concat, shp, by="origin") %>%
    rename(origin_hex = id_hex)
  
  shp <- shp %>%
    rename(destination = origin)
    
  OD_TI_concat <- merge(OD_TI_concat, shp, by="destination") %>%
    rename(destination_hex = id_hex)
  
  OD_TI_concat[, ':=' (median_morning_peak = rowMedians(as.matrix(.SD), na.rm = T)), .SDcols=c("Total_Time_06AM","Total_Time_0615AM","Total_Time_0630AM","Total_Time_0645AM","Total_Time_07AM","Total_Time_0715AM","Total_Time_0730AM","Total_Time_0745AM","Total_Time_08AM")]
  OD_TI_concat[, ':=' (median_afternoon_offpeak = rowMedians(as.matrix(.SD), na.rm = T)), .SDcols=c("Total_Time_02PM","Total_Time_0215PM","Total_Time_0230PM","Total_Time_0245PM","Total_Time_03PM","Total_Time_0315PM","Total_Time_0330PM","Total_Time_0345PM","Total_Time_04PM")]
  
  OD_TI_concat <- OD_TI_concat[, c("origin_hex","destination_hex","median_morning_peak","median_afternoon_offpeak")]
  
  fwrite(OD_TI_concat, paste0("OD_TI_", cidades[i],".csv"))
  rm(OD_TI_concat)
  rm(shp)
  gc()
}







# script somente para as cidades grandes (bsb, cgr, man, rio, spo) --------

cidades <- c("bsb","cgr","man","rio","spo")

for (i in (1:length(cidades))){
  temp1 <- fread(paste0("OD_TI_", cidades[i], "_1",".csv"))
  temp2 <- fread(paste0("OD_TI_", cidades[i], "_2",".csv"))
  temp3 <- fread(paste0("OD_TI_", cidades[i], "_3",".csv"))
  temp4 <- fread(paste0("OD_TI_", cidades[i], "_4",".csv"))
  temp5 <- fread(paste0("OD_TI_", cidades[i], "_5",".csv"))
  temp6 <- fread(paste0("OD_TI_", cidades[i], "_6",".csv"))
  
  temp <- rbind(temp1,temp2,temp3,temp4,temp5,temp6)
  setNames(temp,"origin_hex", "origin")
  setnames(temp, "destination_hex", "destination")
  fwrite(temp, paste0("OD_TI_", cidades[i],".csv"))
  
  rm(temp1)
  rm(temp2)
  rm(temp3)
  rm(temp4)
  rm(temp5)
  rm(temp6)
  rm(temp)
  gc()
}



