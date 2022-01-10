library(data.table)
library(R.utils)
library(sf)
library(dplyr)
library(matrixStats)
setDTthreads(percent = 100)

setwd("//storage6/usuarios/Proj_acess_oport/git_diego/NOVA_MATRIZ_TI")

#cidades <- c("bsb", "cgr", "man", "rio","spo")
cidades <- c("rio","spo")
pastas <- c("06AM","0615AM","0630AM","0645AM","07AM","0715AM","0730AM","0745AM","08AM","02PM","0215PM","0230PM","0245PM","03PM","0315PM","0330PM","0345PM","04PM")

for (i in (1:length(cidades))){
  for (k in (1:6)){
    for (j in (1:length(pastas))){
      print(cidades[i])
      print(pastas[j])
      print(k)
      
      if (k==1){
        OD_TI_pre <- fread(gunzip(paste0(cidades[i],"/wkday", pastas[j],"/",pastas[j], ".csv.gzip"), temporary=T, remove = F, overwrite=T))
        x <- length(unique(OD_TI_pre$OriginName))/6
        OD_TI <- OD_TI_pre[OriginName <= k*x]
        rm(OD_TI_pre)
        gc()
      }
      
      if (k>1 & k<6){
        OD_TI_pre <- fread(gunzip(paste0(cidades[i],"/wkday", pastas[j],"/",pastas[j], ".csv.gzip"), temporary=T, remove = F, overwrite=T))
        OD_TI <- OD_TI_pre[OriginName > (k-1)*x & OriginName <= k*x]
        rm(OD_TI_pre)
        gc()
      }
      
      if (k==6){
        OD_TI_pre <- fread(gunzip(paste0(cidades[i],"/wkday", pastas[j],"/",pastas[j], ".csv.gzip"), temporary=T, remove = F, overwrite=T))
        OD_TI <- OD_TI_pre[OriginName > (k-1)*x]
        rm(OD_TI_pre)
        gc()
      }
      
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
  
  fwrite(OD_TI_concat, paste0("OD_TI_", cidades[i], "_",k,".csv"))
  rm(OD_TI_concat)
  rm(shp)
  gc()
  }
}

