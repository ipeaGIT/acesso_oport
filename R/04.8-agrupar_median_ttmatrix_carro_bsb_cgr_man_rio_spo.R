
library(data.table)
library(dplyr)
setDTthreads(percent = 100)

setwd("//storage6/usuarios/Proj_acess_oport/git_diego")

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

