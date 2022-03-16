library(readr)
library(data.table)

# this script make some fix in the final hex_agregados database




# fix bsb ------------------------------------------------------------
# health count for 2017 are wrong for bsb
# se we will input these counts from 2018

hex <- read_rds("../../data/acesso_oport/hex_agregados/2017/hex_agregado_bsb_09_2017.rds") %>% setDT()
hex1 <- read_rds("../../data/acesso_oport/hex_agregados/2018/hex_agregado_bsb_09_2018.rds") %>% setDT()

# check saude columns
colnames(hex)[colnames(hex) %like% "saude"]

# fix
hex1 <- hex1[, .(id_hex, saude_total, saude_baixa, saude_media, saude_alta )]

# join

sum(hex$saude_total)
hex[hex1, on = "id_hex",
    c("saude_total", "saude_baixa", "saude_media", "saude_alta") := 
      list(i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta)]
sum(hex$saude_total)
# ok

# save it
write_rds(hex, "../../data/acesso_oport/hex_agregados/2017/hex_agregado_bsb_09_2017.rds")
