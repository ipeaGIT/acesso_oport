library(readr)
library(data.table)

# this script make some fix in the final hex_agregados database




# fix bsb ------------------------------------------------------------
# health count for 2017 are wrong for bsb
# se we will input these counts from 2018

hex <- read_rds("../../data/acesso_oport/hex_agregados/2017/hex_agregado_bsb_09_2017.rds") %>% setDT()
hex1 <- read_rds("../../data/acesso_oport/hex_agregados/2018/hex_agregado_bsb_09_2018.rds") %>% setDT()
hex2 <- read_rds("../../data/acesso_oport/hex_agregados/2019/hex_agregado_bsb_09_2019.rds") %>% setDT()

# check saude columns
colnames(hex)[colnames(hex) %like% "saude"]

# fix
hex1_mod <- hex1[, .(id_hex, saude_total, saude_baixa, saude_media, saude_alta )]

# join

sum(hex$saude_total)
hex[hex1_mod, on = "id_hex",
    c("saude_total", "saude_baixa", "saude_media", "saude_alta") := 
      list(i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta)]
sum(hex$saude_total)
# ok

# cras count from 2018 is 0
# were gonna use the counts from 2019
hex2_mod <- hex2[, .(id_hex, cras_total)]
# join

sum(hex1$cras_total)
hex1[hex2_mod, on = "id_hex",
    c("cras_total") := 
      list(i.cras_total)]
sum(hex1$cras_total)


# save it
write_rds(hex, "../../data/acesso_oport/hex_agregados/2017/hex_agregado_bsb_09_2017.rds")
write_rds(hex1, "../../data/acesso_oport/hex_agregados/2018/hex_agregado_bsb_09_2018.rds")
