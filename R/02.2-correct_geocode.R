# carregar bibliotecas
source('./R/fun/setup.R')
library(googlesheets4)



# abrir dados corrigidos

# pegar endereco do gsheets de todas as cidades
a <- googledrive::drive_ls("~/aop_checagem_estabs/")
df_ss <- data.frame(ss = a$id,
                    sigla_muni = stringr::str_sub(a$name, -3, -1))

# empregos ----------------------------------------------------------------

sheets_years <- purrr::cross2(df_ss$ss, c(2017, 2018, 2019)) %>% rbindlist()

cnpj_corrigido <- purrr::map2(sheets_years$V1,
                              as.character(sheets_years$V2),
                              read_sheet,
                              col_types = "c"
) %>%
  rbindlist(fill = TRUE) %>%
  select(sigla_muni, ano, id_estab, geocode_engine, type_year_input, status_checagem, lon_corrigido, lat_corrigido)

# selecionar entao as checagens que vao ser deletados
cnpj_corrigido_deletar <- cnpj_corrigido %>%
  # selecionar somente os que vao ser deletados
  filter(status_checagem %in% c("deletar", "remover")) 

# selecionar entao as checagens que vao ter mudanca de geocode
cnpj_corrigido_geocode <- cnpj_corrigido %>%
  # selecionar somente os que vao ser mudados de geocode
  filter(status_checagem %in% c("geocode")) 

length(unique(cnpj_corrigido_geocode$id_estab))


# fazer a funcao para corrigir cada ano
corrigir_rais <- function(ano1) {
  
  # abrir rais
  rais <- read_rds(
    sprintf("../../data/acesso_oport/%s/%s/%s_%s_etapa4_geocoded_gmaps_gquality.rds", 
            "rais", ano1, "rais", ano1))
  
  # corrigir os estabs a serem deletados
  # table(nchar(rais$id_estab))
  # table(nchar(cnpj_corrigido_deletar$id_estab))
  rais <- rais[id_estab %nin% cnpj_corrigido_deletar$id_estab]
  
  # corrigir os estabs que vao ter mudanca de geocode
  cnpj_corrigido_geocode_ano <- cnpj_corrigido_geocode[ano == ano1]
  cnpj_corrigido_geocode_ano <- cnpj_corrigido_geocode_ano %>%
    select(id_estab, lon = lon_corrigido, lat = lat_corrigido) %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    mutate(geocode_engine = "gmaps_corrigido") %>% 
    setDT()
  
  
  rais[cnpj_corrigido_geocode_ano, on = c("id_estab"),
       c("lon", "lat", "geocode_engine") :=
         list(i.lon, i.lat, i.geocode_engine)]
  
  # table(rais$geocode_engine)
  
  # salvar
  write_rds(rais,
            sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected.rds",
                    ano1, ano1),
            compress = "gz")
  
}

# apply function
walk(c(2017, 2018, 2019), corrigir_rais)







# saude -------------------------------------------------------------------

sheets_years_saude <- purrr::cross2(df_ss$ss, c("2017_saude", "2018_saude", "2019_saude")) %>% rbindlist()

saude_corrigido <- purrr::map2(sheets_years_saude$V1,
                               sheets_years_saude$V2,
                               read_sheet,
                               col_types = "c"
) %>%
  rbindlist(fill = TRUE) %>%
  select(sigla_muni, ano, cnes, status_checagem, lon_corrigido, lat_corrigido)

# selecionar entao as checagens que vao ser deletados
saude_corrigido_deletar <- saude_corrigido %>%
  # selecionar somente os que vao ser deletados
  filter(str_detect(status_checagem, c("deletar|remover")))

# selecionar entao as checagens que vao ter mudanca de geocode
saude_corrigido_geocode <- saude_corrigido %>%
  # selecionar somente os que vao ser mudados de geocode
  filter(str_detect(status_checagem, c("geocode")))

# selecionar entao as checagens que vao ser agrupadas
saude_corrigido_agrupar <- saude_corrigido %>%
  # selecionar somente os que vao ser mudados de geocode
  filter(str_detect(status_checagem, c("agrupar")))



# fazer a funcao para corrigir cada ano
corrigir_saude <- function(ano1) {
  
  # abrir rais
  saude <- read_rds(
    sprintf("../../data/acesso_oport/%s/%s/%s_%s_filter_geocoded_gmaps_gquality.rds", 
            "saude", ano1, "saude", ano1))
  
  # corrigir os estabs a serem deletados
  # table(nchar(saude$cnes))
  # table(nchar(saude_corrigido_deletar$cnes))
  saude <- saude[cnes %nin% saude_corrigido_deletar$cnes]
  
  # corrigir os estabs que vao ter mudanca de geocode
  saude_corrigido_geocode_ano <- saude_corrigido_geocode %>%
    select(cnes, lon = lon_corrigido, lat = lat_corrigido) %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    mutate(geocode_engine = "gmaps_corrigido") %>% 
    setDT()
  
  
  saude[saude_corrigido_geocode_ano, on = c("cnes"),
       c("lon", "lat", "geocode_engine") :=
         list(i.lon, i.lat, i.geocode_engine)]
  
  # table(saude$geocode_engine)
  
  # corrigir os estabs que vao ser agrupados
  saude_corrigido_agrupar_ano <- saude_corrigido_agrupar %>%
    filter(ano == ano1) %>%
    # ordenar por cidade
    arrange(sigla_muni, status_checagem) %>%
    # create new identification
    mutate(a = rleid(sigla_muni, status_checagem))
  
  # trazer esses grupos para a base de saude
  saude[, a := "nao_agrupar"]
  saude[saude_corrigido_agrupar_ano, on = c("cnes"),
        c("a") :=
          list(i.a)]
  
  saude_nao_agrupar <- saude[a == "nao_agrupar"]
  saude_nao_agrupar <- saude_nao_agrupar %>% select(-a)
  saude_agrupar <- saude[cnes %in% saude_corrigido_agrupar_ano$cnes]
  saude_agrupar1 <- saude_agrupar %>%
    group_by(a) %>%
    mutate(health_low  = ifelse(sum(health_low) == 0, 0, 1),
           health_med  = ifelse(sum(health_med) == 0, 0, 1),
           health_high = ifelse(sum(health_high) == 0, 0, 1)) %>%
    summarise(across(everything(), ~ .x[[1]])) %>%
    select(-a)
  
  # juntar
  saude <- rbind(saude_nao_agrupar, saude_agrupar1)
  
  
  
  # salvar
  write_rds(saude,
            sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected.rds",
                    ano1, ano1),
            compress = "gz")
  
}

# apply function
walk(c(2017, 2018, 2019), corrigir_saude)











# educacao -------------------------------------------------------------------

sheets_years_educacao <- purrr::cross2(df_ss$ss, c("2017_educacao", "2018_educacao", "2019_educacao")) %>% rbindlist()

educacao_corrigido <- purrr::map2(sheets_years_educacao$V1,
                               sheets_years_educacao$V2,
                               read_sheet,
                               col_types = "c"
) %>%
  rbindlist(fill = TRUE) %>%
  select(sigla_muni, ano, co_entidade, status_checagem, lon_corrigido, lat_corrigido)

table(educacao_corrigido$status_checagem)

# selecionar entao as checagens que vao ser deletados
educacao_corrigido_deletar <- educacao_corrigido %>%
  # selecionar somente os que vao ser deletados
  filter(str_detect(status_checagem, c("deletar|remover")))

# selecionar entao as checagens que vao ter mudanca de geocode
educacao_corrigido_geocode <- educacao_corrigido %>%
  # selecionar somente os que vao ser mudados de geocode
  filter(str_detect(status_checagem, c("geocode")))

# selecionar entao as checagens que vao ser agrupadas
educacao_corrigido_agrupar <- educacao_corrigido %>%
  # selecionar somente os que vao ser mudados de geocode
  filter(str_detect(status_checagem, c("agrupar")))



# fazer a funcao para corrigir cada ano
corrigir_educacao <- function(ano1) {
  
  # abrir rais
  educacao <- read_rds(
    sprintf("../../data/acesso_oport/%s/%s/%s_%s_filter_geocoded_gmaps_gquality.rds", 
            "educacao", ano1, "educacao", ano1))
  
  # corrigir os estabs a serem deletados
  # table(nchar(educacao$cnes))
  # table(nchar(educacao_corrigido_deletar$cnes))
  educacao <- educacao[co_entidade %nin% educacao_corrigido_deletar$co_entidade]
  
  # corrigir os estabs que vao ter mudanca de geocode
  educacao_corrigido_geocode_ano <- educacao_corrigido_geocode %>%
    select(co_entidade, lon = lon_corrigido, lat = lat_corrigido) %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    mutate(geocode_engine = "gmaps_corrigido") %>% 
    setDT()
  
  
  educacao[educacao_corrigido_geocode_ano, on = c("co_entidade"),
       c("lon", "lat", "geocode_engine") :=
         list(i.lon, i.lat, i.geocode_engine)]
  
  
  # salvar
  write_rds(educacao,
            sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected.rds",
                    ano1, ano1),
            compress = "gz")
  
}

# apply function
walk(c(2017, 2018, 2019), corrigir_educacao)



