#' ## Empregos
#' 
#' Os empregos são extraídos da base da RAIS (Relação Anual de Informações Sociais). A base foi georreferenciada por um software que retorna as coordenadas de latitute e longitude com uma avalição da qualidade do georreferenciamento. Com isso, as etapas do tratamento dessa base foram:
#' 
#' - Deletar observações que tiveram georreferenciamento de 1 estrela (só conseguiu achar a cidade). Isso garante uma precisão suficiente para as análises seguintes;
#' - Selecionar as colunas de interesse: ``id_estab``, que é o id do estabelecimento,  ``qt_vinc_ativos``, que é a quantidade de vínculos ativos, ``cod_mun``, que é o código do município, e coordenadas.
#' - Salvar para o arquivo ``rais_2015.rds``.
#' 
#' 
## ----rais----------------------------------------------------------------

library(foreign)

# Abrir RAIS (formato stata)
rais_raw <- read.dta("../data-raw/rais/estab_2015_vinc_coord.dta")
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
# Transformar para sf
rais_v1 <- st_as_sf(rais_v1, coords = c("lon", "lat"), crs = 4326)


# Salvar
write_rds(rais_v1, "../data/rais/rais_2015.rds")


#' 
#' Sugestão futura: analisar a qualidade do georreferenciamento para as grandes cidades.
#' 
