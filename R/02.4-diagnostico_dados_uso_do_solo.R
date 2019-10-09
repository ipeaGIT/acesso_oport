# spo
# saude - 89a8100c397ffff
# emprego - 89a8100eccfffff
#
# for
#
#
#
# ter
# saude -89800554cc3ffff
# edu - '898005563abffff', '89800554c8fffff' 
# emprego - 898005548b3ffff
#



# carregar bibliotecas
source('./R/fun/setup.R')





# ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------

# Saude --------------------------------------
cnes <- readr::read_rds("../data/hospitais/health_facilities2019_filtered.rds") 

cnes <- cnes[!is.na(lat),] 
# cnes <- cnes[!is.na(NIV_HIER),] 

cnes <- cnes %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)




# Escolas  -------------------------------------
# abrir censo escolar geo
escolas <- read_rds("../data/censo_escolar/educacao_inep_2019.rds") %>%
  # Deletar escolas q nao foram localizadas
  dplyr::filter(!is.na(lat)) %>%
  # Selecionar variaveis
  dplyr::select(cod_escola = CO_ENTIDADE, uf = CO_UF, municipio = NO_MUNICIPIO, 
                cod_mun = CO_MUNICIPIO, 
                NO_ENTIDADE.x,
                mat_infantil, mat_fundamental, mat_medio, lon, lat)
# tidyr::gather(tipo, mat_n, mat_infantil:mat_medio)




# Empregos ----------------------------------------------------------
# Abrir rais geo
empregos <- readr::read_rds("../data/rais/rais_2017_corrigido_cidades_selecionadas2019.rds") # para 2017




# FUNCAO --------------------------------------------------------------------------------------

diagnost_hex_us_prop <- function(sigla_muni, uso_do_solo, corte) {
  
  # sigla_muni <- "bsb"
  # uso_do_solo <- "trabalho"
  # corte <- 10000
  
  # Qual o codigo do municipio em questao?
  cod_mun_ok <- munis_df[abrev_muni == sigla_muni]$code_muni

  if (uso_do_solo == "trabalho") {
    
    base <- empregos %>% 
      filter(codemun == substr(cod_mun_ok, 1, 6)) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      select(codemun, cnae.setor, id_estab, clas_cnae10, total_corrigido)
    
  } else if (uso_do_solo == "educacao") {
    
    base <- escolas %>%
      filter(cod_mun == cod_mun_ok) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  } else if (uso_do_solo == "saude") {
    
    base <- cnes %>% filter(code_muni == substr(cod_mun_ok, 1, 6)) %>%
      select(code_muni, CNES, ESTABELECIMENTO, TIPO_UNIDADE, LOGRADOURO) %>%
      sfc_as_cols()
  }
      
  
  df_apoio <- data.frame(
    us = c("trabalho", "educacao", "saude"),
    var = c("empregos_total", "edu_total", "saude_total"),
    stringsAsFactors = FALSE
  )
  
  # pegar nome da variavel
  var_go <- subset(df_apoio, us == uso_do_solo)$var
      
  # ler hex agregados
  hex <- read_rds(sprintf("../data/hex_agregados/hex_agregado_%s_09.rds", sigla_muni)) %>%
    select(id_hex, UQ(as.symbol(var_go)))

  # filtra hex ids acima do corte
  hex_corte <- hex %>%
    filter(UQ(as.symbol(var_go)) > corte)
  
  # Intersecao do hex ids problema com base de uso do solo
  hex_join <- st_join(hex_corte, base)
  
  mapview(hex_join)
  
  # estabelecimentos com mais de 3000 vinculos? (SO PARA TRABALHO!)
  hex_join %>%
    filter(total_corrigido > 3000) %>%
    View()

}


# SPO:
# - Educacao - OK (mesmo estabelecimento duplicado para diferentes níveis) - 7 hexagonos com mais de 5 escolas

# - Saude - Hexagono com 397 estabelecimentos - CNES com o mesmo lat lon
# - Trabalho