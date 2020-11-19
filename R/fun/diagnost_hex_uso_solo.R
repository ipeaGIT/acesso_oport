### funcao para diagnostico da distribuicao de atividades 9uso do solo) por hexagonos
# A funcao retorna um data.frame / sf com o CNPJ e quantidade de empregos das empresas em cada hexagono.



diagnost_hex_uso_solo <- function(sigla_muni, uso_do_solo, corte) {
  
  # sigla_muni <- "bsb"
  # uso_do_solo <- "saude"
  # corte <- 5
  
  # Qual o codigo do municipio em questao?
  cod_mun_ok <- munis_df_2019[abrev_muni == sigla_muni]$code_muni
  
  if (uso_do_solo == "trabalho") {
    
    base <- empregos %>% 
      filter(codemun == substr(cod_mun_ok, 1, 6)) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      select(codemun, cnae.setor, id_estab, razao_social, clas_cnae20, total_corrigido, SearchedAddress, MatchedAddress,
             geocode_engine, PrecisionDepth)
    
  } else if (uso_do_solo == "educacao") {
    
    base <- escolas %>%
      filter(cod_mun == cod_mun_ok) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      sfc_as_cols()
    
  } else if (uso_do_solo == "saude") {
    
    base <- cnes %>% filter(code_muni == substr(cod_mun_ok, 1, 6)) %>%
      select(code_muni, CNES, ESTABELECIMENTO, TIPO_UNIDADE, LOGRADOURO, NUMERO) %>%
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
  hex <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2018/hex_agregado_%s_09_2018.rds", sigla_muni)) %>%
    select(id_hex, UQ(as.symbol(var_go)))
  
  # filtra hex ids acima do corte
  hex_corte <- hex %>%
    filter(UQ(as.symbol(var_go)) > corte)
  
  # Intersecao do hex ids problema com base de uso do solo
  fim <- st_join(hex_corte, base)
  
  # mapview(hex_join)
  # 
  # # estabelecimentos com mais de 3000 vinculos? (SO PARA TRABALHO!)
  # hex_join %>%
  #   filter(total_corrigido > 3000) %>%
  #   View()
  
}