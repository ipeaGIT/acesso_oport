### funcao para diagnostico da distribuicao de atividades 9uso do solo) por hexagonos
# A funcao retorna um data.frame / sf com o CNPJ e quantidade de empregos das empresas em cada hexagono.



diagnost_hex_empregos <- function(sigla_muni, corte, ano) {
  
  # sigla_muni <- "bsb"
  # sigla_muni <- "rio"
  # uso_do_solo <- "saude"
  # corte <- 5
  
  # Qual o codigo do municipio em questao?
  cod_mun_ok <- munis_df[abrev_muni == sigla_muni]$code_muni
  
  base <- rais %>% 
    filter(codemun == substr(cod_mun_ok, 1, 6)) %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    select(codemun, cnae.setor, id_estab, razao_social, clas_cnae20, total_corrigido, SearchedAddress, MatchedAddress,
           geocode_engine, PrecisionDepth)
  
  # ler hex agregados
  hex <- read_rds(sprintf("../../data/acesso_oport/rais/%s/hex_agregados_teste/hex_agregados_teste_%s_%s.rds", 
                          ano, sigla_muni, ano)) %>%
    select(id_hex, empregos_total)
  
  # filtra hex ids acima do corte
  hex_corte <- hex %>%
    filter(empregos_total > corte)
  
  # Intersecao do hex ids problema com base de uso do solo
  fim <- st_join(hex_corte, base)
  
  
  # maybe these rrepreents a too many hex problem
  hex_probs <- fim %>%
    group_by(id_hex, PrecisionDepth, codemun) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(id_hex, codemun) %>%
    mutate(prop = n/sum(n)) %>%
    filter(PrecisionDepth == "3 Estrelas", prop > 0.5)
  
}