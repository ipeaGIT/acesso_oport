# Agrega informacoes demograficas e de uso do solo nos hexagonos

# carregar bibliotecas -----------------------------------------------------------------------------
sf::sf_use_s2(FALSE)
source('./R/fun/setup.R')

# ano <- 2018

#' A funcao `agrupar_variaveis_hex` agrega as variaveis de emprego, educacao, saude
#' e demograficas das grades estisticas para os hexagonos de cada cidade

agrupar_variaveis_hex <- function(ano, munis = "all") {
  
  # 1) Abrir arquivos com as oportunidades -------------------------------------
  
  # 1.1) Saude
  cnes_data <- readr::read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano, ano)) 
  
  # remove lat lon missing
  cnes_data <- cnes_data[!is.na(lat),] 
  
  # select columns
  cnes_data <- cnes_data[, .(cnes, code_muni,
                             health_low, health_med, health_high,
                             lon, lat)]
  
  
  # 1.2) Escolas
  escolas <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano, ano))
  
  # remove lat lon missing
  escolas <- escolas[!is.na(lat),] 
  
  # select columns
  escolas <- escolas[, .(co_entidade, code_muni,
                         mat_infantil, mat_fundamental, mat_medio,
                         lon, lat)]
  
  
  # 1.3) Empregos
  empregos <- readr::read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected_escola.rds", ano, ano))
  
  # remove lat lon missing
  empregos <- empregos[!is.na(lat), ]
  
  # select columns
  empregos <- empregos[, .(code_muni, id_estab, baixo, medio, alto, lon, lat)]
  
  # 1.3) CRAS
  cras <- read_rds(sprintf("../../data/acesso_oport/cras/cras_%s_geocoded.rds", ano))
  
  # remove lat lon missing
  cras <- cras[!is.na(lat),] 
  
  # select columns
  cras <- cras[, .(code_cras, code_muni,
                   lon, lat)]
  
  
  #' A funcao `agrupar_variaveis` agrupa as variaveis de uso do solo determinadas acima
  #'  nos hexagonos de cada um dos municipios
  #'  Tambem traz as informacoes demograficas da grade do ibge
  
  agrupar_variaveis <- function(sigla_muni) { 
    
    # sigla_muni <- "for"
    # sigla_muni <- "rec"
    # sigla_muni <- "nat"
    # sigla_muni <- "rio"
    # sigla_muni <- "bho"
    # sigla_muni <- "bsb"
    # sigla_muni <- "spo"
    
    # status message
    message('Woking on city ', sigla_muni, '\n')
    
    # resolucoes disponiveis
    res <- c("08", "09")
    
    # Pegar endereco das grades e hexagonos em todas resolucoes
    grade_file <- sprintf("../../data/acesso_oport/grade_municipio_com_renda_cor/%s/grade_renda_cor_%s_%s.rds", ano, sigla_muni, ano)
    
    # Gerar centroide da grade de cada municipio
    centroide_pop <- readr::read_rds(grade_file) %>%
      dplyr::select(id_grade, pop_total, pop_homens, pop_mulheres,
                    renda,  
                    cor_branca, cor_amarela, cor_indigena, cor_negra,  
                    idade_0a5,
                    idade_6a14,
                    idade_15a18,
                    idade_19a24,
                    idade_25a39,
                    idade_40a69,
                    idade_70   ) %>%
    st_centroid()
    
    # sum(centroide_pop$pop_total, na.rm = TRUE) 
    # sum(centroide_pop$pop_homens)+ sum(centroide_pop$pop_mulheres) 
    # centroide_pop %>% filter(is.na(pop_total))
    # centroide_pop %>% filter(pop_total == 0, renda > 0)
    
    # Qual o codigo do municipio em questao?
    cod_mun_ok <- munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
      unlist()
    
    # Filtrar somente as atividades referentes a cada municipio e transforma em sf
    # para rais 2017
    empregos_filtrado <- empregos[code_muni %in% substr(cod_mun_ok, 1, 6)] %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # escolas
    escolas_filtrado <- escolas[code_muni %in% cod_mun_ok] %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # saude
    cnes_filtrado <- cnes_data[code_muni %in% substr(cod_mun_ok, 1, 6)] %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # cras
    cras_filtrado <- cras[code_muni %in% cod_mun_ok] %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    
    
    # FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES
    
    por_resolucao <- function(muni_res) {
      # muni_res <- '09'
      # muni_res <- '08'
      
      # endereco do hexagono na resolucao
      hexf <- sprintf("../../data/acesso_oport/hex_municipio/%s/hex_%s_%s_%s.rds", ano, sigla_muni, muni_res, ano)
      
      # Ler arquivo de hexagono  
      hex_muni <- readr::read_rds(hexf)
      hex_muni <- hex_muni %>% mutate(ano = ano)
      
      # Agrupar populacao, cor e renda
      # join espacial
      hex_pop <- hex_muni %>% st_join(centroide_pop)
      head(centroide_pop)
      head(hex_pop)
      sum(hex_pop$pop_total, na.rm = TRUE)
      sum(centroide_pop$pop_total, na.rm = TRUE)
      
      # Summarize
      hex_pop <- setDT(hex_pop)[, .(cor_branca   = as.numeric(sum(round(cor_branca,0), na.rm = TRUE)),
                                    cor_amarela  = as.numeric(sum(round(cor_amarela,0), na.rm = TRUE)),
                                    cor_indigena = as.numeric(sum(round(cor_indigena,0), na.rm = TRUE)),
                                    cor_negra    = as.numeric(sum(round(cor_negra,0), na.rm = TRUE)),
                                    # age variables
                                    idade_0a5   = as.numeric(sum(idade_0a5   , na.rm = TRUE)),
                                    idade_6a14 =  as.numeric(sum(idade_6a14 , na.rm = TRUE)),
                                    idade_15a18 = as.numeric(sum(idade_15a18 , na.rm = TRUE)),
                                    idade_19a24 = as.numeric(sum(idade_19a24 , na.rm = TRUE)),
                                    idade_25a39 = as.numeric(sum(idade_25a39 , na.rm = TRUE)),
                                    idade_40a69 = as.numeric(sum(idade_40a69 , na.rm = TRUE)),
                                    idade_70 =    as.numeric(sum(idade_70 , na.rm = TRUE)),
                                    # total pop and income
                                    pop_total    = as.numeric(sum(pop_total, na.rm = TRUE)),
                                    pop_homens   = as.numeric(sum(round(pop_homens,0), na.rm = TRUE)),
                                    pop_mulheres = as.numeric(sum(round(pop_mulheres,0), na.rm = TRUE)),
                                    renda_total  = as.numeric(sum(renda, na.rm = TRUE))),
                                by = id_hex ]
      
      hex_pop1 <- hex_pop %>%
        left_join(hex_muni, by = "id_hex") %>% st_sf()
      
      # mapview(hex_pop1)
      # ggplot() + geom_sf(data = hex_pop1, aes(fill = pop_total), color = NA)+
      #   scale_fill_viridis_c()
      
      # sum(hex_pop$pop_total)
      # sum(hex_pop$pop_homens, hex_pop$pop_mulheres)
      # sum(hex_pop$cor_branca, hex_pop$cor_amarela, hex_pop$cor_indigena, hex_pop$cor_negra)
      # hex_pop[pop_total == 0]
      # hex_pop[pop_total == 0 & pop_homens > 0]
      # hex_pop[pop_total == 0 & pop_mulheres > 0]
      # hex_pop[pop_total == 0 & renda_total > 0]
      # hex_pop[pop_total > 0 & renda_total == 0]
      hex_pop[, renda_total := fifelse(pop_total == 0, 0, renda_total)]
      hex_pop[, pop_total := fifelse(renda_total == 0, 0, pop_total)]
      
      
      # Calcular quintil e decil de renda
      # calcula renda per capita de cada hexagono
      hex_pop[, renda_capita := renda_total / pop_total]
      
      
      # 89a8d18d423ffff bsb
      
      # calcular quintis ponderados pela populacao
      deciles  <- Hmisc::wtd.quantile(hex_pop$renda_capita, weights=hex_pop$pop_total, 
                                      probs=c( seq(0 , 1 , 0.1) ), 
                                      type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                      normwt=FALSE, na.rm=T)
      
      quintiles  <- Hmisc::wtd.quantile(hex_pop$renda_capita, weights=hex_pop$pop_total, 
                                        probs=c( seq(0 , 1 , 0.2) ), 
                                        type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                        normwt=FALSE, na.rm=T)
      
      # classificar cada hexagono em cada quintil de renda
      hex_pop[, quintil := findInterval(renda_capita , quintiles[ -length(quintiles) ] ) ]
      hex_pop[, decil := findInterval(renda_capita , deciles[ -length(deciles) ] ) ]
      
      # check if pop size in each decile are roughly equal
      hex_pop[, .(po_in_quintil = sum(pop_total, na.rm=T)), by = quintil]
      hex_pop[, .(po_in_decile  = sum(pop_total, na.rm=T)), by = decil]
      
      # # remove NAs
      # hex_pop <- hex_pop[ !is.na(decil)]
      # hex_pop <- hex_pop[ !is.na(quintil)]
      
      # Agrupar empregos
      # join espacial 
      hex_rais <- hex_muni %>% st_join(empregos_filtrado) %>% setDT()
      
      # Summarize
      hex_rais <- hex_rais[, .(empregos_baixa = sum(baixo, na.rm = TRUE),
                               empregos_media = sum(medio, na.rm = TRUE),
                               empregos_alta  = sum(alto, na.rm = TRUE),
                               empregos_total = sum(alto, medio, baixo, na.rm = TRUE)), 
                           by = id_hex ]
      
      
      # setDT(hex_muni)[, empregos_total := sum(empregos_alta, empregos_media, empregos_baixa), by=id_hex]
      
      
      # agrupar saude
      # join espacial 
      cnes_filtrado <- sf::st_transform(cnes_filtrado, sf::st_crs(hex_muni)) # mesma projecao geografica
      hex_saude <- hex_muni %>% st_join(cnes_filtrado) %>% setDT()
      
      hex_saude[, saude_total := ifelse( is.na(cnes), 0, 1) ]
      
      # Summarize
      hex_saude <- hex_saude[, .(saude_total = sum(saude_total, na.rm=T),
                                 saude_baixa = sum(health_low, na.rm=T),
                                 saude_media = sum(health_med, na.rm=T),
                                 saude_alta  = sum(health_high, na.rm=T)),
                             by = id_hex ]
      
      
      
      
      # agrupar educacao
      # join espacial 
      hex_escolas <- hex_muni %>% st_join(escolas_filtrado) %>% setDT()
      hex_escolas <- hex_escolas[!is.na(co_entidade)]
      
      # Summarize
      hex_escolas[, ':='(edu_infantil    = fifelse(mat_infantil    == 0, 0, 1),
                         edu_fundamental = fifelse(mat_fundamental == 0, 0, 1),
                         edu_medio       = fifelse(mat_medio == 0, 0, 1))]
      
      hex_escolas <- hex_escolas[, .(edu_infantil      = sum(edu_infantil, na.rm = TRUE),
                                     edu_fundamental = sum(edu_fundamental, na.rm = TRUE),
                                     edu_medio       = sum(edu_medio, na.rm = TRUE),
                                     edu_total       = .N,
                                     mat_infantil    = sum(mat_infantil, na.rm = T),
                                     mat_fundamental = sum(mat_fundamental, na.rm = T),
                                     mat_medio       = sum(mat_medio, na.rm = T),
                                     mat_total       = sum(mat_infantil, mat_fundamental, mat_medio, na.rm = TRUE)),
                                 by = id_hex]
      
      
      # summary(hex_escolas$edu_total)
      
      # agrupar cras
      hex_cras <- hex_muni %>% st_join(cras_filtrado) %>% setDT()
      hex_cras[, cras_total := ifelse( is.na(code_cras), 0, 1) ]
      
      # summarise
      hex_cras <- hex_cras[, .(cras_total = sum(cras_total, na.rm = TRUE)),
                           by = id_hex]
      
      # Junta todos os dados agrupados por hexagonos
      hex_muni_fim <- left_join(hex_muni, hex_pop) %>%
        left_join(hex_rais) %>%
        left_join(hex_saude) %>%
        left_join(hex_escolas) %>%
        left_join(hex_cras)
      
      # substitui NAs por zeros
      hex_muni_fim[is.na(hex_muni_fim)] <- 0
      
      
      # Salva grade de hexagonos com todas informacoes de uso do soloe
      dir_output <- sprintf("../../data/acesso_oport/hex_agregados/%s/hex_agregado_%s_%s_%s.rds", ano, sigla_muni, muni_res, ano)
      readr::write_rds(hex_muni_fim, dir_output)
      
    }
    
    # Aplicar funcao para cada resolucao
    walk(res, por_resolucao)
    
  }
  
  # Aplica funcao para cada municipio
  if (munis == "all") {
    
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  # future::plan(future::multiprocess, workers = 10)
  #options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
  # furrr::future_walk(x, agrupar_variaveis)
  walk(x, agrupar_variaveis)
  
  
}



# aplicar funcao -----------------
agrupar_variaveis_hex(ano = 2017)
agrupar_variaveis_hex(ano = 2018)
agrupar_variaveis_hex(ano = 2019)




# basic checks -------------------------------
hex_2017 <- lapply(dir("../../data/acesso_oport/hex_agregados/2017/", full.names = TRUE),
                   read_rds) %>% rbindlist()
hex_2017 <- hex_2017[h3_resolution == 9]
hex_2018 <- lapply(dir("../../data/acesso_oport/hex_agregados/2018/", full.names = TRUE),
                   read_rds) %>% rbindlist()
hex_2018 <- hex_2018[h3_resolution == 9]
hex_junto <- rbind(hex_2017, hex_2018)

hex_2017[pop_total == 0 & renda_total > 0]
hex_2017[pop_total > 0 & renda_total == 0]
hex_pop[pop_total > 0 & idade_0a5 + idade_6a14 + idade_15a18 + idade_19a24 + idade_25a39  + idade_40a69 + idade_70 == 0]
hex_pop[pop_total > 0 & cor_branca + cor_amarela + cor_indigena + cor_negra == 0]



# maps --------------------------------------------------------------------

maps_pop <- function(sigla_munii) {
  
  # sigla_munii <- "for"
  # sigla_munii <- "bho"
  
  hex_2017a <- hex_junto %>%
    filter(sigla_muni == sigla_munii) %>%
    st_sf(crs = 4326)

  
  # put limits to 2000
  hex_2017a <- hex_2017a %>%
    mutate(pop_total = ifelse(pop_total > 5000, 5000, pop_total))
  
  a <- ggplot()+
    geom_sf(data = hex_2017a, aes(fill = pop_total), color = NA)+
    scale_fill_viridis_c()+
    theme_void()+
    facet_wrap(vars(ano), ncol = ifelse(sigla_munii == "rio", 1, 2))+
    labs(title = sigla_munii)
  
  ggsave(plot = a, filename = sprintf("testes/hex_pop/hex_pop_%s.png", sigla_munii))
  
  
  
}

walk(munis_list$munis_modo$abrev_muni, maps_pop)


# quantos hex com pop == 1?

hex_pop1 <- hex_junto[pop_total == 1, .(hex_pop1 = .N), by = .(sigla_muni, ano)]
hex_pop1 <- pivot_wider(hex_pop1, names_from = ano, values_from = hex_pop1,
                        values_fill = 0)
