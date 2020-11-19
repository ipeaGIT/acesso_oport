# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Calcular acessibilidade

# carregar bibliotecas
source('./R/fun/setup.R')



#### 1. CALCULAR ACESSIBILIDADE --------------------------------------------------------------



# sigla_muni <- "bho"; ano=2019
# sigla_muni <- "spo"; ano=2019
# sigla_muni <- "for"; ano=2019
# sigla_muni <- "for"; ano = 2017; engine = 'otp'


calcular_acess_muni <- function(sigla_muni, ano, engine = 'otp') {
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano,  '\n')
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  
  if(engine == "r5") {
    
    ttmatrix_median <- read_rds(sprintf("E:/data/output_ttmatrix/2017/r5/ttmatrix_for_pt_2017.csv",
                                        ano, engine, sigla_muni, ano))
    
  } else {
    
    ttmatrix_median <- read_rds(sprintf("E:/data/ttmatrix_mediana_fix/%s/ttmatrix_mediana_fix_%s_%s.rds", ano, sigla_muni, ano))
    
  }
  
  
  # 2) Agregar dados de uso do solo à ttmatrix --------------------------
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../../data/acesso_oport/hex_agregados/%s/hex_agregado_%s_09_%s.rds", ano, sigla_muni, ano)
  
  # Abrir oportunidades com hexagonos
  hexagonos_sf <- readr::read_rds(dir_hex) 
  
  # Filtrar apenas colunas com info demograficas na origem
  hex_orig <- setDT(hexagonos_sf)[, .(id_hex, pop_total, cor_branca, cor_amarela, cor_indigena, cor_negra, renda_total, renda_capita, quintil, decil)]
  
  # Filtrar apenas colunas com info de uso do solo no destino
  hex_dest <- setDT(hexagonos_sf)[, .(id_hex, empregos_total, empregos_baixa, empregos_media, empregos_alta,  
                                      saude_total, saude_baixa, saude_media, saude_alta,
                                      edu_total, edu_infantil, edu_fundamental, edu_medio)]
  
  
  # Merge dados de origem na matrix de tempo de viagem
  ttmatrix <- ttmatrix_median[hex_orig, on = c("origin" = "id_hex"),  
                              c('pop_total','cor_branca','cor_amarela','cor_indigena','cor_negra','renda_total','renda_capita','quintil','decil') :=
                                list(i.pop_total, i.cor_branca, i.cor_amarela, i.cor_indigena, i.cor_negra, i.renda_total, i.renda_capita, i.quintil, i.decil)]
  
  # Merge dados de destino na matrix de tempo de viagem
  ttmatrix <- ttmatrix[hex_dest, on = c("destination" = "id_hex"),  
                       c("empregos_total", "empregos_baixa","empregos_media","empregos_alta",
                         "saude_total", "saude_baixa", "saude_media", "saude_alta",
                         "edu_total","edu_infantil","edu_fundamental","edu_medio") :=
                         list(i.empregos_total, i.empregos_baixa,i.empregos_media,i.empregos_alta,
                              i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta,
                              i.edu_total,i.edu_infantil,i.edu_fundamental,i.edu_medio)]    
  
  # Calcular emprego com match qualitativo de renda e nivel de escolaridade do emprego 
  # high income people = jobs with high and med education
  # low income people = jobs with low and med education
  ttmatrix[, empregos_match_decil := ifelse(decil>5, 
                                            empregos_alta + empregos_media, 
                                            empregos_baixa + empregos_media)]
  
  ttmatrix[, empregos_match_quintil := ifelse(quintil>=3, 
                                              empregos_alta + empregos_media, 
                                              empregos_baixa + empregos_media)]
  
  
  ### formato final para disponibilizar dados publicamente
  # # Construir base dos dados de populacao, renda e uso do solo
  # vars_df <- hexagonos_sf %>%
  #   select(id_hex, 
  #          # Selecionar variaveis de populacao
  #          P001 = pop_total, P002 = cor_branca, P003 = cor_negra, P004 = cor_indigena, P005 = cor_amarela,
  #          # Selecionar variveis de renda
  #          R001 = renda_capita, R002 = quintil, R003 = decil,
  #          # Selecionar atividades de trabalho
  #          T001 = empregos_total, T002 = empregos_baixa, T003 = empregos_media, T004 = empregos_baixa,
  #          # Selecionar atividades de educacao
  #          E001 = edu_total, E002 = edu_infantil, E003 = edu_fundamental, E004 = edu_medio,
  #          # Selecionar atividades de saude (por enquanto so saude total)
  #          S001 = saude_total)
  
  
  # Calcular totais para cidades
  # populacao
  ttmatrix[, total_pop := sum(hexagonos_sf$pop_total, na.rm=T)]
  ttmatrix[, total_branca := sum(hexagonos_sf$cor_branca, na.rm=T)]
  ttmatrix[, total_amarela := sum(hexagonos_sf$cor_amarela, na.rm=T)]
  ttmatrix[, total_indigena := sum(hexagonos_sf$cor_indigena, na.rm=T)]
  ttmatrix[, total_negra  := sum(hexagonos_sf$cor_negra, na.rm=T)]
  
  # saude
  ttmatrix[, total_saude := sum(hexagonos_sf$saude_total, na.rm=T)]
  ttmatrix[, total_saude_baixa := sum(hexagonos_sf$saude_baixa, na.rm=T)]
  ttmatrix[, total_saude_media := sum(hexagonos_sf$saude_media, na.rm=T)]
  ttmatrix[, total_saude_alta := sum(hexagonos_sf$saude_alta, na.rm=T)]
  
  # educacao
  ttmatrix[, total_edu  := sum(hexagonos_sf$edu_total, na.rm=T)]
  ttmatrix[, total_edu_infantil  := sum(hexagonos_sf$edu_infantil, na.rm=T)]
  ttmatrix[, total_edu_fundamental  := sum(hexagonos_sf$edu_fundamental, na.rm=T)]
  ttmatrix[, total_edu_medio  := sum(hexagonos_sf$edu_medio, na.rm=T)]
  
  # empregos
  # criar totais dos empregos
  total_empregos_media_baixa <- sum(c(hexagonos_sf$empregos_media, hexagonos_sf$empregos_baixa), na.rm=T)
  total_empregos_media_alta <- sum(c(hexagonos_sf$empregos_media, hexagonos_sf$empregos_alta), na.rm=T)
  
  # colocar o total de empregos da cidade de cada classificacao
  ttmatrix[, total_empregos := sum(hexagonos_sf$empregos_total, na.rm=T)]
  ttmatrix[, total_empregos_match_decil := ifelse(decil>5, total_empregos_media_alta, 
                                                  total_empregos_media_baixa)]
  ttmatrix[, total_empregos_match_quintil := ifelse(quintil>=3, total_empregos_media_alta, 
                                                    total_empregos_media_baixa)]
  
  # Dicionario de variaveis:
  # Acessibilidade:
  # - CMA = Acessibilidade Cumulativa Ativa
  # - CMP = Acessibilidade Cumulativa Passiva
  # - CPT = Acessibilidade considerando Competitividade ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!!!!
  # - TMI = Acessibilidade de Tempo Mínimo à Oportunidade
  # Atividades:
  # - PT ~ "pop_total"
  # - PB ~ "cor_branca"
  # - PA ~ "cor_amarela"
  # - PI ~ "cor_indigena"
  # - PN ~ "cor_negra"
  # - TT ~ "empregos_total"
  # - TQ ~ "empregos_match_quintil"
  # - TD ~ "empregos_match_decil"
  # - ST ~ "saude_total"
  # - SB ~ "saude_baixa"
  # - SM ~ "saude_media"
  # - SA ~ "saude_alta"
  # - ET ~ "edu_total"
  # - EI ~ "edu_infantil"
  # - EF ~ "edu_fundamental"
  # - EM ~ "edu_medio"
  # - EI ~ "edu_infantil"
  
  
  # 3) Calcular acessibilidade cumulativa ativa ----------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cma <- "CMA"
  atividade_cma <- c("TT", "TQ", "TD", "ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM")
  # criar dummy para tt
  tt <- c(1, 2, 3, 4)
  
  grid_cma <- expand.grid(acess_cma, atividade_cma, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold  para cada um dos modos
    mutate(tt_tp = case_when(
      tt_sigla == 1 ~ 30,
      tt_sigla == 2 ~ 60,
      tt_sigla == 3 ~ 90,
      tt_sigla == 4 ~ 120
    )) %>%
    mutate(tt_ativo = case_when(
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
    mutate(junto_ativo = paste0(acess_sigla, atividade_sigla, tt_ativo)) %>%
    mutate(atividade_nome = case_when(atividade_sigla == "TT" ~ "empregos_total",
                                      atividade_sigla == "TQ" ~ "empregos_match_quintil",
                                      atividade_sigla == "TD" ~ "empregos_match_decil",
                                      atividade_sigla == "ST" ~ "saude_total",
                                      atividade_sigla == "SB" ~ "saude_baixa",
                                      atividade_sigla == "SM" ~ "saude_media",
                                      atividade_sigla == "SA" ~ "saude_alta",
                                      atividade_sigla == "ET" ~ "edu_total",
                                      atividade_sigla == "EF" ~ "edu_fundamental",
                                      atividade_sigla == "EM" ~ "edu_medio",
                                      atividade_sigla == "EI" ~ "edu_infantil")) %>%
    # adicionar uma variavel com o total de oportunidades da atividade em questao
    mutate(atividade_total = case_when(
      atividade_sigla == "TT" ~ "total_empregos",
      atividade_sigla == "TQ" ~ "total_empregos_match_quintil",
      atividade_sigla == "TD" ~ "total_empregos_match_decil",
      atividade_sigla == "ST" ~ "total_saude",
      atividade_sigla == "SB" ~ "total_saude_baixa",
      atividade_sigla == "SM" ~ "total_saude_media",
      atividade_sigla == "SA" ~ "total_saude_alta",
      atividade_sigla == "ET" ~ "total_edu",
      atividade_sigla == "EI" ~ "total_edu_infantil",
      atividade_sigla == "EF" ~ "total_edu_fundamental",
      atividade_sigla == "EM" ~ "total_edu_medio"
      
    ))
  
  
  # gerar o codigo
  # para tp
  codigo_cma_tp <- sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T)/first(%s))", 
                           grid_cma$junto_tp, 
                           grid_cma$atividade_nome, 
                           grid_cma$tt_tp,
                           grid_cma$atividade_total)
  
  # para ativo
  codigo_cma_ativo <- sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T)/first(%s))", 
                              grid_cma$junto_ativo, 
                              grid_cma$atividade_nome, 
                              grid_cma$tt_ativo,
                              grid_cma$atividade_total)
  
  
  # dar nomes às variaveis
  to_make_cma_tp <- setNames(codigo_cma_tp, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_tp))
  to_make_cma_ativo <- setNames(codigo_cma_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_ativo))
  
  
  # so aplicar a acessibilidade para os modos da cidade
  modo_year <- sprintf("modo_%s", ano)
  modo <- munis_df[abrev_muni == sigla_muni][[modo_year]]
  
  if (modo == "todos") {
    
    # para transporte publico
    acess_cma_tp <- ttmatrix[mode == "transit",
                             lapply(to_make_cma_tp, function(x) eval(parse(text = x)))
                             , by=.(city, mode, origin, pico, quintil, decil)]
    
    # para modos ativos
    acess_cma_ativo <- ttmatrix[mode %in% c("bike", "walk"), 
                                lapply(to_make_cma_ativo, function(x) eval(parse(text = x)))
                                , by=.(city, mode, origin, pico, quintil, decil)]
    
    
    # juntar os cma
    acess_cma <- rbind(acess_cma_tp, acess_cma_ativo, fill = TRUE)
    
  } else {
    
    # so para modos ativos
    acess_cma <- ttmatrix[, 
                          lapply(to_make_cma_ativo, function(x) eval(parse(text = x)))
                          , by=.(city, mode, origin, pico, quintil, decil)]
    
  }
  
  
  
  
  
  # 4) Calcular acessibilidade cumulativa passiva --------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cmp <- "CMP"
  atividade_cmp <- c("PT", "PB", "PA", "PI", "PN")
  # criar dummy para tt
  tt <- c(1, 2, 3, 4)
  
  grid_cmp <- expand.grid(acess_cmp, atividade_cmp, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold para cada um dos modos
    mutate(tt_tp = case_when(
      tt_sigla == 1 ~ 30,
      tt_sigla == 2 ~ 60,
      tt_sigla == 3 ~ 90,
      tt_sigla == 4 ~ 120
    )) %>%
    mutate(tt_ativo = case_when(
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
    mutate(junto_ativo = paste0(acess_sigla, atividade_sigla, tt_ativo)) %>%
    mutate(atividade_nome = case_when(atividade_sigla == "PT" ~ "pop_total",
                                      atividade_sigla == "PB" ~ "cor_branca",
                                      atividade_sigla == "PA" ~ "cor_amarela",
                                      atividade_sigla == "PI" ~ "cor_indigena",
                                      atividade_sigla == "PN" ~ "cor_negra")) %>%
    # adicionar uma variavel com o total de oportunidades da atividade em questao
    mutate(atividade_total = case_when(
      atividade_sigla == "PT" ~ "total_pop",
      atividade_sigla == "PB" ~ "total_branca",
      atividade_sigla == "PA" ~ "total_amarela",
      atividade_sigla == "PI" ~ "total_indigena",
      atividade_sigla == "PN" ~ "total_negra",
      
    ))
  
  
  # gerar o codigo
  codigo_cmp_tp <- sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T)/first(%s))", 
                           grid_cmp$junto_tp, 
                           grid_cmp$atividade_nome, 
                           grid_cmp$tt_tp,
                           grid_cmp$atividade_total)
  
  codigo_cmp_ativo <- sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T)/first(%s))", 
                              grid_cmp$junto_ativo, 
                              grid_cmp$atividade_nome, 
                              grid_cmp$tt_ativo,
                              grid_cmp$atividade_total)
  
  
  # gerar os nomes das variaveis
  to_make_cmp_tp <- setNames(codigo_cmp_tp, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_tp))
  
  to_make_cmp_ativo <- setNames(codigo_cmp_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_ativo))
  
  
  
  # so aplicar a acessibilidade para os modos da cidade
  
  if (modo == "todos") {
    
    # para transporte publico
    acess_cmp_tp <- ttmatrix[mode == "transit",
                             lapply(to_make_cmp_tp, function(x) eval(parse(text = x)))
                             , by=.(city, mode, destination, pico)]
    
    # para modos ativos
    acess_cmp_ativo <- ttmatrix[mode %in% c("bike", "walk"), 
                                lapply(to_make_cmp_ativo, function(x) eval(parse(text = x)))
                                , by=.(city, mode, destination, pico)]
    
    
    # juntar os cma
    acess_cmp <- rbind(acess_cmp_tp, acess_cmp_ativo, fill = TRUE)  
    
  } else {
    
    # so para modos ativos
    acess_cmp <- ttmatrix[, 
                          lapply(to_make_cmp_ativo, function(x) eval(parse(text = x)))
                          , by=.(city, mode, destination, pico)]
    
  }
  
  
  # 5) Calcular acessibilidade tempo minimo ---------------
  # (aqui eh feito junto para os dois modos)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  acess_tmi <- "TMI"
  atividade_tmi <- c("ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM")
  
  grid_tmi <- expand.grid(acess_tmi, atividade_tmi, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2) %>%
    mutate(junto = paste0(acess_sigla, atividade_sigla)) %>%
    mutate(atividade_nome = case_when(atividade_sigla == "ST" ~ "saude_total",
                                      atividade_sigla == "SB" ~ "saude_baixa",
                                      atividade_sigla == "SM" ~ "saude_media",
                                      atividade_sigla == "SA" ~ "saude_alta",
                                      atividade_sigla == "ET" ~ "edu_total",
                                      atividade_sigla == "EI" ~ "edu_infantil",
                                      atividade_sigla == "EF" ~ "edu_fundamental",
                                      atividade_sigla == "EM" ~ "edu_medio",
                                      atividade_sigla == "EI" ~ "edu_infantil"))
  
  
  # gerar o codigo
  codigo_tmi <- sprintf("%s = min(tt_median[which(%s >= 1)])", 
                        grid_tmi$junto, 
                        grid_tmi$atividade_nome)
  
  
  # gerar os nomes das variaveis
  to_make_tmi <- setNames(codigo_tmi, sub('^([[:alnum:]]*) =.*', '\\1', codigo_tmi))
  
  
  # calcular acessibilidade
  acess_tmi <- ttmatrix[, lapply(to_make_tmi, function(x) eval(parse(text = x)))
                        , by=.(city, mode, origin, pico)]
  
  # hexagonos_sf <- st_sf(hexagonos_sf)
  # 
  # ggplot() + geom_sf(data=hexagonos_sf ,fill='gray') +
  #  geom_sf(data=subset(hexagonos_sf, saude_media>0) , aes(fill=saude_media))
  # 
  # ggplot() + geom_sf(data=hexagonos_sf ,fill='gray') +
  #   geom_sf(data=subset(hexagonos_sf, saude_baixa>0) , aes(fill=saude_baixa))
  # 
  # 
  
  # 6) Juntar os arquivos de acess ------------------------------------------------
  
  
  # Juntar os tres (left_join)
  acess <- merge(acess_cma, acess_cmp,
                 all.x = TRUE,
                 by.x = c("city", "mode", "origin", "pico"),
                 by.y = c("city", "mode", "destination", "pico"))
  
  acess <- merge(acess, acess_tmi,
                 all.x = TRUE,
                 by.x = c("city", "mode", "origin", "pico"),
                 by.y = c("city", "mode", "origin", "pico"))
  
  
  # Transformar para sf
  acess_sf <- merge(acess, setDT(hexagonos_sf)[, .(id_hex, geometry)],
                    by.x = "origin",
                    by.y = "id_hex",
                    all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  # 7) Salvar output --------------------------------------
  
  path_out <- sprintf("../../data/acesso_oport/output_access/%s/%s/acess_%s_%s.rds", ano, engine, sigla_muni, ano)
  write_rds(acess_sf, path_out)
  
  # gc colletc
  gc(TRUE)
  
  
}

# 2. APLICAR PARA TODOS AS CIDADADES --------------------------------------------------------------
calcular_acess_muni("for", ano = 2017)









