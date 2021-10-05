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
    
    ttmatrix_median <- read_rds(sprintf("E:/data/output_ttmatrix/%s/%s/ttmatrix_%s_pt_%s.csv",
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
  hex_orig <- hexagonos_sf %>% dplyr::select(id_hex, 
                                             # variaveis de populacao - cor
                                             pop_total, cor_branca, cor_amarela, cor_indigena, cor_negra, 
                                             # variaveis de populacao - idade
                                             matches("idade_"),
                                             # variaveis de renda
                                             renda_total, renda_capita, quintil, decil) %>% setDT()
  
  # Filtrar apenas colunas com info de uso do solo no destino
  hex_dest <- setDT(hexagonos_sf)[, .(id_hex, empregos_total, empregos_baixa, empregos_media, empregos_alta,  
                                      saude_total, saude_baixa, saude_media, saude_alta,
                                      edu_total, edu_infantil, edu_fundamental, edu_medio,
                                      mat_total, mat_infantil, mat_fundamental, mat_medio)]
  
  
  # Merge dados de origem na matrix de tempo de viagem
  ttmatrix <- ttmatrix_median[hex_orig, on = c("origin" = "id_hex"),  
                              c('pop_total','cor_branca','cor_amarela','cor_indigena','cor_negra',
                                "idade_0a5", "idade_6a14", "idade_15a18", "idade_19a24",    
                                "idade_25a39", "idade_40a69", "idade_70",
                                'renda_total','renda_capita','quintil','decil') :=
                                list(i.pop_total, i.cor_branca, i.cor_amarela, i.cor_indigena, i.cor_negra, 
                                     i.idade_0a5, i.idade_6a14, i.idade_15a18, i.idade_19a24,    
                                     i.idade_25a39, i.idade_40a69, i.idade_70,    
                                     i.renda_total, i.renda_capita, i.quintil, i.decil)]
  
  # Merge dados de destino na matrix de tempo de viagem
  ttmatrix <- ttmatrix[hex_dest, on = c("destination" = "id_hex"),  
                       c("empregos_total", "empregos_baixa","empregos_media","empregos_alta",
                         "saude_total", "saude_baixa", "saude_media", "saude_alta",
                         "edu_total","edu_infantil","edu_fundamental","edu_medio",
                         "mat_total", "mat_infantil", "mat_fundamental", "mat_medio") :=
                         list(i.empregos_total, i.empregos_baixa,i.empregos_media,i.empregos_alta,
                              i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta,
                              i.edu_total,i.edu_infantil,i.edu_fundamental,i.edu_medio,
                              i.mat_total, i.mat_infantil, i.mat_fundamental, i.mat_medio)]    
  
  # Calcular emprego com match qualitativo de renda e nivel de escolaridade do emprego 
  # high income people = jobs with high and med education
  # low income people = jobs with low and med education
  ttmatrix[, empregos_match_decil := ifelse(decil>5, 
                                            empregos_alta + empregos_media, 
                                            empregos_baixa + empregos_media)]
  
  ttmatrix[, empregos_match_quintil := ifelse(quintil>=3, 
                                              empregos_alta + empregos_media, 
                                              empregos_baixa + empregos_media)]
  
  
  
  # Dicionario de variaveis:
  # Acessibilidade:
  # - CMA = Acessibilidade Cumulativa Ativa
  # - CMP = Acessibilidade Cumulativa Passiva
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
  atividade_cma <- c("TT", "TQ", "TD", "ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM",
                     "MT", "MI", "MF", "MM")
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
                                      atividade_sigla == "EI" ~ "edu_infantil",
                                      atividade_sigla == "MT" ~ "mat_total",
                                      atividade_sigla == "MF" ~ "mat_fundamental",
                                      atividade_sigla == "MM" ~ "mat_medio",
                                      atividade_sigla == "MI" ~ "mat_infantil"))
  
  
  # gerar o codigo
  # para tp
  codigo_cma_tp <- c(
    
    sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T))", 
            grid_cma$junto_tp, 
            grid_cma$atividade_nome, 
            grid_cma$tt_tp
    )
  )
  
  
  # para ativo
  codigo_cma_ativo <- c(
    
    sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T))", 
            grid_cma$junto_ativo, 
            grid_cma$atividade_nome, 
            grid_cma$tt_ativo
    )
  )
  
  # dar nomes às variaveis
  to_make_cma_tp <-    setNames(codigo_cma_tp,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_tp))
  to_make_cma_ativo <- setNames(codigo_cma_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_ativo))
  
  
  # so aplicar a acessibilidade para os modos da cidade
  # modo_year <- sprintf("modo_%s", ano)
  modo <- munis_list$munis_modo[abrev_muni == sigla_muni & ano_modo == ano]$modo
  
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
    acess_cma <- rbind(acess_cma_tp, acess_cma_ativo,
                       fill = TRUE)
    
  } else {
    
    # so para modos ativos
    acess_cma <- ttmatrix[, 
                          lapply(to_make_cma_ativo, function(x) eval(parse(text = x)))
                          , by=.(city, mode, origin, pico, quintil, decil)]
    
  }
  
  
  
  
  
  # 4) Calcular acessibilidade cumulativa passiva --------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cmp <- "CMP"
  # atividade_cmp <- c("PT", "PB", "PA", "PI", "PN")
  atividade_cmp <- c("PT", "PB", "PA", "PI", "PN", "I1", "I2", "I3", "I4", "I5", "I6", "I7")
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
                                      atividade_sigla == "PN" ~ "cor_negra",
                                      atividade_sigla == "I1" ~ "idade_0a5", 
                                      atividade_sigla == "I2" ~ "idade_6a14", 
                                      atividade_sigla == "I3" ~ "idade_15a18", 
                                      atividade_sigla == "I4" ~ "idade_19a24",    
                                      atividade_sigla == "I5" ~ "idade_25a39", 
                                      atividade_sigla == "I6" ~ "idade_40a69", 
                                      atividade_sigla == "I7" ~ "idade_70"))
  
  # gerar o codigo
  # para tp 
  codigo_cmp_tp <- c(
    
    sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T))", 
            grid_cmp$junto_tp, 
            grid_cmp$atividade_nome, 
            grid_cmp$tt_tp
    )
  )
  
  # para ativo
  codigo_cmp_ativo <- c(
    
    sprintf("%s = (sum(%s[which(tt_median <= %s)], na.rm = T))", 
            grid_cmp$junto_ativo, 
            grid_cmp$atividade_nome, 
            grid_cmp$tt_ativo
    )
  )
  
  
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
  
  
  # 6) Calcular acessibilidade BFCA ---------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  acess_bfc <- "BFC"
  atividade_bfc <- c("ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM",
                     "MT", "MI", "MF", "MM")
  
  # criar dummy para tt
  tt_sigla <- c(1, 2, 3, 4)
  
  grid_bfc <- expand.grid(acess_bfc, atividade_bfc, tt_sigla, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold  para cada um dos modos
    mutate(tt = case_when(
      tt_sigla == 1 ~ 10,
      tt_sigla == 2 ~ 40,
      tt_sigla == 3 ~ 100,
      tt_sigla == 4 ~ 180
    )) %>% 
    mutate(junto = paste0(acess_sigla, atividade_sigla, tt)) %>%
    setDT()
  
  
  # identificar as variaveis de atividade que vao ser utilizadas
  grid_bfc[, atividade_nome := fcase(atividade_sigla == "TT", "empregos_total",
                                     atividade_sigla == "TQ", "empregos_match_quintil",
                                     atividade_sigla == "TD", "empregos_match_decil",
                                     atividade_sigla == "ST", "saude_total",
                                     atividade_sigla == "SB", "saude_baixa",
                                     atividade_sigla == "SM", "saude_media",
                                     atividade_sigla == "SA", "saude_alta",
                                     atividade_sigla == "ET", "edu_total",
                                     atividade_sigla == "EF", "edu_fundamental",
                                     atividade_sigla == "EM", "edu_medio",
                                     atividade_sigla == "EI", "edu_infantil",
                                     atividade_sigla == "MT", "mat_total",
                                     atividade_sigla == "MF", "mat_fundamental",
                                     atividade_sigla == "MM", "mat_medio",
                                     atividade_sigla == "MI", "mat_infantil")]
  
  
  # identificar as variaveis de populacao que vao ser utilizadas para cada atividade
  grid_bfc[, pop_nome := fcase(atividade_sigla == "TT", list("pop_total"),
                               atividade_sigla == "TQ", list("pop_total"),
                               atividade_sigla == "TD", list("pop_total"),
                               atividade_sigla == "ST", list("pop_total"),
                               atividade_sigla == "SB", list("pop_total"),
                               atividade_sigla == "SM", list("pop_total"),
                               atividade_sigla == "SA", list("pop_total"),
                               
                               # a definicao das idades por nivel de ensino vai ser 
                               # limitada pelos intervalos de idade disponiveis
                               # fonte: https://pt.wikipedia.org/wiki/Idade_escolar
                               # educacao total: pop de 0 a 20 anos
                               atividade_sigla == "ET", list(c("idade_0a5", "idade_6a14", "idade_15a18")),
                               # educacao infantil: pop de 0 a 6 anos
                               atividade_sigla == "EI", list("idade_0a5"),
                               # educacao fund.: pop de 6 a 15 anos
                               atividade_sigla == "EF", list(c("idade_6a14")),
                               # educacao media: pop de 15 a 18 anos
                               atividade_sigla == "EM", list(c("idade_15a18")), 
                               
                               # a mesma regra se aplica para as matriculas
                               atividade_sigla == "MT", list(c("idade_0a5", "idade_6a14", "idade_15a18")),
                               atividade_sigla == "MI", list("idade_0a5"),
                               atividade_sigla == "MF", list(c("idade_6a14")),
                               atividade_sigla == "MM", list(c("idade_15a18"))
  )]
  
  
  
  # calculate bfca
  # atividade <- "saude_alta"
  # atividade <- "saude_total"
  # atividade <- "edu_medio"
  # atividade <- "edu_total"
  calculate_bfc <- function(atividade) {
    
    
    # subset the grid of this activity
    grid_bfc1 <- filter(grid_bfc, atividade_nome == atividade)
    
    # get variables names that will compose the population (it depends on the activity)
    # if there more than one pop_name, it will be sumed up
    pop_target <- grid_bfc1$pop_nome %>% .[[1]] %>% unlist() %>% paste0(collapse = " + ")
    
    # filtrar A matrix de viagem até os hospitais com atividades
    hex_dest_filter <- hex_dest[get(grid_bfc1$atividade_nome[1]) >= 1]
    ttmatrix_hosp <- ttmatrix[destination %in% hex_dest_filter$id_hex]
    
    # calculate impedance (choose one)
    
    # gaussian
    mgaus_f <- function(t_ij,b0){exp(-t_ij^2/b0)}
    # ttmatrix_hosp[, impedance := neg_exp_f(dist, 0.45) ]
    # ttmatrix_hosp[, impedance := mgaus_f(dist, 200) ]
    # ttmatrix_hosp[mode == "transit", ':='(impedance1 = fifelse(tt_median <= grid_bfc1$tt_tp[1], 1, 0),
    #                                       impedance2 = fifelse(tt_median <= grid_bfc1$tt_tp[2], 1, 0),
    #                                       impedance3 = fifelse(tt_median <= grid_bfc1$tt_tp[3], 1, 0),
    #                                       impedance4 = fifelse(tt_median <= grid_bfc1$tt_tp[4], 1, 0)) ] # binary
    # ttmatrix_hosp[mode %in% c("bike", "walk"), ':='(impedance1 = fifelse(tt_median <= grid_bfc1$tt_ativo[1], 1, 0),
    #                                                 impedance2 = fifelse(tt_median <= grid_bfc1$tt_ativo[2], 1, 0),
    #                                                 impedance3 = fifelse(tt_median <= grid_bfc1$tt_ativo[3], 1, 0),
    #                                                 impedance4 = fifelse(tt_median <= grid_bfc1$tt_ativo[4], 1, 0)) ] # binary
    
    #' para aplicar a funcao de decaimento gaussiana, foram escolhidos paremetros
    #' de valor 10, 40, 100, 180
    #' esses valores foram extraidos do artigo Kwan (1998)
    ttmatrix_hosp[,':='(impedance1 = mgaus_f(tt_median, 10),
                        impedance2 = mgaus_f(tt_median, 40),
                        impedance3 = mgaus_f(tt_median, 100),
                        impedance4 = mgaus_f(tt_median, 180)) ]
    
    # calculate weights i (normalized impedance by origin id)
    ttmatrix_hosp[, wi1 := impedance1/sum(impedance1), by= .(origin, mode, pico)]
    ttmatrix_hosp[, wi2 := impedance2/sum(impedance2), by= .(origin, mode, pico)]
    ttmatrix_hosp[, wi3 := impedance3/sum(impedance3), by= .(origin, mode, pico)]
    ttmatrix_hosp[, wi4 := impedance4/sum(impedance4), by= .(origin, mode, pico)]
    # summary(ttmatrix_hosp$wi1)
    
    # calculate weights j (normalized impedance by destination)
    ttmatrix_hosp[, wj1 := impedance1/sum(impedance1), by=.(destination, mode, pico)]
    ttmatrix_hosp[, wj2 := impedance2/sum(impedance2), by=.(destination, mode, pico)]
    ttmatrix_hosp[, wj3 := impedance3/sum(impedance3), by=.(destination, mode, pico)]
    ttmatrix_hosp[, wj4 := impedance4/sum(impedance4), by=.(destination, mode, pico)]
    # summary(ttmatrix_hosp$wj1)
    
    ## Step 1 - reaportion the demand to each hospital proportionally to weight i
    ttmatrix_hosp1 <- copy(ttmatrix_hosp)
    ttmatrix_hosp1[, pop_served1 := sum((eval(parse(text = pop_target))) * wi1, na.rm = TRUE), by= .(destination, mode, pico)]
    ttmatrix_hosp1[, pop_served2 := sum((eval(parse(text = pop_target))) * wi2, na.rm = TRUE), by= .(destination, mode, pico)]
    ttmatrix_hosp1[, pop_served3 := sum((eval(parse(text = pop_target))) * wi3, na.rm = TRUE), by= .(destination, mode, pico)]
    ttmatrix_hosp1[, pop_served4 := sum((eval(parse(text = pop_target))) * wi4, na.rm = TRUE), by= .(destination, mode, pico)]
    # summary(ttmatrix_hosp1$pop_served1)
    
    ## Step 2 - calculate provider-to-population ration (ppr) at each destination
    ttmatrix_hosp1[, ':='(ppr1 = get(grid_bfc1$atividade_nome)[1] / pop_served1,
                          ppr2 = get(grid_bfc1$atividade_nome)[1] / pop_served2,
                          ppr3 = get(grid_bfc1$atividade_nome)[1] / pop_served3,
                          ppr4 = get(grid_bfc1$atividade_nome)[1] / pop_served4), 
                   by= .(destination, mode, pico)]
    # summary(ttmatrix_hosp1$ppr)
    
    
    ## Step 3 - reaportion ppr at each origin proportionally to weight j
    bfca <- ttmatrix_hosp1[, .(BFCA1 = sum(ppr1 * wj1, na.rm=T),
                               BFCA2 = sum(ppr2 * wj2, na.rm=T),
                               BFCA3 = sum(ppr3 * wj3, na.rm=T),
                               BFCA4 = sum(ppr4 * wj4, na.rm=T)),
                           by= .(city, origin, mode, pico)]
    # rename BFCA
    colnames(bfca) <- c("city", "origin", "mode", "pico", grid_bfc1$junto)
    # summary(bfca$BFCA)*10000
    
    return(bfca)
    
    
  }
  
  
  # apply fun to calculate bfca
  acess_bfc <- lapply(unique(grid_bfc$atividade_nome), calculate_bfc)
  # o nome das colunas de bfca eh diferente para cada um dos df, e a ideia eh
  # que as colunas de bfca fiquem em formato largo
  # para isso, eh preciso fazer a juncao de todas as bases pelas variaveis em comum
  # a funcao 'reduce' aplica um funcao (no nosso caso, full_join) interativamente,
  # sempre aplicando a funcao com o resultado da aplicacao anterior
  acess_bfc <- acess_bfc %>% reduce(full_join, by = c("city", "origin", "mode", "pico"))
  
  # ATENCAO
  # algumas origens nao vao ter valores calculados do BFCA, e isso acontece princiaplamente
  # para caminhada e bike
  # isso acontece pq esses hex nao conseguem acessar nenhuma atividade que esteja
  # dentro do tempo limite de viagem do roteamento, que eh 90 mins para walk e bike
  
  
  
  
  # 7) Juntar os arquivos de acess ------------------------------------------------
  
  
  # Juntar os tres (left_join)
  acess <- merge(acess_cma, acess_cmp,
                 all.x = TRUE,
                 by.x = c("city", "mode", "origin", "pico"),
                 # como o cmp eh calculado para os destinos, o join eh para destination
                 by.y = c("city", "mode", "destination", "pico"))
  
  acess <- merge(acess, acess_tmi,
                 all.x = TRUE,
                 by.x = c("city", "mode", "origin", "pico"),
                 by.y = c("city", "mode", "origin", "pico"))
  
  acess <- merge(acess, acess_bfc,
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
  
  
  # identificar o ano
  acess_sf <- acess_sf %>%
    mutate(ano = ano)
  
  # 8) Salvar output --------------------------------------
  
  path_out <- sprintf("../../data/acesso_oport/output_access/%s/%s/acess_%s_%s.rds", ano, engine, sigla_muni, ano)
  write_rds(acess_sf, path_out)
  
  # gc colletc
  gc(TRUE)
  
  
}

# 2. APLICAR PARA TODOS AS CIDADADES --------------------------------------------------------------
plan(multiprocess)
furrr::future_walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, calcular_acess_muni, ano = 2017)
furrr::future_walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, calcular_acess_muni, ano = 2018)
furrr::future_walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, calcular_acess_muni, ano = 2019)

