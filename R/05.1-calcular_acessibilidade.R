# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Calcular acessibilidade

# carregar bibliotecas
source('./R/fun/setup.R')

#### 1. CALCULAR ACESSIBILIDADE --------------------------------------------------------------



# sigla_muni <- "bel"; ano=2017
# sigla_muni <- "spo"; ano=2019
# sigla_muni <- "for"; ano=2019
# sigla_muni <- "for"; ano=2017
# sigla_muni <- "sal"; ano=2019; mode1 <- "all"; access <- "all"
# sigla_muni <- "bel"; ano=2019; mode1 <- "all"; access <- "all"
# sigla_muni <- "bho"; ano=2019; mode1 <- "all"; access <- "all"
# sigla_muni <- "for"; ano = 2019; mode_access <- "car"
# sigla_muni <- "spo"; ano = 2019; mode1 <- "car"
# sigla_muni <- "bsb_origin1"; ano = 2017; mode1 <- "car"
# sigla_muni <- "bsb_dest1"; ano = 2019; mode1 <- "car"
# sigla_muni <- "for"; ano = 2019; BFCA <- FALSE
# sigla_muni <- "spo"; ano = 2019; BFCA <- FALSE

#' OS parametros de mode_access e indicator_access so foram adicionados para permitir
#' o calculo da acessibilidade de cidades que nao cabiam na memoria (especialmente matrizes de carro)
#' @param mode_access Identifica os modos de transporte para calcular acessibilidade. 
#' Pode ser "all" (transporte publico + ativo) ou "car" (carro)
#' @param indicator_access Identifica a familia de indicadores de acessibilidade a calcular. 
#' Pode ser "all" (indicadores de acessibilidade cumulativa ativa, passiva e tempo minimo),
#' "active" (somente indicatores de cumulativa ativa e tempo minimo), ou
#' "passive" (somente indicadores de cumulativa passiva) 

calcular_acess_muni <- function(sigla_muni, ano, BFCA = FALSE, mode_access = "all", indicator_access = "all") {
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', ano,  '\n')
  
  # identifficar modo(s) de transportes
  modo <- munis_list$munis_modo[abrev_muni == sigla_muni & ano_modo == ano]$modo
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  
  if (mode_access == "all") {
    
    
    ttmatrix <- read_rds(sprintf("E:/data/ttmatrix_fixed/%s/ttmatrix_fixed_%s_%s.rds",
                                 ano, ano, sigla_muni))
    
    if (modo == "ativo") ttmatrix <- ttmatrix[mode %in% c("bike", "walk")] else ttmatrix
    
    
  } else if (mode_access == "car") {
    
    if (sigla_muni %like% c("bsb|goi")) {
      
      path <- sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_2019_%s.csv", sigla_muni)
      
    } else {path <- sprintf("E:/data/ttmatrix_fixed/car/ttmatrix_fixed_%s_%s.csv", "2019", sigla_muni)}
    
    ttmatrix <- fread(path)
    # setnames(ttmatrix, c("origin", "destination", "median_morning_peak", "median_afternoon_offpeak"))
    # ttmatrix[, city := substr(sigla_muni, 1, 3)]
    # ttmatrix[, mode := "car"]
    # 
    # # add parking time
    # ttmatrix[origin != destination, median_morning_peak := median_morning_peak + 2]
    # ttmatrix[origin != destination, median_afternoon_offpeak :=  median_afternoon_offpeak + 2]
    
    # ttmatrix <- fread(sprintf("E:/data/ttmatrix_fixed/%s/car/ttmatrix_fixed_car_%s_%s.csv", 
    #                           "2019", "2019", sigla_muni))
    
  }
  
  # 2) Agregar dados de uso do solo à ttmatrix --------------------------
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../../data/acesso_oport/hex_agregados/%s/hex_agregado_%s_09_%s.rds", ano, substr(sigla_muni, 1, 3), ano)
  
  # Abrir oportunidades com hexagonos
  hexagonos_sf <- readr::read_rds(dir_hex)
  
  # Filtrar apenas colunas com info demograficas na origem
  hex_orig <- hexagonos_sf %>% dplyr::select(id_hex, 
                                             # variaveis de populacao - total e sexo
                                             matches("pop_"), 
                                             # variaveis de populacao - cor
                                             matches("cor_"),
                                             # variaveis de populacao - idade
                                             matches("idade_"),
                                             # variaveis de renda
                                             renda_total, renda_capita, renda_quintil, renda_decil) %>% setDT()
  
  # Filtrar apenas colunas com info de uso do solo no destino
  # corrigir para dplyr
  hex_dest <- setDT(hexagonos_sf)[, .(id_hex, 
                                      empregos_total, empregos_baixa, empregos_media, empregos_alta,  
                                      saude_total, saude_baixa, saude_media, saude_alta,
                                      edu_total, edu_infantil, edu_fundamental, edu_medio,
                                      mat_total, mat_infantil, mat_fundamental, mat_medio,
                                      cras_total)]
  
  
  # if (access %in% c("all", "active")) {
  if (sigla_muni %like% "goi" & ano %in% c(2017, 2018)) {
    
    
    ttmatrix <- ttmatrix[origin %in% hexagonos_sf$id_hex]
    ttmatrix <- ttmatrix[destination %in% hexagonos_sf$id_hex]
    
  }
  
  message("Joining data...")
  a <- Sys.time()
  
  if (indicator_access == "all") {
    
    # Join1 - Merge dados de origem na matrix de tempo de viagem
    ttmatrix <- ttmatrix[hex_orig, on = c("origin" = "id_hex"),  
                         c('pop_total', 'pop_homens', 'pop_mulheres',  'cor_branca','cor_amarela','cor_indigena','cor_negra',
                           "idade_0a5", "idade_6a14", "idade_15a18", "idade_19a24",    
                           "idade_25a39", "idade_40a69", "idade_70",
                           'renda_total','renda_capita','renda_quintil','renda_decil') :=
                           list(i.pop_total, i.pop_homens, i.pop_mulheres, i.cor_branca, i.cor_amarela, i.cor_indigena, i.cor_negra, 
                                i.idade_0a5, i.idade_6a14, i.idade_15a18, i.idade_19a24,    
                                i.idade_25a39, i.idade_40a69, i.idade_70,    
                                i.renda_total, i.renda_capita, i.renda_quintil, i.renda_decil)]
    
    
    # Join2 -  Merge dados de destino na matrix de tempo de viagem
    ttmatrix <- ttmatrix[hex_dest, on = c("destination" = "id_hex"),  
                         c("empregos_total", "empregos_baixa","empregos_media","empregos_alta",
                           "saude_total", "saude_baixa", "saude_media", "saude_alta",
                           "edu_total","edu_infantil","edu_fundamental","edu_medio",
                           "mat_total", "mat_infantil", "mat_fundamental", "mat_medio", 
                           "cras_total") :=
                           list(i.empregos_total, i.empregos_baixa,i.empregos_media,i.empregos_alta,
                                i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta,
                                i.edu_total,i.edu_infantil,i.edu_fundamental,i.edu_medio,
                                i.mat_total, i.mat_infantil, i.mat_fundamental, i.mat_medio,
                                i.cras_total)]    
    
  } else if(indicator_access == "active") {
    
    
    # Join2 -  Merge dados de destino na matrix de tempo de viagem
    ttmatrix <- ttmatrix[hex_dest, on = c("destination" = "id_hex"),  
                         c("empregos_total", "empregos_baixa","empregos_media","empregos_alta",
                           "saude_total", "saude_baixa", "saude_media", "saude_alta",
                           "edu_total","edu_infantil","edu_fundamental","edu_medio",
                           "mat_total", "mat_infantil", "mat_fundamental", "mat_medio", 
                           "cras_total") :=
                           list(i.empregos_total, i.empregos_baixa,i.empregos_media,i.empregos_alta,
                                i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta,
                                i.edu_total,i.edu_infantil,i.edu_fundamental,i.edu_medio,
                                i.mat_total, i.mat_infantil, i.mat_fundamental, i.mat_medio,
                                i.cras_total)]  
    
    
    
  } else if(indicator_access == "passive") {
    
    
    
    # Join1 - Merge dados de origem na matrix de tempo de viagem
    ttmatrix <- ttmatrix[hex_orig, on = c("origin" = "id_hex"),  
                         c('pop_total', 'pop_homens', 'pop_mulheres',  'cor_branca','cor_amarela','cor_indigena','cor_negra',
                           "idade_0a5", "idade_6a14", "idade_15a18", "idade_19a24",    
                           "idade_25a39", "idade_40a69", "idade_70",
                           'renda_total','renda_capita','renda_quintil','renda_decil') :=
                           list(i.pop_total, i.pop_homens, i.pop_mulheres, i.cor_branca, i.cor_amarela, i.cor_indigena, i.cor_negra, 
                                i.idade_0a5, i.idade_6a14, i.idade_15a18, i.idade_19a24,    
                                i.idade_25a39, i.idade_40a69, i.idade_70,    
                                i.renda_total, i.renda_capita, i.renda_quintil, i.renda_decil)]
    
    
  }
  
  a1 <- Sys.time()
  message("Joining data took ", as.integer(as.numeric(a1 - a, "secs")), " seconds")
  
  
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
  # - TQ ~ "empregos_match_quintil" nao vamos usar
  # - TD ~ "empregos_match_decil"  nao vamos usar
  # - ST ~ "saude_total"
  # - SB ~ "saude_baixa"
  # - SM ~ "saude_media"
  # - SA ~ "saude_alta"
  # - ET ~ "edu_total"
  # - EI ~ "edu_infantil"
  # - EF ~ "edu_fundamental"
  # - EM ~ "edu_medio"
  # - EI ~ "edu_infantil"
  # - CT ~ "cras_total"
  
  
  # 3) Calcular acessibilidade cumulativa ativa ----------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (indicator_access %in% c("all", "active")) {
    
    acess_cma <- "CMA"
    atividade_cma <- c(
      # emprego
      "TT", "TB", "TM", "TA",
      # saude
      "ST", "SB", "SM", "SA", 
      # educacao - escolas
      "ET", "EI", "EF", "EM",
      # educacao - matriculas
      "MT", "MI", "MF", "MM",
      # cras
      "CT"
    )
    # criar dummy para tt
    tt <- c(1, 2, 3, 4, 5)
    
    grid_cma <- expand.grid(acess_cma, atividade_cma, tt, stringsAsFactors = FALSE) %>%
      rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
      # adicionar colunas de time threshold  para cada um dos modos
      mutate(tt_tp = case_when(
        tt_sigla == 1 ~ 15,
        tt_sigla == 2 ~ 30,
        tt_sigla == 3 ~ 60,
        tt_sigla == 4 ~ 90,
        tt_sigla == 5 ~ 120
      )) %>%
      mutate(tt_ativo = case_when(
        tt_sigla == 1 ~ 15,
        tt_sigla == 2 ~ 30,
        tt_sigla == 3 ~ 45,
        tt_sigla == 4 ~ 60,
        tt_sigla == 5 ~ 75
      )) %>%
      mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
      mutate(junto_ativo = paste0(acess_sigla, atividade_sigla, tt_ativo)) %>%
      mutate(atividade_nome = case_when(atividade_sigla == "TT" ~ "empregos_total",
                                        atividade_sigla == "TB" ~ "empregos_baixa",
                                        atividade_sigla == "TM" ~ "empregos_media",
                                        atividade_sigla == "TA" ~ "empregos_alta",
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
                                        atividade_sigla == "MI" ~ "mat_infantil",
                                        atividade_sigla == "CT" ~ "cras_total"))
    
    
    # gerar o codigo
    # para tp
    codigo_cma_tp <- c(
      
      sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
              grid_cma$junto_tp, 
              grid_cma$atividade_nome, 
              grid_cma$tt_tp
      )
    )
    codigo_cma_carro_pico <- c(
      
      sprintf("%s = (sum(%s[which(median_morning_peak <= %s)], na.rm = T))", 
              grid_cma$junto_tp, 
              grid_cma$atividade_nome, 
              grid_cma$tt_tp
      )
    )
    codigo_cma_carro_fpico <- c(
      
      sprintf("%s = (sum(%s[which(median_afternoon_offpeak <= %s)], na.rm = T))", 
              grid_cma$junto_tp, 
              grid_cma$atividade_nome, 
              grid_cma$tt_tp
      )
    )
    
    
    # para ativo
    codigo_cma_ativo <- c(
      
      sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
              grid_cma$junto_ativo, 
              grid_cma$atividade_nome, 
              grid_cma$tt_ativo
      )
    )
    
    # dar nomes às variaveis
    to_make_cma_tp <-    setNames(codigo_cma_tp,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_tp))
    to_make_cma_carro_pico <-    setNames(codigo_cma_carro_pico,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_carro_pico))
    to_make_cma_carro_fpico <-    setNames(codigo_cma_carro_fpico,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_carro_fpico))
    to_make_cma_ativo <- setNames(codigo_cma_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_ativo))
    
    
    # so aplicar a acessibilidade para os modos da cidade
    # modo_year <- sprintf("modo_%s", ano)
    modo <- munis_list$munis_modo[abrev_muni == sigla_muni & ano_modo == ano]$modo
    
    message('Calculate access CMA')
    a <- Sys.time()
    # se for carro..
    if (mode_access == "car") {
      
      
      # acess_cma_carro_pico2 <- ttmatrix[, lapply(to_make_cma_carro_pico, function(x) eval(parse(text = x)))
      # , by=.(city, mode, origin)]
      cols <- unique(grid_cma$atividade_nome)
      acess_cma_carro_pico <- list(
        ttmatrix[median_morning_peak <= 15,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 30,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 60,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 90,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 120,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ]
      )
      
      acess_cma_carro_pico <- reduce(acess_cma_carro_pico, full_join, by = c("city", "mode", "origin")) %>%
        setDT()
      
      colnames(acess_cma_carro_pico) <- c("city", "mode", "origin", grid_cma$junto_tp)
      acess_cma_carro_pico[, pico := 1]
      
      
      # # access cma pico -----------------------------------------------------------------------------
      # acess_cma_carro_pico <- list(
      #   a1 <- ttmatrix[median_morning_peak <= 15,
      #                  .(CMATT15 = (sum(empregos_total, na.rm = T)),   
      #                    CMATB15 = (sum(empregos_baixa, na.rm = T)),   
      #                    CMATM15 = (sum(empregos_media, na.rm = T)),   
      #                    CMATA15 = (sum(empregos_alta, na.rm = T)),    
      #                    CMAST15 = (sum(saude_total, na.rm = T)),      
      #                    CMASB15 = (sum(saude_baixa, na.rm = T)),      
      #                    CMASM15 = (sum(saude_media, na.rm = T)),      
      #                    CMASA15 = (sum(saude_alta, na.rm = T)),       
      #                    CMAET15 = (sum(edu_total, na.rm = T)),        
      #                    CMAEI15 = (sum(edu_infantil, na.rm = T)),     
      #                    CMAEF15 = (sum(edu_fundamental, na.rm = T)),  
      #                    CMAEM15 = (sum(edu_medio, na.rm = T)),        
      #                    CMAMT15 = (sum(mat_total, na.rm = T)),        
      #                    CMAMI15 = (sum(mat_infantil, na.rm = T)),     
      #                    CMAMF15 = (sum(mat_fundamental, na.rm = T)),  
      #                    CMAMM15 = (sum(mat_medio, na.rm = T)),        
      #                    CMACT15 = (sum(cras_total, na.rm = T))),
      #                  by=.(city, mode, origin)
      #                  
      #   ],
      #   ttmatrix[median_morning_peak <= 30,
      #            .(CMATT30 = (sum(empregos_total, na.rm = T)),   
      #              CMATB30 = (sum(empregos_baixa, na.rm = T)),   
      #              CMATM30 = (sum(empregos_media, na.rm = T)),   
      #              CMATA30 = (sum(empregos_alta, na.rm = T)),    
      #              CMAST30 = (sum(saude_total, na.rm = T)),      
      #              CMASB30 = (sum(saude_baixa, na.rm = T)),      
      #              CMASM30 = (sum(saude_media, na.rm = T)),      
      #              CMASA30 = (sum(saude_alta, na.rm = T)),       
      #              CMAET30 = (sum(edu_total, na.rm = T)),        
      #              CMAEI30 = (sum(edu_infantil, na.rm = T)),     
      #              CMAEF30 = (sum(edu_fundamental, na.rm = T)),  
      #              CMAEM30 = (sum(edu_medio, na.rm = T)),        
      #              CMAMT30 = (sum(mat_total, na.rm = T)),        
      #              CMAMI30 = (sum(mat_infantil, na.rm = T)),     
      #              CMAMF30 = (sum(mat_fundamental, na.rm = T)),  
      #              CMAMM30 = (sum(mat_medio, na.rm = T)),        
      #              CMACT30 = (sum(cras_total, na.rm = T))),
      #            by=.(city, mode, origin)
      #            
      #   ],
      #   ttmatrix[median_morning_peak <= 60,
      #            .(CMATT60 = (sum(empregos_total, na.rm = T)),   
      #              CMATB60 = (sum(empregos_baixa, na.rm = T)),   
      #              CMATM60 = (sum(empregos_media, na.rm = T)),   
      #              CMATA60 = (sum(empregos_alta, na.rm = T)),    
      #              CMAST60 = (sum(saude_total, na.rm = T)),      
      #              CMASB60 = (sum(saude_baixa, na.rm = T)),      
      #              CMASM60 = (sum(saude_media, na.rm = T)),      
      #              CMASA60 = (sum(saude_alta, na.rm = T)),       
      #              CMAET60 = (sum(edu_total, na.rm = T)),        
      #              CMAEI60 = (sum(edu_infantil, na.rm = T)),     
      #              CMAEF60 = (sum(edu_fundamental, na.rm = T)),  
      #              CMAEM60 = (sum(edu_medio, na.rm = T)),        
      #              CMAMT60 = (sum(mat_total, na.rm = T)),        
      #              CMAMI60 = (sum(mat_infantil, na.rm = T)),     
      #              CMAMF60 = (sum(mat_fundamental, na.rm = T)),  
      #              CMAMM60 = (sum(mat_medio, na.rm = T)),        
      #              CMACT60 = (sum(cras_total, na.rm = T))),
      #            by=.(city, mode, origin)
      #            
      #            
      #   ],
      #   ttmatrix[median_morning_peak <= 90,
      #            .(CMATT90 = (sum(empregos_total, na.rm = T)),   
      #              CMATB90 = (sum(empregos_baixa, na.rm = T)),   
      #              CMATM90 = (sum(empregos_media, na.rm = T)),   
      #              CMATA90 = (sum(empregos_alta, na.rm = T)),    
      #              CMAST90 = (sum(saude_total, na.rm = T)),      
      #              CMASB90 = (sum(saude_baixa, na.rm = T)),      
      #              CMASM90 = (sum(saude_media, na.rm = T)),      
      #              CMASA90 = (sum(saude_alta, na.rm = T)),       
      #              CMAET90 = (sum(edu_total, na.rm = T)),        
      #              CMAEI90 = (sum(edu_infantil, na.rm = T)),     
      #              CMAEF90 = (sum(edu_fundamental, na.rm = T)),  
      #              CMAEM90 = (sum(edu_medio, na.rm = T)),        
      #              CMAMT90 = (sum(mat_total, na.rm = T)),        
      #              CMAMI90 = (sum(mat_infantil, na.rm = T)),     
      #              CMAMF90 = (sum(mat_fundamental, na.rm = T)),  
      #              CMAMM90 = (sum(mat_medio, na.rm = T)),        
      #              CMACT90 = (sum(cras_total, na.rm = T))),
      #            by=.(city, mode, origin)
      #            
      #            
      #   ],
      #   ttmatrix[median_morning_peak <= 120,
      #            .(CMATT120 = (sum(empregos_total, na.rm = T)),   
      #              CMATB120 = (sum(empregos_baixa, na.rm = T)),   
      #              CMATM120 = (sum(empregos_media, na.rm = T)),   
      #              CMATA120 = (sum(empregos_alta, na.rm = T)),    
      #              CMAST120 = (sum(saude_total, na.rm = T)),     
      #              CMASB120 = (sum(saude_baixa, na.rm = T)),     
      #              CMASM120 = (sum(saude_media, na.rm = T)),     
      #              CMASA120 = (sum(saude_alta, na.rm = T)),     
      #              CMAET120 = (sum(edu_total, na.rm = T)),      
      #              CMAEI120 = (sum(edu_infantil, na.rm = T)),     
      #              CMAEF120 = (sum(edu_fundamental, na.rm = T)),  
      #              CMAEM120 = (sum(edu_medio, na.rm = T)),      
      #              CMAMT120 = (sum(mat_total, na.rm = T)),      
      #              CMAMI120 = (sum(mat_infantil, na.rm = T)),     
      #              CMAMF120 = (sum(mat_fundamental, na.rm = T)),  
      #              CMAMM120 = (sum(mat_medio, na.rm = T)),      
      #              CMACT120 = (sum(cras_total, na.rm = T))),
      #            by=.(city, mode, origin)
      #            
      #            
      #   ]
      #   
      #   
      #   
      #   
      #   
      # )
      
      
      
      # access cma fora pico ------------------------------------------------------------------------
      
      
      # acess_cma_carro_fpico <- ttmatrix[, lapply(to_make_cma_carro_fpico, function(x) eval(parse(text = x)))
      # , by=.(city, mode, origin)]
      
      cols <- unique(grid_cma$atividade_nome)
      acess_cma_carro_fpico <- list(
        ttmatrix[median_afternoon_offpeak <= 15,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 30,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 60,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 90,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 120,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin), .SDcols=cols ]
      )
      
      acess_cma_carro_fpico <- reduce(acess_cma_carro_fpico, full_join, by = c("city", "mode", "origin")) %>%
        setDT()
      
      colnames(acess_cma_carro_fpico) <- c("city", "mode", "origin", grid_cma$junto_tp)
      acess_cma_carro_fpico[, pico := 0]
      
      
      # bind
      acess_cma <- rbind(acess_cma_carro_pico, acess_cma_carro_fpico)
      
      rm(acess_cma_carro_pico, acess_cma_carro_fpico)
      
      
    } else {
      
      # para modos ativos
      acess_cma <- ttmatrix[mode %in% c("bike", "walk"), 
                            lapply(to_make_cma_ativo, function(x) eval(parse(text = x)))
                            , by=.(city, mode, origin, pico)]
      
      # para transporte publico e (so 2019) carro
      if (modo == "todos") {
        # acess_cma_tp <- ttmatrix[mode %in% c("transit"),
        #                          lapply(to_make_cma_tp, function(x) eval(parse(text = x)))
        #                          , by=.(city, mode, origin, pico)]
        
        cols <- unique(grid_cma$atividade_nome)
        acess_cma_tp <- list(
          ttmatrix[travel_time <= 15 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 30 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 60 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 90 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 120 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, origin, pico), .SDcols=cols ]
        )
        
        acess_cma_tp <- reduce(acess_cma_tp, full_join, by = c("city", "mode", "origin", "pico")) %>%
          setDT()
        colnames(acess_cma_tp) <- c("city", "mode", "origin", "pico", grid_cma$junto_tp)
        
        # juntar os cma
        acess_cma <- rbind(acess_cma, acess_cma_tp,
                           fill = TRUE)
      }
      
    }
    
  }
  
  # 4) Calcular acessibilidade cumulativa passiva --------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (indicator_access %in% c("all", "passive")) {
    acess_cmp <- "CMP"
    atividade_cmp <- c("PT", 
                       "PH", "PM",
                       "PB", "PA", "PI", "PN", 
                       "P0005I", "P0614I","P1518I","P1924I","P2539I","P4069I","P70I")
    # criar dummy para tt
    tt <- c(1, 2, 3, 4, 5)
    
    grid_cmp <- expand.grid(acess_cmp, atividade_cmp, tt, stringsAsFactors = FALSE) %>%
      rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
      # adicionar colunas de time threshold para cada um dos modos
      mutate(tt_tp = case_when(
        tt_sigla == 1 ~ 15,
        tt_sigla == 2 ~ 30,
        tt_sigla == 3 ~ 60,
        tt_sigla == 4 ~ 90,
        tt_sigla == 5 ~ 120
      )) %>%
      mutate(tt_ativo = case_when(
        tt_sigla == 1 ~ 15,
        tt_sigla == 2 ~ 30,
        tt_sigla == 3 ~ 45,
        tt_sigla == 4 ~ 60,
        tt_sigla == 5 ~ 75
      )) %>%
      mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
      mutate(junto_ativo = paste0(acess_sigla, atividade_sigla, tt_ativo)) %>%
      mutate(atividade_nome = case_when(atividade_sigla == "PT"     ~ "pop_total",
                                        atividade_sigla == "PH"     ~ "pop_homens",
                                        atividade_sigla == "PM"     ~ "pop_mulheres",
                                        atividade_sigla == "PB"     ~ "cor_branca",
                                        atividade_sigla == "PA"     ~ "cor_amarela",
                                        atividade_sigla == "PI"     ~ "cor_indigena",
                                        atividade_sigla == "PN"     ~ "cor_negra",
                                        atividade_sigla == "P0005I" ~ "idade_0a5", 
                                        atividade_sigla == "P0614I" ~ "idade_6a14", 
                                        atividade_sigla == "P1518I" ~ "idade_15a18", 
                                        atividade_sigla == "P1924I" ~ "idade_19a24",    
                                        atividade_sigla == "P2539I" ~ "idade_25a39", 
                                        atividade_sigla == "P4069I" ~ "idade_40a69", 
                                        atividade_sigla == "P70I"    ~ "idade_70"))
    
    # gerar o codigo
    # para tp 
    codigo_cmp_tp <- c(
      
      sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
              grid_cmp$junto_tp, 
              grid_cmp$atividade_nome, 
              grid_cmp$tt_tp
      )
    )
    
    codigo_cmp_carro_pico <- c(
      
      sprintf("%s = (sum(%s[which(median_morning_peak <= %s)], na.rm = T))", 
              grid_cmp$junto_tp, 
              grid_cmp$atividade_nome, 
              grid_cmp$tt_tp
      )
    )
    codigo_cmp_carro_fpico <- c(
      
      sprintf("%s = (sum(%s[which(median_afternoon_offpeak <= %s)], na.rm = T))", 
              grid_cmp$junto_tp, 
              grid_cmp$atividade_nome, 
              grid_cmp$tt_tp
      )
    )
    
    # para ativo
    codigo_cmp_ativo <- c(
      
      sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
              grid_cmp$junto_ativo, 
              grid_cmp$atividade_nome, 
              grid_cmp$tt_ativo
      )
    )
    
    
    # gerar os nomes das variaveis
    to_make_cmp_tp <- setNames(codigo_cmp_tp, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_tp))
    to_make_cmp_carro_pico <- setNames(codigo_cmp_carro_pico, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_carro_pico))
    to_make_cmp_carro_fpico <- setNames(codigo_cmp_carro_fpico, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_carro_fpico))
    
    to_make_cmp_ativo <- setNames(codigo_cmp_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_ativo))
    
    
    
    # so aplicar a acessibilidade para os modos da cidade
    
    message('Calculate access CMP')
    # se for carro..
    if (mode_access == "car") {
      # acess_cmp_carro_pico <- ttmatrix[, lapply(to_make_cmp_carro_pico, function(x) eval(parse(text = x)))
      # , by=.(city, mode, destination)]
      
      cols <- unique(grid_cmp$atividade_nome)
      acess_cmp_carro_pico <- list(
        ttmatrix[median_morning_peak <= 15,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 30,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 60,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 90,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_morning_peak <= 120,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ]
      )
      
      acess_cmp_carro_pico <- reduce(acess_cmp_carro_pico, full_join, by = c("city", "mode", "destination")) %>%
        setDT()
      
      colnames(acess_cmp_carro_pico) <- c("city", "mode", "destination", grid_cmp$junto_tp)
      acess_cmp_carro_pico[, pico := 1]
      
      
      # acess_cmp_carro_fpico <- ttmatrix[, lapply(to_make_cmp_carro_fpico, function(x) eval(parse(text = x)))
      #                                   , by=.(city, mode, destination)]
      
      cols <- unique(grid_cmp$atividade_nome)
      acess_cmp_carro_fpico <- list(
        ttmatrix[median_afternoon_offpeak <= 15,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 30,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 60,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 90,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ],
        ttmatrix[median_afternoon_offpeak <= 120,
                 lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination), .SDcols=cols ]
      )
      
      acess_cmp_carro_fpico <- reduce(acess_cmp_carro_fpico, full_join, by = c("city", "mode", "destination")) %>%
        setDT()
      
      colnames(acess_cmp_carro_fpico) <- c("city", "mode", "destination", grid_cmp$junto_tp)
      acess_cmp_carro_fpico[, pico := 0]
      
      
      # bind
      acess_cmp <- rbind(acess_cmp_carro_pico, acess_cmp_carro_fpico)
      
      rm(acess_cmp_carro_pico, acess_cmp_carro_fpico)
      
    } else {
      
      # para modos ativos
      acess_cmp <- ttmatrix[mode %in% c("bike", "walk"), 
                            lapply(to_make_cmp_ativo, function(x) eval(parse(text = x)))
                            , by=.(city, mode, destination, pico)]
      
      # para transporte publico e (so 2019) carro
      if (modo == "todos") {
        # acess_cmp_tp <- ttmatrix[mode %in% c("transit"),
        #                          lapply(to_make_cmp_tp, function(x) eval(parse(text = x)))
        #                          , by=.(city, mode, destination, pico)]
        
        cols <- unique(grid_cmp$atividade_nome)
        acess_cmp_tp <- list(
          ttmatrix[travel_time <= 15 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 30 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 60 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 90 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination, pico), .SDcols=cols ],
          ttmatrix[travel_time <= 120 & mode %in% c("transit"),
                   lapply(.SD, sum, na.rm=TRUE), by=.(city, mode, destination, pico), .SDcols=cols ]
        )
        
        acess_cmp_tp <- reduce(acess_cmp_tp, full_join, by = c("city", "mode", "destination", "pico")) %>%
          setDT()
        colnames(acess_cmp_tp) <- c("city", "mode", "destination", "pico", grid_cmp$junto_tp)
        
        # juntar os cmp
        acess_cmp <- rbind(acess_cmp, acess_cmp_tp,
                           fill = TRUE)
      }
      
    }
    
  }
  
  
  
  # 5) Calcular acessibilidade tempo minimo ---------------
  # (aqui eh feito junto para os dois modos)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  if (indicator_access %in% c("all", "active")) {
    acess_tmi <- "TMI"
    atividade_tmi <- c("ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM", "CT")
    
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
                                        atividade_sigla == "EI" ~ "edu_infantil",
                                        atividade_sigla == "CT" ~ "cras_total"))
    
    
    # gerar o codigo
    codigo_tmi <- sprintf("%s = min(travel_time[which(%s >= 1)])", 
                          grid_tmi$junto, 
                          grid_tmi$atividade_nome)
    codigo_tmi_carro_pico <- sprintf("%s = min(median_morning_peak[which(%s >= 1)], na.rm = TRUE)", 
                                     grid_tmi$junto, 
                                     grid_tmi$atividade_nome)
    codigo_tmi_carro_fpico <- sprintf("%s = min(median_afternoon_offpeak[which(%s >= 1)], na.rm = TRUE)", 
                                      grid_tmi$junto, 
                                      grid_tmi$atividade_nome)
    
    
    # gerar os nomes das variaveis
    to_make_tmi <- setNames(codigo_tmi, sub('^([[:alnum:]]*) =.*', '\\1', codigo_tmi))
    to_make_tmi_carro_pico <- setNames(codigo_tmi_carro_pico, sub('^([[:alnum:]]*) =.*', '\\1', codigo_tmi_carro_pico))
    to_make_tmi_carro_fpico <- setNames(codigo_tmi_carro_fpico, sub('^([[:alnum:]]*) =.*', '\\1', codigo_tmi_carro_fpico))
    
    message('Calculate access TMI')
    # calcular acessibilidade
    if (mode_access == "car") {
      
      # tictoc::tic()
      # acess_tmi_carro_pico <- ttmatrix[, lapply(to_make_tmi_carro_pico, function(x) eval(parse(text = x)))
      #                                  , by=.(city, mode, origin)]
      # tictoc::toc()
      
      # tictoc::tic()
      acess_tmi_carro_pico <- ttmatrix[, .(
        TMIST = min(median_morning_peak[which(saude_total >= 1)], na.rm = TRUE),
        TMISB = min(median_morning_peak[which(saude_baixa >= 1)], na.rm = TRUE),
        TMISM = min(median_morning_peak[which(saude_media >= 1)], na.rm = TRUE),
        TMISA = min(median_morning_peak[which(saude_alta >= 1)], na.rm = TRUE),
        TMIET = min(median_morning_peak[which(edu_total >= 1)], na.rm = TRUE),
        TMIEI = min(median_morning_peak[which(edu_infantil >= 1)], na.rm = TRUE),
        TMIEF = min(median_morning_peak[which(edu_fundamental >= 1)], na.rm = TRUE),
        TMIEM = min(median_morning_peak[which(edu_medio >= 1)], na.rm = TRUE),
        TMICT = min(median_morning_peak[which(cras_total >= 1)], na.rm = TRUE)
      ),
      by=.(city, mode, origin)]
      # tictoc::toc()
      
      # tictoc::tic()
      # acess_tmi_carro_pico <- list(
      #   
      #   ttmatrix[saude_total >= 1, .(TMIST = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[saude_baixa >= 1, .(TMISB = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[saude_media >= 1, .(TMISM = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[saude_alta >= 1, .(TMISA = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_total >= 1, .(TMIET = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_infantil >= 1, .(TMIEI = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_fundamental >= 1, .(TMIEF = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_medio >= 1, .(TMIEM = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[cras_total >= 1, .(TMICT = min(median_morning_peak, na.rm = TRUE)), by=.(city, mode, origin)]
      #   
      # )
      # tictoc::toc()
      # acess_tmi_carro_pico <- reduce(acess_tmi_carro_pico, full_join, by=c("city", "mode", "origin")) %>% setDT()
      acess_tmi_carro_pico[, pico := 1]
      
      # acess_tmi_carro_fpico <- ttmatrix[, lapply(to_make_tmi_carro_fpico, function(x) eval(parse(text = x)))
      #                                   , by=.(city, mode, origin)]
      
      acess_tmi_carro_fpico <- ttmatrix[, .(
        TMIST = min(median_afternoon_offpeak[which(saude_total >= 1)], na.rm = TRUE),
        TMISB = min(median_afternoon_offpeak[which(saude_baixa >= 1)], na.rm = TRUE),
        TMISM = min(median_afternoon_offpeak[which(saude_media >= 1)], na.rm = TRUE),
        TMISA = min(median_afternoon_offpeak[which(saude_alta >= 1)], na.rm = TRUE),
        TMIET = min(median_afternoon_offpeak[which(edu_total >= 1)], na.rm = TRUE),
        TMIEI = min(median_afternoon_offpeak[which(edu_infantil >= 1)], na.rm = TRUE),
        TMIEF = min(median_afternoon_offpeak[which(edu_fundamental >= 1)], na.rm = TRUE),
        TMIEM = min(median_afternoon_offpeak[which(edu_medio >= 1)], na.rm = TRUE),
        TMICT = min(median_afternoon_offpeak[which(cras_total >= 1)], na.rm = TRUE)
      ),
      by=.(city, mode, origin)]
      
      # tictoc::tic()
      # acess_tmi_carro_fpico <- list(
      #   
      #   ttmatrix[saude_total >= 1,     .(TMIST = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[saude_baixa >= 1,     .(TMISB = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[saude_media >= 1,     .(TMISM = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[saude_alta >= 1,      .(TMISA = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_total >= 1,       .(TMIET = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_infantil >= 1,    .(TMIEI = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_fundamental >= 1, .(TMIEF = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[edu_medio >= 1,       .(TMIEM = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)],
      #   ttmatrix[cras_total >= 1,      .(TMICT = min(median_afternoon_offpeak, na.rm = TRUE)), by=.(city, mode, origin)]
      #   
      # )
      # tictoc::toc()
      # acess_tmi_carro_fpico <- reduce(acess_tmi_carro_fpico, full_join, by=c("city", "mode", "origin")) %>% setDT()
      acess_tmi_carro_fpico[, pico := 0]
      
      
      # bind
      acess_tmi <- rbind(acess_tmi_carro_pico, acess_tmi_carro_fpico)
      
      rm(acess_tmi_carro_pico, acess_tmi_carro_fpico)
      
      
    } else {
      
      # tictoc::tic()
      # acess_tmi <- ttmatrix[, lapply(to_make_tmi, function(x) eval(parse(text = x)))
      #                       , by=.(city, mode, origin, pico)]
      # tictoc::toc()
      
      # tictoc::tic()
      acess_tmi <- ttmatrix[,  .(TMIST = min(travel_time[which(saude_total >= 1)]),   
                                 TMISB = min(travel_time[which(saude_baixa >= 1)]), 
                                 TMISM = min(travel_time[which(saude_media >= 1)]),      
                                 TMISA = min(travel_time[which(saude_alta >= 1)]), 
                                 TMIET = min(travel_time[which(edu_total >= 1)]),   
                                 TMIEI = min(travel_time[which(edu_infantil >= 1)]), 
                                 TMIEF = min(travel_time[which(edu_fundamental >= 1)]),       
                                 TMIEM = min(travel_time[which(edu_medio >= 1)]), 
                                 TMICT = min(travel_time[which(cras_total >= 1)])),
                                 by=.(city, mode, origin, pico)]
      # tictoc::toc()

# acess_tmi1 <- list(
#   
#   ttmatrix[saude_total >= 1, .(TMIST = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[saude_baixa >= 1, .(TMISB = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[saude_media >= 1, .(TMISM = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[saude_alta >= 1, .(TMISA = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[edu_total >= 1, .(TMIET = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[edu_infantil >= 1, .(TMIEI = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[edu_fundamental >= 1, .(TMIEF = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[edu_medio >= 1, .(TMIEM = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)],
#   ttmatrix[cras_total >= 1, .(TMICT = min(travel_time, na.rm = TRUE)), by=.(city, mode, origin, pico)]
#   
# )
# tictoc::toc()
# acess_tmi1 <- reduce(acess_tmi1, full_join, by=c("city", "mode", "origin", "pico")) %>% setDT()

    }
  }
  
  
  # 6) Calcular acessibilidade BFCA ---------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  if (BFCA) {
    
    
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
      ttmatrix_hosp[,':='(impedance1 = mgaus_f(travel_time, 10),
                          impedance2 = mgaus_f(travel_time, 40),
                          impedance3 = mgaus_f(travel_time, 100),
                          impedance4 = mgaus_f(travel_time, 180)) ]
      
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
    
    
    
    
  }
  
  
  # 7) Juntar os arquivos de acess ------------------------------------------------
  
  message('Output ...')
  # Juntar os tres (left_join)
  if (indicator_access == "all") {
    
    acess <- merge(acess_cma, acess_cmp,
                   all = TRUE,
                   by.x = c("city", "mode", "origin", "pico"),
                   # como o cmp eh calculado para os destinos, o join eh para destination
                   by.y = c("city", "mode", "destination", "pico"))
    
    acess <- merge(acess, acess_tmi,
                   all = TRUE,
                   by.x = c("city", "mode", "origin", "pico"),
                   by.y = c("city", "mode", "origin", "pico"))
    
    rm(acess_cma)
    rm(acess_cmp)
    rm(acess_tmi)
    
  } else if (indicator_access == "active") {
    
    acess <- merge(acess_cma, acess_tmi,
                   all = TRUE,
                   by.x = c("city", "mode", "origin", "pico"),
                   by.y = c("city", "mode", "origin", "pico"))
    
    rm(acess_cma)
    rm(acess_tmi)
    
    
  } else {
    
    acess <- acess_cmp 
    setnames(acess, "destination", "origin")
    
    rm(acess_cmp)
    
  }
  
  if (BFCA) {
    acess <- merge(acess, acess_bfc,
                   all = TRUE,
                   by.x = c("city", "mode", "origin", "pico"),
                   by.y = c("city", "mode", "origin", "pico"))
  }
  
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
  out1 <- ifelse(mode_access == "all", "tp_active", "car")
  
  path_out <- sprintf("../../data/acesso_oport/output_access/%s/%s/acess_%s_%s_%s_access-%s.rds", ano, out1, ano, sigla_muni, mode_access, indicator_access)
  
  
  write_rds(acess_sf, path_out)
  
  # gc colletc
  rm(ttmatrix)
  rm(acess_sf)
  rm(acess)
  gc(TRUE)
  
  
}

# # 2. APLICAR PARA TODOS AS CIDADADES --------------------------------------------------------------
# plan(multiprocess, workers = 5)
# furrr::future_walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, calcular_acess_muni, ano = 2017)
# furrr::future_walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, calcular_acess_muni, ano = 2018)
# furrr::future_walk(c("for", "cur","poa","bho",
#                      "sal","man","rec","bel",
#                      "gua","cam","slz","sgo","mac",
#                      "duq","cgr", 'nat'), calcular_acess_muni, ano = 2019)
# calcular_acess_muni("rio", 2019)
# calcular_acess_muni("bsb", 2019)
# calcular_acess_muni("spo", 2019)
# calcular_acess_muni("goi", 2019)
# 
# 
# 
# 
# # para carro --------------
# # big boys first
calcular_acess_muni("man", 2019, mode_access = "car")
# calcular_acess_muni("spo", 2019, mode_access = "car") #ok
# calcular_acess_muni("man", 2019, mode_access = "car") # ok
# calcular_acess_muni("rio", 2019, mode_access = "car") #ok
calcular_acess_muni("bsb_origin1", 2019, mode_access = "car", indicator_access = "active")
calcular_acess_muni("bsb_origin2", 2019, mode_access = "car", indicator_access = "active")
calcular_acess_muni("bsb_origin3", 2019, mode_access = "car", indicator_access = "active")
# # calcular_acess_muni("bsb_dest1",   2019, mode_access = "car", indicator_access = "passive")
# # calcular_acess_muni("bsb_dest2",   2019, mode_access = "car", indicator_access = "passive")
# # calcular_acess_muni("bsb_dest3",   2019, mode_access = "car", indicator_access = "passive")
# calcular_acess_muni("goi_origin1", 2017, mode_access = "car", indicator_access = "active")
# calcular_acess_muni("goi_origin2", 2017, mode_access = "car", indicator_access = "active")
# calcular_acess_muni("goi_origin3", 2017, mode_access = "car", indicator_access = "active")
# # calcular_acess_muni("goi_dest1",   2019, mode_access = "car", indicator_access = "passive")
# # calcular_acess_muni("goi_dest2",   2019, mode_access = "car", indicator_access = "passive")
# # calcular_acess_muni("goi_dest3",   2019, mode_access = "car", indicator_access = "passive")
# 
# others
plan(multiprocess, workers = 14)
furrr::future_walk(c("for", "cur","poa","bho",
                     "sal","rec","bel","gua",
                     "cam","slz","sgo",
                     "mac","duq",'nat'),
                   calcular_acess_muni, ano = 2019,
                   mode_access = "car")
# 
