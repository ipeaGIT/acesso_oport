# Agrega informacoes demograficas e de uso do solo nos hexagonos

# carregar bibliotecas -----------------------------------------------------------------------------
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
  cnes_data <- cnes_data[, .(cnes, ibge,
                             health_low, health_med, health_high,
                             lon, lat)]
  
  cnes_data <- cnes_data %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  
  # 1.2) Escolas
  escolas <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano, ano))
  
  # remove lat lon missing
  escolas <- escolas[!is.na(lat),] 
  
  # select columns
  escolas <- escolas[, .(co_entidade, code_muni,
                         mat_infantil, mat_fundamental, mat_medio,
                         lon, lat)]
  
  
  # 1.3) Empregos
  empregos <- readr::read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected.rds", ano, ano))
  
  # remove lat lon missing
  empregos <- empregos[!is.na(lat), ]
  
  # select columns
  empregos <- empregos[, .(codemun, id_estab, baixo, medio, alto, lon, lat)]
  
  #' A funcao `agrupar_variaveis` agrupa as variaveis de uso do solo determinadas acima
  #'  nos hexagonos de cada um dos municipios
  #'  Tambem traz as informacoes demograficas da grade do ibge
  
  agrupar_variaveis <- function(sigla_muni) { 
    
    # sigla_muni <- "for"
    # sigla_muni <- "nat"
    
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
    
    # Qual o codigo do municipio em questao?
    cod_mun_ok <- munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
      unlist()
    
    # Filtrar somente as atividades referentes a cada municipio e transforma em sf
    # para rais 2017
    empregos_filtrado <- empregos[codemun == substr(cod_mun_ok, 1, 6)] %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # escolas
    escolas_filtrado <- setDT(escolas)[code_muni == cod_mun_ok] %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # saude
    cnes_filtrado <- setDT(cnes_data)[code_muni == substr(cod_mun_ok, 1, 6)] %>% st_sf()
    
    
    
    # FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES
    
    por_resolucao <- function(muni_res) {
      # muni_res <- '09'
      # muni_res <- '08'
      
      # endereco do hexagono na resolucao
      hexf <- sprintf("../../data/acesso_oport/hex_municipio/%s/hex_%s_%s_%s.rds", ano, sigla_muni, muni_res, ano)
      
      # Ler arquivo de hexagono  
      hex_muni <- readr::read_rds(hexf)
      
      # Agrupar populacao, cor e renda
      # join espacial 
      hex_pop <- hex_muni %>% st_join(centroide_pop)
      
      
      ### a. substituir CENTROID pela proporcao de intersecao
      ### b. arredondamento %>%
      ###  mutate_at(vars(matches("pop|renda|moradores|cor|idade")), round)
      
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
                                    pop_total    = as.numeric(sum(round(pop_total,0), na.rm = TRUE)),
                                    pop_homens   = as.numeric(sum(round(pop_homens,0), na.rm = TRUE)),
                                    pop_mulheres = as.numeric(sum(round(pop_mulheres,0), na.rm = TRUE)),
                                    renda_total  = as.numeric(sum(renda, na.rm = TRUE))), 
                                by = id_hex ]
      
      # Calcular quintil e decil de renda
      # calcula renda per capita de cada hexagono
      hex_pop[, renda_capita := renda_total / pop_total]
      
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
      
      # Junta todos os dados agrupados por hexagonos
      hex_muni_fim <- left_join(hex_muni, hex_pop) %>%
        left_join(hex_rais) %>%
        left_join(hex_saude) %>%
        left_join(hex_escolas)
      
      # substitui NAs por zeros
      hex_muni_fim[is.na(hex_muni_fim)] <- 0
      
      # adiciona sigla do municipio
      hex_muni_fim$muni <- sigla_muni
      
      
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
  future::plan(future::multiprocess)
  #options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
  future.apply::future_lapply(X = x, FUN=agrupar_variaveis, future.packages=c('sf', 'dplyr', 'data.table', 'Hmisc'))
  
  
}



# aplicar funcao -----------------
agrupar_variaveis_hex(ano = 2017)
agrupar_variaveis_hex(ano = 2018)
agrupar_variaveis_hex(ano = 2019)








# # compare years -------------------------------------------------------------------------------
# 
# 
# 
# 
# # sigla_munii <- 'for'
# 
# compare_jobs_distribution <- function(sigla_munii) {
#   
#   # open hex files
#   hex_jobs_2017 <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2017/hex_agregado_%s_09_2017.rds",
#                                     sigla_munii)) %>%
#     mutate(ano_jobs = 2017)
#   
#   hex_jobs_2018 <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2018/hex_agregado_%s_09_2018.rds",
#                                     sigla_munii)) %>%
#     mutate(ano_jobs = 2018)
#   
#   # hex_jobs_2017_old <- read_rds(sprintf("../../data/acesso_oport/hex_agregados/2019/hex_agregado_%s_09_2019.rds",
#   #                                       sigla_munii)) %>%
#   #   mutate(ano_jobs = "2017_old")
#   
#   hex_jobs <- rbind(hex_jobs_2017, hex_jobs_2018)
#   # hex_jobs <- rbind(hex_jobs_2017, hex_jobs_2018, hex_jobs_2017_old)
#   hex_jobs <- select(hex_jobs, id_hex, sigla_muni, empregos_total, ano_jobs, geometry) %>% setDT()
#   
#   hex_jobs_wide <- pivot_wider(hex_jobs, names_from = ano_jobs, values_from = empregos_total,
#                                names_prefix = "jobs_")
#   
#   # compare!
#   hex_jobs_wide <- hex_jobs_wide %>%
#     mutate(dif1_abs = jobs_2018 - jobs_2017) %>%
#     mutate(dif1_log = log(jobs_2018/jobs_2017)) %>%
#     # truncate
#     mutate(dif1_abs_tc = case_when(dif1_abs < -500 ~ -500,
#                                    dif1_abs > 500 ~ 500,
#                                    TRUE ~ dif1_abs)) %>%
#     mutate(dif1_log_tc = case_when(dif1_log < -1 ~ -1,
#                                    dif1_log > 1 ~ 1,
#                                    TRUE ~ dif1_log)) %>%
#     st_sf()
#   
#   
#   hex_jobs_wide <- hex_jobs_wide %>%
#     filter(!(jobs_2017 == 0 & jobs_2018 == 0))
#   
#   map1 <- ggplot()+
#     geom_sf(data = hex_jobs_wide, aes(fill = dif1_log_tc), color = NA)+
#     scale_fill_distiller(palette = "RdBu"
#                          , limits  = c(-1, 1)*max(abs(hex_jobs_wide$dif1_log_tc))
#     )+
#     labs(title = "Diferença relativa de empregos entre 2017 e 2018",
#          subtitle = "Em vermelho: Jobs 2018 > Jobs 2017")+
#     theme_aop_map()
#   
#   # ggplot()+
#   #   geom_sf(data = hex_jobs_wide, aes(fill = dif1_abs), color = NA)+
#   #   scale_fill_distiller(palette = "RdBu"
#   #                        , limits  = c(-1, 1)*max(abs(hex_jobs_wide$dif1_abs))
#   #   )+
#   #   labs(title = "Diferença relativa de empregos entre 2017 e 2018",
#   #        subtitle = "Em vermelho: Jobs 2018 > Jobs 2017")+
#   #   theme_for_CMA()
#   
#   plot1 <- ggplot()+
#     geom_boxplot(data = hex_jobs_wide, aes(x = 1, y = dif1_log))+
#     labs(title = "Diferença relativa de empregos entre 2017 e 2018",
#          subtitle = "Maior que zero: Jobs 2018 > Jobs 2017")+
#     # theme_ipsum()+
#     theme_bw()+
#     ggforce::facet_zoom(ylim = c(-0.5, 0.5))
#   
#   map2 <- ggplot()+
#     geom_sf(data = hex_jobs_wide, aes(fill = dif1_abs_tc), color = NA)+
#     scale_fill_distiller(palette = "RdBu"
#                          , limits  = c(-1, 1)*max(abs(hex_jobs_wide$dif1_abs_tc))
#     )+
#     labs(title = "Diferença absoluta de empregos entre 2017 e 2018",
#          subtitle = "Em vermelho: Jobs 2018 > Jobs 2017")+
#     theme_aop_map()
#   
#   plot2 <- ggplot()+
#     geom_boxplot(data = hex_jobs_wide, aes(x = 1, y = dif1_abs))+
#     # labs(title = "Diferença absoluta empregos entre 2017 e 2018",
#     #      subtitle = "Maior que zero: Jobs 2018 > Jobs 2017")+
#     # theme_ipsum()+
#     theme_bw()+
#     theme(axis.text.x = element_blank())
#     # ggforce::facet_zoom(ylim = c(-100, 100))
#   
#   summary(hex_jobs_wide$dif1_abs)
#   
#   library(patchwork)
#   figure2 <- map2 + plot2 + plot_layout(widths = c(3, 1)) 
#   
#   ggsave(filename = sprintf("reports/comparacao_distribuicao_anos/comp_jobs_%s.png", sigla_munii),
#          plot = figure2,
#          device = "png",
#          width = 16,
#          units = "cm")
#   
#   
#   # ggsave(filename = sprintf("reports/%s_teste2.png", sigla_munii), plot = plot2)
#   # ggsave(filename = sprintf("reports/%s_teste3.png", sigla_munii), plot = plot3)
#   
#   
#   
# }
# 
# 
# 
# lapply(munis_df_2019$abrev_muni, compare_jobs_distribution)
# 
# 
# 
# 
# # Checagem de resultados ---------------------------------------------------------------------------
# 
# # Fun para criar mapas interativos (html) de distribuicao espacial de uso do solo
# salva_mapas <- function(sigla_muni, ano) {
#   
#   # sigla_muni <- "bsb"; ano <- 2019
#   # sigla_muni <- "for"; ano <- 2018
#   
#   
#   # trazer dados de uso do solo
#   us <- readr::read_rds(sprintf("../../data/acesso_oport/hex_agregados/%s/hex_agregado_%s_09_%s.rds", ano, sigla_muni, ano))
#   
#   
#   # # saude
#   # map_saude <- mapview(subset(us, saude_total>0), zcol = "saude_total")
#   # 
#   # # empregos
#   # map_empregos <- mapview(subset(us, empregos_total>0), zcol = "empregos_total")
#   # 
#   # # educacao
#   # map_edu <- mapview(subset(us, edu_total>0), zcol = "edu_total")
#   # 
#   # 
#   # single_map <- mapview(subset(us, saude_total>0), zcol = "saude_total") +  
#   #   mapview(subset(us, empregos_total>0), zcol = "empregos_total") + 
#   #   mapview(subset(us, edu_total>0), zcol = "edu_total")
#   
#   # save
#   # mapview::mapshot(single_map, debug=T, remove_controls=NULL, url = sprintf("reports/distribuicao_us/us_%s_%s.html", sigla_muni, ano))
#   
#   maxs <- data.frame(sigla_muni = sigla_muni,
#                      max_saude = max(us$saude_total),
#                      max_edu = max(us$edu_total))
#   
# }  
# 
# # Aplica funcao para cada municipio
# 
# # Parallel processing using future.apply
# maxs <- lapply(X = munis_df_2019$abrev_muni, FUN=salva_mapas, ano = 2018)
# 
# maxs_dt <- rbindlist(maxs)
