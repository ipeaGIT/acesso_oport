# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Calcular acessibilidade

# carregar bibliotecas
source('./R/fun/setup.R')






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 1. FUNCAO PARA CALCULAR ACESSIBILIDADE --------------------------------------------------------------


calcular_acess <- function(sigla_muni, ano) {
  
  # sigla_muni <- "for"; ano=2019
  
  # status message
  message('Woking on city ', sigla_muni, '\n')
  
  
  # Listar arquivos de matriz em formato .rds
  tt_files <- dir(path= sprintf("../data/output_ttmatrix/%s/", sigla_muni), pattern = '.rds', full.names = T)
  
  # Ler e empilhar ttmatrix
  future::plan(future::multiprocess)
  ttmatrix_allmodes <- future.apply::future_lapply(X =tt_files, FUN=readr::read_rds, future.packages=c('readr')) %>% data.table::rbindlist(fill = T)
  # ttmatrix_allmodes <- lapply(X=tt_files, FUN= readr::read_rds) %>% data.table::rbindlist(fill = T)
  
  # TESTE correcao do mode
  # ttmatrix_allmodes[,  mode := ifelse(mode=='NA', 'transit',mode)]
  # ttmatrix_allmodes[,  mode := ifelse(is.na(mode), 'transit',mode)]
  # table(ttmatrix_allmodes$mode)
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem (para qualquer modo) como 350s
    # 350s equivale ao tempo necessario para cruzar um hexagono a pe (1 metro/sec = 3.6 km/h)
    ttmatrix_allmodes[, travel_time := ifelse(origin == destination, 350, travel_time)]
  
   # convert depart_time para formato itime
    ttmatrix_allmodes[, depart_time := as.ITime(depart_time)]
      
    # Classificar informacao de horario de partida como pico ou fora pico
      ttmatrix_allmodes[, pico := ifelse(mode %in% c("bike", "walk"), 1,
                              ifelse( depart_time %between% c(as.ITime("06:0:00"), as.ITime("18:00:00")),1,0))]
  
  
  
  
# Calcular a mediana do tempo de viagem entre cada par OD para pico e fora pico ------------------
  

  # Calcular a mediana agrupando por sigla_muni, modo, origin, destination, pico
  ttmatrix_median <- ttmatrix_allmodes[, .(tt_median = median(travel_time, na.rm = TRUE)), 
                                       by = .(city, mode, origin, destination, pico)]
  
  # clean RAM memory
      # rm(ttmatrix_allmodes); gc(reset = T)
      
### Agrega dados de uso do solo
      
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../data/hex_agregados/hex_agregado_%s_09.rds", sigla_muni)
  
  # abrir oportunidades com hexagonos
  hexagonos_sf <- readr::read_rds(dir_hex) 
  
  
  # filtra apenas colunas com info demograficas na origem
  hex_orig <- setDT(hexagonos_sf)[, .(id_hex, pop_total, cor_branca, cor_amarela, cor_indigena, cor_negra, renda_total, renda_capta, quintil, decil)]
  
  # filtra apenas colunas com info de uso do solo no destino
  hex_dest <- setDT(hexagonos_sf)[, .(id_hex, empregos_baixa, empregos_media, empregos_alta, empregos_total, saude_total, edu_total, edu_infantil, edu_fundamental, edu_medio)]
  
  
  
  # Merge de dados de origem na matrix de tempo de viagem

    ttmatrix <- ttmatrix_median[hex_orig, on = c("origin" = "id_hex"),  
                                c('pop_total','cor_branca','cor_amarela','cor_indigena','cor_negra','renda_total','renda_capta','quintil','decil') :=
                                list(i.pop_total, i.cor_branca, i.cor_amarela, i.cor_indigena, i.cor_negra, i.renda_total, i.renda_capta, i.quintil, i.decil)]
                                
    # Merge de dados de destino na matrix de tempo de viagem
    ttmatrix <- ttmatrix[hex_dest, on = c("destination" = "id_hex"),  
                         c("empregos_baixa","empregos_media","empregos_alta","empregos_total","saude_total","edu_total","edu_infantil","edu_fundamental","edu_medio") :=
                         list(i.empregos_baixa,i.empregos_media,i.empregos_alta,i.empregos_total,i.saude_total,i.edu_total,i.edu_infantil,i.edu_fundamental,i.edu_medio)]    
    
    
    
  # Transformar o traveltime para minutos
    ttmatrix[, tt_median := tt_median/60]
  
  # Calcular emprego com match qualitativo de renda e nivel de escolaridade do emprego 
    # high income people = jobs with high and med education
    # low income people = jobs with low and med education
    ttmatrix[, empregos_match_decil := ifelse(decile>5, sum(empregos_media, empregos_alta),  sum(empregos_media, empregos_baixa)), by=.(origin, destination)]
    ttmatrix[, empregos_match_quintil := ifelse(decile>5, sum(empregos_media, empregos_alta),  sum(empregos_media, empregos_baixa)), by=.(origin, destination)]
    66666666666666666666666666666666666666666
    
    CORRIGIR DECIL E QUINTIL

  # Dicionario de variaveis:
  # - CMA = Acessibilidade Cumulativa Ativa
  # - CMP = Acessibilidade Cumulativa Passiva
  # - CPT = Acessibilidade considerando Competitividade ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!!!!
  # - TMI = Acessibilidade de Tempo Mínimo à Oportunidade
  
  # 1 - All accessible activities from each ORIGIN across they day
  access_ative <- ttmatrix_variaveis[, 
                                     .(CMA_ST_15 = sum( saude_total[which( tt_median <= 15)], na.rm=T)
                                       , CMA_ST_30 = sum( saude_total[which( tt_median <= 30)], na.rm=T)
                                       , CMA_ST_60 = sum( saude_total[which( tt_median <= 60)], na.rm=T)
                                       , CMA_ST_90 = sum( saude_total[which( tt_median <= 90)], na.rm=T)
                                       , CMA_ST_120 = sum(saude_total[which( tt_median <= 120)], na.rm=T)
                                       
                                       , CMA_EI_15 = sum( escolas_infantil[which( tt_median <= 15)], na.rm=T)
                                       , CMA_EI_30 = sum( escolas_infantil[which( tt_median <= 30)], na.rm=T)
                                       , CMA_EI_60 = sum( escolas_infantil[which( tt_median <= 60)], na.rm=T)
                                       , CMA_EI_90 = sum( escolas_infantil[which( tt_median <= 90)], na.rm=T)
                                       , CMA_EI_120 = sum(escolas_infantil[which( tt_median <= 120)], na.rm=T)
                                       
                                       , CMA_EF_15 = sum( escolas_fundamental[which( tt_median <= 15)], na.rm=T)
                                       , CMA_EF_30 = sum( escolas_fundamental[which( tt_median <= 30)], na.rm=T)
                                       , CMA_EF_60 = sum( escolas_fundamental[which( tt_median <= 60)], na.rm=T)
                                       , CMA_EF_90 = sum( escolas_fundamental[which( tt_median <= 90)], na.rm=T)
                                       , CMA_EF_120 = sum(escolas_fundamental[which( tt_median <= 120)], na.rm=T)
                                       
                                       , CMA_EM_15 = sum( escolas_medio[which( tt_median <= 15)], na.rm=T)
                                       , CMA_EM_30 = sum( escolas_medio[which( tt_median <= 30)], na.rm=T)
                                       , CMA_EM_60 = sum( escolas_medio[which( tt_median <= 60)], na.rm=T)
                                       , CMA_EM_90 = sum( escolas_medio[which( tt_median <= 90)], na.rm=T)
                                       , CMA_EM_120 = sum(escolas_medio[which( tt_median <= 120)], na.rm=T)
                                       
                                       , CMA_TT_15 = sum( empregos_total[which( tt_median <= 15)], na.rm=T)
                                       , CMA_TT_30 = sum( empregos_total[which( tt_median <= 30)], na.rm=T)
                                       , CMA_TT_60 = sum( empregos_total[which( tt_median <= 60)], na.rm=T)
                                       , CMA_TT_90 = sum( empregos_total[which( tt_median <= 90)], na.rm=T)
                                       , CMA_TT_120 = sum(empregos_total[which( tt_median <= 120)], na.rm=T)
                                       
                                       , TMI_ST = min(tt_median[which(saude_total >= 1)])
                                       , TMI_EI = min(tt_median[which(escolas_infantil >= 1)])
                                       , TMI_EF = min(tt_median[which(escolas_fundamental >= 1)])
                                       , TMI_EM = min(tt_median[which(escolas_medio >= 1)])
                                       
                                     ),
                                     by=.(city, mode, origin, decil, quintil, pico) ]
  
  
  # Calculo da acessibilidade passiva
  access_passive <- ttmatrix_variaveis[,
                                       
                                       .(CMP_PT_15 = sum(pop_total[which( tt_median <= 15)], na.rm=T)
                                         , CMP_PT_30 = sum(pop_total[which( tt_median <= 30)], na.rm=T)
                                         , CMP_PT_60 = sum(pop_total[which( tt_median <= 60)], na.rm=T)
                                         , CMP_PT_90 = sum(pop_total[which( tt_median <= 90)], na.rm=T)
                                         , CMP_PT_120 = sum(pop_total[which( tt_median <= 120)], na.rm=T)
                                         
                                       ),   
                                       by =.(city, mode, destination, pico)]
  
  # Juntar os dois
  access <- merge(access_ative, access_passive,
                  all.x = TRUE,
                  by.x = c("city", "mode", "origin", "pico"),
                  by.y = c("city", "mode", "destination", "pico"))
  
  
  # # Nao faz mais sentido o timetrhresold ser menor para walk e bike?
  # access_at <- ttmatrix_variaveis[mode %in% c("walk", "bike"), 
  #                                 .(CMA_ST_10 = sum( saude_total[which( tt_median <= 10)], na.rm=T)
  #                                  , CMA_ST_20 = sum( saude_total[which( tt_median <= 20)], na.rm=T)
  #                                  , CMA_ST_30 = sum( saude_total[which( tt_median <= 30)], na.rm=T)
  #                                  , CMA_ST_40 = sum( saude_total[which( tt_median <= 40)], na.rm=T)
  #                                  , CMA_ST_50 = sum( saude_total[which( tt_median <= 50)], na.rm=T)
  #                                  
  #                                  , CMA_EI_10 = sum( escolas_infantil[which( tt_median <= 10)], na.rm=T)
  #                                  , CMA_EI_20 = sum( escolas_infantil[which( tt_median <= 20)], na.rm=T)
  #                                  , CMA_EI_30 = sum( escolas_infantil[which( tt_median <= 30)], na.rm=T)
  #                                  , CMA_EI_40 = sum( escolas_infantil[which( tt_median <= 40)], na.rm=T)
  #                                  , CMA_EI_50 = sum( escolas_infantil[which( tt_median <= 50)], na.rm=T)
  #                                  
  #                                  , CMA_EF_10 = sum( escolas_fundamental[which( tt_median <= 10)], na.rm=T)
  #                                  , CMA_EF_20 = sum( escolas_fundamental[which( tt_median <= 20)], na.rm=T)
  #                                  , CMA_EF_30 = sum( escolas_fundamental[which( tt_median <= 30)], na.rm=T)
  #                                  , CMA_EF_40 = sum( escolas_fundamental[which( tt_median <= 40)], na.rm=T)
  #                                  , CMA_EF_50 = sum( escolas_fundamental[which( tt_median <= 50)], na.rm=T)
  #                                  
  #                                  , CMA_EM_10 = sum( escolas_medio[which( tt_median <= 10)], na.rm=T)
  #                                  , CMA_EM_20 = sum( escolas_medio[which( tt_median <= 20)], na.rm=T)
  #                                  , CMA_EM_30 = sum( escolas_medio[which( tt_median <= 30)], na.rm=T)
  #                                  , CMA_EM_40 = sum( escolas_medio[which( tt_median <= 40)], na.rm=T)
  #                                  , CMA_EM_50 = sum( escolas_medio[which( tt_median <= 50)], na.rm=T)
  #                                  
  #                                  , CMA_TT_10 = sum( empregos_total[which( tt_median <= 10)], na.rm=T)
  #                                  , CMA_TT_20 = sum( empregos_total[which( tt_median <= 20)], na.rm=T)
  #                                  , CMA_TT_30 = sum( empregos_total[which( tt_median <= 30)], na.rm=T)
  #                                  , CMA_TT_40 = sum( empregos_total[which( tt_median <= 40)], na.rm=T)
  #                                  , CMA_TT_50 = sum( empregos_total[which( tt_median <= 50)], na.rm=T)
  #                                  
  #                                  , TMI_ST = min(tt_median[which(saude_total >= 1)])
  #                                  , TMI_EI = min(tt_median[which(escolas_infantil >= 1)])
  #                                  , TMI_EF = min(tt_median[which(escolas_fundamental >= 1)])
  #                                  , TMI_EM = min(tt_median[which(escolas_medio >= 1)])
  # ),
  # by=.(city, mode, origin, pico) ]
  
  # # Juntar as bases
  # access <- rbind(access_pt, access_at)
  
  # # Trazer de volta a geometria das origens
  # 
  # access_sf <- map(list(access_at, access_pt), merge, 
  #                  setDT(hexagonos_sf)[, .(id_hex, geometry)],
  #                  by.x = "origin", 
  #                  by.y = "id_hex", 
  #                  all.x = TRUE)
  # 
  # access_sf <- map(access_sf, st_sf)
  # 
  # names(access_sf) <- c("ativo", "transit")
  # 
  # return(access_sf)
  
  access_sf <- merge(access, setDT(hexagonos_sf)[, .(id_hex, geometry)],
                     by.x = "origin",
                     by.y = "id_hex",
                     all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  # Salvar
  path_out <- sprintf("../data/output_access/acess_%s.rds", sigla_muni)
  write_rds(access_sf, path_out)
  
  
}


```

Função para produzir mapas de acessibilidade:
  
  ```{r fun_criar_mapas}


# Abrir linhas de alta/media capasigla_muni
linhas_hm <- read_rds("../data/linhas_HMcapasigla_muni/linhas_HMcapasigla_muni.rds") %>%
  mutate(city = substr(sigla_muni, 1, 3) %>% tolower())


# acess <- acess_for
# indicador <- "CMA"
# modo <- "walk"
# atividade <- "ST"

# Fazer mapas para cada uma das atividades, fazendo o facet_wrap pelo threshold --------------------
fazer_mapa_acess_sigla_muni <- function(acess, indicador, modo, atividade, salvar = FALSE, 
                                        nrow = 1) {
  
  # Extrair string da sigla_muni
  sigla_muni <- str_sub(deparse(substitute(acess)), -3, -1)
  
  # Filtra linhas hm
  linhas_hm_sigla_muni <- linhas_hm %>% filter(city %in% sigla_muni)
  
  # Filtrar o modo
  acess_modo <- acess %>% filter(mode == modo)
  
  # Filtrar indicador e fazer mapa
  if (indicador == "TMI") {
    
    modo_title <- ifelse(modo == "walk", "caminhada", 
                         ifelse(modo == "bike", "bicicleta", 
                                ifelse(modo == "transit", "transporte público")))
    
    # title <- sprintf("Tempo mínimo até a oportunidade mais próxima por %s", modo_title)
    
    title <- bquote("Tempo mínimo até a oportunidade mais próxima por"~bold(.(modo_title)))
    
    
    
    fim <- acess_modo %>%
      # Filtrar indicador
      select(matches(indicador)) %>%
      gather(atividade, acess_abs, -geometry) %>%
      mutate(acess_discrete = ifelse(acess_abs >= 30, 30, acess_abs)) %>%
      mutate(atividade1 = case_when(
        atividade == "TMI_EF" ~ "Educação Fundamental",
        atividade == "TMI_EI" ~ "Educação Infantil",
        atividade == "TMI_EM" ~ "Educação Média",
        atividade == "TMI_ST" ~ "Saúde"
      )
      ) %>%
      mutate(atividade1 = factor(atividade1, 
                                 levels = c("Saúde", "Educação Infantil", "Educação Fundamental", "Educação Média")))
    
    mapa <- 
      ggplot(data = fim)+
      # annotation_map_tile(zoomin = -1) +
      geom_sf(aes(fill = acess_discrete), color = NA)+
      geom_sf(data = linhas_hm_sigla_muni, size=0.7, color="#2340e7")+
      facet_wrap(~atividade1, nrow = nrow)+
      viridis::scale_fill_viridis(option = "B", 
                                  direction = -1, 
                                  breaks = c(0, 10, 20, 30), labels = c("0", "10", "20", ">30")) +
      theme_for_TMI()+
      labs(fill = "Tempo até a oportunidade\n mais próxima",
           title = title)
    # for_tile
    # guides(fill = guide_legend(title.position = 'top'))
    
  } else if (indicador == "CMA") {
    
    modo_title <- ifelse(modo == "walk", "caminhada", 
                         ifelse(modo == "bike", "bicicleta", 
                                ifelse(modo == "transit", "transporte público")))
    
    atividade_title <- ifelse(atividade == "ST", "saúde",
                              ifelse(atividade == "TT", "trabalho",
                                     ifelse(atividade == "EI", "educação infantil",
                                            ifelse(atividade == "EM", "educação média",
                                                   ifelse(atividade == "EF", "educação fundamental")))))
    
    
    # title <- sprintf("Indicador cumulativo para oportunidades de %s\n %s", atividade_title, modo_title)
    
    title <- bquote("Indicador cumulativo para oportunidades de"~bold(.(atividade_title))~"por"~bold(.(modo_title)))
    
    if (modo %in% c("walk", "bike")) {
      
      fim <- acess_modo %>%
        # Filtrar indicador
        select(matches(indicador)) %>%
        # Filtrar atividade
        select(matches(atividade)) %>%
        # Wide to long
        gather(threshold, acess_abs, -geometry) %>%
        mutate(threshold1 = as.integer(str_extract(threshold, "\\d+$"))) %>%
        # Pegar somente esses threshoold
        filter(threshold1 %in% c(15, 30, 60)) %>%
        mutate(threshold_name = paste0(str_extract(threshold, "\\d+$"), " minutos")) %>%
        mutate(threshold_name = forcats::fct_reorder(factor(threshold_name), threshold1))
      
      mapa <-
        ggplot(data = fim)+
        geom_sf(aes(fill = acess_abs), color = NA)+
        geom_sf(data = linhas_hm_sigla_muni, size=0.7, color="#2340e7")+
        facet_wrap(~threshold_name, nrow = nrow) +
        viridis::scale_fill_viridis(option = "B") +
        # ggthemes::theme_map() + 
        theme_for_CMA()+
        labs(fill = "Quantidade de oportunidades\n acessíveis",
             title = title)
      # guides(fill = guide_legend(title.position = 'top'))
      
    } else if (modo == "transit") {
      
      fim <- acess_modo %>%
        # Filtrar indicador
        select(matches(indicador)) %>%
        # Filtrar atividade
        select(matches(atividade)) %>%
        # Wide to long
        gather(threshold, acess_abs, -geometry) %>%
        mutate(threshold1 = as.integer(str_extract(threshold, "\\d+$"))) %>%
        # Pegar somente esses threshoold
        filter(threshold1 %in% c(30, 60, 90)) %>%
        mutate(threshold_name = paste0(str_extract(threshold, "\\d+$"), " minutos")) %>%
        mutate(threshold_name = forcats::fct_reorder(factor(threshold_name), threshold1))
      
      mapa <-
        ggplot(data = fim)+
        geom_sf(aes(fill = acess_abs), color = NA)+
        geom_sf(data = linhas_hm_sigla_muni, size=0.7, color="#2340e7")+
        facet_wrap(~threshold_name, nrow = nrow) +
        viridis::scale_fill_viridis(option = "B") +
        # ggthemes::theme_map() + 
        theme_for_CMA()+
        labs(fill = "Quantidade de oportunidades\n acessíveis",
             title = title)
    }
    
  }
  
  
  if (salvar == TRUE) {
    
    
    path_out <- sprintf("figure/acess/%s_%s_P_%s_%s.png", sigla_muni, indicador, modo, atividade)
    
    ggsave(plot = mapa, filename = path_out, dpi = 300, units = "cm", height = 9, width = 16)
    
  } else if (salvar == FALSE) {mapa}
  
  
}

# CRIAR TEMAS PARA CADA UMA DAS sigla_muniS ------------------------------------------------------------
theme_for_CMA <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4)
      
      
    )
}

theme_for_TMI <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4)
      # legend.key.width=unit(0.5,"cm")
      
    )
}

# # Fazer mapas para cada uma das atividades, fazendo comparacao entre sigla_munis -----------------------
# 
# fazer_mapa_acess_comparar <- function(variables) {
#   
# }


```


## Fortaleza

```{r acess acumulativa for}


# Aplicar
# calcular_acess("for")

# Abrir
acess_for <- read_rds("../data/output_access/acess_for.rds")


# for_tile <- annotation_map_tile(data = acess_for$transit)
# Fazer mapa para fortaleza ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_sigla_muni(acess_for, indicador = "TMI", modo = "transit", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "TMI", modo = "walk", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "TMI", modo = "bike", atividade = "ST",
                            salvar = TRUE)

# Para indicador CMA
# transit
fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "transit", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "transit", atividade = "TT",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "transit", atividade = "EI",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "transit", atividade = "EF",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "transit", atividade = "EM",
                            salvar = TRUE)

# walk
fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "walk", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "walk", atividade = "TT",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "walk", atividade = "EI",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "walk", atividade = "EF",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "walk", atividade = "EM",
                            salvar = TRUE)

# bike
fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "bike", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "bike", atividade = "TT",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "bike", atividade = "EI",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "bike", atividade = "EF",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_for, indicador = "CMA", modo = "bike", atividade = "EM",
                            salvar = TRUE)




```

## Belo Horizonte

Para Belo Horizonte:
  
  ```{r acess acumulativa bel}

# Aplicar
# calcular_acess("bel")

# Abrir
acess_bel <- read_rds("../data/output_access/acess_bel.rds")

# Fazer mapa para bel ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_sigla_muni(acess_bel, indicador = "TMI", modo = "transit", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_bel, indicador = "TMI", modo = "walk", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_bel, indicador = "TMI", modo = "bike", atividade = "ST",
                            salvar = TRUE)

# Para indicador CMA
fazer_mapa_acess_sigla_muni(acess_bel, indicador = "CMA", modo = "transit", atividade = "ST",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_bel, indicador = "CMA", modo = "transit", atividade = "TT",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_bel, indicador = "CMA", modo = "transit", atividade = "EI",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_bel, indicador = "CMA", modo = "transit", atividade = "EF",
                            salvar = TRUE)

fazer_mapa_acess_sigla_muni(acess_bel, indicador = "CMA", modo = "transit", atividade = "EM",
                            salvar = TRUE)


```

## Rio de Janeiro

Para o Rio de Janeiro:
  
  ```{r acess acumulativa rio}

# Aplicar
# calcular_acess("rio")

# Abrir
acess_rio <- read_rds("../data/output_access/acess_rio.rds")

# Fazer mapa para rio ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_sigla_muni(acess_rio, indicador = "TMI", modo = "transit", atividade = "ST",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_rio, indicador = "TMI", modo = "walk", atividade = "ST",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_rio, indicador = "TMI", modo = "bike", atividade = "ST",
                            salvar = TRUE, nrow = 2)

# Para indicador CMA
fazer_mapa_acess_sigla_muni(acess_rio, indicador = "CMA", modo = "transit", atividade = "ST",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_rio, indicador = "CMA", modo = "transit", atividade = "TT",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_rio, indicador = "CMA", modo = "transit", atividade = "EI",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_rio, indicador = "CMA", modo = "transit", atividade = "EF",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_rio, indicador = "CMA", modo = "transit", atividade = "EM",
                            salvar = TRUE, nrow = 2)

```

## Curitiba

Para o Curitiba:
  
  ```{r acess acumulativa cur}

# Aplicar
# calcular_acess("cur")

# Abrir
acess_cur <- read_rds("../data/output_access/acess_cur.rds")

# Fazer mapa para cur ------------------------------------------------------------------------
# Para indicador TMI
fazer_mapa_acess_sigla_muni(acess_cur, indicador = "TMI", modo = "transit", atividade = "ST",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_cur, indicador = "TMI", modo = "walk", atividade = "ST",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_cur, indicador = "TMI", modo = "bike", atividade = "ST",
                            salvar = TRUE, nrow = 2)

# Para indicador CMA
fazer_mapa_acess_sigla_muni(acess_cur, indicador = "CMA", modo = "transit", atividade = "ST",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_cur, indicador = "CMA", modo = "transit", atividade = "TT",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_cur, indicador = "CMA", modo = "transit", atividade = "EI",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_cur, indicador = "CMA", modo = "transit", atividade = "EF",
                            salvar = TRUE, nrow = 2)

fazer_mapa_acess_sigla_muni(acess_cur, indicador = "CMA", modo = "transit", atividade = "EM",
                            salvar = TRUE, nrow = 2)

```

