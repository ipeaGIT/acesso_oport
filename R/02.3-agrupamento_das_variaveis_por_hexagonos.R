# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.2.2 Agrega informacoes demograficas e uso do solo nos hexagonos

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
                cod_mun = CO_MUNICIPIO, mat_infantil, mat_fundamental, mat_medio, lon, lat)
  # tidyr::gather(tipo, mat_n, mat_infantil:mat_medio)




# Empregos ----------------------------------------------------------
# Abrir rais geo
empregos <- readr::read_rds("../data/rais/rais_2017_corrigido_cidades_selecionadas2019.rds") # para 2017


# FUNCAO PARA REALIZAR EM CADA MUNICIPIO ----------------------------------

# Funcao para agregar dados de uso do solo na grade de hexagonos
agrupar_variaveis <- function(sigla_muni) { 
  
  # sigla_muni <- "for"
  
  # status message
  message('Woking on city ', sigla_muni, '\n')
  
  # resolucoes disponiveis
  res <- str_extract(dir("../data/hex_municipio/", pattern = sigla_muni), "\\d+")
  
  # Pegar endereco das grades e hexagonos em todas resolucoes
  grade_file <- paste0("../data/grade_municipio_com_renda_cor/grade_renda_cor_", sigla_muni, ".rds")
  
  # Gerar centroide da grade grade de cada municipio
  centroide_pop <- readr::read_rds(grade_file) %>%
    dplyr::select(id_grade, pop_total, renda,  cor_branca, cor_amarela, cor_indigena, cor_negra) %>%
    st_centroid()
  
  # Qual o codigo do municipio em questao?
  cod_mun_ok <- munis_df[abrev_muni == sigla_muni]$code_muni
  
# Filtrar somente as atividades referentes a cada municipio e transforma em sf
  # para rais 2017
  empregos_filtrado <- empregos[codemun == substr(cod_mun_ok, 1, 6)] %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # escolas
  escolas_filtrado <- setDT(escolas)[cod_mun == cod_mun_ok] %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # saude
  cnes_filtrado <- setDT(cnes)[code_muni == substr(cod_mun_ok, 1, 6)] %>% st_sf()
  
  
  
# FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES ------------------------------
  
  por_resolucao <- function(muni_res) {
    # muni_res <- '09'
    
    # endereco do hexagono na resolucao
    hexf <- paste0("../data/hex_municipio/hex_",sigla_muni,'_',muni_res, '.rds')
    
    # Ler arquivo de hexagono  
    hex_muni <- readr::read_rds(hexf)
    
    
# Agrupar populacao, cor e renda
    # join espacial 
    hex_pop <- hex_muni %>% st_join(centroide_pop)
    
    # Summarize
    hex_pop <- setDT(hex_pop)[, .(cor_branca = sum(round(cor_branca,0), na.rm = TRUE),
                                  cor_amarela = sum(round(cor_amarela,0), na.rm = TRUE),
                                  cor_indigena = sum(round(cor_indigena,0), na.rm = TRUE),
                                  cor_negra = sum(round(cor_negra,0), na.rm = TRUE),
                                  pop_total = sum(round(pop_total,0), na.rm = TRUE),
                                  renda_total = sum(renda, na.rm = TRUE)), by = id_hex ]
    
    # Calcular quintil e decil de renda
      # calcula renda per capta de cada hexagono
      hex_pop[, renda_capta := renda_total / pop_total]
      
      # calcular quintis ponderados pela populacao
      deciles  <- Hmisc::wtd.quantile(hex_pop$renda_capta, weights=hex_pop$pop_total, 
                                        probs=c( seq(0 , 1 , 0.1) ), 
                                        type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                        normwt=FALSE, na.rm=T)
      
      quintiles  <- Hmisc::wtd.quantile(hex_pop$renda_capta, weights=hex_pop$pop_total, 
                                      probs=c( seq(0 , 1 , 0.2) ), 
                                      type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                      normwt=FALSE, na.rm=T)
    
    # classificar cada hexagono em cada quintil de renda
      hex_pop[, quintil := findInterval(renda_capta , quintiles[ -length(quintiles) ] ) ]
      hex_pop[, decil := findInterval(renda_capta , deciles[ -length(deciles) ] ) ]
      
      # check if pop size in each decile are roughly equal
      hex_pop[, .(po_in_quintil = sum(pop_total, na.rm=T)), by = quintil]
      hex_pop[, .(po_in_decile = sum(pop_total, na.rm=T)), by = decil]
      
      # # remove NAs
      # hex_pop <- hex_pop[ !is.na(decil)]
      # hex_pop <- hex_pop[ !is.na(quintil)]
      
# Agrupar empregos
    # join espacial 
    hex_rais <- hex_muni %>% st_join(empregos_filtrado)
    
    # Summarize
    hex_rais <- setDT(hex_rais)[, .(empregos_baixa = sum(baixo, na.rm = TRUE),
                                    empregos_media = sum(medio, na.rm = TRUE),
                                    empregos_alta = sum(alto, na.rm = TRUE),
                                    empregos_total = sum(alto, medio, baixo, na.rm = TRUE)), by = id_hex ]
    
  
    # setDT(hex_muni)[, empregos_total := sum(empregos_alta, empregos_media, empregos_baixa), by=id_hex]
    
    
  # agrupar saude
    # join espacial 
      cnes_filtrado <- sf::st_transform(cnes_filtrado, sf::st_crs(hex_muni)) # mesma projecao geografica
      hex_saude <- hex_muni %>% st_join(cnes_filtrado)
      

      # Summarize
      hex_saude <- setDT(hex_saude)[, .(saude_total = sum(health_low, health_med, health_high, na.rm=T),
                                 saude_baixa = sum(health_low, na.rm=T),
                                 saude_media=sum(health_med, na.rm=T),
                                 saude_alta=sum(health_high, na.rm=T)),
                             by = id_hex ]

            

              
        
    # agrupar educacao
      # join espacial 
        hex_escolas <- hex_muni %>% st_join(escolas_filtrado) %>% setDT()
      
      # # Dummy para nivel de educacao em cada hexagono
      #   hex_escolas[, edu_infantil := ifelse( mat_infantil > 0, 1, 0) ]
      #   hex_escolas[, edu_fundamental := ifelse( mat_fundamental > 0, 1, 0) ]      
      #   hex_escolas[, edu_medio := ifelse( mat_medio > 0, 1, 0) ]      
        hex_escolas[, edu_total := ifelse( is.na(cod_escola), 0, 1) ]
        
      # Summarize
        hex_escolas <- hex_escolas[, .(edu_total = sum(edu_total, na.rm = T),
                                      edu_infantil = sum(mat_infantil, na.rm = T),
                                       edu_fundamental=sum(mat_fundamental, na.rm = T),
                                       edu_medio=sum(mat_medio, na.rm = T)), by = id_hex ]
        
      

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
    dir_output <- sprintf("../data/hex_agregados/hex_agregado_%s_%s.rds", sigla_muni, muni_res)
    readr::write_rds(hex_muni_fim, dir_output)
  }
  
  # Aplicar funcao para cada resolucao
  walk(res, por_resolucao)
}


# Aplica funcao para cada municipio

# Parallel processing using future.apply
future::plan(future::multiprocess)
#options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
future.apply::future_lapply(X =munis_df$abrev_muni, FUN=agrupar_variaveis, future.packages=c('sf', 'dplyr', 'data.table', 'Hmisc'))





# Checagem de resultados -------------------------------------

# Fun para criar mapas interativos (html) de distribuicao espacial de uso do solo
salva_mapas <- function(sigla_muni, var) {
  
  # sigla_muni <- "bsb"; ano <- 2019
  
  # path in da acessibilidade
  path_in <- sprintf("../data/output_access/acess_%s_%s.rds", sigla_muni, ano)
  
  
  # trazer dados de uso do solo
  us <- readr::read_rds(sprintf("../data/hex_agregados/hex_agregado_%s_09.rds", sigla_muni))
  
  # saude
  map_saude <- mapview(subset(us, saude_total>0), zcol = "saude_total")
  
  # mapshot(map_saude, file = sprintf("figures/teste_distribuicao_us/us_%s_saude.html", sigla_muni))
  
  # empregos
  map_empregos <- mapview(subset(us, empregos_total>0), zcol = "empregos_total")
  
  #   mapshot(map_empregos, file = sprintf("figures/teste_distribuicao_us/us_%s_empregos.html", sigla_muni))
  
  # educacao
  map_edu <- mapview(subset(us, edu_total>0), zcol = "edu_total")
  
  # mapshot(map_edu, file = sprintf("figures/teste_distribuicao_us/us_%s_educacao.html", sigla_muni))
  
  
  single_map <- mapview(subset(us, saude_total>0), zcol = "saude_total") +  
                mapview(subset(us, empregos_total>0), zcol = "empregos_total") + 
                mapview(subset(us, edu_total>0), zcol = "edu_total")
  
  
  # save
  mapview::mapshot(single_map, remove_controls=NULL, url = sprintf("figures/teste_distribuicao_us/us_%s_all.html", sigla_muni))
  }  

# Aplica funcao para cada municipio

# Parallel processing using future.apply
future::plan(future::multiprocess)
#options(future.globals.maxSize= Inf) # permitir processamento de arquivo grande
future.apply::future_lapply(X = munis_df$abrev_muni, FUN=salva_mapas, future.packages=c('sf', 'dplyr', 'mapview'))


