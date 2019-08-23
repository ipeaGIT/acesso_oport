# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.2.2 Agrega informacoes demograficas e uso do solo nos hexagonos

# carregar bibliotecas
source('./R/fun/setup.R')




# ABRIR ARQUIVOS COM AS OPORTUNIDADES -------------------------------------

# Saude --------------------------------------
cnes <- readr::read_rds("../data-raw/hospitais/cnes_geocoded.rds") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


# Escolas  -------------------------------------
# abrir censo escolar geo
escolas <- fread("../data/censo_escolar/censo_escolar_2015.csv") %>%
  # Deletar escolas q nao foram localizadas
  dplyr::filter(!is.na(lat)) %>%
  # Selecionar variaveis
  dplyr::select(cod_escola, uf, municipio, cod_mun = CO_MUNICIPIO, rede, mat_infantil, mat_fundamental, mat_medio, lon, lat) #%>%
  # # Transformar para formato longo
  # tidyr::gather(tipo, mat_n, mat_infantil:mat_medio)


# Empregos ----------------------------------------------------------
# Abrir rais geo
empregos <- readr::read_rds("../data/rais/rais_2017_corrigido_cidades_selecionadas2019.rds") # para 2017


# FUNCAO PARA REALIZAR EM CADA MUNICIPIO ----------------------------------

# sigla_muni <- "for"
# Funcao para agregar dados de uso do solo na grade de hexagonos
agrupar_variaveis <- function(sigla_muni) {
  
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
    # muni_res <- '08'
    
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
      hex_pop[, .(po_in_decile = sum(pop_total, na.rm=T)), by = decil]
      hex_pop[, .(po_in_quintil = sum(pop_total, na.rm=T)), by = quintil]
      
      
      
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
      setDT(hex_saude)[, indice := ifelse(is.na(code_cnes), 0, 1) ]
      hex_saude <- setDT(hex_saude)[, .(saude_total = sum(indice)), by = id_hex ]
      

              
        
    # agrupar educacao
      # join espacial 
        hex_escolas <- hex_muni %>% st_join(escolas_filtrado) %>% setDT()
      
      # Dummy para nivel de educacao em cada hexagono
        hex_escolas[, edu_infantil := ifelse( mat_infantil > 0, 1, 0) ]
        hex_escolas[, edu_fundamental := ifelse( mat_fundamental > 0, 1, 0) ]      
        hex_escolas[, edu_medio := ifelse( mat_medio > 0, 1, 0) ]      
        hex_escolas[, edu_total := ifelse( is.na(cod_escola), 0, 1) ]      
        
      # Summarize
        hex_escolas <- hex_escolas[, .(edu_total = sum(edu_total),
                                      edu_infantil = sum(edu_infantil),
                                       edu_fundamental=sum(edu_fundamental),
                                       edu_medio=sum(edu_medio)), by = id_hex ]
        
      

# Junta todos os dados agrupados por hexagonos
hex_muni_fim <- left_join(hex_muni, hex_pop) %>%
  left_join(hex_rais) %>%
  left_join(hex_saude) %>%
  left_join(hex_escolas)

# substitui NAs por zeros
hex_muni_fim[is.na(hex_muni_fim)] <- 0



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
