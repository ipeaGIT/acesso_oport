# carregar bibliotecas
source('./R/fun/setup.R')




agrupar_variaveis_hex <- function(ano) {
  
  # ano <- 2017; sigla_muni <- "for"
  # ano <- 2019; sigla_muni <- "goi"
  
  # 1) Abrir arquivos com as oportunidades -------------------------------------
  
  # 1.1) Saude
  cnes_data <- readr::read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality.rds", ano, ano)) 
  
  # remove lat lon missing
  cnes_data <- cnes_data[!is.na(lat),] 
  
  # select columns
  cnes_data <- cnes_data %>% select(cnes, code_muni = ibge,
                                    health_low, health_med, health_high,
                                    lon, lat)
  
  cnes_data <- cnes_data %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  
  # 1.2) Escolas
  escolas <- read_rds(sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality.rds", ano, ano))
  
  # remove lat lon missing
  escolas <- escolas[!is.na(lat),] 
  
  # select columns
  escolas <- escolas[, .(co_entidade, code_muni,
                         mat_infantil, mat_fundamental, mat_medio,
                         lon, lat)]
  
  
  # 1.3) Empregos
  empregos <- readr::read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality.rds", ano, ano))
  
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
    
    # Qual o codigo do municipio em questao?
    cod_mun_ok <- munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
      unlist()
    
    # Filtrar somente as atividades referentes a cada municipio e transforma em sf
    # para rais 2017
    empregos_filtrado <- empregos[codemun %in% substr(cod_mun_ok, 1, 6)] %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # escolas
    escolas_filtrado <- setDT(escolas)[code_muni %in% cod_mun_ok] %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # saude
    cnes_filtrado <- setDT(cnes_data)[code_muni %in% substr(cod_mun_ok, 1, 6)] %>% st_sf()
    
    
    
    
    muni_res <- '09'
    
    # endereco do hexagono na resolucao
    hexf <- sprintf("../../data/acesso_oport/hex_municipio/%s/hex_%s_%s_%s.rds", ano, sigla_muni, muni_res, ano)
    
    # Ler arquivo de hexagono  
    hex_muni <- readr::read_rds(hexf)
    
    
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
    hex_muni_fim <- hex_muni %>%
      left_join(hex_rais) %>%
      left_join(hex_saude) %>%
      left_join(hex_escolas)
    
    # substitui NAs por zeros
    hex_muni_fim[is.na(hex_muni_fim)] <- 0
    
    # adiciona sigla do municipio
    hex_muni_fim$sigla_muni <- sigla_muni
    
    # adicionar ano
    hex_muni_fim <- hex_muni_fim %>% mutate(ano = ano)
    
    
    # Salva grade de hexagonos com todas informacoes de uso do soloe
    dir_output <- sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/hex_agregado_%s_%s_%s.rds", ano, sigla_muni, muni_res, ano)
    readr::write_rds(hex_muni_fim, dir_output)
  }
  
  
  walk(munis_list$munis_df$abrev_muni, agrupar_variaveis)
  
}

agrupar_variaveis_hex(2017)
agrupar_variaveis_hex(2018)
agrupar_variaveis_hex(2019)

# gerar mapas

# sigla_muni <- "bho"
# ano <- 2017
# sigla_muni <- "sgo"; ano <- 2018
# sigla_muni <- "goi"; ano <- 2019

maps_check_geocode <- function(sigla_muni, ano) {
  
  mapviewOptions(platform = "leafgl")
  
  dir.create(sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/maps/", ano))
  dir.create(sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/maps/%s", ano, sigla_muni))
  
  hex_teste <- read_rds(sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/hex_agregado_%s_09_%s.rds", ano, sigla_muni, ano))
  
  # hex_teste %>% filter(id_hex %in% "89a8a2a641bffff") %>% View()
  
  hex_teste <- hex_teste %>%
    filter(empregos_total >= 10)
  
  map_empregos <- mapview(hex_teste, zcol = "empregos_total")
  map_saude <- mapview(hex_teste, zcol = "saude_total")
  map_educacao <- mapview(hex_teste, zcol = "edu_total")
  
  # hex_teste %>%
    # filter(id_hex == "89a8c0014bbffff")
  
  # hist(setDT(hex_teste)[empregos_total != 0]$empregos_total)
  # summary(setDT(hex_teste)[empregos_total != 0]$empregos_total)
  # boxplot(setDT(hex_teste)[empregos_total != 0]$empregos_total)
  # a <- quantile(hex_teste %>% filter(empregos_total != 0) %>% pull(empregos_total), 0.99)
  # hex_teste <- hex_teste %>%
  #   mutate(empregos_total_perc = ifelse(empregos_total >= a, a, empregos_total))
  # 
  # mapview(hex_teste, zcol = "empregos_total_perc")
  # 
  # ggplot()+
  #   geom_sf(data = hex_teste, aes(fill = empregos_total_perc))
  
  
  mapview::mapshot(map_empregos, debug=T, selfcontained = FALSE,
                   url = sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/maps/%s/distribuicao_us_%s_%s_empregos.html", ano, sigla_muni, ano, sigla_muni))
  
  mapview::mapshot(map_saude, debug=T, remove_controls=NULL,  selfcontained = FALSE,
                   url = sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/maps/%s/distribuicao_us_%s_%s_saude.html", ano, sigla_muni, ano, sigla_muni))
  
  mapview::mapshot(map_educacao, debug=T, remove_controls=NULL,  selfcontained = FALSE,
                   url = sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/maps/%s/sdistribuicao_us_%s_%s_educacao.html", ano, sigla_muni, ano, sigla_muni))
  
  
  
}

walk(munis_list$munis_df$abrev_muni, maps_check_geocode, ano = 2017)
walk(munis_list$munis_df$abrev_muni, maps_check_geocode, ano = 2018)
walk(munis_list$munis_df$abrev_muni, maps_check_geocode, ano = 2019)



# calculate job distribution
address_2017 <-   sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/hex_agregado_%s_%s_%s.rds", 
                          c(2017), 
                          munis_list$munis_df$abrev_muni, 
                          "09", 
                          c(2017))
address_2018 <-   sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/hex_agregado_%s_%s_%s.rds", 
                          c(2018), 
                          munis_list$munis_df$abrev_muni, 
                          "09", 
                          c(2018))
address_2019 <-   sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/hex_agregado_%s_%s_%s.rds", 
                          c(2019), 
                          munis_list$munis_df$abrev_muni, 
                          "09", 
                          c(2019))



jobs_total <- lapply(c(address_2017, address_2018,address_2019),
                     read_rds
) %>%
  rbindlist()







