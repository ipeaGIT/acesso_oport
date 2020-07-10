#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.4 Extrai grade estatistica de cada municipio

# carregar bibliotecas
source('./R/fun/setup.R')



### Funcao

# sigla <- "for"

criar_grade_muni_all <- function(ano, munis = "all") {
  
  
  # Select the corerspondent munis_df
  munis_df <- get(sprintf("munis_df_%s", ano))
  
  # Criar pasta para salvar arquivos
  dir.create(sprintf("../data-raw/grade_municipio/%s", ano))
  
  # funcao para criar grade por municipio
  criar_grade_muni <- function(sigla){
    
    message(paste0('Rodando cidade ', sigla,"\n"))
    
    # codigo do estado do municipio
    cod_estado <- subset(munis_df, abrev_muni==sigla)$abrev_estado %>% as.character()
    
    # Leitura das grades estatisticas dos estados
    grade <- read_statistical_grid(code_grid = cod_estado, year = 2010)
    # Leitura do municipio
    muni <- readr::read_rds( sprintf("../data-raw/municipios/%s/municipio_%s_%s.rds", ano, sigla, ano) )
    
    # mesma projecao geografica
    grade <- grade %>% st_transform(crs = 4326)
    muni <- muni   %>%  st_transform(crs = 4326)
    
    # Intersecao
    grade_muni <- st_join(grade, muni)
    # Tirar grades so do municipio
    grade_muni <- setDT(grade_muni)[!is.na(code_muni)]
    # Transformar para sf
    grade_muni <- st_sf(grade_muni)
    
    # limpa memoria
    rm(grade, muni)
    gc(reset=T)
    
    # salvar no disco
    write_rds(grade_muni, sprintf("../data-raw/grade_municipio/%s/grade_%s_%s.rds", ano, sigla, ano))
  
    }
  
  
  # Aplicar funcao ----------------------
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  
  # Parallel processing using future.apply
  future::plan(future::multiprocess)
  invisible(future.apply::future_lapply(X = x, FUN=criar_grade_muni, future.packages=c('sf', 'dplyr')))
  
  
}


# aplicar funcao ------------------------------------------------------------------------------
criar_grade_muni_all(ano = 2019)

