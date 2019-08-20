~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.4 Extrai grade estatistica de cada municipio

# carregar bibliotecas
source('./R/fun/setup.R')


# Criar pasta para salvar arquivos
dir.create(paste0("../data/grade_municipio"))



# Cria data.frame com municipios do projeto
munis_df <- data.frame( code_muni= c(2304400, 3550308, 3304557, 4106902, 4314902, 3106200, 2211001),
                        abrev_muni=c('for', 'sao', 'rio', 'cur', 'por', 'bel', 'ter'),
                        name_muni=c('Fortaleza', 'Sao Paulo', 'Rio de Janeiro', 'Curitiba', 'Porto Alegre', 'Belo Horizonte', 'Teresina'),
                        abrev_state=c('CE', 'SP', 'RJ', 'PR', 'RS', 'MG', 'PI'))



### Funcao

criar_grade_muni <- function(sigla){
  
  message(paste0('Rodando ', sigla,"\n"))
  
  # codigo do estado do municipio
  cod_estado <- subset(munis_df, abrev_muni==sigla)$abrev_state %>% as.character()
  
  # Leitura das grades estatisticas dos estados
  grade <- read_statistical_grid(code_grid = cod_estado, year = 2010)
  # Leitura do municipio
  muni <- readr::read_rds( paste0("../data-raw/municipios/",sigla,"/municipio_", sigla,".rds") )
  
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
  write_rds(grade_muni, paste0("../data/grade_municipio/grade_", tolower(sigla), ".rds"))
}


# Aplicar funcao
#purrr::walk(munis_df$abrev_muni, criar_grade_muni)

pblapply(X = munis_df$abrev_muni, FUN=criar_grade_muni)

