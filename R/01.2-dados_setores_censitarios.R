#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.2 Seleciona e agrega microdados dos setores censitarios


# carregar bibliotecas
source('./R/fun/setup.R')


# # Cria data.frame com municipios do projeto
# munis_df <- data.frame( code_muni= c(2304400, 3550308, 3304557, 4106902, 4314902, 3106200, 2211001),
#                            abrev_muni=c('for', 'sao', 'rio', 'cur', 'por', 'bel', 'ter'),
#                            name_muni=c('Fortaleza', 'Sao Paulo', 'Rio de Janeiro', 'Curitiba', 'Porto Alegre', 'Belo Horizonte', 'Teresina'))


### 1. Carrega micro dados dos setores censitarios --------------------------------------------------

## Leitura dos dados
setores1 <- data.table::fread("../data-raw/setores_censitarios/dados_censo2010A.csv", select= c('Cod_UF', 'Cod_municipio', 'Cod_setor', 'DomRend_V003', 'Dom2_V002', 'Pess3_V002', 'Pess3_V003', 'Pess3_V004', 'Pess3_V005', 'Pess3_V006'))
names(setores1)

# Raca/cor
# Pess3_V002 # Pessoas Residentes e cor ou raça - branca
# Pess3_V003 # Pessoas Residentes e cor ou raça - preta
# Pess3_V004 # Pessoas Residentes e cor ou raça - amarela
# Pess3_V005 # Pessoas Residentes e cor ou raça - parda
# Pess3_V006 # Pessoas Residentes e cor ou raça - indígena 


# filtra apenas municipio do projeto
setores1 <- setores1[Cod_municipio %in% munis_df$code_muni,]


## Renomeia variaveis
# Renda 6.19 - variavel escolhida: V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
setores_renda <-  setores1 %>% 
  dplyr::select(cod_uf = Cod_UF, cod_muni = Cod_municipio, cod_setor = Cod_setor, renda_total = DomRend_V003, moradores_total = Dom2_V002, cor_branca=Pess3_V002, cor_preta=Pess3_V003, cor_amarela=Pess3_V004, cor_parda=Pess3_V005, cor_indigena=Pess3_V006)




# Criar variavel de renda domicilias per capita de cada setor censitario
setDT(setores_renda)[, renda_per_capta := renda_total / moradores_total]
setores_renda[, cod_setor := as.character(cod_setor)]




### 2. Merge dos dados de renda com shapes dos setores censitarios --------------------------------------------------

# funcao para fazer merge dos dados e salve arquivos na pata 'data'
merge_renda_setores <- function(sigla){
  
  #  sigla <- "for"
  
  # status message
  message('Woking on city ', sigla, '\n')
  
  # codigo do municipios
  code_muni <- subset(munis_df, abrev_muni==sigla )$code_muni
  
  # subset dados dos setores
  dados <- subset(setores_renda, cod_muni == code_muni)
  
  # leitura do shape dos setores
  sf <- readr::read_rds( paste0("../data-raw/setores_censitarios/", sigla,"/setores_", sigla,".rds") )
  
  # merge
  sf2 <- dplyr::left_join(sf, dados, c('code_tract'='cod_setor'))
  
  # salvar
  readr::write_rds(sf2,  paste0("../data/setores_agregados/setores_agregados_", sigla,".rds"))
}


# aplicar funcao
purrr::walk(munis_df$abrev_muni, merge_renda_setores)
