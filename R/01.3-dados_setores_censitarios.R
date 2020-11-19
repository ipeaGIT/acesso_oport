#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.2 Seleciona e agrega microdados dos setores censitarios


# carregar bibliotecas
source('./R/fun/setup.R')

### 1. Carrega micro dados dos setores censitarios --------------------------------------------------

# vars to select
vars <- c('Cod_UF', 'Cod_municipio', 'Cod_setor', 
          'DomRend_V003', 'Dom2_V002', 
          'Pess3_V002', 'Pess3_V003', 'Pess3_V004', 
          'Pess3_V005', 'Pess3_V006',
          'Pess3_V007', 'Pess3_V008','Pess3_V009','Pess3_V010','Pess3_V011','Pess3_V012','Pess3_V013', 
          'Pess3_V014', 'Pess3_V015','Pess3_V016','Pess3_V017','Pess3_V018','Pess3_V019','Pess3_V020', 
          'Pess3_V021', 'Pess3_V022','Pess3_V023','Pess3_V024','Pess3_V025','Pess3_V026', 
          'Pess3_V037', 'Pess3_V038','Pess3_V039','Pess3_V040','Pess3_V041','Pess3_V042','Pess3_V043', 
          'Pess3_V044', 'Pess3_V045','Pess3_V046','Pess3_V047','Pess3_V048','Pess3_V049','Pess3_V050', 
          'Pess3_V051', 'Pess3_V052','Pess3_V053','Pess3_V054','Pess3_V055','Pess3_V056','Pess3_V057', 
          'Pess3_V058', 'Pess3_V059','Pess3_V060','Pess3_V061','Pess3_V062','Pess3_V063','Pess3_V064', 
          'Pess3_V065', 'Pess3_V066','Pess3_V067','Pess3_V068','Pess3_V069','Pess3_V070','Pess3_V071', 
          'Pess3_V072', 'Pess3_V073','Pess3_V074','Pess3_V075','Pess3_V076','Pess3_V077','Pess3_V078', 
          'Pess3_V079', 'Pess3_V080','Pess3_V081','Pess3_V082','Pess3_V083','Pess3_V084','Pess3_V085', 
          'Pess3_V086')

## Leitura dos dados
setores1 <- data.table::fread("../../data-raw/setores_censitarios/dados_censo2010A.csv", 
                              select= vars)
names(setores1)

# Dom2_V002 # Moradores em domicílios particulares permanentes
# DomRen_V003 # Total do rendimento nominal mensal dos domicílios particulares permanentes 

# Raca/cor
# Pess3_V002 # Pessoas Residentes e cor ou raça - branca
# Pess3_V003 # Pessoas Residentes e cor ou raça - preta
# Pess3_V004 # Pessoas Residentes e cor ou raça - amarela
# Pess3_V005 # Pessoas Residentes e cor ou raça - parda
# Pess3_V006 # Pessoas Residentes e cor ou raça - indígena 
# Idade
# 0 a 9
# Pess3_V007 a Pess3_V016
# 10 a 14
# Pess3_V017 a Pess3_V021
# 15 a 19
# Pess3_V022 a Pess3_V026
# 20 a 29
# Pess3_V037 a Pess3_V046
# 30 a 39
# Pess3_V047 a Pess3_V056
# 40 a 49
# Pess3_V057 a Pess3_V066
# 50 a 59
# Pess3_V067 a Pess3_V076
# 60 a 69
# Pess3_V077 a Pess3_V081
# 70+
# Pess3_V082 a Pess3_V086


### 2. Merge dos dados de renda com shapes dos setores censitarios --------------------------------------------------
merge_renda_setores_all <- function(ano, munis = "all") {
  
  dir.create(sprintf("../../data/acesso_oport/setores_agregados/%s", ano))
  
  # Select the corerspondent munis_df
  munis_df <- munis_df_2019
  # munis_df <- get(sprintf("munis_df_%s", ano))
  
  # filtra apenas municipio do projeto
  setores1 <- setores1[Cod_municipio %in% munis_df$code_muni,]
  
  # convert NA's to 0
  
  
  # agrupar variaveis de idade e soma-las
  setores1 <- setDT(setores1)[, ':='(idade_0a9 = Pess3_V007 + Pess3_V008 + Pess3_V009 + Pess3_V010 + Pess3_V011 + Pess3_V012 + Pess3_V013 + 
                                       Pess3_V014 + Pess3_V015 + Pess3_V016,
                                     idade_10a14 = Pess3_V017 + Pess3_V018 + Pess3_V019 + Pess3_V020 + Pess3_V021,
                                     idade_15a19 = Pess3_V022 + Pess3_V023 + Pess3_V024 + Pess3_V025 + Pess3_V026,
                                     idade_20a29 = Pess3_V037 + Pess3_V038 + Pess3_V039 + Pess3_V040 + Pess3_V041 +
                                       Pess3_V042 + Pess3_V043 + Pess3_V044 + Pess3_V045 + Pess3_V046,
                                     idade_30a39 = Pess3_V047 + Pess3_V048 + Pess3_V049 + Pess3_V050 + Pess3_V051 + 
                                       Pess3_V052 + Pess3_V053 + Pess3_V054 + Pess3_V055 + Pess3_V056,
                                     idade_40a49 = Pess3_V057 + Pess3_V058 + Pess3_V059 + Pess3_V060 + Pess3_V061 + 
                                       Pess3_V062 + Pess3_V063 + Pess3_V064 + Pess3_V065 + Pess3_V066,
                                     idade_50a59 = Pess3_V067 + Pess3_V068 + Pess3_V069 + Pess3_V070 + Pess3_V071 + 
                                       Pess3_V072 + Pess3_V073 + Pess3_V074 + Pess3_V075 + Pess3_V076,
                                     idade_60a69 = Pess3_V077 + Pess3_V078 + Pess3_V079 + Pess3_V080 + Pess3_V081,
                                     idade_70 = Pess3_V082 +  Pess3_V083 +  Pess3_V084 +  Pess3_V085 + Pess3_V086)
                              ]
  
  
  ## Renomeia variaveis
  # Renda 6.19 - variavel escolhida: V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
  setores_renda <-  setores1 %>% 
    dplyr::select(cod_uf = Cod_UF, cod_muni = Cod_municipio, cod_setor = Cod_setor, 
                  renda_total = DomRend_V003, moradores_total = Dom2_V002, 
                  cor_branca=Pess3_V002, cor_preta=Pess3_V003, 
                  cor_amarela=Pess3_V004, cor_parda=Pess3_V005, cor_indigena=Pess3_V006,
                  # age variables
                  matches("idade"))
  
  
  # Criar variavel de renda domicilias per capita de cada setor censitario
  setDT(setores_renda)[, renda_per_capta := renda_total / moradores_total]
  setores_renda[, cod_setor := as.character(cod_setor)]
  
  
  # funcao para fazer merge dos dados e salve arquivos na pasta 'data'
  merge_renda_setores <- function(sigla){
    
    # status message
    message('Woking on city ', sigla, '\n')
    
    # codigo do municipios
    code_muni <- subset(munis_df_2019, abrev_muni==sigla )$code_muni
    
    # subset dados dos setores
    dados <- subset(setores_renda, cod_muni == code_muni)
    
    # leitura do shape dos setores
    sf <- readr::read_rds( sprintf("../../data-raw/setores_censitarios/%s/setores_%s_%s.rds", ano, sigla, ano) )
    
    # merge
    sf2 <- dplyr::left_join(sf, dados, c('code_tract'='cod_setor'))
    
    # salvar
    readr::write_rds(sf2,  sprintf("../../data/acesso_oport/setores_agregados/%s/setores_agregados_%s_%s.rds", ano, sigla, ano))
  }
  
  
  # aplicar funcao -----------------
  if (munis == "all") {
    
    x = munis_df$abrev_muni
    
  } else (x = munis)
  
  purrr::walk(x, merge_renda_setores)
  
}

# Aplicar funcao
merge_renda_setores_all(ano = 2017)
merge_renda_setores_all(ano = 2018)
merge_renda_setores_all(ano = 2019)
