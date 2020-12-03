#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.2 Seleciona e agrega microdados dos setores censitarios


# carregar bibliotecas
source('./R/fun/setup.R')

### 1. Carrega micro dados dos setores censitarios --------------------------------------------------

# # vars to select
# vars <- c('Cod_UF', 'Cod_municipio', 'Cod_setor', 
#           'DomRend_V003', 'Dom2_V002', 
#           'Pess3_V002', 'Pess3_V003', 'Pess3_V004', 
#           'Pess3_V005', 'Pess3_V006')

## Leitura dos dados
setores1 <- data.table::fread("../../data-raw/setores_censitarios/dados_censo2010A.csv")
names(setores1)

setores1 <- setores1 %>% select(Cod_UF, Cod_municipio, Cod_setor, 
                                DomRend_V003, Dom2_V002, 
                                Pess3_V002, Pess3_V003, Pess3_V004, 
                                Pess3_V005, Pess3_V006,
                                # idade
                                Pess12_V025:Pess12_V039, Pess13_V025:Pess13_V039,
                                Pess12_V040:Pess12_V048, Pess13_V040:Pess13_V048,
                                Pess12_V049:Pess12_V052, Pess13_V049:Pess13_V052,
                                Pess12_V053:Pess12_V058, Pess13_V053:Pess13_V058,
                                Pess12_V059:Pess12_V073, Pess13_V059:Pess13_V073,
                                Pess12_V074:Pess12_V103, Pess13_V074:Pess13_V103,
                                Pess12_V104:Pess12_V134, Pess13_V104:Pess13_V134
  
)

# Dom2_V002 # Moradores em domicílios particulares permanentes
# DomRen_V003 # Total do rendimento nominal mensal dos domicílios particulares permanentes 

# Raca/cor
# Pess3_V002 # Pessoas Residentes e cor ou raça - branca
# Pess3_V003 # Pessoas Residentes e cor ou raça - preta
# Pess3_V004 # Pessoas Residentes e cor ou raça - amarela
# Pess3_V005 # Pessoas Residentes e cor ou raça - parda
# Pess3_V006 # Pessoas Residentes e cor ou raça - indígena 

# Idade
# 0 a 5
# Pess12_V025:Pess12_V039
# Pess13_V025:Pess13_V039

# 6 a 14
# Pess12_V040:Pess12_V048
# Pess13_V040:Pess13_V048

# 15 a 18
# Pess12_V049:Pess12_V052
# Pess13_V049:Pess13_V052

# 19 a 24
# Pess12_V053:Pess12_V058
# Pess13_V053:Pess13_V058

# 25 a 39
# Pess12_V059:Pess12_V073
# Pess13_V059:Pess13_V073

# 40 a 69
# Pess12_V074:Pess12_V0103
# Pess13_V074:Pess13_V0103

# 70+
# Pess12_V0104:Pess12_V134
# Pess13_V0104:Pess13_V134

# munis <- "for"

### 2. Merge dos dados de renda com shapes dos setores censitarios --------------------------------------------------
merge_renda_setores_all <- function(ano, munis = "all") {
  
  dir.create(sprintf("../../data/acesso_oport/setores_agregados/%s", ano))
  
  # filtra apenas municipio do projeto
  setores2 <- setores1[Cod_municipio %in% munis_df$code_muni,]
  
  # convert NA's to 0
  
  
  # agrupar variaveis de idade e soma-las
  setores2 <- setores2 %>% mutate(idade_0a5 = rowSums(across(Pess12_V025:Pess13_V039), na.rm = TRUE),
                                     
                                     idade_6a14 = rowSums(across(Pess12_V040:Pess13_V048), na.rm = TRUE),

                                     idade_15a18 = rowSums(across(Pess12_V049:Pess13_V052), na.rm = TRUE),

                                     idade_19a24 = rowSums(across(Pess12_V053:Pess13_V058), na.rm = TRUE),

                                     idade_25a39 = rowSums(across(Pess12_V059:Pess13_V073), na.rm = TRUE),

                                     idade_40a69 = rowSums(across(Pess12_V074:Pess13_V103), na.rm = TRUE),

                                     idade_70 = rowSums(across(Pess12_V104:Pess13_V134), na.rm = TRUE)
                                     
                                     )
  
  
  ## Renomeia variaveis
  # Renda 6.19 - variavel escolhida: V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
  setores_renda <-  setores2 %>% 
    dplyr::select(cod_uf = Cod_UF, cod_muni = Cod_municipio, cod_setor = Cod_setor, 
                  renda_total = DomRend_V003, moradores_total = Dom2_V002, 
                  cor_branca=Pess3_V002, cor_preta=Pess3_V003, 
                  cor_amarela=Pess3_V004, cor_parda=Pess3_V005, cor_indigena=Pess3_V006,
                  # age variables
                  matches("idade"))
  
  # fazer a correcao das idades e da populacao por cor/raca
  setores_renda_corrigido <- setores_renda %>%
    # corrigir idade
    mutate(age_total = rowSums(across(idade_0a5:idade_70), na.rm = TRUE)) %>%
    mutate_at(vars(matches("idade")), ~ .x / age_total) %>%
    mutate_at(vars(matches("idade")), ~ round(.x * moradores_total)) %>%
    # corrigir cor/raca
    mutate(race_total = rowSums(across(cor_branca:cor_indigena), na.rm = TRUE)) %>%
    mutate_at(vars(matches("cor")), ~ .x / race_total) %>%
    mutate_at(vars(matches("cor")), ~ round(.x * moradores_total)) %>%
    select(-age_total, -race_total)
  
  
  # Criar variavel de renda domicilias per capita de cada setor censitario
  setDT(setores_renda)[, renda_per_capta := renda_total / moradores_total]
  setores_renda[, cod_setor := as.character(cod_setor)]
  
  
  # funcao para fazer merge dos dados e salve arquivos na pasta 'data'
  merge_renda_setores <- function(sigla){
    
    # status message
    message('Woking on city ', sigla, '\n')
    
    # codigo do municipios
    code_muni <- subset(munis_df, abrev_muni==sigla )$code_muni
    
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
