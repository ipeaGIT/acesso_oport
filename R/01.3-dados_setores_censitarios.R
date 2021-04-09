#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.2 Carrega dados socioeconomicos dos setores censitarios


# carregar bibliotecas
source('./R/fun/setup.R')

### 1. Carrega micro dados dos setores censitarios --------------------------------------------------

# # vars to select
# vars <- c('Cod_UF', 'Cod_municipio', 'Cod_setor', 
#           'DomRend_V003', 'Dom2_V002', 
#           'Pess3_V002', 'Pess3_V003', 'Pess3_V004', 
#           'Pess3_V005', 'Pess3_V006')

## Leitura dos dados gerais ------------------
setores1 <- data.table::fread("../../data-raw/setores_censitarios/dados_censo2010A.csv",
                              colClasses = 'character')
# names(setores1)

# select vars
setores1 <- setores1 %>% select(Cod_UF, Cod_municipio, Cod_setor, 
                                DomRend_V003, Dom2_V002, 
                                # income brackets
                                # PessRenda_V001:PessRendaV010,
                                # raca, cor
                                Pess3_V002, Pess3_V003, Pess3_V004, 
                                Pess3_V005, Pess3_V006,
                                # idade
                                Pess13_V023:Pess13_V134
)

## leitura dos dados de renda ----------------------
income_files <- list.files(path = "//storage6/bases/DADOS/PUBLICO/CENSO/Setor_Censitario/2010/Originais/", 
                           pattern = 'Entorno04', recursive = T, full.names = TRUE)
income_files <- income_files[income_files %ilike% '.xls']

# variaveis v683 ate v694
colstoread <- c('Cod_setor', paste0("V", rep(683:694)))
setores_renda <- lapply(X=income_files, FUN=readxl::read_xls) %>%
  rbindlist(fill = TRUE)

# select columns
setores_renda1 <- setores_renda[, ..colstoread]

# change format
setores_renda1[, Cod_setor := as.character(Cod_setor)]

# table(nchar(setores_renda1$Cod_setor)) # Ok !

# there are some 'x' in some of the variables
# substitute them for 0s
setores_renda1 <- setores_renda1 %>%
  mutate_at(vars(starts_with("V")), ~ ifelse(.x == "X", 0, .x))

# rename these variables
setores_renda1 <- setores_renda1 %>%
  rename_at(vars(starts_with("V")), ~ paste0("Entorno04_", .x))



# join datasets -----------------------------
# table(nchar(setores1$Cod_setor))
# table(nchar(setores_renda1$Cod_setor))

setores2 <- left_join(setores1, setores_renda1,
                      by = "Cod_setor")

setores2 <- setores2 %>% 
  # transform variables to numeric
  mutate_at(vars(matches("V\\d{3}")), as.numeric)



# Dom2_V002 # Moradores em domicílios particulares permanentes
# DomRen_V003 # Total do rendimento nominal mensal dos domicílios particulares permanentes 

# Income brackets: PessoaRenda
# V001 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# até ½ salário mínimo
# V002 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de ½ a 1 salário mínimo
# V003 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 1 a 2 salários mínimos
# V004 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 2 a 3 salários mínimos
# V005 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 3 a 5 salários mínimos
# V006 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 5 a 10 salários mínimos
# V007 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 10 a 15 salários mínimos
# V008 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 15 a 20 salários mínimos
# V009 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de
# mais de 20 salários mínimos
# V010 Pessoas de 10 anos ou mais de idade sem rendimento nominal mensal 


# Raca/cor
# Pess3_V002 # Pessoas Residentes e cor ou raça - branca
# Pess3_V003 # Pessoas Residentes e cor ou raça - preta
# Pess3_V004 # Pessoas Residentes e cor ou raça - amarela
# Pess3_V005 # Pessoas Residentes e cor ou raça - parda
# Pess3_V006 # Pessoas Residentes e cor ou raça - indígena 

# Idade
# 0 a 5
# Pess13_V023:Pess13_V039

# 6 a 14
# Pess13_V040:Pess13_V048

# 15 a 18
# Pess13_V049:Pess13_V052

# 19 a 24
# Pess13_V053:Pess13_V058

# 25 a 39
# Pess13_V059:Pess13_V073

# 40 a 69
# Pess13_V074:Pess13_V0103

# 70+
# Pess13_V0104:Pess13_V134


### 2. Merge dos dados de renda com shapes dos setores censitarios --------------------------------------------------

# filtra apenas municipios do projeto
code_munis <- munis_list$munis_metro$code_muni %>% unlist %>% unique()
setores2 <- setores2[Cod_municipio %in% code_munis,]


merge_renda_setores_all <- function(ano, munis = "all") { # munis <- sigla <- "goi"
  
  dir.create(sprintf("../../data/acesso_oport/setores_agregados/%s", ano))
  
  
  # agrupar variaveis de idade/income brackets e soma-las
  setores3 <- setores2 %>% mutate(idade_0a5 = rowSums(across(Pess13_V023:Pess13_V039), na.rm = TRUE),
                                  
                                  idade_6a14 = rowSums(across(Pess13_V040:Pess13_V048), na.rm = TRUE),
                                  
                                  idade_15a18 = rowSums(across(Pess13_V049:Pess13_V052), na.rm = TRUE),
                                  
                                  idade_19a24 = rowSums(across(Pess13_V053:Pess13_V058), na.rm = TRUE),
                                  
                                  idade_25a39 = rowSums(across(Pess13_V059:Pess13_V073), na.rm = TRUE),
                                  
                                  idade_40a69 = rowSums(across(Pess13_V074:Pess13_V103), na.rm = TRUE),
                                  
                                  idade_70 = rowSums(across(Pess13_V104:Pess13_V134), na.rm = TRUE)
                                  
  ) %>%
    mutate(moradores_SM_0_1Q = rowSums(across(c(Entorno04_V693:Entorno04_V694, Entorno04_V683:Entorno04_V684)), na.rm = TRUE),
           
           moradores_SM_1Q_1M = rowSums(across(c(Entorno04_V685, Entorno04_V686)), na.rm = TRUE),
           
           moradores_SM_1M_1 = rowSums(across(Entorno04_V687:Entorno04_V688), na.rm = TRUE),
           
           moradores_SM_1_2 = rowSums(across(Entorno04_V689:Entorno04_V690), na.rm = TRUE),
           
           moradores_SM_2 = rowSums(across(Entorno04_V691:Entorno04_V692), na.rm = TRUE)
           
    )
  
  
 
  
  ## Renomeia variaveis
  # Renda 6.19 - variavel escolhida: V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
  setores4 <-  setores3 %>% 
    dplyr::select(cod_uf = Cod_UF, cod_muni = Cod_municipio, cod_setor = Cod_setor, 
                  renda_total = DomRend_V003, moradores_total = Dom2_V002, 
                  # income brackets
                  matches("moradores_SM"),
                  # raca, cor
                  cor_branca=Pess3_V002, cor_preta=Pess3_V003, 
                  cor_amarela=Pess3_V004, cor_parda=Pess3_V005, cor_indigena=Pess3_V006,
                  # age variables
                  matches("idade"))
  
  # convert NAs para zero
  setores4[is.na(setores4)] <- 0
  
  # fazer a correcao das idades e da populacao por cor/raca
  # dado de pop total (domicilios part permanentes) é menor do que soma de pop por idade.
  # esse trecho do codigo faz correção pelas proporções de cada idade e cor/raca
  setores5 <- setores4 %>%
    # corrigir idade
    mutate(age_total = rowSums(across(idade_0a5:idade_70), na.rm = TRUE)) %>%
    mutate_at(vars(matches("idade")), ~ .x / age_total) %>%
    mutate_at(vars(matches("idade")), ~ round(.x * moradores_total)) %>%
    # corrigir cor/raca
    mutate(race_total = rowSums(across(cor_branca:cor_indigena), na.rm = TRUE)) %>%
    mutate_at(vars(matches("cor")), ~ .x / race_total) %>%
    mutate_at(vars(matches("cor")), ~ round(.x * moradores_total)) %>%
    select(-age_total, -race_total)
  
  # convert NAs para zero
  setDT(setores5)
  setores5[is.na(setores5)] <- 0
  
  # Criar variavel de renda domicilias per capita de cada setor censitario
  setores5[, renda_per_capita := renda_total / moradores_total]
  setores5[, cod_setor := as.character(cod_setor)]
  
  
  
  
  
  # funcao para fazer merge dos dados e salve arquivos na pasta 'data'
  merge_renda_setores <- function(sigla){
    
    # status message
    message('Working on city ', sigla, '\n')
    
    # codigo do municipios
    code_muni <- subset(munis_list$munis_metro, abrev_muni==sigla & ano_metro == ano )$code_muni %>% unlist()
    
    # subset dados dos setores
    dados <- subset(setores5, cod_muni %in% code_muni)
    
    # leitura do shape dos setores
    sf <- readr::read_rds( sprintf("../../data-raw/setores_censitarios/%s/setores_%s_%s.rds", ano, sigla, ano) )
    
    # merge
    sf2 <- dplyr::left_join(sf, dados, c('code_tract'='cod_setor'))
    
    # salvar
    readr::write_rds(sf2,  sprintf("../../data/acesso_oport/setores_agregados/%s/setores_agregados_%s_%s.rds", ano, sigla, ano), compress = 'gz')
  }
  
  
  # aplicar funcao -----------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    
  } else (x = munis)
  
  # Parallel processing using future.apply
  # purrr::walk(x, merge_renda_setores)
  future::plan(future::multicore)
  invisible(future.apply::future_lapply(X = x, FUN=merge_renda_setores, future.packages=c('sf', 'dplyr', 'data.table')))
  
}

# Aplicar funcao
merge_renda_setores_all(ano = 2017)
merge_renda_setores_all(ano = 2018)
merge_renda_setores_all(ano = 2019)
merge_renda_setores_all(ano = 2020)

