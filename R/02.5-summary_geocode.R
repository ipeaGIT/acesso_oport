# carregar bibliotecas
source('./R/fun/setup.R')

# atividade <- 'rais'

summary_geocode <- function(ano) {
  
  
  siglas <- munis_list$munis_metro[ano_metro == ano] %>% unnest(code_muni) %>% select(sigla_muni = abrev_muni, code_muni)
  
  
  # empregos ----------------------------------------------------------------
  path_antes <- sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa3_geocoded.rds", ano, ano)
  path_depois <- sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected.rds", ano, ano)
  antes <- read_rds(path_antes)
  depois <- read_rds(path_depois)
  antes <- antes %>% left_join(siglas %>% mutate(code_muni =   as.integer(substr(code_muni, 1, 6))), by = c("codemun" = "code_muni"))
  depois <- depois %>% left_join(siglas %>% mutate(code_muni = as.integer(substr(code_muni, 1, 6))), by = c("code_muni" = "code_muni"))
  antes_cidade <- antes %>% group_by(sigla_muni) %>% summarise(n = sum(total_corrigido, na.rm = TRUE)) %>% arrange(desc(n))
  depois_cidade <- depois %>% group_by(sigla_muni) %>% summarise(n = sum(total_corrigido, na.rm = TRUE)) %>% arrange(desc(n))
  comp_empregos <- left_join(antes_cidade, depois_cidade, by = "sigla_muni", suffix = c("_antes", "_depois"))
  comp_empregos <- comp_empregos %>% mutate(perc = n_depois/n_antes) %>% mutate(atividade = "empregos")
  
  path_antes  <-  sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps.rds", ano, ano)
  path_depois <-  sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected.rds", ano, ano)
  antes <- read_rds(path_antes)
  depois <- read_rds(path_depois)
  antes <- antes %>% left_join(siglas, by = "code_muni")
  depois <- depois %>% left_join(siglas, by = "code_muni")
  antes_cidade <- antes %>% count(sigla_muni, sort = TRUE) 
  depois_cidade <- depois %>% count(sigla_muni, sort = TRUE)
  comp_educacao <- left_join(antes_cidade, depois_cidade, by = "sigla_muni", suffix = c("_antes", "_depois"))
  comp_educacao <- comp_educacao %>% mutate(perc = n_depois/n_antes) %>% mutate(atividade = "educacao")
    
  

  path_antes  <-  sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps.rds", ano, ano)
  path_depois <-  sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano, ano)
  antes <- read_rds(path_antes)
  depois <- read_rds(path_depois)
  antes <- antes %>% left_join(siglas %>% mutate(code_muni =   as.character(substr(code_muni, 1, 6))), by = c("ibge" = "code_muni"))
  depois <- depois %>% left_join(siglas %>% mutate(code_muni = as.character(substr(code_muni, 1, 6))), by = c("code_muni" = "code_muni"))
  antes_cidade <- antes %>% count( sigla_muni, sort = TRUE)
  depois_cidade <- depois %>% count(sigla_muni, sort = TRUE)
  comp_saude <- left_join(antes_cidade, depois_cidade, by = "sigla_muni", suffix = c("_antes", "_depois"))
  comp_saude <- comp_saude %>% mutate(perc = n_depois/n_antes) %>% mutate(atividade = "saude") 
    
  go <- rbind(comp_empregos, comp_educacao, comp_saude) %>%
    mutate(ano = ano)
  
  
}

go <- lapply(c(2017, 2018, 2019), summary_geocode) %>%
  rbindlist()

go_new <- go %>%
  select(-n_antes) %>%
  pivot_wider(names_from = ano,
              values_from = n_depois:perc)

googlesheets4::write_sheet(data = go,
                           ss = "https://docs.google.com/spreadsheets/d/11pIp1Ioiua7NWDp41DKXMMWI_3JvmzonhNMoNpvR1aE/edit#gid=215944254",
                           sheet = "geocode")

googlesheets4::write_sheet(data = go_new,
                           ss = "https://docs.google.com/spreadsheets/d/11pIp1Ioiua7NWDp41DKXMMWI_3JvmzonhNMoNpvR1aE/edit#gid=215944254",
                           sheet = "geocode_wide")
