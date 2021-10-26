
# atividade <- 'rais'

summary_geocode <- function(ano) {
  
  # empregos ----------------------------------------------------------------
  path_antes <- sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa3_geocoded.rds", ano, ano)
  path_depois <- sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected.rds", ano, ano)
  antes <- read_rds(path_antes)
  depois <- read_rds(path_depois)
  antes_cidade <- antes %>% group_by(name_muni) %>% summarise(n = sum(total_corrigido, na.rm = TRUE)) %>% arrange(desc(n)) %>% slice(1:20)
  depois_cidade <- depois %>% group_by(name_muni) %>% summarise(n = sum(total_corrigido, na.rm = TRUE)) %>% arrange(desc(n)) %>% slice(1:20)
  comp_empregos <- left_join(antes_cidade, depois_cidade, by = "name_muni", suffix = c("_antes", "_depois"))
  comp_empregos <- comp_empregos %>% mutate(perc = n_depois/n_antes) %>% mutate(atividade = "empregos")
  
  path_antes  <-  sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps.rds", ano, ano)
  path_depois <-  sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano, ano)
  antes <- read_rds(path_antes)
  depois <- read_rds(path_depois)
  antes_cidade <- antes %>% count(cidade, sort = TRUE) %>% slice(1:20)
  depois_cidade <- depois %>% count(cidade, sort = TRUE) %>% slice(1:20)
  comp_educacao <- left_join(antes_cidade, depois_cidade, by = "cidade", suffix = c("_antes", "_depois"))
  comp_educacao <- comp_educacao %>% mutate(perc = n_depois/n_antes) %>% mutate(atividade = "educacao") %>%
    rename(name_muni = cidade)
    
  

  path_antes  <-  sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps.rds", ano, ano)
  path_depois <-  sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps_gquality_corrected2.rds", ano, ano)
  antes <- read_rds(path_antes)
  depois <- read_rds(path_depois)
  antes_cidade <- antes %>% count( municipio, sort = TRUE) %>% slice(1:20)
  depois_cidade <- depois %>% count(municipio, sort = TRUE) %>% slice(1:20)
  comp_saude <- left_join(antes_cidade, depois_cidade, by = "municipio", suffix = c("_antes", "_depois"))
  comp_saude <- comp_saude %>% mutate(perc = n_depois/n_antes) %>% mutate(atividade = "saude") %>%
    rename(name_muni = municipio)
    
  go <- rbind(comp_empregos, comp_educacao, comp_saude) %>%
    mutate(ano = ano)
  
  
}

go <- lapply(c(2017, 2018, 2019), summary_geocode) %>%
  rbindlist()


googlesheets4::write_sheet(data = go,
                           ss = "https://docs.google.com/spreadsheets/d/11pIp1Ioiua7NWDp41DKXMMWI_3JvmzonhNMoNpvR1aE/edit#gid=215944254",
                           sheet = "geocode")
