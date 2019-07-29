# TRATAR LINHAS HM

# abrir
linhas <- st_read("../data-raw/linhas_hm/RM_linhas_TMA_2017_WGS84.shp", 
                  quiet = T, 
                  stringsAsFactors=F, 
                  options = "ENCODING=UTF-8") %>%
  dplyr::select(Cidade, Modo, Corredor) %>%
  mutate(cidade1 = tolower(substr(Cidade, 1, 3))) %>%
  mutate(cidade1 = ifelse(cidade1 %in% c("sã£", "são"), "sao", cidade1)) %>%
  mutate(Modo = ifelse(Modo %in% c("MetrÃ´"), "Metro", Modo))

# salvar
write_rds(linhas, "../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")




iconv("MetrÃ´", to = 'UTF-8')


ifelse(linhas$Modo == "MetrÃ´", "Metro", linhas$Modo)


linhas$Modo[10]


ifelse(linhas$cidade == "sã£", "sao", linhas$cidade)
