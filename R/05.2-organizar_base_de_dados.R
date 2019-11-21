# carregar bibliotecas ----------------
source('./R/fun/setup.R')

# Script para organizar bases de dados para publicao
# Respeitar a seguinte nomeclatura: 

### formato final para disponibilizar dados publicamente
#          # Selecionar variaveis de populacao
#          P001 = pop_total, P002 = cor_branca, P003 = cor_negra, P004 = cor_indigena, P005 = cor_amarela,
#          # Selecionar variveis de renda
#          R001 = renda_capta, R002 = quintil, R003 = decil,
#          # Selecionar atividades de trabalho
#          T001 = empregos_total, T002 = empregos_baixa, T003 = empregos_media, T004 = empregos_baixa,
#          # Selecionar atividades de educacao
#          E001 = edu_total, E002 = edu_infantil, E003 = edu_fundamental, E004 = edu_medio,
#          # Selecionar atividades de saude (por enquanto so saude total)
#          S001 = saude_total, S002 = saude_baixa, S003 = saude_media, S004 = saude_alta)


# 1) Carrega dados ---------------------------

# dados hex agregados
hex_agreg <- lapply(dir("../data/hex_agregados/", full.names = TRUE, pattern = "09"), read_rds) %>% rbindlist(fill = TRUE)

# dados acessibilidade
acess <- lapply(dir("../data/output_access/", full.names = TRUE), read_rds) %>% rbindlist(fill = TRUE)
setDT(acess)
setnames(acess, 'origin', 'id_hex' )

# join data sets
hex_dt <- left_join(acess, hex_agreg[, -"geometry", with =F], by=c("id_hex", "quintil", "decil"))
setDT(hex_dt)   

# incorporar nome da cidade e codigo do municipio
hex_dt <- hex_dt %>% left_join(munis_df %>% select(cod_muni = code_muni, abrev_muni, nome_muni = name_muni), 
                               by = c("city" = "abrev_muni"))


# 2) Organizar a nomeclatura das variaveis ---------------------------

hex_dt_fim <- hex_dt %>%
  select(sigla_muni = city, nome_muni, cod_muni, id_hex, 
         
         # Selecionar variaveis de populacao
         P001 = pop_total, P002 = cor_branca, P003 = cor_negra, P004 = cor_indigena, P005 = cor_amarela,
         
         # Selecionar variveis de renda
         R001 = renda_capta, R002 = quintil, R003 = decil,
         
         # Selecionar atividades de trabalho
         T001 = empregos_total, T002 = empregos_baixa, T003 = empregos_media, T004 = empregos_alta,
         
         # Selecionar atividades de educacao
         E001 = edu_total, E002 = edu_infantil, E003 = edu_fundamental, E004 = edu_medio,
         
         # Selecionar atividades de saude
         S001 = saude_total, S002 = saude_baixa, S003 = saude_media, S004 = saude_alta,
         
         # Selecionar variaveis de acessibilidade
         modo = mode, pico, 
         starts_with("CMA"), starts_with("TMI"),
         # Selecionar a geometria
         geometry) %>%
  
  # transformar Infs para 0 na renda per capta
  mutate(R001 = ifelse(is.infinite(R001), 0, R001)) %>%
  
  # arredondar variaveis
  # arredondar renda per capta para 0 casas decimais
  mutate(R001 = round(R001, 0)) %>%
  # arredondar acessibilidad para tres casas decimais
  mutate_at(vars(matches("CMA|TMI")), round, digits = 3)

# dividir as bases por modo?
hex_dt_fim_transit <- hex_dt_fim %>%
  filter(modo == "transit") %>%
  # mudar nome do modo
  mutate(modo = ifelse(modo == "transit", "tp", modo)) %>%
  # filtrar so as variaveis de acessibilidade referentes ao modo
  select_if(~sum(!is.na(.)) > 0)

  
hex_dt_fim_ativo <- hex_dt_fim %>%
  filter(modo %in% c("bike", "walk")) %>%
  # mudar nome do modo
  mutate(modo = case_when(
    modo == "bike" ~"bicicleta",
    modo == "walk" ~ "caminhada"
  )) %>%
  # filtrar so as variaveis de acessibilidade referentes ao modo
  select_if(~sum(!is.na(.)) > 0)


names(hex_dt_fim_ativo)




# 3) Exportar ---------------------------

# salvar dados de transporte publico
st_write(hex_dt_fim_transit, "acess_tp_2019.shp")

# salvar dados para transporte ativo
st_write(hex_dt_fim_ativo, "acess_ativo_2019.shp")