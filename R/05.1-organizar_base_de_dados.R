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

# incorporar nome da cidade e codigo do municipio
hex_agreg <- hex_agreg %>% left_join(munis_df %>% select(cod_muni = code_muni, abrev_muni, nome_muni = name_muni), 
                               by = c("muni" = "abrev_muni"))


# dados acessibilidade
acess <- lapply(dir("../data/output_access/", full.names = TRUE), read_rds) %>% rbindlist(fill = TRUE)
setDT(acess)
setnames(acess, 'origin', 'id_hex' )


# join data sets
hex_dt <- left_join(hex_agreg, acess[, -"geometry", with =F], by=c("id_hex", "quintil", "decil"))
setDT(hex_dt)  
head(hex_dt)


# 2) Organizar a nomeclatura das variaveis ---------------------------

hex_dt_fim <- hex_dt %>%
  select(sigla_muni = muni, nome_muni, cod_muni, id_hex, 
         
         # Selecionar variaveis de populacao
         P001 = pop_total, 
         P002 = cor_branca, 
         P003 = cor_negra, 
         P004 = cor_indigena, 
         P005 = cor_amarela,
         
         # Selecionar variveis de renda
         R001 = renda_capta, 
         R002 = quintil, 
         R003 = decil,
         
         # # Selecionar atividades de trabalho
         # T001 = empregos_total, 
         # T002 = empregos_baixa, 
         # T003 = empregos_media, 
         # T004 = empregos_alta,
         
         # Selecionar atividades de educacao
         E001 = edu_total, 
         E002 = edu_infantil,
         E003 = edu_fundamental, 
         E004 = edu_medio,
         
         # Selecionar atividades de saude
         S001 = saude_total, 
         S002 = saude_baixa, 
         S003 = saude_media, 
         S004 = saude_alta,
         
         # Selecionar variaveis de acessibilidade
         modo = mode, pico, 
         starts_with("CMA"), starts_with("TMI"),
         # Selecionar a geometria
         geometry) %>%
  
  # transformar Infs para NA na renda per capta (renda Inf ocorre quando nao há pop no hexagono)
  mutate(R001 = ifelse(P001==0, NA , R001)) %>%
  mutate(R002 = ifelse(P001==0, NA , R002)) %>%
  mutate(R003 = ifelse(P001==0, NA , R003)) %>%
  
  
  # arredondar variaveis
  # arredondar renda per capta para 1 casas decimais
  mutate(R001 = round(R001, 1)) %>%
  # arredondar acessibilidade para 4 casas decimais
  mutate_at(vars(matches("CMA|TMI")), round, digits = 4) %>%
  
  # renomear os modos
  mutate(modo = case_when(
    modo == "transit" ~ "tp",
    modo == "bike" ~"bicicleta",
    modo == "walk" ~ "caminhada"
  )) %>%
  
  # # truncar os valores de TMI quando eh infinito
  # # para caminhada: 60 minutos;  para bicicleta de transporte publico: 120 minutos
  # mutate_at(vars(matches("TMI")), list(~ ifelse(is.infinite(.) & modo == "caminhada", 60, 
  #                                            ifelse(is.infinite(.) & modo %in% c("tp", "bicicleta"), 120, .)))) %>%
  
  # transformar para sf
  st_sf()
  


names(hex_dt_fim)





# 3) Exportar ---------------------------

# salvar rds
write_rds(hex_dt_fim, "../data/output_base_final/acess_oport_2019.rds", compress = "gz")

# salvar dados em geopakage
st_write(hex_dt_fim, "../data/output_base_final/acess_oport_2019.gpkg")

  # zip
  zip::zipr(zipfile = "../data/output_base_final/acess_oport_2019.zip", 
            files = dir("../data/output_base_final/", pattern = "*.gpkg", full.names = TRUE))

  
  
  
  
  
  
aaa <- subset(hex_dt_fim, sigla_muni %in% c('bel', 'poa'))  
  
  
  
  write_rds(aaa, "../data/output_base_final/acess_oport_2019_test.rds", compress = "gz")
  
  
  
  