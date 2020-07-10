# carregar bibliotecas ----------------
source('./R/fun/setup.R')

# funcao para organizar por ano
organizar_base_acess <- function(ano) {
  
  # Script para organizar bases de dados para publicao
  # Respeitar a seguinte nomeclatura: 
  
  ### formato final para disponibilizar dados publicamente
  #          # Selecionar variaveis de populacao
  #          P001 = pop_total, P002 = cor_branca, P003 = cor_negra, P004 = cor_indigena, P005 = cor_amarela,
  #          # Selecionar variveis de renda
  #          R001 = renda_capita, R002 = quintil, R003 = decil,
  #          # Selecionar atividades de trabalho
  #          T001 = empregos_total, T002 = empregos_baixa, T003 = empregos_media, T004 = empregos_baixa,
  #          # Selecionar atividades de educacao
  #          E001 = edu_total, E002 = edu_infantil, E003 = edu_fundamental, E004 = edu_medio,
  #          # Selecionar atividades de saude (por enquanto so saude total)
  #          S001 = saude_total, S002 = saude_baixa, S003 = saude_media, S004 = saude_alta)
  
  # Select the corerspondent munis_df
  munis_df <- get(sprintf("munis_df_%s", ano))
  
  
  # 1) Carrega dados ---------------------------
  
  # dados hex agregados
  hex_agreg <- lapply(dir(sprintf("../data/hex_agregados/%s/", ano), full.names = TRUE, pattern = "09"), read_rds) %>% 
    rbindlist(fill = TRUE)
  
  # incorporar nome da cidade e codigo do municipio
  hex_agreg <- hex_agreg %>% left_join(munis_df %>% select(cod_muni = code_muni, abrev_muni, nome_muni = name_muni), 
                                       by = c("muni" = "abrev_muni"))
  
  
  # dados acessibilidade
  acess <- lapply(dir(sprintf("../data/output_access/%s", ano), full.names = TRUE), read_rds) %>% rbindlist(fill = TRUE)
  setDT(acess)
  setnames(acess, 'origin', 'id_hex' )
  
  
  # # join data sets
  # hex_dt <- left_join(hex_agreg, acess[, -"geometry", with =F], by=c("id_hex", "quintil", "decil"))
  # setDT(hex_dt)  
  # head(hex_dt)
  
  
  # 2) Organizar a nomeclatura das variaveis ---------------------------
  
  # munii <- "for"
  
  join_by_muni <- function(munii) {
    
    # filter muni
    hex_agreg_muni <- hex_agreg %>% filter(muni == munii)
    acess_muni <- acess %>% filter(city == munii)
    
    # join data sets
    hex_dt <- left_join(setDT(hex_agreg_muni), setDT(acess_muni)[, -"geometry", with =F], 
                        by=c("id_hex", "quintil", "decil"))
    
    hex_dt_fim <- hex_dt %>%
      select(sigla_muni = muni, nome_muni, cod_muni, id_hex, 
             
             # Selecionar variaveis de populacao (cor)
             P001 = pop_total, 
             P002 = cor_branca, 
             P003 = cor_negra, 
             P004 = cor_indigena, 
             P005 = cor_amarela,
             
             # Selecionar variaveis de populacao (idade)
             P006  = idade_0a9  , 
             P007  = idade_10a14, 
             P008  = idade_15a19, 
             P009  = idade_20a29,  
             P010 = idade_30a39, 
             P011 = idade_40a49, 
             P012 = idade_50a59, 
             P013 = idade_60a69, 
             P014 = idade_70   , 
             
             # Selecionar variveis de renda
             R001 = renda_capita, 
             R002 = quintil, 
             R003 = decil,
             
             # # Selecionar atividades de trabalho
             T001 = empregos_total,
             T002 = empregos_baixa,
             T003 = empregos_media,
             T004 = empregos_alta,
             
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
      
      # transformar Infs para NA na renda per capita (renda Inf ocorre quando nao há pop no hexagono)
      mutate(R001 = ifelse(P001==0, NA , R001)) %>%
      mutate(R002 = ifelse(P001==0, NA , R002)) %>%
      mutate(R003 = ifelse(P001==0, NA , R003)) %>%
      
      
      # arredondar variaveis
      # arredondar renda per capita para 1 casas decimais
      mutate(R001 = round(R001, 1)) %>%
      # arredondar acessibilidade para 4 casas decimais
      mutate_at(vars(matches("CMA|TMI")), round, digits = 4) %>%
      
      # renomear os modos
      mutate(modo = case_when(
        modo == "transit" ~ "tp",
        modo == "bike" ~"bicicleta",
        modo == "walk" ~ "caminhada"
      )) %>%
      
      # filtrar cidades que so tem ativo que porventura tenham tp
      mutate(ok = ifelse(sigla_muni %in% munis_df[modo == "ativo"]$abrev_muni & modo == "tp", 1, 0)) %>%
      filter(is.na(ok) | ok == 0) %>%
      select(-ok) %>%
      
      # garantir que so tenha um unico hexagono para cada cidade, modo e pico
      distinct(id_hex, sigla_muni, modo, pico, .keep_all = TRUE)
    
    
    
    # # truncar os valores de TMI quando eh infinito
    # # para caminhada: 60 minutos;  para bicicleta de transporte publico: 120 minutos
    # mutate_at(vars(matches("TMI")), list(~ ifelse(is.infinite(.) & modo == "caminhada", 60, 
    #                                            ifelse(is.infinite(.) & modo %in% c("tp", "bicicleta"), 120, .)))) %>%
    
  }
  
  
  # aplicar funcao
  
  a <- lapply(munis_df$abrev_muni, join_by_muni) %>% rbindlist()
  
  a_sf <- st_sf(a, crs = 4326)
  
  
  # 3) Exportar ---------------------------
  
  # salvar rds com dados de trabalho (base interna)
  write_rds(a_sf, sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_20200116_interno.rds", ano, ano), compress = "gz")
  
  # salvar base externa sem dados de trabalho
  a_sf_externo <- a_sf %>% select(-T001, -T002, -T003, -T004)
    
  
  # salvar rds
  write_rds(a_sf_externo, sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_20200116.rds", ano, ano), compress = "gz")
  
  
  # salvar dados em geopackage
  st_write(a_sf_externo, sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_20200116.gpkg", ano, ano))
  
  # # zip
  # zip::zipr(zipfile = sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_%s0116.zip", ano, ano, ano), 
  #           files = dir(sprintf("../data/output_base_final/%s", ano), pattern = "*.gpkg", full.names = TRUE))
  
  
  
}

# aplicar
organizar_base_acess(ano = 2019)
organizar_base_acess(ano = 2020)
  
  
## exportanto dados para ITDP Mobilidados
a_sf <- readr::read_rds("../data/output_base_final/dados2019_AcessOport_v1.0_20200116.rds")
  
# so modos ativos e acesso a saude e educacao
atv <- subset(a_sf, modo %in% c('bicicleta', 'caminhada'))
atv <- atv %>% select(-contains("CMATT"))
atv <- atv %>% select(-contains("CMATD"))
atv <- atv %>% select(-contains("CMATQ"))
 
table(atv$modo) 

# save
write_rds(atv, "../data/output_base_final/ativo_edu_saude_2019.rds", compress = "gz")
  
  
  
  