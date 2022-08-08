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
  munis_df <- munis_list$munis_df
  # munis_df <- munis_df %nlike% "bsb"
  
  
  # 1) Carrega dados ---------------------------
  
  # dados hex agregados --------------
  hex_agreg <- lapply(dir(sprintf("../../data/acesso_oport/hex_agregados/%s/", ano), full.names = TRUE, pattern = "09"), read_rds) %>% 
    rbindlist(fill = TRUE)
  
  # incorporar nome da cidade e codigo do municipio
  hex_agreg <- hex_agreg %>% left_join(munis_df %>% dplyr::select(cod_muni = code_muni, abrev_muni, nome_muni = name_muni), 
                                       by = c("sigla_muni" = "abrev_muni"))
  
  
  # dados acessibilidade ---------------------------
  acess_paths <- dir(sprintf("../../data/acesso_oport/output_access/%s", ano), full.names = TRUE, pattern = "acess_", recursive = TRUE)
  acess_paths_ok <- acess_paths[acess_paths %nlike% "bsb_origin|bsb_dest|goi_origin|goi_dest"]
  acess <- lapply(acess_paths_ok, read_rds) %>% 
    rbindlist(fill = TRUE)
  
  # abrir bsb/goi carro - especial
  acess_paths_bsb_carro1 <- acess_paths[acess_paths %like% "bsb_origin"]
  acess_paths_bsb_carro2 <- acess_paths[acess_paths %like% "bsb_dest"]
  acess_bsb_carro1 <- lapply(acess_paths_bsb_carro1, function(x) read_rds(x)) %>% rbindlist()
  acess_bsb_carro2 <- lapply(acess_paths_bsb_carro2, function(x) read_rds(x)) %>% rbindlist() %>% dplyr::select(-geometry)
  # join them all!
  acess_bsb_carro <- full_join(acess_bsb_carro1, acess_bsb_carro2,
                               by = c("origin", "city", "mode", "ano", "pico"))
  
  # ui <- acess_bsb_carro %>% filter(is.na(CMATT60))
  # ui %>% filter(!is.infinite(TMIST))

  # bring geom
  # acess_bsb_carro <- left_join(acess_bsb_carro, hex_agreg %>% dplyr::select(id_hex, geometry), by = c("origin" = "id_hex")) %>% setDT()
  
  
  # rbind both access
  # we have to set fill = TRUE because bsb is only for car and car doesnt have some
  # variables from walk and bike
  acess <- rbind(acess, acess_bsb_carro, fill = TRUE)
  # ui <- acess %>% filter(is.na(CMATT120))
  # ui <- acess %>% filter(CMATT120 == 0)
  # ui <- acess %>% filter(is.infinite(TMIST))
  # ui <- acess %>% filter(is.na(TMICT))
  # table(ui$mode)
  # table(ui$city)
  
  # fill NA
  acess[is.na(acess)] <- 0
  setDT(acess)
  setnames(acess, 'origin', 'id_hex' )
  
  
  
  # 2) Organizar a nomeclatura das variaveis ---------------------------
  
  # munii <- "for"
  # munii <- "rio"
  # munii <- "bsb"
  # munii <- "spo"
  # munii <- "goi"
  
  join_by_muni <- function(munii) {
    
    # filter muni
    hex_agreg_muni <- hex_agreg[sigla_muni == munii]
    acess_muni <- acess[city == munii]
    
    # trazer infos tipo codemuni, namemuni pra base de acess
    acess_muni <- acess_muni %>%
      left_join(hex_agreg_muni %>% dplyr::select(id_hex, sigla_muni, cod_muni, nome_muni),
                by = "id_hex")
    
    # a acessibilidade goiania foi calculada para a toda RM em todos anos, mas so vamos usar para
    # 2019
    acess_muni <- acess_muni %>% filter(!is.na(sigla_muni))
    
    # # join data sets
    # hex_dt <- left_join(setDT(hex_agreg_muni), setDT(acess_muni)[, -"geometry", with =F], 
    #                     by=c("id_hex", "quintil", "decil"))
    
    # TODO: dividir bases:
    # - 1 arquivo com as atividaes(saude educacao etc)
    # - 1 arquivo de acessibilidade para transporte publico / carro
    # - 1 arquivo de acessibilidade para modos ativos
    
    
    hex_landuse <- hex_agreg_muni %>%
      dplyr::select(id_hex, abbrev_muni = sigla_muni, name_muni = nome_muni, code_muni = cod_muni, year = ano,
             
             # Selecionar variaveis de populacao (cor)
             P001 = pop_total, 
             P002 = cor_branca, 
             P003 = cor_negra, 
             P004 = cor_indigena, 
             P005 = cor_amarela,
             
             # Selecionar variaveis de populacao (sexo)
             P006 = pop_mulheres, 
             P007 = pop_homens,
             
             # Selecionar variaveis de populacao (idade)
             P010  = idade_0a5  , 
             P011  = idade_6a14  , 
             P012  = idade_15a18, 
             P013  = idade_19a24,  
             P014  = idade_25a39, 
             P015  = idade_40a69, 
             P016  = idade_70   , 
             
             # Selecionar variveis de renda
             R001 = renda_capita, 
             R002 = renda_quintil, 
             R003 = renda_decil,
             
             # # Selecionar atividades de trabalho
             T001 = empregos_total,
             T002 = empregos_baixa,
             T003 = empregos_media,
             T004 = empregos_alta,
             
             # Selecionar atividades de educacao - escolas
             E001 = edu_total, 
             E002 = edu_infantil,
             E003 = edu_fundamental, 
             E004 = edu_medio,
             
             # Selecionar atividades de educacao - matriculas
             M001 = mat_total, 
             M002 = mat_infantil,
             M003 = mat_fundamental, 
             M004 = mat_medio,
             
             # Selecionar atividades de saude
             S001 = saude_total, 
             S002 = saude_baixa, 
             S003 = saude_media, 
             S004 = saude_alta,
             
             # Selecionar cras/creas
             C001 = cras_total, 
             
             # Selecionar a geometria
             geometry) %>%
      
      # transformar Infs para NA na renda per capita (renda Inf ocorre quando nao há pop no hexagono)
      mutate(R001 = ifelse(P001==0, NA , R001)) %>%
      mutate(R002 = ifelse(P001==0, NA , R002)) %>%
      mutate(R003 = ifelse(P001==0, NA , R003)) %>%
      
      # arredondar renda per capita para 1 casas decimais
      mutate(R001 = round(R001, 1)) %>%
      setDT()
    
    modo <- munis_list$munis_modo[abrev_muni == munii & ano_modo == ano]$modo
    
    hex_dt_tpcarro <- acess_muni %>%
      filter(mode %in% c("transit", "car")) %>%
      dplyr::select(id_hex, abbrev_muni = sigla_muni, name_muni = nome_muni, code_muni = cod_muni, year = ano,
             # Selecionar variaveis de acessibilidade
             mode = mode, 
             peak = pico, 
             ends_with(c("15", "30", "60", "90", "120")), 
             # matches("^CMA[:upper:]{3}(15|30|45|60)"), 
             # matches("^CMP[:upper:]{3}(15|30|45|60)"), 
             starts_with("TMI"),
             geometry) %>%
      # renomear os modos
      mutate(mode = case_when(
        mode == "transit" ~ "public_transport",
        mode == "car" ~ "car"
      )) %>%
      # arredondar acessibilidade para 4 casas decimais
      mutate_at(vars(matches("CMA|CMP|TMI")), round, digits = 4) %>%
      
      # garantir que so tenha um unico hexagono para cada cidade, modo e pico
      distinct(id_hex, abbrev_muni, mode, peak, .keep_all = TRUE) %>%
      setDT()
    
    # # abrir pontos de fortalewza
    # points <- fread("../../r5/points/2017/points_spo_09_2017.csv")
    # 
    # setdiff(hex_dt_tpcarro$id_hex, points$id_hex)
    # setdiff(points$id_hex, hex_dt_tpcarro$id_hex)
    # a1 <- colnames(hex_dt_tpcarro)
    # a1 <- a1[grepl("^(CMA|CMP|TMI)", a1)]
    
    
    hex_dt_ativo <- acess_muni %>%
      filter(mode %in% c("bike", "walk")) %>%
      dplyr::select(id_hex, abbrev_muni = sigla_muni, name_muni = nome_muni, code_muni = cod_muni, year = ano,
             # Selecionar variaveis de acessibilidade
             mode, 
             peak = pico, 
             ends_with(c("15", "30", "45", "60")), 
             starts_with("TMI"),
             geometry) %>%
      # renomear os modos
      mutate(mode = case_when(
        mode == "bike" ~"bicycle",
        mode == "walk" ~ "walk"
      )) %>%
      # arredondar acessibilidade para 4 casas decimais
      mutate_at(vars(matches("CMA|CMP|TMI")), round) %>%
      
      # # filtrar cidades que so tem ativo que porventura tenham tp
      # mutate(ok = ifelse(sigla_muni %in% munis_list$munis_modo[ano_modo == ano & modo == "ativo"]$abrev_muni & modo == "tp", 1, 0)) %>%
      # filter(is.na(ok) | ok == 0) %>%
      # select(-ok) %>%
      
      # garantir que so tenha um unico hexagono para cada cidade, modo e pico
      distinct(id_hex, abbrev_muni, mode, peak, .keep_all = TRUE) %>%
      setDT()
    
    # setdiff(hex_dt_ativo$id_hex, points$id_hex)
    
    # # truncar os valores de TMI quando eh infinito
    # # para caminhada: 60 minutos;  para bicicleta de transporte publico: 120 minutos
    # mutate_at(vars(matches("TMI")), list(~ ifelse(is.infinite(.) & modo == "caminhada", 60,
    #                                            ifelse(is.infinite(.) & modo %in% c("tp", "bicicleta"), 120, .)))) %>%
    
    
    fim <- list(hex_landuse, hex_dt_tpcarro, hex_dt_ativo)
    names(fim) <- c("hex_landuse", "hex_dt_tpcarro", "hex_dt_ativo")
    
    return(fim)
    
  }
  
  
  # aplicar funcao
  
  # a <- lapply(munis_df$abrev_muni, purrr::possibly(join_by_muni, otherwise = "erro"))
  # teste: ui <- munis_df$abrev_muni[munis_df$abrev_muni %nlike% "bsb"]
  # a <- lapply(munis_df$abrev_muni[munis_df$abrev_muni %nlike% "bsb"], join_by_muni)
  a <- lapply(munis_df$abrev_muni, join_by_muni)
  b <- purrr::transpose(a)
  
  # for now, delete bsb by tp and car (its empty anyway, but is thorwing an error when rbinding)
  # b$hex_dt_tpcarro[[7]] <- NULL
  
  c <- lapply(b, rbindlist)
  
  # 3) Exportar ---------------------------
  
  write_rds(c$hex_landuse %>% st_sf(crs = 4326),
            sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_landuse_v1.0.rds", ano, ano))
  
  write_rds(c$hex_dt_tpcarro %>% st_sf(crs = 4326),
            sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_tpcar_v1.0.rds", ano, ano))
  
  write_rds(c$hex_dt_ativo %>% st_sf(crs = 4326),
            sprintf("../../data/acesso_oport/output_base_final/%s/dados%s_AcessOport_access_active_v1.0.rds", ano, ano))
  
  # salvar dados em geopackage
  # st_write(a_sf_externo, sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_20200116.gpkg", ano, ano))
  
  # # zip
  # zip::zipr(zipfile = sprintf("../data/output_base_final/%s/dados%s_AcessOport_v1.0_%s0116.zip", ano, ano, ano), 
  #           files = dir(sprintf("../data/output_base_final/%s", ano), pattern = "*.gpkg", full.names = TRUE))
  
  
  
}

# aplicar
organizar_base_acess(ano = 2017)
organizar_base_acess(ano = 2018)
organizar_base_acess(ano = 2019)





