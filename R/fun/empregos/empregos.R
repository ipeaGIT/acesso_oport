


rais_filter_raw_data <- function(ano) {
  
  ### 0) get raw data ------------------------------------
  
  #### companies data
  # read raw data
  estabs <- fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
                  #, nrows = 5
                  , colClasses='character')
  
  # subset municipalities
  estabs_mun <- estabs[ codemun %in% substring(munis_df$code_muni, 1,6)]
  
  # subset columns
  estabs_mun <- estabs_mun[, 1:33]
  
  # save
  fwrite(estabs_mun, sprintf('../../data-raw/rais/%s/rais_estabs_raw_%s.csv', ano, ano))
  
  
  rm(estabs_mun, estabs)
  gc(reset = T)
  
  #### workers data
  # read raw data
  
  # Leitura dos dados da RAIS pessoas com colunas que vamos usar
  colnames <- data.table::fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil%s.csv', ano)
                                , colClasses='character'
                                , nrows = 10
  ) %>% colnames()
  
  
  # select columns
  columns <- c("id_estab", "grau_instr", colnames[colnames %like% "nat_jur"], "emp_31dez", 'clas_cnae20', 'uf', 'codemun')
  
  trabal <- fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil%s.csv', ano)
                  , select = columns
                  , colClasses='character')
  
  # rename nat_jur
  colnames(trabal) <- c("id_estab", "grau_instr", "nat_jur", "emp_31dez", 'clas_cnae20', 'uf', 'codemun')
  
  
  # subset municipalities
  # nrow(trabal)
  trabal <- trabal[ codemun %in% substring(munis_df$code_muni, 1,6)]
  # nrow(trabal)
  
  
  # save
  fwrite(trabal, sprintf('../../data-raw/rais/%s/rais_trabal_raw_%s.csv', ano, ano))
  
}






rais_filter_pessoas <- function(ano) {
  
  ##### 1) Filtrar Rais pessoas-------------------------------------------------------
  
  
  # Leitura dos dados da RAIS pessoas com colunas que vamos usar
  rais_trabs <- data.table::fread(sprintf('../../data-raw/rais/%s/rais_trabal_raw_%s.csv', ano, ano),
                                  colClasses='character'
  )
  
  # unique(rais_trabs$id_estab) %>% length() # 3845034 estabs
  
  
  # Filtro 1: selecionar so vinculos ativos
  rais_filtro_1 <- rais_trabs[emp_31dez == 1]
  
  # unique(rais_filtro_1$id_estab) %>% length()
  
  
  # Filtro 2 deletar todas as intituicoes com Natureza Juridica 'publica' (ver ../data-raw/rais/ManualRAIS2018.pdf) pagina 19
  # todos que comecam com o numeral 1 sao de administracao publica
  
  
  # identifica natureza de adm publica
  rais_filtro_1[, adm_pub := ifelse( substr(nat_jur, 1, 1)==1, 1, 0)]
  
  # fica apenas com adm publica
  rais_filtro_2 <- rais_filtro_1[ adm_pub != 1 ]
  
  # quantos vinculos de natureza juridica publica a gente perde? 23.7%
  # nrow(rais_filtro_2) / nrow(rais_filtro_1)
  
  # Filtro 3 deletar todas as empresas publicas (a entidade 2011 eh empresa publica)
  rais_filtro_3 <- rais_filtro_2[nat_jur != "2011"]
  
  
  # Salvar em formato rds para formato rapido
  write_rds(rais_filtro_3, sprintf("../../data/acesso_oport/rais/%s/rais_%s_ind_filtrada.rds", ano, ano))
  
}







rais_categorize_inst <- function(ano) {
  
  
  ####### Categorizar trabalhadores por grau de instrucao  ----------------------------------------------------------------------------------
  
  # Abrir RAIS  em formato rapido rds
  rais_trabs <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_ind_filtrada.rds", ano, ano))
  
  # str(rais_trabs)
  rais_trabs[, grau_instr := str_replace(grau_instr, "0", "")]
  
  # Categorizar trabalhadores por grau de instrucao
  rais_cats <- rais_trabs[, instrucao := fifelse(grau_instr %in% c(1:6), "baixo",                                    # menor do que ensino medio (inclui ensino medio incompleto)
                                                 fifelse(grau_instr %in% c(7, 8), "medio",                           # ensino medio
                                                         fifelse(grau_instr %in% c(9, 10, 11), "alto", grau_instr)))] # ensino superior
  
  
  # Calcula quantidade de vinculo por grau de instrucao em cada estabelecimento
  rais_fim <- rais_trabs[, .(vinculos = .N), by = .(id_estab, clas_cnae20, instrucao)]
  
  # soma quantidade total de empregados em cada empresa
  rais_fim <- rais_fim[, total := sum(vinculos), by = .(id_estab, clas_cnae20)]
  # head(rais_fim)
  
  # Reshape da base de formato long para wide
  rais_fim_wide <- tidyr::spread(rais_fim, instrucao, vinculos, fill = 0)
  # head(rais_fim_wide)
  
  # Salvar agregado do numero de trabalhadores por empresa
  write_rds(rais_fim_wide, sprintf("../../data/acesso_oport/rais/%s/rais_%s_vin_instrucao.rds", ano, ano))
  
}








rais_treat_outliers <- function(ano) {
  
  
  ### Limpar outliers (empresas que ainda declara muitos trabalhadores na mesma sede) -----------------------------------------------------
  
  
  rais <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_vin_instrucao.rds", ano, ano))
  setDT(rais)
  
  
  # * 3a) Corrige total de vínculos de ouliers de setores problemáticos da CNAE ----
  
  
  # Extrai o setor da CNAE
  rais[, cnae.setor := substr(clas_cnae20, 1, 2)]
  
  # Setores considerados problemáticos:
  # * 35 ELETRICIDADE, GÁS E OUTRAS UTILIDADES
  # * 36 CAPTAÇÃO, TRATAMENTO E DISTRIBUIÇÃO DE ÁGUA
  # * 38 COLETA, TRATAMENTO E DISPOSIÇÃO DE RESÍDUOS; RECUPERAÇÃO DE MATERIAIS
  # * 41 CONSTRUÇÃO DE EDIFÍCIOS
  # * 42 OBRAS DE INFRA-ESTRUTURA
  # * 43 SERVIÇOS ESPECIALIZADOS PARA CONSTRUÇÃO
  # * 49 TRANSPORTE TERRESTRE
  # * 51 TRANSPORTE AÉREO
  # * 64 ATIVIDADES DE SERVIÇOS FINANCEIROS
  # * 78 SELEÇÃO, AGENCIAMENTO E LOCAÇÃO DE MÃO-DE-OBRA
  # * 80 ATIVIDADES DE VIGILÂNCIA, SEGURANÇA E INVESTIGAÇÃO
  # * 81 SERVIÇOS PARA EDIFÍCIOS E ATIVIDADES PAISAGÍSTICAS
  # * 82 SERVIÇOS DE ESCRITÓRIO, DE APOIO ADMINISTRATIVO E OUTROS SERVIÇOS PRESTADOS PRINCIPALMENTE ÀS EMPRESAS
  cnaes_problema <- c("35", "36", "38", "41", "42", "43", "49", "51", "64", "78", "80", "81", "82")
  
  # Definição de outlier: estabelecimentos de setores problemáticos cujo total de
  # vínculos fique acima do percentil 95 da distribuição de vínculos daquele setor
  rais[, p95_setor := round(quantile(total, probs = 0.95), 0), by = .(cnae.setor)]
  
  # Cria coluna total_corrigido com total de vínculos corrigidos
  # Caso o estabelecimento de um setor problemático tenha mais vínculos que o p95 
  # daquele setor, o total corrigido será igual ao p95 do setor. Caso não seja de 
  # um setor problemático, e caso não tenha mais vínculos que o p95, o total 
  # permanece igual
  rais[, total_corrigido := total]
  rais[
    cnae.setor %chin% cnaes_problema, 
    total_corrigido := fifelse(total_corrigido > p95_setor, p95_setor, total_corrigido)
    ]
  
  # Zera todos os empregos da administração pública
  # * 84 ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL
  rais[cnae.setor == "84", total_corrigido := 0]
  
  # Remove a coluna que guarda o valor do p95
  rais[, p95_setor := NULL]
  
  
  # * 3b) Corrige vínculos de setores problemáticos por grau de escolaridade ----
  
  
  # Substitui a quantidade de vínculos por grau de escolaridade (apenas em setores 
  # que tiveram seu total de vínculos corrigidos) de forma a manter a mesma 
  # proporção anterior
  rais[
    total_corrigido < total, 
    ":="(
      alto  = round(alto / total * total_corrigido, 0),
      medio = round(medio / total * total_corrigido, 0),
      baixo = round(baixo / total * total_corrigido, 0)
    )
    ]
  
  # Salva RAIS de estabelecimentos com números de vínculos (totais e por grau de 
  # escolaridade) corrigidos
  write_rds(rais, sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido.rds", ano, ano))
  
}



# check size of each year
# map(sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido.rds", 2017:2019, 2017:2019), file.info)




rais_bring_geocode <- function(ano) {
  
  
  
  # TRAZER GEOCODE DOS ESTABELECIMENTOS -----------------------------------------------------
  
  
  rais_estabs <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido.rds", ano, ano))
  rais_estabs_geocode <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_estabs_geocode_final.rds", ano, ano))
  rais_estabs_geocode <- select(rais_estabs_geocode, -qt_vinc_ativos)
  
  # pad everyone to 14 characters
  rais_estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  rais_estabs_geocode[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # unique(rais_estabs$id_estab) %>% length()
  # unique(rais_estabs_geocode$id_estab) %>% length()
  
  
  # join them!
  rais_estabs_geocode_end <- merge(
    rais_estabs,
    rais_estabs_geocode,
    by = "id_estab",
    sort = FALSE,
    all.x = TRUE
  )
  
  # table(rais_estabs_geocode_end$PrecisionDepth, useNA = 'always')
  # table(rais_estabs_geocode_end$geocode_engine, useNA = 'always')
  # table(rais_estabs_geocode_end$type_input_galileo, useNA = 'always')
  
  
  # filter(rais_estabs_geocode_end, is.na(geocode_engine)) %>% View()
  
  # save it
  write_rds(rais_estabs_geocode_end, sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_geocoded_all.rds", ano, ano))
  
}




rais_bring_schools <- function(ano) {
  
  # 6) Trazer informacoes de funcionarios de escolas publicas do censo escolar --------------------------
  # (a partir do script 01.3-educacao)
  
  
  # abri rais corrigida
  rais <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_estabs_%s_geocoded_all.rds", ano, ano))
  
  
  table(rais$PrecisionDepth, useNA = 'always')
  table(rais$geocode_engine, useNA = 'always')
  table(rais$type_input_galileo, useNA = 'always')
  
  
  # abrir censo escolar geo
  escolas <- read_rds("../../data/acesso_oport/censo_escolar/2018/educacao_inep_2018.rds") %>%
    # Deletar escolas q nao foram localizadas
    dplyr::filter(!is.na(lat)) %>%
    # Selecionar variaveis
    dplyr::select(id_estab = CO_ENTIDADE, codemun = CO_MUNICIPIO, lon, lat, total_corrigido = QT_FUNCIONARIOS)
  
  
  # pegar distribuicao de nivel des escolaridade da cidade
  rais_prop_escol <- rais %>%
    group_by(codemun) %>%
    summarise(prop_alto = sum(alto)/sum(total_corrigido),
              prop_medio = sum(medio)/sum(total_corrigido),
              prop_baixo = sum(baixo)/sum(total_corrigido))
  
  # trazer proporcoes para as escolas
  escolas_prop <- escolas %>%
    # ajeitar codemun
    mutate(codemun = substr(codemun, 1, 6)) %>%
    left_join(rais_prop_escol, by = "codemun") %>%
    # multiplicar totais por composicao
    mutate(alto = round(prop_alto * total_corrigido),
           medio = round(prop_medio * total_corrigido),
           baixo = round(prop_baixo * total_corrigido)) %>%
    select(id_estab, codemun, lon, lat, alto, medio, baixo, total_corrigido)
  
  setDT(escolas_prop)[, geocode_engine := "galileo"]
  escolas_prop[, PrecisionDepth := "4 Estrelas"]
  escolas_prop[, type_input_galileo := "inep"]
  
  # juntar rais com escolas proporcionais
  rais2 <- rbind(rais, escolas_prop, fill = T)
  
  
  # pad everyone to 14 characters
  rais2[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  
  
  # table(rais2$PrecisionDepth, useNA = 'always')
  # table(rais2$geocode_engine, useNA = 'always')
  # table(rais2$type_input_galileo, useNA = 'always')
  
  
  # Salvar
  write_rds(rais2, sprintf("../../data/acesso_oport/rais/%s/rais_%s_corrigido_geocoded_censoEscolar.rds", ano, ano))
  
  
  
  
}
