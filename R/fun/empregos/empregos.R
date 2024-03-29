# carregar bibliotecas
# source('./R/fun/setup.R')


#' Pegar os dados brutos da RAIS do servidor do IPEA, filtrar pra somente os 
#' municipios do projeto e salvar na pasta data-raw
#' Etapas:
#' 1) Abrir dados
#' 2) Filtar municipios do projeto
#' 3) Salvar

rais_filter_raw_data <- function(ano) {
  
  
  # 1) Dados dos estabelecimentos
  # 1.1) Abrir dados
  estabs <- fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/estab%s.csv', ano)
                  #, nrows = 5
                  , colClasses='character'
                  , select = 1:33)
  
  # 1.2) Subset municipalities
  # extract code muni
  code_munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% 
    unlist() %>% substring(., 1, 6)
  estabs_mun <- estabs[ codemun %in% code_munis]
  
  # 1.3) Subset columns that we use
  estabs_mun <- estabs_mun[, 1:33]
  
  # 1.4) Salvar
  fwrite(estabs_mun, sprintf('../../data-raw/rais/%s/rais_estabs_raw_%s.csv', ano, ano))
  
  
  rm(estabs_mun, estabs)
  gc(reset = T)
  
  # 2) Dados dos trabalhadores
  # 2.1) Primeiro, ler somente um subset dos dados da RAIS para selecionar colunas
  # de interesse
  colnames <- data.table::fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil%s.csv', ano)
                                , colClasses='character'
                                , nrows = 10
  ) %>% colnames()
  
  # select columns
  columns <- c("id_estab", "grau_instr", colnames[colnames %like% "nat_jur"], "emp_31dez", 'clas_cnae20', 'uf', 'codemun')
  
  # 2.2) Abrir dados dos trabalhadores 
  trabal <- fread(sprintf('//storage6/bases/DADOS/RESTRITO/RAIS/csv/brasil%s.csv', ano)
                  , select = columns
                  , colClasses='character')
  
  # 2.3) Renamear nat_jur
  colnames(trabal) <- c("id_estab", "grau_instr", "nat_jur", "emp_31dez", 'clas_cnae20', 'uf', 'codemun')
  
  
  # 2.3) Subset municipalities
  trabal <- trabal[ codemun %in% code_munis]
  
  
  # 2.4) Salvar
  fwrite(trabal, sprintf('../../data-raw/rais/%s/rais_trabal_raw_%s.csv', ano, ano))
  
}


#' Filtrar a base de pessoas, selecionando somente os vinculos ativos e deletando
#' todos os empregos que de alguma forma
#' sejam publicos
#' Etapas:
#' 1) Abrir Rais pessoas
#' 2) Filtro 1: selecionar so vinculos ativos
#' 3) Filtro 2: deletar todas as intituicoes com Natureza Juridica 'publica' 
#' 4) Filtro 3: deletar todas as empresas publicas 
#' 5) Salvar

rais_filter_pessoas <- function(ano) {
  
  # 1) Abrir Rais pessoas com colunas que vamos usar
  rais_trabs <- data.table::fread(sprintf('../../data-raw/rais/%s/rais_trabal_raw_%s.csv', ano, ano),
                                  colClasses='character'
  )
  
  # unique(rais_trabs$id_estab) %>% length()
  
  
  # 2) Filtro 1: selecionar so vinculos ativos
  rais_filtro_1 <- rais_trabs[emp_31dez == 1]
  
  # number of companies
  # unique(rais_filtro_1$id_estab) %>% length()
  
  
  # 3) Filtro 2: deletar todas as intituicoes com Natureza Juridica 'publica' 
  # (ver ../data-raw/rais/ManualRAIS2018.pdf) pagina 19
  # todos que comecam com o numeral 1 sao de administracao publica
  
  # 3.1) Identificar natureza de adm publica
  rais_filtro_1[, adm_pub := ifelse( substr(nat_jur, 1, 1)==1, 1, 0)]
  
  # 3.2) Filtrar apenas o que nao for adm publica
  rais_filtro_2 <- rais_filtro_1[ adm_pub != 1 ]
  
  # quantos vinculos de natureza juridica publica a gente perde?
  # nrow(rais_filtro_2) / nrow(rais_filtro_1)
  
  # 4) Filtro 3: deletar todas as empresas publicas (a entidade 2011 eh empresa publica)
  rais_filtro_3 <- rais_filtro_2[nat_jur != "2011"]
  
  
  # 5) Salvar
  rais_filtro_3 <- rais_filtro_3[order(id_estab, grau_instr, nat_jur)]
  write_rds(rais_filtro_3, sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa0_trabal_filtrada.rds", ano, ano), compress ='gz')
  
}





#' Categorizar trabalhadores por grau de instrucao
#' Essa funcao categoriza os trabalhores por grau de instrucao, de acordo com
#' sua escolaridade
#' Etapas:
#' 1) Abrir RAIS
#' 2) Categorizar trabalhadores por grau de instrucao por empresa
#' 3) Calcular quantidade de vinculo por grau de instrucao
#' 4) Somar quantidade total de empregados
#' 5) Reshape da base de formato long para wide
#' 6) Salvar

rais_categorize_inst <- function(ano) {
  
  # 1) Abrir RAIS anteior
  rais_trabs <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa0_trabal_filtrada.rds", ano, ano))
  
  # Formatar corretamente o grau de instrucao
  rais_trabs[, grau_instr := as.numeric(grau_instr)]
  rais_trabs[, grau_instr := as.character(grau_instr)]
  
  # 2) Categorizar trabalhadores por grau de instrucao
  rais_cats <- rais_trabs[, instrucao := fifelse(grau_instr %in% c(1:6), "baixo",                                    # menor do que ensino medio (inclui ensino medio incompleto)
                                                 fifelse(grau_instr %in% c(7, 8), "medio",                           # ensino medio
                                                         fifelse(grau_instr %in% c(9, 10, 11), "alto", grau_instr)))] # ensino superior
  
  
  # 3) Calcular quantidade de vinculo por grau de instrucao em cada estabelecimento
  rais_fim <- rais_trabs[, .(vinculos = .N), by = .(id_estab, clas_cnae20, instrucao)]
  
  # 4) Somar quantidade total de empregados em cada empresa
  rais_fim[, total := sum(vinculos), by = .(id_estab, clas_cnae20)]
  
  # 5) Reshape da base de formato long para wide
  rais_fim_wide <- tidyr::spread(rais_fim, instrucao, vinculos, fill = 0)
  
  # 6) Salvar
  write_rds(rais_fim_wide, sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa1_instrucao.rds", ano, ano), compress = 'gz')
  
}




#' Limpar outliers da RAIS
#' Muitos empresas de certos CNAES declaram muitos trabalhadores na mesma sede
#' Esse script traca um limite maximo de trabalhadores por empresa para essas CNAEs 
#' Etapas:
#' 1) Abrir RAIS
#' 2) Corrigir total de vínculos de ouliers de setores problemáticos da CNAE
#' 3) Corrige vínculos de setores problemáticos por grau de escolaridade (mantem composicao educacional da empresa)
#' 4) Salvar

rais_treat_outliers <- function(ano) {
  
  # 1) Abrir rais da etapa anterior
  rais <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa1_instrucao.rds", ano, ano))
  setDT(rais)
  
  
  # 2) Corrigir total de vínculos de outliers de setores problemáticos da CNAE 
  # Extrai o setor da CNAE
  rais[, cnae.setor := substr(clas_cnae20, 1, 2)]
  rais[, cnae.subsetor := substr(clas_cnae20, 1, 3)]
  
  # Setores considerados problemáticos:
  # * 35 ELETRICIDADE, GÁS E OUTRAS UTILIDADES
  # * 36 CAPTAÇÃO, TRATAMENTO E DISTRIBUIÇÃO DE ÁGUA
  # * 38 COLETA, TRATAMENTO E DISPOSIÇÃO DE RESÍDUOS; RECUPERAÇÃO DE MATERIAIS
  # * 41 CONSTRUÇÃO DE EDIFÍCIOS
  # * 42 OBRAS DE INFRA-ESTRUTURA
  # * 43 SERVIÇOS ESPECIALIZADOS PARA CONSTRUÇÃO
  # * 49 TRANSPORTE TERRESTRE
  # * 51 TRANSPORTE AÉREO
  # * 56.2 Serviços de catering, bufê e outros serviços de comida preparada
  # * 64 ATIVIDADES DE SERVIÇOS FINANCEIROS
  # * 78 SELEÇÃO, AGENCIAMENTO E LOCAÇÃO DE MÃO-DE-OBRA
  # * 80 ATIVIDADES DE VIGILÂNCIA, SEGURANÇA E INVESTIGAÇÃO
  # * 81 SERVIÇOS PARA EDIFÍCIOS E ATIVIDADES PAISAGÍSTICAS
  # * 82 SERVIÇOS DE ESCRITÓRIO, DE APOIO ADMINISTRATIVO E OUTROS SERVIÇOS PRESTADOS PRINCIPALMENTE ÀS EMPRESAS
  # * 84 ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL
  cnaes_problema <- c("35", "36", "38", "41", "42", "43", "49", "51", "64", "78", "80", "81", "82", "84")
  
  rais[, cnae_problema := ifelse(cnae.setor %in% cnaes_problema, cnae.setor,
                                 ifelse(cnae.subsetor == '562', '562', '')) ]
  
  
  
  # Definição de outlier: estabelecimentos de setores problemáticos cujo total de
  # vínculos fique acima do percentil 95 da distribuição de vínculos daquele setor
  rais[, p95_setor := round(quantile(total, probs = 0.95), 0), by = .(cnae_problema)]
  
  # Cria coluna total_corrigido com total de vínculos corrigidos
  # Caso o estabelecimento de um setor problemático tenha mais vínculos que o p95 
  # daquele setor, o total corrigido será igual ao p95 do setor. Caso não seja de 
  # um setor problemático, e caso não tenha mais vínculos que o p95, o total 
  # permanece igual
  rais[, total_corrigido := total]
  rais[
    cnae_problema != "", 
    total_corrigido := fifelse(total_corrigido >= p95_setor, p95_setor, total_corrigido)
  ]
  
  
  # Remove a coluna que guarda o valor do p95
  rais[, p95_setor := NULL]
  
  
  # 3) Corrige vínculos de setores problemáticos por grau de escolaridade 
  # Substitui a quantidade de vínculos por grau de escolaridade (apenas em setores 
  # que tiveram seu total de vínculos corrigidos) de forma a manter a mesma 
  # proporção anterior
  rais[
    total_corrigido < total, 
    ":="(
      alto  = round( (alto / total )* total_corrigido, 0),
      medio = round( (medio / total) * total_corrigido, 0),
      baixo = round( (baixo / total) * total_corrigido, 0)
    )
  ]
  
  # 4) Salvar
  write_rds(rais, sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa2_corrigido.rds", ano, ano), compress = 'gz')
  
}





#' Trazer o geocode dos estabelecimentos para rais  ---------------------------------------
#' Etapas:
#' 1) Abrir a rais dos trabalhadores
#' 2) Abrir a rais dos estabs georeferenciados
#' 3) Juntar as duas bases
#' 4) Selecionar apenas enderecos encontrados com boa precisao
#' 5) Salvar

rais_bring_geocode <- function(ano) {
  
  # 1) Abrir a rais dos trabalhadores (agregada por estab) corrigida a ser georef
  rais_estabs <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa2_corrigido.rds", ano, ano))
  
  # 2) Abrir a rais dos estabs georeferenciados
  rais_estabs_geocode <- read_rds(sprintf("../../data/acesso_oport/rais/%s/geocode/rais_%s_filter_geocoded_gmaps.rds", ano, ano))
  
  # pad everyone to 14 characters
  rais_estabs[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  rais_estabs_geocode[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  # unique(rais_estabs$id_estab) %>% length()
  # unique(rais_estabs_geocode$id_estab) %>% length()
  
  
  # 3) Juntar as duas bases
  rais_estabs_geocode_end <- merge( rais_estabs,
                                    rais_estabs_geocode,
                                    by = "id_estab",
                                    sort = FALSE,
                                    all.x = TRUE
  )
  
  # table(rais_estabs_geocode_end$PrecisionDepth, useNA = 'always')
  # table(rais_estabs_geocode_end$geocode_engine, useNA = 'always')
  # table(rais_estabs_geocode_end$type_input_galileo, useNA = 'always')
  
  
  # 5) Salvar
  write_rds(rais_estabs_geocode_end, sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa3_geocoded.rds", ano, ano), compress = 'gz')
  
}





#' Trazer dados do censo escolar 
#' Trazer os empregos das escolas para complementar a base da RAIS
#' Etapas:
#' 1) Abrir RAIS
#' 2) Abrir censo escolar georeferenciado
#' 3) Calcular a distribuicao de nivel de escolaridade da RAIS na cidade
#' 4) Trazer proporcoes para as escolas
#' 5) Criar classificaoes de geocode
#' 6) Juntar rais com escolas proporcionais
#' 7) Salvar

rais_bring_schools <- function(ano) {
  
  # 1) Abrir RAIS corrigida
  rais <- read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected.rds",
                           ano, ano))
  
  # table(rais$PrecisionDepth, useNA = 'always')
  # table(rais$geocode_engine, useNA = 'always')
  # table(rais$type_input_galileo, useNA = 'always')
  
  
  # 2) Abrir censo escolar georeferenciado
  
  if (ano == 2017) {
    
    escolas <- read_rds("../../data/acesso_oport/educacao/2017/educacao_2017_filter_geocoded_gmaps_gquality_corrected2.rds") %>%
      # Deletar escolas q nao foram localizadas
      dplyr::filter(!is.na(lat)) %>%
      # Selecionar variaveis
      dplyr::select(id_estab = co_entidade, code_muni, lon, lat, 
                    total_corrigido = nu_funcionarios,
                    geocode_engine, Addr_type)
    
  } else if (ano %in% c(2018, 2019)) {
    
    escolas <- read_rds("../../data/acesso_oport/educacao/2018/educacao_2018_filter_geocoded_gmaps_gquality_corrected2.rds") %>%
      # Deletar escolas q nao foram localizadas
      dplyr::filter(!is.na(lat)) %>%
      # Selecionar variaveis
      dplyr::select(id_estab = co_entidade, code_muni, lon, lat, 
                    total_corrigido = nu_funcionarios,
                    geocode_engine, Addr_type)
    
  }
  
  
  
  # 3) Calcular a distribuicao de nivel de escolaridade da cidade
  rais_prop_escol <- rais %>%
    group_by(code_muni) %>%
    summarise(prop_alto = sum(alto)/sum(total_corrigido),
              prop_medio = sum(medio)/sum(total_corrigido),
              prop_baixo = sum(baixo)/sum(total_corrigido)) %>%
    mutate(code_muni = as.character(code_muni))
  
  # 4) Trazer proporcoes para as escolas
  escolas_prop <- escolas %>%
    # ajeitar code_muni
    mutate(code_muni = substr(code_muni, 1, 6)) %>%
    left_join(rais_prop_escol, by = "code_muni") %>%
    # multiplicar totais por composicao
    mutate(alto = round(prop_alto * total_corrigido),
           medio = round(prop_medio * total_corrigido),
           baixo = round(prop_baixo * total_corrigido)) %>%
    select(id_estab, code_muni, lon, lat, alto, medio, baixo, total_corrigido, 
           geocode_engine, Addr_type)
  
  # 5) Criar classificaoes de geocode
  setDT(escolas_prop)[, type_year_input := "inep_google"]
  
  # 6) Juntar rais com escolas proporcionais
  rais2 <- rbind(rais, escolas_prop, fill = T)
  
  # pad everyone to 14 characters
  rais2[, id_estab := str_pad(id_estab, width = 14, pad = 0)]
  
  
  
  # table(rais2$PrecisionDepth, useNA = 'always')
  # table(rais2$geocode_engine, useNA = 'always')
  # table(rais2$type_input_galileo, useNA = 'always')
  
  
  # 7) Salvar
  write_rds(rais2, sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality_corrected_escola.rds", ano, ano),
            compress = "gz")
  
  
}
