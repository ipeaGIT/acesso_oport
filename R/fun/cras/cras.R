#> Esse script faz Download, Limpeza dos dados brutos e geolocalização dos CRAS

#> Após o download dos dados brutos, a função cras_geocode tem 6 etapas:

#. 1.) Leitura dos Dados Originais
#. 2.) Filtra os CRAS localizados nas cidades do projeto, renomeia e recodifica variáveis
#. 3.) Corrige coordenadas defeituosas: baseado no script fun/saude.R, ajusta os dados brutos para o nº
#      correto de caracteres, posição da vírgula etc, e identifica dois tipos de erros: (i) coordenadas 
#      com menos de 2 dígitos após a vírgula; (ii) CRAS diferentes na mesma lat/lon
#. 4.) Gera dois outputs: uma base intermediária com os endereços e ok e outra com os erros identificados em 3.)
#      para geolocalização
#. 5.) Geocode dos endereços com problema: checa quais endereços já foram geolocalizados em anos anteriores (arquivo
#       geocode_cras.csv) e faz o geocode via Google Maps dos remanescentes.
#. 6.) Salva dois arquivos: (i) cras_ano.csv, com todas as informações dos CRAS; (ii) geocode-cras.csv atualizado com
#      os novos endereços geolocalizados.


cras_geocode <- function(ano, run_gmaps = F) {
  
  # 1) Ler os dados da base do SUAS baixado do site do MDS ---------------------------------
  
  if(ano == '2019') {
    
    cras <- data.table::fread('../../data-raw/CRAS/2019/CRAS/Censo_SUAS_2019_dados_gerais_RH_CRAS_divulga‡Æo.csv',
                                   select = c("NU_IDENTIFICADOR","q0_1","q0_2", "q0_3","q0_4","q0_6","q0_8","q0_9","q0_10", "q0_11",
                                              "q0_12","q0_15", 'Latitude', 'Longitude', 'q39'))
    
  } else if (ano == '2018') {
    
    cras <- data.table::fread('../../data-raw/CRAS/2018/1.CRAS/Censo_SUAS_2018_CRAS_Dados_Gerais_divulgacao.csv',
                              select = c("NU_IDENTIFICADOR","ident_0_1","ident_0_2","ident_0_3","ident_0_4","ident_0_6",
                                          "ident_0_8","ident_0_9","ident_0_10", "ident_0_11",
                                          "ident_0_12","ident_0_15", 'Latitude', 'Longitude', 'q_40'))
  
  }  else if (ano == '2017') {
    
    cras <- data.table::fread('../../data-raw/CRAS/2017/Censo_SUAS_2017_CRAS/Censo SUAS 2017_CRAS_divulgacao_Base de dados.csv',
                              select = c("NºIDENTIFICADOR","ident.1.Nome","ident.2.TPLog","ident.3.Endereço","ident.4.Núm",
                                         "ident.6.Bairro","ident.8.CEP","IBGE7","ident.10.UF", "ident.11.Email",
                                         "ident.12.Tel","ident.15.DTImp", 'Latitude', 'Longitude', 'q35'))
    
  }
  
  # 2) Renomeia colunas, filtra cidades e recodifica variáveis ---------------------------------
  
  # renomeia colunas
  data.table::setnames(cras,
                       old = names(cras),
                       new = c("code_cras","name_suas","tp_log","logradouro","numero","bairro","cep","code_muni", 'code_uf','email',
                               "telefone","open_date",'lat_suas','lon_suas', 'cad_unico'))
  
  # filtra somente cras nos municipios do projeto
  cras <- data.table::setDT(cras, key = 'code_cras')[code_muni %in% munis_df$code_muni]
  
  # traz info dos municipios para dados dos CRAS
  cras <- data.table::merge.data.table(cras,
                                       munis_df[,.(name_muni,code_muni,abrev_estado)],
                                       all.x = TRUE,
                                       by = 'code_muni')
  head(cras)
  
  
  # Recodifia tipo de logradouro e indicador de Cadastramento no CADUnico
  
  if (ano == '2019'){
    
    cras[, tp_log := fcase(
      tp_log == 3, "Avenida", tp_log == 32, "Rua", tp_log == 35, "Travessa", tp_log == 12, "Estrada",
      tp_log == 2, "Área", tp_log == 1, "Alameda", tp_log == 40, "Via", tp_log == 31, "Rodovia",
      tp_log == 8, "Conjunto", tp_log == 27, "Praça", tp_log == 28, "Quadra", tp_log == 38, "Travessa", 
      tp_log == 12, "Estrada", tp_log == 21, "Loteamento", tp_log == 20, "Largo")]
    
    cras[, cad_unico := ifelse(cad_unico == 0, 'Não', 'Sim')]
    
  } else if (ano != '2019'){
    
    # se faz cadastro do cadUnico
    cras[, cad_unico := stringr::str_sub(cad_unico,1,3)]
    
  }
  

  # Recodifica endereços sem número
  cras[, numero := abs(numero)]
  cras[, numero := as.character(numero)]
  cras[, numero := gsub('^0','S/N', numero)]
  
  # Cria coluna com endereço completo
  cras[, endereco := paste(paste(tp_log, logradouro, sep = ' '),numero,sep = ', ')]
  table(cras$endereco)
  
  # Endereco em maiusculo, email em minusculo
  cras[, endereco := stringr::str_to_upper(endereco)]
  cras[, bairro := stringr::str_to_upper(bairro)]
  
  # Remove tipo de logradouro duplicado
  cras[, endereco := gsub('RUA RUA','RUA', endereco)]
  cras[, endereco := gsub('AVENIDA AVENIDA','AVENIDA', endereco)]
  cras[, endereco := gsub('QUADRA QUADRA','QUADRA', endereco)]
  cras[, endereco := gsub('ÁREA ÁREA','ÁREA', endereco)]
  

  
  # 3) Corrige coordenadas defeituosas ------------------------------------------------------------------
  
  # identificar numero de digitos de lat antes da virgula pra cada cidade do projeto
  munis <- purrr::map_dfr(dir("../../data-raw/municipios/2017/", full.names = TRUE), read_rds) %>%
    as_tibble() %>%
    st_sf() %>%
    st_centroid() %>%
    # transformar para lon-lat %>%
    sfc_as_cols() %>%
    # quantos digitos as latitudes tem antes da virgula?
    mutate(lat_digits = sub("^-?(\\d+)[[:punct:]]{1}\\d+$", "\\1", lat)) %>%
    mutate(lat_digits = nchar(lat_digits)) %>%
    # selecionar so as colunas necessarias
    dplyr::select(code_muni, lat_digits) %>% 
    mutate(code_muni = as.integer(code_muni))
    
  # criar dataframe com as coordenadas ajeitadas
  cras_fixed <- cras %>%
    # Seleciona Colunas 
    select(code_muni,code_cras,lon_suas,lat_suas) %>% 
    # Left join com número de dígitos da latitude
    left_join(munis,
              by = 'code_muni') %>% 
    # primeiro, tirar tudo que for ponto ou virgula
    mutate(lon = gsub("(\\.|,)", "", lon_suas),
           lat = gsub("(\\.|,)", "", lat_suas)) %>%
    # tirar sinal de negativo
    mutate(lon = gsub("-", "", lon),
           lat = gsub("-", "", lat)) %>%
    # o ponto na longitude vai ser sempre depois do segundo numerico, e vai ser sempre negativo
    mutate(lon = sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lon)) %>%
    # o ponto na latitude vai depender do nchar
    mutate(lat = ifelse(lat_digits == 1, sub("(^\\d{1})(\\d+)", "-\\1\\.\\2", lat),
                        sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lat))) %>%
    # delete E+16 from coords
    mutate(lon = str_replace(lon, "E\\+\\d{2}", ""),
           lat = str_replace(lat, "E\\+\\d{2}", "")) %>%
    # delete undefined
    mutate(lon = ifelse(lon == "undefined", NA, lon),
           lat = ifelse(lat == "undefined", NA, lat)) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat))
  
  # Encontra coordenadas problemáticas 
  # Número de Dígitos após o ponto (menos de 3 digitos apos pontos)
  setDT(cras_fixed)
  cras_fixed[, ndigitos_lat := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  cras_fixed[, ndigitos_lon := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lon))]
  cras_fixed[, ndigitos := pmin(ndigitos_lon, ndigitos_lat) , by= code_cras ]
  
  # Número de CRAS no mesmo ponto
  cras_fixed <- cras_fixed %>%
    group_by(lat, lon) %>%
    mutate(cras_rep = n()) %>%
    ungroup()
  
  # Indicador se geocode está ok
  setDT(cras_fixed)[, check := ifelse(ndigitos <= 2 | cras_rep > 1, 0, 1)]
  table(cras_fixed$check)
  
  
  # 4) Merge com dataframe original, output intermediário e input geocode -------------------------------------------
  cras <- merge.data.table(cras, 
                           cras_fixed[,.(code_cras,lon,lat,check)],
                           all.x = TRUE,
                           by = 'code_cras')
  
  intermediate <- cras[check == 1, .(code_cras,name_suas,email,telefone,code_muni,endereco,bairro,cep,name_muni,abrev_estado,lon,lat)]
  cras_wrong <- cras[check == 0, .(code_cras,name_suas,email,telefone,code_muni,endereco,bairro,cep,name_muni,abrev_estado)]
  

  # 5) Geocode CRAS defeituosos --------------------------------------------
  
  # Verifica se algum CRAS defeituoso já foi geocodificado em anos anteriores
  
  # Lê historico de todos CRAS que estiveram errados e que um dia foram corrigidos
  geocode_hist <- data.table::fread('../../data/acesso_oport/cras/geocode_cras.csv')
  
  # Merge com CRAS defeituosos
  cras_wrong <- merge.data.table(cras_wrong, geocode_hist, all.x = TRUE, by = 'code_cras')
  
  # CRAS que ja foram corrigidos em rodadas passadas
  cras_ok <- cras_wrong[!is.na(lat)]
  
  # Input geocode dos CRAS que ainda nao foram corrigidos
  input_geocode <- cras_wrong[is.na(lat)]
  
  # prepara string de endereços - input para google maps
  input_geocode[, input := 
               paste(paste(paste(paste(endereco, bairro,sep = " - "),name_muni, sep=", "),abrev_estado,sep=" - "),cep,sep=", CEP ")]
  
  # lista de enderecos com problema
  input <- input_geocode$input
  
  # registrar Google API Key
  my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
  register_google(key = my_api$V1[3])
  
  if (run_gmaps) {
      
      message("Running gmaps, this may take a while")
      
      coords_google <- lapply(input,geocode) %>% data.table::rbindlist()
      
      input_geocode <- input_geocode[, -c('lat', 'lon')] %>% dplyr::bind_cols(coords_google) %>% select(-input)
      
      cras_ano <- dplyr::bind_rows(intermediate, cras_ok, input_geocode) %>% setDT(key = 'code_cras')
      
  } else {
      
    # Arquivo final
    cras_ano <- dplyr::bind_rows(intermediate, cras_ok) %>% setDT(key = 'code_cras')
    
    }
    
     # 6) arquivos Finais  --------------------------------------------
  
    # Update do histórico de geocode
    update <- cras_ano[code_cras %nin% geocode_hist$code_cras,.(code_cras,lon,lat)]
    geocode_hist_final <- dplyr::bind_rows(geocode_hist, update) %>% setDT(key = 'code_cras')
    
    # Salva arquivos
    data.table::fwrite(cras_ano, sprintf("../../data/acesso_oport/cras/cras_%s.csv", ano))
    data.table::fwrite(geocode_hist_final, "../../data/acesso_oport/cras/geocode_cras.csv")
    
  }