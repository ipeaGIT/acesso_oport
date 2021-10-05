

# abrir shhet  com os hex problematicos
sheet <- "https://docs.google.com/spreadsheets/d/19RXGIcjKNGaJAJdmy8HrkuVYss0b99lGD0HNQBeJ2n8/edit#gid=0"

### funcao para diagnostico da distribuicao de atividades 9uso do solo) por hexagonos
# A funcao retorna um data.frame / sf com o CNPJ e quantidade de empregos das empresas em cada hexagono.


# primeiro, criar uma spreadsheet para cada cidade
# create_move_ss <- function(sigla_munii) {
#   
#   ss <- gs4_create(name = sprintf("aop_checagem_%s", sigla_munii),
#                    sheets = c('2017', '2018', '2019', '2020', '2021', '2022'))
#   
#   # move
#   googledrive::drive_mv(file = as_id(ss),
#                         path = "~/aop_checagem_estabs/")
#   
#   # create df
#   df <- data.frame(
#     ss = as.character(ss),
#     sigla_muni = sigla_munii
#   )
#   
#   
# }

# df_ss <- map_dfr(munis_list$munis_df$abrev_muni, create_move_ss)

a <- googledrive::drive_ls("~/aop_checagem_estabs/")
df_ss <- data.frame(ss = a$id,
                    sigla_muni = stringr::str_sub(a$name, -3, -1))


googlesheets4::gs4_auth()

diagnost_hex_uso_solo <- function(ano1, uso_do_solo = "empregos") {
  
  # ano1 <- "2017"; uso_do_solo <- "empregos"; sigla_munii <- "for"
  # ano1 <- "2019"; uso_do_solo <- "empregos"; sigla_munii <- "sgo"
  # ano1 <- "2019"; uso_do_solo <- "empregos"; sigla_munii <- "duq"
  # ano1 <- "2019"; uso_do_solo <- "educacao"; sigla_munii <- "duq"
  # ano1 <- "2019"; uso_do_solo <- "educacao"; sigla_munii <- "goi"
  # ano1 <- "2019"; uso_do_solo <- "saude"; sigla_munii <- "goi"
  # ano1 <- "2017"; uso_do_solo <- "educacao"
  
  
  if (uso_do_solo == "empregos") {
    
    
    rais <- readr::read_rds(sprintf("../../data/acesso_oport/rais/%s/rais_%s_etapa4_geocoded_gmaps_gquality.rds", ano1, ano1))
    rais <- rais[!is.na(lat), ]
    rais_vars <- fread(sprintf('../../data-raw/rais/%s/rais_estabs_raw_%s.csv', ano1, ano1),
                       select = c("id_estab", "razao_social"))
    rais_vars[, id_estab := stringr::str_pad(id_estab, width = 14, pad = 0)]
    rais_vars <- distinct(rais_vars, id_estab, .keep_all = TRUE)
    # juntar
    rais <- left_join(rais, rais_vars, by = "id_estab") %>% setDT()
    rais <- rais %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    rais <- rais %>% select(id_estab, cnae.setor, qt_vinc_ativo = total_corrigido, razao_social,
                            logradouro, bairro, codemun, name_muni, uf, cep, Addr_type,
                            Score, Status, matched_address, lon, lat, type_year_input, geocode_engine)
    
  } else if (uso_do_solo == "educacao") {
    
    escolas <- read_rds(sprintf("../../data/acesso_oport/%s/%s/%s_%s_filter_geocoded_gmaps_gquality.rds", 
                       "educacao", ano1, "educacao", ano1))  %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    
  } else if (uso_do_solo == "saude") {
    
    cnes <- read_rds(sprintf("../../data/acesso_oport/%s/%s/%s_%s_filter_geocoded_gmaps_gquality.rds", 
                                "saude", ano1, "saude", ano1))  %>%
      select(cnes, uf, municipio, ibge, estabelecimento, 
             logradouro, bairro, cep, matched_address, Score, Addr_type, lon, lat, type_year_input,
             geocode_engine) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  }
  
  
  
  hex_probs <- googlesheets4::read_sheet(ss = sheet,
                                         sheet = sprintf("hex_prob_%s", uso_do_solo),
                                         col_types = "c",
                                         range = "A:D") %>%
    mutate(ano = as.character(ano))
  
  diagnost_hex_uso_solo_muni <- function(sigla_munii) {
    
    # sigla_munii <- "rio";
    # sigla_munii <- "cur";
    # sigla_munii <- "sal";
    # sigla_munii <- "nat"
    
    # Qual o codigo do municipio em questao?
    cod_mun_ok <- munis_list$munis_metro[abrev_muni == sigla_munii & ano_metro == ano1]$code_muni %>% 
      unlist()
    
    # filter hex probs
    id_hex_prob <- hex_probs %>%
      filter(sigla_muni %in% sigla_munii) %>%
      filter(str_detect(ano, pattern = as.character(ano1)))
    
    if (uso_do_solo == "empregos") {
      
      base <- rais %>% 
        filter(codemun %in% substr(cod_mun_ok, 1, 6))
      
    } else if (uso_do_solo == "educacao") {
      
      base <- escolas %>%
        filter(code_muni %in% cod_mun_ok)
      
    } else if (uso_do_solo == "saude") {
      
      base <- cnes %>% filter(ibge %in% substr(cod_mun_ok, 1, 6))
    }
    
    
    df_apoio <- data.frame(
      us = c("empregos", "educacao", "saude"),
      var = c("empregos_total", "edu_total", "saude_total"),
      stringsAsFactors = FALSE
    )
    
    # pegar nome da variavel
    var_go <- subset(df_apoio, us == uso_do_solo)$var
    
    # ler hex agregados
    hex <- read_rds(sprintf("../../data/acesso_oport/hex_agregados_check_geocode/%s/hex_agregado_%s_09_%s.rds", ano1, sigla_munii, ano1)) %>%
      select(id_hex, UQ(as.symbol(var_go))) %>%
      # filter hex problematico
      filter(id_hex %in% id_hex_prob$id_hex)
    
    
    # base %>% filter(id_estab == "09652823000508")
    
    # Intersecao do hex ids problema com base de uso do solo
    fim <- st_join(hex, base) %>% st_set_geometry(NULL) %>%
      mutate(sigla_muni = sigla_munii) %>%
      select(sigla_muni, everything()) %>%
      mutate(ano = ano1) %>%
      # criar link do gmaps
      mutate(link_gmaps = sprintf("https://www.google.com/maps/search/?api=1&query=%s,%s", lat, lon))
      
    if (uso_do_solo == "empregos") fim <- arrange(fim, id_hex, desc(qt_vinc_ativo))
    
    
    # pegar o ss do shpeadsheet da cidade
    ss_cidade <- df_ss %>% filter(sigla_muni == sigla_munii) %>% pull(ss)
    
    # write data to the spreadsheet
    googlesheets4::write_sheet(
      ss = ss_cidade,
      data = fim, 
      sheet = ifelse(uso_do_solo == "empregos", as.character(ano1), paste0(as.character(ano1),"_",uso_do_solo)))
    
    
    
  }
  
  # munis_list$munis_df$abrev_muni
  # walk(munis_list$munis_df$abrev_muni, diagnost_hex_uso_solo_muni)
  walk(c("sal", "mac", "bel"), diagnost_hex_uso_solo_muni)
  
}


diagnost_hex_uso_solo(ano1 = 2017, uso_do_solo = "empregos")
diagnost_hex_uso_solo(ano1 = 2018, uso_do_solo = "empregos")
diagnost_hex_uso_solo(ano1 = 2019, uso_do_solo = "empregos")

diagnost_hex_uso_solo(ano1 = 2017, uso_do_solo = "educacao")
diagnost_hex_uso_solo(ano1 = 2018, uso_do_solo = "educacao")
diagnost_hex_uso_solo(ano1 = 2019, uso_do_solo = "educacao")

diagnost_hex_uso_solo(ano1 = 2017, uso_do_solo = "saude")
diagnost_hex_uso_solo(ano1 = 2018, uso_do_solo = "saude")
diagnost_hex_uso_solo(ano1 = 2019, uso_do_solo = "saude")
