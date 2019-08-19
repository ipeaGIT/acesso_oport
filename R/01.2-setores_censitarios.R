#' ## Setores censitários
#' 
#' 
## ----setores_shapes------------------------------------------------------

# ajeitar os setores


arquivos <- dir("../data-raw/municipios", full.names = T, pattern = "*censitarios.zip$", recursive = T)

out_dir <- paste0("../data-raw/municipios/", str_sub(arquivos, -26, -25))

walk2(arquivos, out_dir, ~unzip(zipfile = .x, exdir = .y))

# # criar pastas
# walk(str_sub(arquivos, -17, -16), ~dir.create(paste0("../data/municipios/", .)))

# nome dos arquivos .shp para abrir
arquivos_shp <- dir("../data-raw/municipios", full.names = T, pattern = "^\\d{2}SEE.+.shp$", recursive = T)

# shp <- arquivos_shp[1]

# funcao

shp_to_rds <- function(shp) {
  
  shp_files <- st_read(shp, crs = 4326, options = "ENCODING=WINDOWS-1252") %>%
    dplyr::select(cod_setor = CD_GEOCODI, muni = NM_MUNICIP) %>%
    mutate(cod_setor = as.character(cod_setor)) %>%
    mutate(cod_setor = as.numeric(cod_setor))
  
  uf <- gsub(".+/(\\D{2})/.+", "\\1", shp)
  
  out_dir <- paste0("../data/setores/setores_", uf, ".rds")
  
  write_rds(shp_files, out_dir)
  
}


walk(arquivos_shp, shp_to_rds)



#' 
#' 
## ----setores_var---------------------------------------------------------

setores1 <- fread("../data-raw/setores_censitarios/dados_censo2010A.csv")

names(setores1)


# Renda 6.19 - variavel escolhida: V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
setores_renda <-  setores1 %>% 
  dplyr::select(cod_uf = Cod_UF, cod_muni = Cod_municipio, cod_setor = Cod_setor, renda_total = DomRend_V003)
  
# Moradores 6.3 - variavel escolhida: V002 = Moradores em domicílios particulares permanentes
setores_moradores <- setores1 %>% 
  dplyr::select(cod_setor = Cod_setor, moradores_total = Dom2_V002)

# juntar

setores_total <- setores_renda %>%
  left_join(setores_moradores, by = "cod_setor") %>%
  mutate(renda_per_capta = renda_total / moradores_total) %>%
  mutate(cod_setor = as.numeric(cod_setor))

# write_rds(setores_total, "../data/renda_por_setor/renda_por_setor.rds")


#' 
#' 
## ----setores_juncao------------------------------------------------------

setores_total <- read_rds("../data/renda_por_setor/renda_por_setor.rds")

# dividir por uf

ufs <- tibble::tribble(
  ~cod_uf,                     ~nome_uf,  ~uf,
   11,              "Rondônia", "RO",
   12,                  "Acre", "AC",
   13,              "Amazonas", "AM",
   14,               "Roraima", "RR",
   15,                  "Pará", "PA",
   16,                 "Amapá", "AP",
   17,             "Tocantins", "TO",
   21,              "Maranhão", "MA",
   22,                 "Piauí", "PI",
   23,                 "Ceará", "CE",
   24,   "Rio Grande do Norte", "RN",
   25,               "Paraíba", "PB",
   26,            "Pernambuco", "PE",
   27,               "Alagoas", "AL",
   28,               "Sergipe", "SE",
   29,                 "Bahia", "BA",
   31,          "Minas Gerais", "MG",
   32,        "Espírito Santo", "ES",
   33,        "Rio de Janeiro", "RJ",
   35,             "São Paulo", "SP",
   41,                "Paraná", "PR",
   42,        "Santa Catarina", "SC",
   43, "Rio Grande do Sul (*)", "RS",
   50,    "Mato Grosso do Sul", "MS",
   51,           "Mato Grosso", "MT",
   52,                 "Goiás", "GO",
   53,      "Distrito Federal", "DF"
  )

# adicionar coluna para uf

setores_total_v1 <- setores_total %>%
  # Join com a tabela com o codigo das ufs
  left_join(ufs, by = "cod_uf") %>%
  # Transformar em minusculo
  mutate(uf = tolower(uf)) %>%
  # Ordernar por uf
  arrange(uf) %>%
  # Criar uma lista com um data.frame por UF
  split(.$uf)

# abrir os shapes dos setores por uf

files <- dir("../data/setores/", full.names = TRUE)

# Abrir em forma de uma lista com um data.frame para cada UF
setores_shapes <- map(files, read_rds)

# Funcao para criar arquivo agregada (com as variaveis de renda e populacao) para uf

agregar_setores <- function(setores_variaveis1, setores_shapes1) {
  
  # Extrair a uf do data.frame em questao
  uf <- unique(setores_variaveis1$uf)[1]
  
  # Join os shapes com o data.frame das variaveis
  setores_fim <- setores_shapes1 %>%
    left_join(setores_variaveis1, by = "cod_setor")
  
  # salvar
  dir_out <- sprintf("../data/setores_agregados_uf/setores_agregados_%s.rds", uf)
  
  write_rds(setores_fim, dir_out)
  
  
}

# aplicar

walk2(setores_total_v1, setores_shapes, agregar_setores)



#' 
#' 
## ----setores_por_municipio-----------------------------------------------

# # nome do municipio completo, minusculo, sem acentos
# municipio_logname <- "brasilia"
# uf <- "ce"

setores_por_municipio <- function(municipio_logname, uf) {
  
  
  # Abrir setores da uf
  path_setor_uf <- sprintf("../data/setores_agregados_uf/setores_agregados_%s.rds", uf)
  setor_uf <- read_rds(path_setor_uf) %>%
    # transformar para minusculo o nome do municipio
    mutate(muni = tolower(muni)) %>%
    # tirar acentos do nome do municipio
    mutate(muni = rm_accent(muni))
  
  # # Abrir tabela com as siglas dos municipios
  # tabela_muni <- read_delim("../data-raw/tabela_muni_codigos_2010.csv", delim = ";", skip = 2, 
  #                           locale = locale(encoding = 'WINDOWS-1252')) %>%
  #   select(municipio, nome_municipio) %>%
  #   # Mudar para minusculo
  #   mutate(nome_municipio1 = tolower(nome_municipio)) %>%
  #   # Tirar acentos do nome do municipio
  #   # mutate(nome_municipio1 = rm_accent(nome_municipio1)) %>%
  #   mutate(nome_municipio1 = trimws(nome_municipio1))
  #   # # Determinar a sigla (tres primeiras letras)
  #   # mutate(nome_municipio1 = substr(nome_municipio1, 1, 3))
  # 
  # # Fazer juncao
  # muni_desejado <- tabela_muni %>%
  #   filter(nome_municipio1 == municipio_logname)
  
  setor_municipio <- setor_uf %>%
    filter(muni == municipio_logname)
  
  # Salvar
  muni_shortname <- substr(municipio_logname, 1, 3)
  path_out <- sprintf("../data/setores_agregados/setores_agregados_%s.rds", muni_shortname)
  write_rds(setor_municipio, path_out)
  
}

# Aplicar funcao

municipio_logname <- c("fortaleza", "rio de janeiro", "belo horizonte", "teresina", "porto alegre", "sao paulo", "curitiba")
ufs <- c("ce", "rj", "mg", "pi", "rs", "sp", "pr")

walk2(municipio_logname, ufs, setores_por_municipio)

# para brasilia
source("R/fun/setup.R")
setores_por_municipio("brasilia", "df")


#' 
