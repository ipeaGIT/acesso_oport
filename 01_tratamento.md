Tratamento dos dados - Acesso a Oportunidades
================
Ipea
19 de março de 2019

Tratamento dos dados brutos
===========================

Esse arquivo tem como objetivo tratar os dados brutos do projeto de acesso a oportunidades. As bases de dados tratadas aqui são:

-   `Municípios`;
-   `Setores Censitários`;
-   `Renda`;
-   `Censo escolar`;
-   `Grade censo`;
-   `Hospitais`;
-   `GTFS`;
-   `Elevação`

Municípios
----------

Os dados de município estão compactados em formato de *shapefile*, divididos por UF. O tratamento desse dados consiste em:

-   Descompactação dos arquivos;
-   Leitura dos shapefiles municipais;
-   Salvos em disco em formato `rds`.

``` r
# ajeitar os municipios


arquivos <- dir("../data-raw/municipios", full.names = T, pattern = "_municipios.zip", recursive = T)


out_dir <- paste0("../data-raw/municipios/", str_sub(arquivos, -17, -16))

walk2(arquivos, out_dir, ~unzip(zipfile = .x, exdir = .y))

# # criar pastas
# walk(str_sub(arquivos, -17, -16), ~dir.create(paste0("../data/municipios/", .)))

# nome dos arquivos .shp para abrir
arquivos_shp <- dir("../data-raw/municipios", full.names = T, pattern = "*.shp", recursive = T)

# # arquivo com output
# out_dir_data <- paste0("../data/municipios/", str_sub(arquivos, -17, -16))

# funcao

shp_to_rds <- function(shp) {
  
  shp_files <- st_read(shp, crs = 4326, options = "ENCODING=WINDOWS-1252")
  
  uf <- gsub(".+/(\\D{2})/.+", "\\1", shp)
  
  out_dir <- paste0("../data/municipios/municipios_", uf, ".rds")
  
  write_rds(shp_files, out_dir)
  
  
}


walk(arquivos_shp, shp_to_rds)
```

Setores censitários
-------------------

Primeiramente os shapes dos setores são dividos por UF e guardados em formato .rds:

``` r
# ajeitar os setores


arquivos <- dir("../data-raw/municipios", full.names = T, pattern = "_setores_censitarios.zip", recursive = T)

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
```

Em seguida, os dados estatísticos dos setores do censo são tratados: é feita a retirada das variáveis relacionadas à renda e à quantidade de pessoas, depois é feito o cálculo da renda por pessoa. A tabela resultante é salva como`renda_por_setor.rds`, e tem as variáveis para todos os setores juntos.

``` r
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
```

É feita então a junção dos shapes com as estatísticas, resultando numa base dos setores censitários georreferenciada, dividida por uf, com informações de renda e habitantes. A paste final `setores_agregados` contém todos os setores (com as variaveis de população e renda) com um arquivo para cada uf de nome `setores_agregados_UF.rds`.

``` r
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
  dir_out <- sprintf("../data/setores_agregados/setores_agregados_%s.rds", uf)
  
  write_rds(setores_fim, dir_out)
  
  
}

# aplicar

walk2(setores_total_v1, setores_shapes, agregar_setores)
```

Há a necessidade de filtrar os setores agregados por uf nas cidades dos projetos:

``` r
# nome do municipio completo, minusculo, sem acentos
municipio_logname <- "fortaleza"
uf <- "ce"

setores_por_municipio <- function(municipio_logname, uf) {
  
  # Abrir setores da uf
  path_setor_uf <- sprintf("../data/setores_agregados_uf/setores_agregados_%s.rds", uf)
  setor_uf <- read_rds(path_setor_uf)
  
  # Abrir tabela com as siglas dos municipios
  tabela_muni <- read_delim("../data-raw/tabela_muni_codigos_2010.csv", delim = ";", skip = 2, 
                            locale = locale(encoding = 'WINDOWS-1252')) %>%
    select(municipio, nome_municipio) %>%
    # Mudar para minusculo
    mutate(nome_municipio1 = tolower(nome_municipio)) %>%
    # Tirar acentos do nome do municipio
    # mutate(nome_municipio1 = rm_accent(nome_municipio1)) %>%
    mutate(nome_municipio1 = trimws(nome_municipio1))
    # # Determinar a sigla (tres primeiras letras)
    # mutate(nome_municipio1 = substr(nome_municipio1, 1, 3))
  
  # Fazer juncao
  muni_desejado <- tabela_muni %>%
    filter(nome_municipio1 == municipio_logname)
  
  setor_municipio <- setor_uf %>%
    filter(cod_muni %in% muni_desejado$municipio)
  
  # Salvar
  muni_shortname <- substr(municipio_logname, 1, 3)
  path_out <- sprintf("../data/setores_agregados/setores_agregados_%s.rds", muni_shortname)
  write_rds(setor_municipio, path_out)
  
}

# Aplicar funcao

municipio_logname <- c("fortaleza", "rio de janeiro", "belo horizonte", "recife", "porto alegre", "sao paulo", "curitiba")
ufs <- c("ce", "rj", "mg", "pe", "rs", "sp", "pr")

walk2(municipio_logname, ufs, setores_por_municipio)

setores_por_municipio("teresina", "pi") 
```

Censo escolar
-------------

Dentre todas as variáveis disponíveis no censo escolar foram escolhidas as seguintes:

-   `cod_escola`: código único da escola;
-   `uf`: sigla da uf em questão;
-   `municipio`: código do município;
-   `rede`: informações se a escola pertence à rede estadual, municipal, federal, ou é privada;
-   `num_funcionarios`: número total de funcionários da escola;
-   `presencial`: se o ensino naquela escola é presencial ou não;
-   `mat_infantil`: a quantidade de matrículas daquela escola no ensino infantil;
-   `mat_fundamental`: a quantidade de matrículas daquela escola no ensino fundamental;
-   `mat_medio`: a quantidade de matrículas daquela escola no ensino médio;
-   `mat_profissional`: a quantidade de matrículas daquela escola no ensino profissional;
-   `mat_eja`: a quantidade de matrículas daquela escola na educação de jovens e adultos;
-   `mat_especial`: a quantidade de matrículas daquela escola no ensino especial;
-   `docentes`: o número total de docentes naquela escola;
-   `lon` e `lat`: coordenadas.

Esse arquivo foi então salvo com o nome `censo_escolar_2015.csv`.

``` r
source("R/converter_censo_coords.R")
# ABRIR ARQUIVO


censo_escolar <- 
  # Abrir e selecionar as colunas de interesse
  fread("data-raw/censo_escolar/CAD_ESC_MAT_DOC_2015.csv", sep = ";",
        select = c(17,3,6,14,128,138,144,150,165,187,196,201,206,27,28)) %>%
  # Renomear as colunas
  rename(cod_escola = CO_ENTIDADE,uf = SIGLA, municipio = NO_MUNICIPIO, rede = REDE, num_funcionarios = NU_FUNCIONARIOS,
         presencial = IN_MEDIACAO_PRESENCIAL, mat_infantil = MAT_INF, mat_fundamental = MAT_FUND,
         mat_medio = MAT_MED, mat_profissional = MAT_PROF, mat_eja = MAT_EJA, mat_especial = MAT_ESP, 
         docentes = DOCTOTAL, lon = NU_LONGITUDE, lat = NU_LATITUDE) %>%
  # Tratar as coordenadas
  mutate(lon = convert_coords(lon),
         lat = convert_coords(lat))


# SALVAR

write_csv(censo_escolar, "../data/censo_escolar/censo_escolar_2015.csv")


# # TIDYING UP!!!
# 
# censo_escolar_long <- censo_escolar %>%
#   gather(key = "tipo", value = "total", mat_infantil:docentes)
# 
# write_csv(censo_escolar_long, "data/censo_escolar/censo_escolar_2015_long.csv")
```

Grade censo
-----------

As grades do censo são agregações espaciais estimadas de tamanho padrão que contém informações populacionais (população de homens e mulheres), e são divididas por ID, onde cada um desses pode encorporar vários municípios. O arquivo `Tabela_UF_ID.csv` contém uma tabela auxiliar que identifica os IDs contidos em cada estado. O tratamento desse arquivo corrige alguns erros e cria uma correspondência entre o nome e a sigla de cada UF, salvando o arquivo tratado em disco.

``` r
# TRATAMENTO DO ARQUIVO COM OS IDs

# criar encoding para abrir arquivo
brazil <- locale("pt", encoding = "Windows-1252")

# abrir tabela de ids
ids_corresp <- read_delim("../data-raw/Tabela_UF_ID.csv", delim = ";", locale = brazil) %>%
  arrange(Estados) %>%
  mutate(Estados = ifelse(Estados == "Pernanbuco", "Pernambuco", Estados))

lookup_ufs <- data.frame(stringsAsFactors=FALSE,
                      Estados = c("Acre", "Alagoas", "Amazonas",
                                          "Amapá", "Bahia", "Ceará",
                                          "Distrito Federal", "Espírito Santo", "Goiás",
                                          "Maranhão", "Minas Gerais",
                                          "Mato Grosso do Sul", "Mato Grosso", "Pará",
                                          "Paraíba", "Pernambuco", "Piauí", "Paraná",
                                          "Rio de Janeiro", "Rio Grande do Norte",
                                          "Rondônia", "Roraima",
                                          "Rio Grande do Sul", "Santa Catarina", "Sergipe",
                                          "São Paulo", "Tocantins"),
                         uf = c("AC", "AL", "AM", "AP", "BA", "CE",
                                          "DF", "ES", "GO", "MA", "MG", "MS",
                                          "MT", "PA", "PB", "PE", "PI", "PR",
                                          "RJ", "RN", "RO", "RR", "RS", "SC", "SE",
                                          "SP", "TO")
)



ids_corresp_v1 <- ids_corresp %>%
  left_join(lookup_ufs) %>%
  mutate(uf = tolower(uf),
         Quadrante = tolower(Quadrante)) %>%
  mutate(Quadrante = gsub("_", "", Quadrante))

write_csv(ids_corresp_v1, "../data-raw/lookup_grade_ufs.csv")
# write_rds(ids_corresp_v1, "../data-raw/lookup_grade_ufs.rds")
```

A função para extrair os municípios das grades do IBGE requer dois inputs: o `municipio` e a `uf`:

-   Com a `uf` é feita uma seleção dos IDs que estão presentes na uf desejada daquele município;
-   É aberto então o shape do `municipio` desejado;
-   O geoprocessamento extrai somente as grades que estão inseridas dentro dos limites do município;
-   O resultado é salvo em disco.

``` r
grade_para_municipio <- function(muni, uf_input) {
  
  files <- read_csv("../data-raw/lookup_grade_ufs.csv") %>%
    filter(uf == uf_input) %>%
    mutate(Quadrante = paste0("grade_", Quadrante)) %>%
    .$Quadrante
  
  arquivos <- paste0("../data-raw/dadosrds/", files, ".rds")
  
  # abrir quadrantes da uf
  
  grades <- map_dfr(arquivos, read_rds) %>%
    as_tibble() %>%
    st_sf(crs = 4326)
  
  # extrair municipio -------------------------------------------------------
  
  municipio_ok <- toupper(muni)
  
  
  # abrir arquivos ----------------------------------------------------------
  
  dir_muni <- paste0("../data/municipios/municipios_", uf_input, ".rds")
  
  grade_estado <- grades %>%
    mutate(id_grade = 1:n()) %>%
    dplyr::select(id_grade, MASC, FEM, POP, DOM_OCU)
  
  # grade_estado_centroids <- grade_estado %>%
  #   st_centroid()
  
  cidade <- read_rds(dir_muni) %>%
    filter(NM_MUNICIP == municipio_ok) %>%
    dplyr::select(municipio = NM_MUNICIP)
  
  
  # geoprocessamento --------------------------------------------------------
  
  vai <- st_join(grade_estado, cidade) %>%
    filter(!is.na(municipio))
  
  
  grade_municipio <- grade_estado %>%
    dplyr::filter(id_grade %in% vai$id_grade) %>%
    mutate(municipio = municipio_ok)
  
  
  # salvar ------------------------------------------------------------------
  
  # tirar os espaços e colocar underscore
  municipio_nome_salvar <- substring(municipio_ok, 1, 3)
  
  # # criar pasta para o municipio
  # dir.create(paste0("data/grade_municipio/", municipio_nome_salvar))
  
  # salvar no disco
  write_rds(grade_municipio, 
           paste0("../data/grade_municipio/grade_", tolower(municipio_nome_salvar), ".rds"))
  
  
  
}
```

A função é então aplicada para as cidades desejadas:

``` r
municipios <- c("fortaleza", "rio de janeiro", "belo horizonte", "recife", "porto alegre", "são paulo", "curitiba")
ufs <- c("ce", "rj", "mg", "pe", "rs", "sp", "pr")

grade_para_municipio("são paulo", "sp")
grade_para_municipio("curitiba", "pr")
grade_para_municipio("teresina", "pi")

walk2(municipios, ufs, grade_para_municipio)
```

Hospitais
---------

``` r
# hospitais <- read_csv("../data-raw/hospitais/cnesnone_2018.csv") %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326)
```

Empregos
--------

Os empregos são extraídos da base da RAIS (Relação Anual de Informações Sociais). A base foi georreferenciada por um software que retorna as coordenadas de latitute e longitude com uma avalição da qualidade do georreferenciamento. Com isso, as etapas do tratamento dessa base foram:

-   Deletar observações que tiveram georreferenciamento de 1 estrela (só conseguiu achar a cidade). Isso garante uma precisão suficiente para as análises seguintes;
-   Selecionar as colunas de interesse: `id_estab`, que é o id do estabelecimento, `qt_vinc_ativos`, que é a quantidade de vínculos ativos, `cod_mun`, que é o código do município, e coordenadas.
-   Salvar para o arquivo `rais_2015.rds`.

``` r
library(foreign)

# Abrir RAIS (formato stata)
rais_raw <- read.dta("../data-raw/rais/estab_2015_vinc_coord.dta")
# Transformar em data.table
setDT(rais_raw)
# Deletar as localizacoes com precisao de 1 estrela
rais_v1 <- rais_raw[Precison_original != "1 Estrela"]
# Ajeitar as coordenadas
rais_v1 <- rais_v1[, ':='(lon = as.numeric(gsub(",", ".", longitude)), lat = as.numeric(gsub(",", ".", latitude)))]
# Selecionar as colunas de interesse
rais_v1 <- rais_v1[, .(id_estab, qt_vinc_ativos, cod_mun = ibge_cod7, lon, lat)]
# Dropar coordenadas NA
rais_v1 <- na.omit(rais_v1, cols = c("lon", "lat"))
# Transformar para sf
rais_v1 <- st_as_sf(rais_v1, coords = c("lon", "lat"), crs = 4326)


# Salvar
write_rds(rais_v1, "../data/rais/rais_2015.rds")
```

Sugestão futura: analisar a qualidade do georreferenciamento para as grandes cidades.

GTFS
----

O GTFS do Rio de Janeiro apresenta algumas inconsistências no arquivo `stop_times.txt`.

``` r
# OTP RIO!!!!!!!!!1 -------------------------------------------------------

# path_otp <- "otp/programs/otp.jar" # On Linux
# 
# path_data <- "otp"
# 
# log <- otp_build_graph(otp = path_otp, dir = path_data, router = "rio",
#                        memory = 16)
# 
# 
# otpcon <- otp_connect()
# 
# 
# system("java -Xmx4G -jar \"otp/programs/otp.jar\" --build \"otp/graphs/rio")

# Error:
# Caused by: org.onebusaway.gtfs.serialization.mappings.InvalidStopTimeException: invalid stop time: 00:00:-6


# VERIFICAR O ERRO --------------------------------------------------------

stop_times <- fread("gtfs_teste/gtfs_rio_00_20171218/stop_times.txt", sep = ",") 

teste1 <- stop_times %>%
  select(arrival_time, departure_time) %>%
  filter(!grepl("\\d{2}:\\d{2}:\\d{2}", arrival_time))



# CORRIGIR O ERRO ---------------------------------------------------------


stop_times_new <- stop_times %>%
  mutate(arrival_time = ifelse(grepl("\\d{2}:\\d{2}:\\d{2}", arrival_time), arrival_time, "00:00:06")) %>%
  mutate(departure_time = ifelse(grepl("\\d{2}:\\d{2}:\\d{2}", departure_time), departure_time, "00:00:06"))


# TESTAR SE A CORREÇÃO FUNCIONOU ------------------------------------------


stop_times_new %>%
  filter(!grepl("\\d{2}:\\d{2}:\\d{2}", arrival_time))

# OK!!!!!!!!!!

# SALVAR, ENTAO! ----------------------------------------------------------


data.table::fwrite(stop_times_new, "gtfs_teste/gtfs_rio_novo/stop_times.txt", quote = TRUE)
```

Elevação
--------

Os dados brutos de elevação são retirados do [Earth Explorer](https://earthexplorer.usgs.gov/). Lá, é necessário especificar a região e data que se quer extrair os dados de elevação. Na aba de *Select Your Data Set(s)*, seleciona-se `Digital Elevation` -&gt; `SRTM`. SRTM (*Shuttle Radar Topography Mission*) é um esforço de pesquisa internacional que obtém dados de elevação numa precisão de 30 metros. Os dados de elevação do SRTM são divididos por quadrículo de 1 grau de latidude e 1 longitude, então é necessário cortar os municípios desejados dessa área.

A função `crop_save_raster` foi criada para tratar e salvar os dados de elevação, e requer dois argumentos: `municipio`, que é a sigla (três primeiras letras) do município desejado, e `bb`, que é o *bounding box* do município (pares de coordenadas que delimitam a área do município). Esse argumento pode ser extraído do [Bounding Box Tool](https://boundingbox.klokantech.com/), onde na aba de busca é pesquisada e selecionada a cidade em questão. Por fim, na parte inferior esquerda, é selecionada a opção `CSV RAW` na aba *Copy & Paste*, e as coordenadas são inseridas na função como um vetor.

A função será aplicada para três cidades inicialmente: Fortaleza, Belo Horizonte e Rio de Janeiro.

``` r
source("R/tratar_elevation.R")

crop_save_raster("for", bb = c(-38.63656796,-3.88812428,-38.40154132,-3.69197903))
crop_save_raster("bel", bb = c(-44.06329161,-20.0594646,-43.85721992,-19.77654377))
crop_save_raster("rio", bb = c(-43.79625205,-23.08270518,-43.09908114,-22.74608786))
```
