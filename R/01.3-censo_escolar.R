#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Leitura e filtro de dados do censo escolar

## info:
# dados originais fornecidos pelo INEP
  
  
# carregar bibliotecas
source('./R/fun/setup.R')


# 1. Funcao para leitura e limpeza dos dados ------------------------------------------------------------------

#  funcao para corrigir coordenadas lat lon, porque dados originais estao em Excel.xls
convert_coords <- function(coords) {
  
  x <- gsub("\\.", "", coords)
  x <- stringr::str_sub(x, 1, -3)
  x <- as.numeric(x)
  x <- scales::comma(x)
  
  x <- gsub("\\,", "\\.", x)
  x1 <- str_extract(x, "-?\\d+\\.")
  x2 <- gsub("(-?\\d+\\.)(.*)", "\\2", x)
  x3 <- gsub("\\.", "", x2)
  xfim <- paste0(x1, x3)
  xfim <- as.numeric(xfim)
  
}


# leitura e limpeza dos dados
censo_escolar <- 
 
   # Abrir e selecionar as colunas de interesse
     data.table::fread("../data-raw/censo_escolar/CAD_ESC_MAT_DOC_2015.csv", sep = ";",
                       select = c(17,3,6,7,14,128,138,144,150,165,187,196,201,206,27,28)) %>%
  
  # Renomear as colunas
    rename(cod_escola = CO_ENTIDADE,uf = SIGLA, municipio = NO_MUNICIPIO, rede = REDE, num_funcionarios = NU_FUNCIONARIOS,
           presencial = IN_MEDIACAO_PRESENCIAL, mat_infantil = MAT_INF, mat_fundamental = MAT_FUND,
           mat_medio = MAT_MED, mat_profissional = MAT_PROF, mat_eja = MAT_EJA, mat_especial = MAT_ESP, 
           docentes = DOCTOTAL, lon = NU_LONGITUDE, lat = NU_LATITUDE) %>%
  
  # Tratar as coordenadas
    mutate(lon = convert_coords(lon),
           lat = convert_coords(lat))

head(censo_escolar)

setDT(censo_escolar)[is.na(lat)]$municipio %>% unique()



# SALVAR
readr::write_csv(censo_escolar, "../data/censo_escolar/censo_escolar_2015.csv")
