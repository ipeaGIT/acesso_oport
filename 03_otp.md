OTP - Setup
================
Ipea
19 de março de 2019

OTP Setup
=========

Criação dos graphs, basicamente.

A função `construir_graph` constrói o arquivo `Graph.obj`, que é necessário para as operações do OTP. O único argumento necessário para a construção do graph é o nome da cidade, que já deve estar com uma pasta criada com os arquivo `.pbf`e `GTFS` referentes.

``` r
source("R/otp.R")

construir_graph("for")
construir_graph("bel")
construir_graph("rio")
```

Atestar qualidade do GTFS através do `feedvalidator`:

``` r
source("R/feed_validator.R")
```

Próxima etapa: baixar os arquivos .pbf a partir do pacote `osmdata`.

``` r
getbb ("belo horizonte")

vai <- available_tags("highway")

q <- opq ("belo horizonte") %>%
  add_osm_feature(key = "highway", value = vai) %>%
  osmdata_sf()
  
opq ("belo horizonte") %>%
  add_osm_feature(key = "highway", value = vai) %>%
  osmdata_pbf()


meu <- q[["osm_lines"]] %>%
  st_sf()

# Before that
# sudo apt-get install sqlite3 libsqlite3-dev

q %>%
  st_write("teste_bel.pbf")
```
