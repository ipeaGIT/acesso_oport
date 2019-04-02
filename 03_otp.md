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

construir_graph("fortaleza")
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

q <- opq ("belo horizonte") %>%
  add_osm_feature(key = "amenity", value = "") %>%
  osmdata_sf()
```
