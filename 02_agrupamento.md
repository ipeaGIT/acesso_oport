Agrupamento dos dados
================
Ipea
27 de março de 2019

Agrupamento de dados socioeconômicos de uso do solo
===================================================

Esse arquivo tem como objetivo agrupar os dados pelas unidades de agregação espacial (hexágonos) e salvá-los em disco. As seguintes variáveis são agrupadas:

-   Estabelecimentos de saúde;
-   Escolas;
-   População;
-   Empregos (*very soon*).

Primeiramente é necessário criar as unidades de agregação que serão utilizadas.

Criação de hexágonos
--------------------

As cidade brasileiras analisadas serão dividadas em hexágonos. A função `poligono_para_hexagono` pega os municípios e cria hexágonos de acordo com a resolução preferida, que no caso foi uma resolução de 960 metros (comprimento da menor diagonal do hexágono).

``` r
source("R/poligono_para_hexagono.R")

# # aplicando ---------------------------------------------------------------

munis <- c("fortaleza", "rio de janeiro", "recife", "belo horizonte")
ufs <- c("ce", "rj", "pe", "mg")

purrr::walk2(munis, ufs, shape_to_hexagon)
```

Agrupamento das variáveis por hexágonos
---------------------------------------

A função `agrupar_variaveis` aceita como *input* o nome do município desejado e retorna um tabela com o shape dos hexágonos e a quantidade de estabelecimentos de saúde, educação e população agregados, salvos em disco.

``` r
# abrir funcao
source("R/agrupar_variaveis.R")

# escolher cidades

munis <- c("fortaleza", "rio de janeiro", "recife", "belo horizonte")

# aplicar funcao

purrr::walk(munis, agrupar_variaveis)
```
