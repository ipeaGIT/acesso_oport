Identificação de clusters de acessibilidade
================
Ipea
27 de março de 2019

Identificacão de cluster de acessibilidade a escolas e equipamentos de saúde
----------------------------------------------------------------------------

Para identificar clusters do indicador de acessibilidade calculados na seção anterior, é proposta a utilização do LISA Maps. Esse tipo de ferramenta de estatística espacial permite identificar (com significância estatística) onde há aglomerados de alta acessibilidade (*high-high*) e baixa acessibilidade (*low-low*). Isso permite, com a futura incorporação de dados socioeconômicos, avaliar desigualdades sócio-espaciais.

Foi criado uma função (adaptada [daqui](https://stackoverflow.com/questions/37664728/create-a-map-of-spatial-clusters-lisa-in-r)) que retorna LISA Maps de escolas e equipamentos de saúde a partir da sigla do município. Essa função então foi aplicada para Fortaleza, Belo Horizonte e Rio de Janeiro (soon).

Fortaleza
---------

Aplicando para a capital cearense:

``` r
source("R/create_lisamaps.R")

criar_lisamaps("for")
```

![](06_clusters_files/figure-markdown_github/lisa%20for-1.png)

Belo Horizonte
--------------

Aplicando para a capital mineira:

``` r
source("R/create_lisamaps.R")

criar_lisamaps("bel")
```

![](06_clusters_files/figure-markdown_github/lisa%20bel-1.png)

Rio de Janeiro
--------------

Para o Rio de Janeiro:

``` r
source("R/create_lisamaps.R")

criar_lisamaps("rio", cols = 1)
```

![](06_clusters_files/figure-markdown_github/lisa%20rio-1.png)
