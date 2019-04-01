Indicadores de acessibilidade
================
Ipea
27 de março de 2019

Indicador de acesso cumulativo a oportunidades
==============================================

Com a quantidade de oportunidades (saúde, educação, empregos) e a matriz de tempo de viagem calculadas entre os hexágonos, é hora da etapa de calcular o indicador de acessibilidade. Como projeto piloto, será calculado o indicador para as cidades de Fortaleza, Belo Horizonte e Rio de Janeiro.

Fortaleza
---------

``` r
# abrir matriz
matriz_for <- read_csv("../data/output_ttmatrix/traveltime_matrix_for_python.csv") %>%
  select(origin, destination, travel_time) %>%
  mutate(travel_time = travel_time/60)

# abrir oportunidades com hexagonos
hexagonos_for_sf <- read_rds("../data/hex_agregados/hex_agregado_for.rds") %>%
  ungroup()

hexagonos_for <- hexagonos_for_sf %>%
  st_set_geometry(NULL)

# quantas oportunidades de escolas podem ser acessadas em menos de 40 minutos?

access_ac_for <- matriz_for %>%
  left_join(hexagonos_for, by = c("destination" = "id_hex")) %>%
  group_by(origin) %>%
  filter(travel_time < 40) %>%
  summarise_at(vars(saude_total, escolas_total), sum)

access_ac_for_fim <- hexagonos_for_sf %>%
  select(id_hex) %>%
  left_join(access_ac_for, by = c("id_hex" = "origin"))


# mapview(access_ac_for_fim, zcol = "saude_total")

# write_rds(access_ac_for_fim, "../data/output_access/access_for.rds")
```

Visualizar o indicador de acessibilidade para Fortaleza:

``` r
access_ac_for_fim %>%
 select(-escolas_total) %>%
  ggplot()+
  geom_sf(aes(fill = saude_total))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd")) +
access_ac_for_fim %>%
 select(-saude_total) %>%
  ggplot()+
  geom_sf(aes(fill = escolas_total))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd"))
```

![](05_acessibilidade_files/figure-markdown_github/viz%20for-1.png)

Belo Horizonte
--------------

Para Belo Horizonte:

``` r
# abrir matriz
matriz_bel <- read_csv("../data/output_ttmatrix/traveltime_matrix_bel_python.csv") %>%
  select(origin, destination, travel_time) %>%
  mutate(travel_time = travel_time/60)

# abrir oportunidades com hexagonos
hexagonos_bel_sf <- read_rds("../data/hex_agregados/hex_agregado_bel.rds") %>%
  ungroup()

hexagonos_bel <- hexagonos_bel_sf %>%
  st_set_geometry(NULL)

# quantas oportunidades de escolas podem ser acessadas em menos de 40 minutos?

access_ac_bel <- matriz_bel %>%
  left_join(hexagonos_bel, by = c("destination" = "id_hex")) %>%
  group_by(origin) %>%
  filter(travel_time < 40) %>%
  summarise_at(vars(saude_total, escolas_total), sum)

access_ac_bel_fim <- hexagonos_bel_sf %>%
  select(id_hex) %>%
  left_join(access_ac_bel, by = c("id_hex" = "origin"))


# mapview(access_ac_bel, zcol = "escolas_total")


# write_rds(access_ac_bel_fim, "../data/output_access/access_bel.rds")
```

Visualizar o indicador de acessibilidade para Belo Horizonte:

``` r
access_ac_bel_fim %>%
 select(-escolas_total) %>%
  ggplot()+
  geom_sf(aes(fill = saude_total))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd")) +
access_ac_bel_fim %>%
 select(-saude_total) %>%
  ggplot()+
  geom_sf(aes(fill = escolas_total))+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd"))
```

![](05_acessibilidade_files/figure-markdown_github/viz%20bel-1.png)

Rio de Janeiro
--------------

Para o Rio de Janeiro:

``` r
# abrir matriz
matriz_rio <- read_csv("../data/output_ttmatrix/traveltime_matrix_rio_python.csv") %>%
  select(origin, destination, travel_time) %>%
  mutate(travel_time = travel_time/60)

# abrir oportunidades com hexagonos
hexagonos_rio_sf <- read_rds("../data/hex_agregados/hex_agregado_rio.rds") %>%
  ungroup()

hexagonos_rio <- hexagonos_rio_sf %>%
  st_set_geometry(NULL)

# quantas oportunidades de escolas podem ser acessadas em menos de 40 minutos?

access_ac_rio <- matriz_rio %>%
  left_join(hexagonos_rio, by = c("destination" = "id_hex")) %>%
  group_by(origin) %>%
  filter(travel_time < 40) %>%
  summarise_at(vars(saude_total, escolas_total), sum)

access_ac_rio_fim <- hexagonos_rio_sf %>%
  select(id_hex) %>%
  left_join(access_ac_rio, by = c("id_hex" = "origin"))
  

# mapview(access_ac_rio_fim, zcol = "saude_total")

# write_rds(access_ac_rio_fim, "../data/output_access/access_rio.rds")
```

Visualizar o indicador de acessibilidade para Rio de Janeiro:

``` r
access_ac_rio_fim %>%
 select(-escolas_total) %>%
  ggplot()+
  geom_sf(aes(fill = saude_total))+
  theme_bw()+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd")) +
access_ac_rio_fim %>%
 select(-saude_total) %>%
  ggplot()+
  geom_sf(aes(fill = escolas_total))+
  theme_bw()+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(6, "PuRd"))+
    plot_layout(ncol = 1)
```

![](05_acessibilidade_files/figure-markdown_github/viz%20rio-1.png)
