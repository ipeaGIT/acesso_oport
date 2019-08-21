# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Download de dados de malha de ruas do OSM em formato .pbf (que sera utilizado no OpenTripPlanner)

# Script falhando - dados dos OSM .pbf são baixados manualmente

#' O arquivo pbf de cada cidade pode ser baixado a partir do 
#' site [HOT Export Tool](https://export.hotosm.org/en/v3/exports/new/describe). 
#' Lá, escreva o nome da cidade na barra de pesquisa do lado direito e selecione o município (checar o país). 
#' Automaticamente o mapa vai dar um zoom no município, com o bounding box delimitado. Feito isso, no lado direito, 
#' coloque o nome do seu export (sugestão: três primeiras letras da cidade) e aperte ``NEXT``. Na aba ``2 Formats`` 
#' que vai aparecer, selecione o formato OSM ``.pbf`` e aperte ``NEXT``. Na aba ``3 Data`` que aparece, selecione 
#' somente o tipo ``Transportation``, que é o necessário para o projeto, e aperte ``NEXT``. Por fim, na aba ``4 Summary``,
#'  clique em ``Create Export`` e espere o arquivo estar disponível para download. Descompactar o arquivo na pasta do
#'   município na paste ``../otp/graphs``.


 
# # carregar bibliotecas
# source('./R/fun/setup.R')
# 
# 
# # cidade <- "porto alegre"
# 
# download_pbf <- function(sigla_muni) {
#   
#   
#   # Nome da cidade
#   cidade <- munis_df[abrev_muni==sigla_muni]$name_muni
#   cidade_string <- paste0(cidade, ", brazil")
#   
#   # Tags disponiveis
#   vai <- osmdata::available_tags("highway")
#   
#   features <- osmdata::opq(cidade_string) %>%
#     osmdata::add_osm_feature(key = "highway", value = vai)
#   
#   # Exportar arquivo .pbf para o disco
#   path_out <- sprintf("../otp/graphs/%s/ruas_%s.pbf", sigla_muni, sigla_muni)
#   osmdata::osmdata_pbf(q=features, filename=path_out, quiet = F)
# 
# }
