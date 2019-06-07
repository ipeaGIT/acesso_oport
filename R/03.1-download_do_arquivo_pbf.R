#' ## Download do arquivo .pbf
#' 
#' O arquivo pbf de cada cidade pode ser baixado a partir do site [HOT Export Tool](https://export.hotosm.org/en/v3/exports/new/describe). Lá, escreva o nome da cidade na barra de pesquisa do lado direito e selecione o município (checar o país). Automaticamente o mapa vai dar um zoom no município, com o bounding box delimitado. Feito isso, no lado direito, coloque o nome do seu export (sugestão: três primeiras letras da cidade) e aperte ``NEXT``. Na aba ``2 Formats`` que vai aparecer, selecione o formato OSM ``.pbf`` e aperte ``NEXT``. Na aba ``3 Data`` que aparece, selecione somente o tipo ``Transportation``, que é o necessário para o projeto, e aperte ``NEXT``. Por fim, na aba ``4 Summary``, clique em ``Create Export`` e espere o arquivo estar disponível para download. Descompactar o arquivo na pasta do município na paste ``../otp/graphs``.
#' 
#' Há outra forma de fazer o download do arquivo ``.pbf``através do pacote ``osmdata``. Uma função foi criada na tentativa de aplicar essa metodologia reproduzível, porém o resultado das funções estão sujeitos a inconsistências.
#' 
## ----download_pbf--------------------------------------------------------

cidade <- "porto alegre"

download_pbf <- function(cidade) {
  
  cidade_string <- paste0(cidade, ", brazil")
  
  # Tags disponiveis
  vai <- available_tags("highway")
  
  features <- opq (cidade_string) %>%
    add_osm_feature(key = "highway", value = vai)
  
  # Exportar arquivo .pbf para o disco
  cidade_short <- substr(cidade, 1, 3)
  path_out <- sprintf("../otp/graphs/%s/%s.pbf", cidade_short, cidade_short)
  osmdata_pbf(features, path_out)

}


#' 
#' 
