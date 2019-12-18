library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(readr)
library(data.table)
library(mapdeck)

# register mapbox api key
my_api <- data.table::fread("../../../data-raw/mapbox_key.txt", header = F)
set_token(my_api$V1)




# abrir acessibilidade

acess_cum <- read_rds("acess_tp_cum_app.rds") %>%
  data.table()

acess_min <- read_rds("acess_tp_min_app.rds") %>%
  data.table()

linhas <- read_rds("../../../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")

limits <- read_rds("cities_centroids.rds")


# Define a server for the Shiny app
function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada <- reactive({
    acess_cum[nome_muni == input$cidade]
  })
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada_min <- reactive({
    acess_min[nome_muni == input$cidade]
  })
  
  
  
  # Reactive para a atividade
  atividade_filtrada <- reactive({
    
    if(input$atividade_cum == "Trabalho"){
      
      cidade_filtrada()[atividade == "TT"]
      
    } else if (input$atividade_cum == "Saúde") {
      
      cidade_filtrada()[atividade == "ST"]
      
    } else if (input$atividade_cum == "Educação") {
      
      cidade_filtrada()[atividade == "ET"]
      
    }
  })
  
  # Atividade filtrada para o indicador minimo
  
  atividade_filtrada_min <- reactive({
    
    if (input$atividade_min == "Saúde") {
      
      cidade_filtrada_min()[atividade == "ST"] %>% st_sf(crs = 4326) 

      
    } else if (input$atividade_min == "Educação") {
      

      cidade_filtrada_min()[atividade == "ET"] %>% st_sf(crs = 4326) 
      
    }
    
  })
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    atividade_filtrada()[tempo_viagem == input$tempo] %>% st_sf(crs = 4326) 
  })
  
  
  
  # # Reactive para a cor do mapa
  # colorpal <- reactive({
  #   colorNumeric("inferno", tempo_filtrado()$valor)
  # })
  # 
  # colorpal_min <- reactive({
  #       colorNumeric("inferno", atividade_filtrada_min()$valor)
  # })
  
  
  
  
  # baseMap
  output$map <- renderMapdeck({
    
    # vai <- acess_cum[nome_muni == input$cidade] %>% st_sf(crs = 4326)
    # 
    # bounds <- st_bbox(vai)
    # min_lon <- bounds["xmin"] %>% as.numeric()
    # min_lat <- bounds["ymin"] %>% as.numeric()
    # max_lon <- bounds["xmax"] %>% as.numeric()
    # max_lat <- bounds["ymax"] %>% as.numeric()
    # 
    # # cidade_filtrada() %>%
    #   leaflet(data = vai) %>%
    #     addProviderTiles(provider = providers$CartoDB) %>%
    #     fitBounds(~min_lon, ~min_lat, ~max_lon, ~max_lat)
    
    centroid <- filter(limits, name_muni == input$cidade)
    
    mapdeck() %>%
      mapdeck_view(location = c(centroid$lon, centroid$lat), zoom = 10)
  
    
    })
  

  
  observe({
    
    
    linhas_cidade <- linhas %>%
      filter(Cidade == input$cidade) %>%
      filter(!st_is_empty(.))
    
    colorpal_linhas <- colorFactor("Accent", linhas_cidade$Modo)
    
    if (input$indicador == "Cumulativo") {
      
      mapdeck_update(map_id = "map") %>%
        clear_polygon(layer_id = "acess_min_go") %>%
        clear_legend(layer_id = "acess_min_go") %>%
        add_polygon(
          data = tempo_filtrado(),
          fill_colour = "valor",
          fill_opacity = 200,
          layer_id = "acess_cum_go",
          palette = "inferno",
          update_view = FALSE,
          focus_layer = FALSE,
          legend = TRUE,
          legend_options = list(title = "% de Oportunidades Acessíveis")
          # legend_format = list(fill_colour = scales::percent)
        )
      
      
    } else if (input$indicador == "Oportunidade mais próxima") {
      
      mapdeck_update(map_id = "map") %>%
        clear_polygon(layer_id = "acess_cum_go") %>%
        clear_legend(layer_id = "acess_cum_go") %>%
        add_polygon(
          data = atividade_filtrada_min(),
          fill_colour = "valor",
          fill_opacity = 200,
          layer_id = "acess_min_go",
          palette = "inferno",
          update_view = FALSE,
          legend = TRUE,
          legend_options = list(title = "Minutos atè a oportunidade mais próxima")
        )
      
      
      
    }
      
      
    
      
  })
  
  
  
}
