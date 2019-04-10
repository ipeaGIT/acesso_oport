library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(shinydashboard)
library(readr)
library(data.table)
# library(hrbrthemes)
# library(forcats)
# library(sp)
# library(rgdal)


# abrir acessibilidade

acess <- read_rds("acess_junto.rds") %>%
  data.table()





# Define a server for the Shiny app
function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada <- reactive({
    ai <- acess[cidade_nome == input$cidade]
  })
  
  
  # Reactive para a atividade
  atividade_filtrada <- reactive({
    if (input$atividade == "Saúde") {
      
    cidade_filtrada()[, .(id_hex, saude_total, tempo_viagem, geometry)] %>% rename(atividade = saude_total)
      
    } else if (input$atividade == "Educação")
      
    {
      
    cidade_filtrada()[, .(id_hex, escolas_total, tempo_viagem, geometry)] %>% rename(atividade = escolas_total)
      
    }
  })
  
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    atividade_filtrada()[tempo_viagem == input$tempo] %>% st_as_sf() 
  })
  
  
  
  # Reactive para a cor do mapa
  colorpal <- reactive({
    colorNumeric("inferno", tempo_filtrado()$atividade)
  })
  
  
  # Mapa
  output$map <- renderLeaflet({
    
    vai <- filter(acess, cidade_nome == input$cidade) %>% st_as_sf()
    
    
    bounds <- st_bbox(vai)
    min_lon <- bounds["xmin"] %>% as.numeric()
    min_lat <- bounds["ymin"] %>% as.numeric()
    max_lon <- bounds["xmax"] %>% as.numeric()
    max_lat <- bounds["ymax"] %>% as.numeric()
    
    # cidade_filtrada() %>%
      leaflet(data = vai) %>%
        addTiles() %>%
        fitBounds(~min_lon, ~min_lat, ~max_lon, ~max_lat)
      # addLegend(data = filter(access, cidade_nome == "Fortaleza"), 
      #           "bottomright", pal = pal, values = ~saude_total,
      #           title = "Oportunidade de saúde em<br/> até 40 minutos",
      #           opacity = 1)
  })
  

# EU NAO AGUENTO MAIS!!!!!!!!! --------------------------------------------

  # observe({
  #   
  #   p2 <- st_as_sf(acess[cidade_nome == input$cidade])
  #   
  #   min_lon <- st_bbox(p2)["xmin"] %>% as.numeric()
  #   min_lat <- st_bbox(p2)["ymin"] %>% as.numeric()
  #   max_lon <- st_bbox(p2)["xmax"] %>% as.numeric()
  #   max_lat <- st_bbox(p2)["ymax"] %>% as.numeric()
  # 
  #     leafletProxy("map", data = p2) %>% 
  #       fitBounds(~min_lon, ~min_lat, ~max_lon, ~max_lat)
  # })
  
  
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = tempo_filtrado()) %>%
      clearShapes() %>%
      addPolygons(stroke = FALSE, weight = 2, color = "black",
                  fillOpacity = 0.7,
                  fillColor = ~pal(atividade))
      
  })
  
  
  
}
