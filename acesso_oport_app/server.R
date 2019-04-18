library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(shinydashboard)
library(readr)
library(data.table)
library(mapview)
# library(hrbrthemes)
# library(forcats)
# library(sp)
# library(rgdal)


# abrir acessibilidade

acess_cum <- read_rds("acess_cum_junto.rds") %>%
  data.table()

acess_min <- read_rds("acess_min_junto.rds") %>%
  data.table()

linhas <- read_rds("../../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")




# Define a server for the Shiny app
function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada <- reactive({
    ai <- acess_cum[cidade_nome == input$cidade]
  })
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada_min <- reactive({
    ai <- acess_min[cidade_nome == input$cidade]
  })
  
  # Reactive para a atividade
  atividade_filtrada <- reactive({
    if (input$atividade == "Saúde") {
      
    cidade_filtrada()[, .(id_hex, pop_total, saude_total, tempo_viagem, geometry)] %>% rename(atividade = saude_total)
      
    } else if (input$atividade == "Educação")
      
    {
      
    cidade_filtrada()[, .(id_hex, pop_total, escolas_total, tempo_viagem, geometry)] %>% rename(atividade = escolas_total)
      
    }
  })
  
  # Atividade filtrada para o indicador minimo
  
  atividade_filtrada_min <- reactive({
    if (input$atividade == "Saúde") {
      
    cidade_filtrada_min()[, .(id_hex, pop_total, atividade, travel_time, geometry)] %>% 
        filter(atividade == "saude_total") %>%
        st_as_sf() 
      
    } else if (input$atividade == "Educação") {
      
    cidade_filtrada_min()[, .(id_hex, pop_total, atividade, travel_time, geometry)] %>% 
        filter(atividade == "escolas_total") %>% 
        st_as_sf() 
      
    }
    
  })
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    atividade_filtrada()[tempo_viagem == input$tempo] %>% st_as_sf() 
  })
  
  
  
  # Reactive para a cor do mapa
  colorpal <- reactive({
    colorNumeric("BuPu", tempo_filtrado()$atividade)
  })
  
  colorpal_min <- reactive({
    colorNumeric("BuPu", atividade_filtrada_min()$travel_time)
  })
  
  # Mapa
  output$map <- renderLeaflet({
    
    vai <- filter(acess_cum, cidade_nome == input$cidade) %>% st_as_sf()
    
    
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
    
    
    linhas_cidade <- linhas %>%
      filter(Cidade == input$cidade) %>%
      filter(!st_is_empty(.))
    
    colorpal_linhas <- colorFactor("Accent", linhas_cidade$Modo)
    
    if (input$indicador == "Indicador Cumulativo") {
      pal <- colorpal()
      
      popup_hex <- paste0("<b>População: </b>", tempo_filtrado()$pop_total, "<br/>",
                      "<b>Oportunidades: </b>", tempo_filtrado()$atividade, "<br/>")
      
      popup_linha <- paste0("<b>Tipo da linha</b>: ", linhas_cidade$Modo)
      
      names(linhas_cidade$geometry) <- NULL
      
      leafletProxy("map", data = tempo_filtrado()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(stroke = TRUE, weight = 0.7, color = "black",
                    fillOpacity = 0.7,
                    fillColor = ~pal(atividade),
                    popup = popup_hex) %>%
        addPolylines(data = linhas_cidade,
                     opacity = 1,
                     group = "Corredores de Alta e Média Capacidade", 
                     popup = popup_linha, color = ~colorpal_linhas(Modo)) %>%
        addLayersControl(overlayGroups = "Corredores de Alta e Média Capacidade", 
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(data = tempo_filtrado(), "bottomright", pal = pal, values = ~atividade,
                  title = sprintf("Oportunidades acessíveis<br/> em %s minutos", input$tempo)) %>%
        addLegend(data = linhas_cidade, "topright", pal = colorpal_linhas, values = ~Modo,
                  title = "Modo da Linha", 
                  group = "Corredores de Alta e Média Capacidade")
      
      
    } else if (input$indicador == "Indicador de oportunidade mais próxima") {
      
      pal <- colorpal_min()
      
      popup_hex <- paste0("<b>População: </b>", atividade_filtrada_min()$pop_total, "<br/>")
      
      popup_linha <- paste0("<b>Tipo da linha</b>: ", linhas_cidade$Modo)
      
      names(linhas_cidade$geometry) <- NULL
      
      leafletProxy("map", data = atividade_filtrada_min()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(stroke = TRUE, weight = 0.7, color = "black",
                    fillOpacity = 0.7,
                    fillColor = ~pal(travel_time),
                    popup = popup_hex) %>%
        addPolylines(data = linhas_cidade,
                     opacity = 1,
                     group = "Corredores de Alta e Média Capacidade", 
                     popup = popup_linha, color = ~colorpal_linhas(Modo)) %>%
        addLayersControl(overlayGroups = "Corredores de Alta e Média Capacidade", 
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(data = atividade_filtrada_min(), "bottomright", pal = pal, values = ~travel_time,
                  title = c("Minutos atè a oportunidade mais próxima")) %>%
        addLegend(data = linhas_cidade, "topright", pal = colorpal_linhas, values = ~Modo,
                  title = "Modo da Linha", 
                  group = "Corredores de Alta e Média Capacidade")
      
    }
      
      
    
      
  })
  
  
  
}
