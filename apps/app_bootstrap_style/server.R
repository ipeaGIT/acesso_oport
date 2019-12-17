library(shiny)
library(dplyr)
library(sf)
library(leaflet)
# library(shinydashboard) 
library(readr)
library(data.table)
# library(mapview)
# library(leaflet.mapboxgl)
# library(hrbrthemes)
# library(forcats)
# library(sp)
# library(rgdal)

# register mapbox api key
my_api <- data.table::fread("../../../data-raw/mapbox_key.txt", header = F)
options(mapbox.accessToken = my_api$V1)



# abrir acessibilidade

acess_cum <- read_rds("acess_tp_cum_app.rds") %>%
  data.table()

acess_min <- read_rds("acess_tp_min_app.rds") %>%
  data.table()

linhas <- read_rds("../../../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")


# Define a server for the Shiny app
function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada <- reactive({
    ai <- acess_cum[nome_muni == input$cidade]
  })
  
  # Reactive expression for the data subsetted to what the user selected
  cidade_filtrada_min <- reactive({
    ai <- acess_min[nome_muni == input$cidade]
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
      
      cidade_filtrada_min()[atividade == "ST"] %>% st_as_sf() 

      
    } else if (input$atividade_min == "Educação") {
      

      cidade_filtrada_min()[atividade == "ET"] %>% st_as_sf() 
      
    }
    
  })
  
  # Reactive para o tempo
  tempo_filtrado <- reactive({
    atividade_filtrada()[tempo_viagem == input$tempo] %>% st_as_sf() 
  })
  
  
  
  # Reactive para a cor do mapa
  colorpal <- reactive({
    colorNumeric("inferno", tempo_filtrado()$valor)
  })
  
  colorpal_min <- reactive({
        colorNumeric("inferno", atividade_filtrada_min()$valor)
  })
  
  
  
  
  # baseMap
  output$map <- renderLeaflet({
    
    vai <- filter(acess_cum, nome_muni == input$cidade) %>% st_as_sf()
    
    # limite <- vai %>%
    #   mutate(ui = 1) %>%
    #   count(ui)
    
    bounds <- st_bbox(vai)
    min_lon <- bounds["xmin"] %>% as.numeric()
    min_lat <- bounds["ymin"] %>% as.numeric()
    max_lon <- bounds["xmax"] %>% as.numeric()
    max_lat <- bounds["ymax"] %>% as.numeric()
    
    # cidade_filtrada() %>%
      leaflet(data = vai) %>%
        addProviderTiles(provider = providers$CartoDB) %>%
        # addMapboxGL(style = "mapbox://styles/mapbox/streets-v9") %>%
        # addPolygons(data = limite, stroke = TRUE, color = "black", weight = 5, fill = FALSE)
        fitBounds(~min_lon, ~min_lat, ~max_lon, ~max_lat)
      # addLegend(data = filter(access, nome_muni == "Fortaleza"), 
      #           "bottomright", pal = pal, values = ~saude_total,
      #           title = "Oportunidade de saúde em<br/> até 40 minutos",
      #           opacity = 1)
  })
  

# EU NAO AGUENTO MAIS!!!!!!!!! --------------------------------------------

  # observe({
  #   
  #   p2 <- st_as_sf(acess[nome_muni == input$cidade])
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
    
    if (input$indicador == "Cumulativo") {
      pal <- colorpal()
      
      popup_hex <- paste0("<b>População: </b>", tempo_filtrado()$P001, "<br/>",
                      "<b>% Oportunidades: </b>", tempo_filtrado()$valor, "<br/>")
      
      popup_linha <- paste0("<b>Tipo da linha</b>: ", linhas_cidade$Modo)
      
      names(linhas_cidade$geometry) <- NULL
      
      leafletProxy("map", data = tempo_filtrado()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(stroke = FALSE, 
                    fillOpacity = 0.7,
                    fillColor = ~pal(valor),
                    popup = popup_hex) %>%
        addPolylines(data = linhas_cidade,
                     opacity = 1,
                     group = "Corredores de Alta e Média Capacidade", 
                     popup = popup_linha, color = ~colorpal_linhas(Modo)) %>%
        addLayersControl(overlayGroups = "Corredores de Alta e Média Capacidade", position = "topleft",
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(data = tempo_filtrado(), "bottomleft", pal = pal, values = ~valor,
                  title = sprintf("Porcento de Oportunidades acessíveis<br/> em %s minutos", input$tempo)) %>%
        addLegend(data = linhas_cidade, "topleft", pal = colorpal_linhas, values = ~Modo,
                  title = "Modo da Linha", 
                  group = "Corredores de Alta e Média Capacidade")
      
      
    } else if (input$indicador == "Oportunidade mais próxima") {
      
      pal <- colorpal_min()
      
      popup_hex <- paste0("<b>População: </b>", atividade_filtrada_min()$P001, "<br/>")
      
      popup_linha <- paste0("<b>Tipo da linha</b>: ", linhas_cidade$Modo)
      
      names(linhas_cidade$geometry) <- NULL
      
      leafletProxy("map", data = atividade_filtrada_min()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(stroke = FALSE,
                    fillOpacity = 0.7,
                    fillColor = ~pal(valor),
                    popup = popup_hex) %>%
        addPolylines(data = linhas_cidade,
                     opacity = 1,
                     group = "Corredores de Alta e Média Capacidade", 
                     popup = popup_linha, color = ~colorpal_linhas(Modo)) %>%
        addLayersControl(overlayGroups = "Corredores de Alta e Média Capacidade", position = "topleft",
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(data = atividade_filtrada_min(), "bottomleft", pal = pal, values = ~valor,
                  title = c("Minutos atè a oportunidade mais próxima")) %>%
        addLegend(data = linhas_cidade, "topleft", pal = colorpal_linhas, values = ~Modo,
                  title = "Modo da Linha", 
                  group = "Corredores de Alta e Média Capacidade")
      
    }
      
      
    
      
  })
  
  
  
}
