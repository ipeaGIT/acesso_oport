library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(readr)
library(data.table)
library(mapdeck)
library(shinydashboard)



# abrir acessibilidade

acess_cum <- read_rds("acess_tp_cum_app.rds") %>%
  data.table()

acess_min <- read_rds("acess_tp_min_app.rds") %>%
  data.table()

# linhas <- read_rds("../../../data/linhas_HMcapacidade/linhas_HMcapacidade.rds")

limits <- read_rds("cities_centroids.rds")




# aquiiiiiiii ---------------------------------------------------------------------------------




ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput(inputId = "cidade",
                label = h1("Escolha a cidade:"),
                choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro",
                            "São Paulo", "Curitiba", "Porto Alegre", "Recife")),
    radioButtons(inputId = "indicador",
                 label = h1("Escolha o indicador de acessibilidade:"),
                 choices = c("Cumulativo", "Oportunidade mais próxima"),
                 selected = "Cumulativo"),
    conditionalPanel(condition = "input.indicador == 'Cumulativo'",
                     selectInput(inputId = "atividade_cum",
                                 label = h1("Escolha a atividade:"),
                                 choices = c("Trabalho", "Saúde", "Educação"),
                                 selected = "Trabalho")),
    conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
                     selectInput(inputId = "atividade_min",
                                 label = h1("Escolha a atividade:"),
                                 choices = c("Saúde", "Educação"),
                                 selected = "Saúde")),
    conditionalPanel(condition = "input.indicador == 'Cumulativo'",
                     sliderInput(inputId = "tempo",
                                 label = h1("Escolha o tempo de viagem:"),
                                 min = 30, max = 120,
                                 step = 30, value = 30,
                                 animate = TRUE))
  ),
  dashboardBody(
    mapdeckOutput(outputId = "map")
  )
  
)



# register mapbox api key
my_api <- data.table::fread("../../../data-raw/mapbox_key.txt", header = F)
set_token(my_api$V1)








# Define a server for the Shiny app
server <- function(input, output) {
  
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
  
  
  
  # baseMap
  output$map <- renderMapdeck({
    
    
    mapdeck()
    
    
  })
  
  
  centroid_go <- reactive({
    filter(limits, name_muni == input$cidade)
  })
  
  observe({
    
    # zoom in the city
    proxy <- mapdeck_update(map_id = "map") %>%
      mapdeck_view(location = c(centroid_go()$lon, centroid_go()$lat), zoom = 10,
                   transition = "fly")
      
    
    # print(paste0("aaah", centroid_go$lon, centroid_go$lat))
    
    if (input$indicador == "Cumulativo") {
      
      proxy %>%
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
      
      proxy %>%
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

shinyApp(ui, server)
