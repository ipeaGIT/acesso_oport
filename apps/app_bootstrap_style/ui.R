library(shiny)
library(leaflet)
library(RColorBrewer)

bootstrapPage(
  titlePanel(HTML("<h1>&emsp;Acesso a Oportunidades</h1>")), 
  tags$head(includeCSS("www/styles.css")),
  leafletOutput("map", width = "100%", height = "90%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                top = 60, right = 20, width = 300, height = 600,
                selectInput(inputId = "cidade", 
                            label = HTML("<h1>Escolha a cidade:</h1>"), 
                            choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro"), 
                            selected = "Fortaleza"),
                selectInput(inputId = "atividade", 
                            label = HTML("<h1>Escolha a atividade</h1>"), 
                            choices = c("Saúde", "Educação"), 
                            selected = "Saúde"),
                radioButtons(inputId = "indicador", 
                             label = HTML("<h1>Escolha o indicador de acessibilidade:</h1>"), 
                             choices = c("Indicador Cumulativo", "Indicador de oportunidade mais próxima"), 
                             selected = "Indicador Cumulativo"),
                conditionalPanel(condition = "input.indicador == 'Indicador Cumulativo'",
                                 sliderInput(inputId = "tempo", 
                                             label = HTML("<h1>Escolha o tempo de viagem:</h1>"),
                                             min = 15, max = 60,
                                             step = 15, value = 15,
                                             animate = TRUE))
  )
)
