library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(mapdeck)



# UI SO COM UMA ABA -------------------------------------------------------

div(class = "navbar-default",
    navbarPage("Acesso a Oportunidades",
               tabPanel("Mapa",
                        tags$head(includeCSS("www/styles.css")),
                        # https://divadnojnarg.github.io/blog/customsliderinput/
                        chooseSliderSkin("Modern"),
                        # titlePanel(HTML("<h1>&emsp;Acesso a Oportunidades</h1>")),
                        mapdeckOutput("map"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                      top = 80, right = 20, width = 350, height = 600,
                                      selectInput(inputId = "cidade",
                                                  label = h1("Escolha a cidade:"),
                                                  choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro",
                                                              "São Paulo", "Curitiba", "Porto Alegre", "Recife"),
                                                  selected = "Fortaleza"),
                                      awesomeRadio(inputId = "indicador",
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
                        )
               ),
               tabPanel("Sobre o projeto",
                        sidebarPanel(includeMarkdown("about.md"))
               )

    )
)



# TESTE -------------------------------------------------------------------


# navbarPage("Acesso a Oportunidades",
#            tabPanel("Visualização",
#                     tags$head(includeCSS("www/styles2.css")),
#                     # https://divadnojnarg.github.io/blog/customsliderinput/
#                     chooseSliderSkin("Modern"),
#                     sidebarPanel(id = "controls", class = "panel panel-default", width = 2,      
#                                  selectInput(inputId = "cidade", 
#                                              label = h1("Escolha a cidade:"), 
#                                              choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro"), 
#                                              selected = "Fortaleza"),
#                                  selectInput(inputId = "atividade", 
#                                              label = h1("Escolha a atividade:"), 
#                                              choices = c("Saúde", "Educação"),
#                                              selected = "Saúde"),
#                                  awesomeRadio(inputId = "indicador", 
#                                               label = h1("Escolha o indicador de acessibilidade:"), 
#                                               choices = c("Cumulativo", "Oportunidade mais próxima"), 
#                                               selected = "Cumulativo"),
#                                  conditionalPanel(condition = "input.indicador == 'Cumulativo'",
#                                                   sliderInput(inputId = "tempo", 
#                                                               label = h1("Escolha o tempo de viagem:"),
#                                                               min = 15, max = 60,
#                                                               step = 15, value = 15,
#                                                               animate = TRUE))
#                     ),
#                     mainPanel(
#                       
#                       tabsetPanel(
#                         tabPanel("Map",leafletOutput("map")),
#                         tabPanel("Gráficos",
#                                  div(id = "controls", class = "panel panel-default", 
#                                      h1("So fazendo uns testes!"))   
#                         )
#                       )
#                     )
#            ),
#            tabPanel("Sobre o projeto",
#                     sidebarPanel(includeMarkdown("about.md")))
# )
  
