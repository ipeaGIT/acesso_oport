library(shiny)
library(mapview)
library(sf)
library(shinydashboard)
# library(shinythemes)
# library(semantic.dashboard)
library(readr)
library(leaflet)
library(dplyr)

header <- dashboardHeader(
  title = div(icon("bus"), " Acesso a Oportunidades"),
  # title = "Linhas de Transporte Público de Fortaleza",
  titleWidth = 400
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(width = NULL, 
               status = "warning",
               selectInput(inputId = "cidade", 
                           label = "Escolha a cidade:", 
                           choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro"), 
                           selected = "Fortaleza")),
           box(width = NULL,
               status = "warning",
               selectInput(inputId = "atividade", 
                           label = "Escolha a atividade:", 
                           choices = c("Saúde", "Educação"), 
                           selected = "Saúde")),
           box(width = NULL,
               status = "warning",
               radioButtons(inputId = "indicador", 
                           label = "Escolha o indicador de acessibilidade:", 
                           choices = c("Indicador Cumulativo", "Indicador de oportunidade mais próxima"), 
                           selected = "Indicador Cumulativo")),
           conditionalPanel(
             condition = "input.indicador == 'Indicador Cumulativo'",
             box(width = NULL,
                 status = "warning",
                 sliderInput(inputId = "tempo", 
                             label = "Escolha o tempo de viagem:",
                             min = 15, max = 60,
                             step = 15, value = 15,
                             animate = TRUE)))),
           # box(width = NULL, 
           #     status = "warning",
           #     sliderInput(inputId = "tempo", 
           #                  label = "Escolha o tempo de viagem:", 
           #                  min = 15, max = 60, 
           #                  step = 15, value = 15,
           #                 animate = TRUE))),
    column(width = 9,
           box(width = NULL, 
               solidHeader = TRUE,
               title = "Oportunidades",
               leafletOutput("map", height = 500)
           ))
           
  ),
  tags$head(
    tags$link(href = "custom.css", rel = "stylesheet")
  )
)


dashboardPage(
  # title = "Indicadores de Acessibilidade Cumulativa",
  header,
  dashboardSidebar(disable = T),
  body,
  skin = "black"
)