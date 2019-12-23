library(shiny)
library(RColorBrewer)
library(shinyWidgets)
library(mapdeck)
library(shinyBS)



# UI SO COM UMA ABA -------------------------------------------------------

div(class = "navbar-default",
    navbarPage("Acesso a Oportunidades",
               tabPanel("Mapa",
                        tags$head(includeCSS("www/styles.css")),
                        # https://divadnojnarg.github.io/blog/customsliderinput/
                        chooseSliderSkin("Round"),
                        # titlePanel(HTML("<h1>&emsp;Acesso a Oportunidades</h1>")),
                        mapdeckOutput("map"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                                      top = 80, right = 20, width = 350, height = 700,
                                      selectInput(inputId = "cidade",
                                                  label = h1("Escolha a cidade:"),
                                                  choices = c("Belo Horizonte" = "bho", 
                                                              "Fortaleza" = "for", 
                                                              "Rio de Janeiro" = "rio",
                                                              "São Paulo" = "spo", 
                                                              "Curitiba" = "cur", 
                                                              "Porto Alegre" = "poa", 
                                                              "Recife" = "rec"),
                                                  selected = "Fortaleza"),
                                      awesomeRadio(inputId = "indicador",
                                                   # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                   label = HTML("<h1>Escolha o indicador de acessibilidade: 
                                                                  <button id=\"q1\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-question\"></i></button></h1>"),
                                                   choices = c("Cumulativo", "Oportunidade mais próxima"),
                                                   selected = "Cumulativo"),
                                      radioButtons(inputId = "modo",
                                                   # label = HTML("<h1>Escolha o indicador de acessibilidade: <img src=\"ipea.jpg\" align=\"leftright\" width=\"70\"/></h1>"),
                                                   label = HTML("<h1>Escolha o modo de transporte: 
                                                                  <button id=\"q2\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-question\"></i></button></h1>"),
                                                   choiceNames = list(HTML("<i class=\"fas fa-bus fa-2x\"></i>"), 
                                                                      HTML("<i class=\"fas fa-walking fa-2x\"></i>"),
                                                                      HTML("<i class=\"fas fa-bicycle fa-2x\"></i>")),
                                                   choiceValues = list("tp", 
                                                                       "caminhada",
                                                                       "bicicleta"),
                                                   inline = TRUE,
                                                   width = "100%"
                                      ),
                                      div(
                                        # edit2
                                        bsPopover(id = "q1", 
                                                  title = "Indicadores de acessibilidade",
                                                  content = "Indicador cumulativo representa quantas oportunidades podem ser alcançadas dado o tempo de viagem",
                                                  placement = "top",
                                                  trigger = "click",
                                                  options = list(container = "body"))
                                        ),
                                      # img(src='ipea.jpg', align = "right", width = "150"),
                                      conditionalPanel(condition = "input.indicador == 'Cumulativo'",
                                                       selectInput(inputId = "atividade_cum",
                                                                   label = h1("Escolha a atividade:"),
                                                                   choices = list(
                                                                     'Trabalho' = c("Trabalho Total" = "TT",
                                                                                    "Trabalho Quintil" = "TQ",
                                                                                    "Trabalho Decil" = "TD"),
                                                                     'Saúde' = c("Saúde Total" = "ST",
                                                                                 "Saúde Baixa" = "SB",
                                                                                 "Saúde Média" = "SM",
                                                                                 "Saúde Alta" = "SA"),
                                                                     'Educação' = c("Educação Total" = "ET",
                                                                                    "Educação Infantil" = "EI",
                                                                                    "Educação Fundamental" = "EF",
                                                                                    "Educação Média" = "EM")),
                                                                   selected = "Trabalho Total")),
                                      conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
                                                       selectInput(inputId = "atividade_min",
                                                                   label = h1("Escolha a atividade:"),
                                                                   choices = list(
                                                                     'Saúde' = c("Saúde Total" = "ST",
                                                                                 "Saúde Baixa" = "SB",
                                                                                 "Saúde Média" = "SM",
                                                                                 "Saúde Alta" = "SA"),
                                                                     'Educação' = c("Educação Total" = "ET",
                                                                                    "Educação Infantil" = "EI",
                                                                                    "Educação Fundamental" = "EF",
                                                                                    "Educação Média" = "EM")),
                                                                   selected = "Saúde")),
                                      conditionalPanel(condition = "input.indicador == 'Cumulativo'",
                                                       sliderInput(inputId = "tempo",
                                                                   label = h1("Escolha o tempo de viagem:"),
                                                                   min = 30, max = 120,
                                                                   step = 30, value = 30,
                                                                   animate = TRUE)),
                                      conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
                                                       strong("Observação"), p("Valores truncados para 30 minutos"))
                        )
               ),
               tabPanel("Sobre o projeto",
                        sidebarPanel(includeMarkdown("about.md"))
               )

    )
)




# div(class = "navbar-default",
#     navbarPage("Acesso a Oportunidades",
#                tabPanel("Mapa",
#                         tags$head(includeCSS("www/styles.css")),
#                         # https://divadnojnarg.github.io/blog/customsliderinput/
#                         chooseSliderSkin("Modern"),
#                         # titlePanel(HTML("<h1>&emsp;Acesso a Oportunidades</h1>")),
#                         mapdeckOutput("map"),
#                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
#                                       top = 80, right = 20, width = 300, height = 600,
#                                       selectInput(inputId = "cidade",
#                                                   label = h1("Escolha a cidade:"), 
#                                                   choices = c("Belo Horizonte", "Fortaleza", "Rio de Janeiro",
#                                                               "São Paulo", "Curitiba", "Porto Alegre", "Recife"),
#                                                   selected = "Fortaleza"),
#                                       div(
#                                         style="display: inline-block;vertical-align:top;width: 230px;",
#                                         awesomeRadio(inputId = "indicador",
#                                                      label = h1("Escolha o indicador de acessibilidade: "),
#                                                      choices = c("Cumulativo", "Oportunidade mais próxima"),
#                                                      selected = "Cumulativo")),
#                                       div(
#                                         # edit2
#                                         style="display: inline-block;vertical-align:top;float:right;",
#                                         bsButton("q1", label = "", icon = icon("question"),
#                                                  style = "info", size = "extra-small"),
#                                         bsPopover(id = "q1", title = "Tidy data",
#                                                   content = paste0("You should read the ", 
#                                                                    a("tidy data paper", 
#                                                                      href = "http://vita.had.co.nz/papers/tidy-data.pdf",
#                                                                      target="_blank")),
#                                                   placement = "left", 
#                                                   trigger = "click"
#                                         )),
#                                       # img(src='ipea.jpg', align = "right", width = "150"),
#                                       conditionalPanel(condition = "input.indicador == 'Cumulativo'",
#                                                        selectInput(inputId = "atividade_cum",
#                                                                    label = h1("Escolha a atividade:"),
#                                                                    choices = list(
#                                                                      'Trabalho' = c("Trabalho Total" = "TT",
#                                                                                     "Trabalho Quintil" = "TQ",
#                                                                                     "Trabalho Decil" = "TD"),
#                                                                      'Saúde' = c("Saúde Total" = "ST",
#                                                                                  "Saúde Baixa" = "SB",
#                                                                                  "Saúde Média" = "SM",
#                                                                                  "Saúde Alta" = "SA"),
#                                                                      'Educação' = c("Educação Total" = "ET",
#                                                                                     "Educação Infantil" = "EI",
#                                                                                     "Educação Fundamental" = "EF",
#                                                                                     "Educação Média" = "EM")),
#                                                                    selected = "Trabalho Total")),
#                                       conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
#                                                        selectInput(inputId = "atividade_min",
#                                                                    label = h1("Escolha a atividade:"),
#                                                                    choices = list(
#                                                                      'Saúde' = c("Saúde Total" = "ST",
#                                                                                  "Saúde Baixa" = "SB",
#                                                                                  "Saúde Média" = "SM",
#                                                                                  "Saúde Alta" = "SA"),
#                                                                      'Educação' = c("Educação Total" = "ET",
#                                                                                     "Educação Infantil" = "EI",
#                                                                                     "Educação Fundamental" = "EF",
#                                                                                     "Educação Média" = "EM")),
#                                                                    selected = "Saúde")),
#                                       conditionalPanel(condition = "input.indicador == 'Cumulativo'",
#                                                        sliderInput(inputId = "tempo",
#                                                                    label = h1("Escolha o tempo de viagem:"),
#                                                                    min = 30, max = 120,
#                                                                    step = 30, value = 30,
#                                                                    animate = TRUE)),
#                                       conditionalPanel(condition = "input.indicador == 'Oportunidade mais próxima'",
#                                                        strong("Observação"), p("Valores truncados para 30 minutos"))
#                         )
#                ),
#                tabPanel("Sobre o projeto",
#                         sidebarPanel(includeMarkdown("about.md"))
#                )
#                
#     )
# )