library(shiny)
library(tidyverse)

source("global.R")

datos <- read_csv("director.csv", col_types = cols(.default = "c")) %>% 
    filter(`Nombre de la institución` != "") %>% 
    mutate(across(everything(), str_to_upper)) %>% 
    filter(row_number() <= 30)

# datos_GP <- read_csv("GP.csv", col_types = cols(.default = "c"))%>% 
#     filter(`Nombre de la institución` != "") %>% 
#     mutate(across(everything(), str_to_upper)) %>% 
#     filter(row_number() <= 10)

preguntas <- read_csv("preguntas.csv") %>% 
    filter(listo) %>% 
    mutate(num = str_extract(columna, "^.{1,5} "))

ui <- fluidPage(

    # Application title
    titlePanel("Reporte de Gestión Institucional"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "pregunta",
                        label = "Selecciona la pregunta", 
                        choices = preguntas$columna,
                        selected = preguntas$columna[1]),
            selectInput(inputId = "paleta",
                        label = "Selecciona la paleta de colores", 
                        choices = rownames(RColorBrewer::brewer.pal.info),
                        selected = "Set1")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Gráfico",
                         plotOutput("grafico")),
                tabPanel("Paletas",
                         plotOutput("brewer"))
            )
        )
    )
)

server <- function(input, output) {
    mi_p <- reactive({
        filter(preguntas, columna == input$pregunta)
    })
    
    output$grafico <- renderPlot({
        datos %>% 
            my_plot(columna = mi_p()$num,
                    separate = mi_p()$separate,
                    orientation = mi_p()$orientation) +
            my_wrap() + 
            scale_fill_brewer(palette = input$paleta) +
            theme(legend.position = "none")
            
    },
    height = 600)
    
    output$brewer <- renderPlot({
        RColorBrewer::display.brewer.all()
    },
    height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)
