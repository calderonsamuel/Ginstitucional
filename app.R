library(shiny)
library(tidyverse)

source("global.R")

datos <- read_csv("director.csv", col_types = cols(.default = "c")) %>% 
    filter(`Nombre de la institución` != "") %>% 
    mutate(across(everything(), str_to_upper)) %>% 
    filter(row_number() <= 10)

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
                        selected = preguntas$columna[1], 
                        selectize = TRUE)
        ),

        mainPanel(
            plotOutput("grafico")
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
            geom_label()
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
