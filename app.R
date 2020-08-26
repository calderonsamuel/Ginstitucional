library(shiny)
library(tidyverse)
library(googlesheets4)
library(pals)

source("global.R")

datos_link <- "https://docs.google.com/spreadsheets/d/1WXUHyylk4brUA6DrDhEWnZbsAOoKqxP_eB83vbHTxU0/edit#gid=1442854212"
datos_GP_link <- "https://docs.google.com/spreadsheets/d/1x1504OFbZlJY14WaVJ7eRXCXoHucTYkozS0Sapkx-FM/edit#gid=1753305861"
gs4_deauth()
datos <- 
    read_sheet(datos_link, "Resp Recodificadas", col_types = "c")%>% 
        dplyr::filter(`Nombre de la institución` != "") %>%
        mutate(across(everything(), str_to_upper))

gs4_deauth()
datos_GP <- 
    read_sheet(datos_GP_link, "Resp Recodificadas", col_types = "c") %>% 
    # read_csv("GP.csv", col_types = cols(.default = "c"))%>%
    filter(`Nombre de la institución` != "") %>%
    mutate(across(everything(), str_to_upper))

gs4_deauth()
preguntas <- 
    read_sheet(datos_link, "Hoja 4")%>%
    dplyr::filter(as.logical(listo)) %>% 
    mutate(num = str_extract(columna, "^.{1,5} "))

regiones <- unique(datos$Region)[order(unique(datos$Region))]

ui <- fluidPage(

    # Application title
    titlePanel("Reporte de Gestión Institucional"),

    sidebarLayout(
        sidebarPanel(
            # Seleccionar región
            selectInput(inputId = "region",
                        label = "Regiones", 
                        choices = c("Todas", regiones),
                        selected = "Todas"),
            # Seleccionar tipo de institución
            selectInput(inputId = "tipo_inst",
                        label = "Tipo de institución",
                        choices = c("IEST", "CETPRO", "Ambos"),
                        selected = "IEST"),
            #Seleccionar cuestionario
            selectInput(inputId = "cuestionario",
                        label =  "Selecciona el cuestionario",
                        choices = c("Director", "Gestión Pedagógica"),
                        selected = "Director"),
            # Seleccionar pregunta test
            uiOutput("render_pregunta"),
            # check de ver  como porcentaje
            checkboxInput(inputId = "tipo_resumen",
                          label = "Ver como porcentaje")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Gráfico",
                         plotOutput("grafico"))
            )
        )
    )
)

server <- function(input, output) {
    # filtrar preguntas según cuestionario
    mis_preguntas <- reactive({
        filter(preguntas, cuestionario == input$cuestionario)$columna
    })
    # renderizar preguntas test
    output$render_pregunta <- renderUI({
        selectInput(inputId = "pregunta",
                    label = "Selecciona la pregunta", 
                    choices = mis_preguntas(),
                    selected = mis_preguntas()[1])
    })
    # reactive de argumentos para funciones
    mi_p <- reactive({
        filter(preguntas, columna == input$pregunta)
    })
    # filtrar columna que contiene codigo de pregunta
    filtrado <- reactive({
        if (input$cuestionario == "Director"){
            data <- datos
        } else {
            data <- datos_GP
        }
        
        if (input$tipo_inst != "Ambos"){
            data <- filter(data, `Tipo de institución` == input$tipo_inst)
        }
        
        if (input$region != "Todas" | input$region == ""){
            data <- filter(data, Region %in% input$region)
        }
        data %>%
            select(x = contains(mi_p()$num)) %>% 
            filter(!is.na(x))
    })
    
    my_data <- reactive({
        if(mi_p()$num %in% c("")){
            filtrado() %>% 
                separate_rows(x, sep = ", ") %>% 
                filter(!is.na(x), x != ",", x != " ") %>% 
                mutate(x = as.numeric(x))
        } else {
            filtrado()%>% 
                my_pipe(separate = mi_p()$separate,
                        usar_porc = input$tipo_resumen)  
        }
    })
    
    my_custom_heigth <- reactive({
        filas_data <- nrow(my_data())
        if(filas_data < 8 | mi_p()$num %in% c("")) 400 else if(filas_data < 12) 600 else 800
    })
    
    my_subtitle <- reactive({
        tipo_resp <- if_else(mi_p()$separate, "múltiple", "único")
        nivel <- if_else(input$region == "Todas", 
                         "todas las regiones de la muestra", 
                         str_to_title(input$region))
        paste("Análisis de", 
              nrow(filtrado()), "respuestas de tipo", tipo_resp,
              "en", nivel)
    })
    
    output$grafico <- renderPlot({
        my_labs <- labs(title = str_wrap(input$pregunta, width = 100),
                       subtitle = my_subtitle(),
                       x = "Instituciones educativas")
        if (mi_p()$num %in% c("")){
            p <- ggplot(my_data(), aes(y = x)) + geom_histogram()
        } else {
           p <-  my_plot(my_data(), 
                    paleta = input$paleta,
                    usar_porc = input$tipo_resumen)
        }
        p + my_labs + my_theme
    },
    height = my_custom_heigth)
    
    output$brewer <- renderPlot({
        RColorBrewer::display.brewer.all()
    },
    height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)
