library(shiny)
library(tidyverse)
library(pals)

source("global.R")

datos <- 
    read_csv("director.csv", col_types = cols(.default = "c")) %>% 
    # read_sheet(datos_link, "Resp Recodificadas", col_types = "c")%>% 
        dplyr::filter(`Nombre de la institución` != "") %>%
        mutate(across(everything(), str_to_upper))

preguntas <- 
    read_csv("preguntas.csv") %>% 
    dplyr::filter(as.logical(listo)) %>% 
    mutate(num = str_extract(columna, "^.{1,5} "))

muestra <- 
    read_csv("muestra.csv", col_types = cols(.default = "c"))

datos_GP <- 
    read_csv("GP.csv", col_types = cols(.default = "c"))%>%
    filter(`Nombre de la institución` != "") %>%
    mutate(across(everything(), str_to_upper))

regiones <- unique(datos$Region)[order(unique(datos$Region))]
instituciones <- unique(datos$`Nombre de la institución`)[order(unique(datos$`Nombre de la institución`))]

ui <- fluidPage(

    # Application title
    titlePanel("Reporte de Gestión Institucional"),
    
    tabsetPanel(
        tabPanel("Reporte Gráfico",
                 sidebarLayout(
                     sidebarPanel(
                         #Seleccionar cuestionario
                         selectInput(inputId = "cuestionario",
                                     label =  "Selecciona el cuestionario",
                                     choices = c("Director", "Gestión Pedagógica"),
                                     selected = "Director"),
                         # Seleccionar pregunta test
                         uiOutput("render_pregunta"),
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
                         # Seleccionar ruralidad
                         
                         # Seleccionar otra priorización
                         
                         # check de ver  como porcentaje
                         checkboxInput(inputId = "tipo_resumen",
                                       label = "Ver como porcentaje")
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Gráfico",
                                    plotOutput("grafico")
                          ),
                            tabPanel("Instituciones",
                                    tableOutput("instituciones_filtradas") 
                                     ))
                             
                         )
                 )
                 ),
        tabPanel("Reporte Individual",
                 sidebarLayout(
                     sidebarPanel(
                         # seleccionar institución
                         selectInput(inputId = "institucion", 
                                     label = "Selecciona una institución",
                                     choices = instituciones,
                                     selected = instituciones[1]),
                         selectInput(inputId = "tipo_reporte",
                                     label = "Seleccione el tipo de reporte",
                                     choices = c("Director", "Responsable de GP"),
                                     selected = "Director")
                     ),
                     mainPanel(
                         dataTableOutput("reporte_individual")
                     )
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
        data
    })
    
    pre_data <- reactive({
        filtrado() %>%
            select(x = contains(mi_p()$num)) %>% 
            filter(!is.na(x))
    })
    
    
    
    my_data <- reactive({
        if(mi_p()$num %in% c("")){
            pre_data() %>% 
                separate_rows(x, sep = ", ") %>% 
                filter(!is.na(x), x != ",", x != " ") %>% 
                mutate(x = as.numeric(x))
        } else {
            pre_data()%>% 
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
              nrow(pre_data()), "respuestas de tipo", tipo_resp,
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
    
    output$instituciones_filtradas <- renderTable({
        muestra %>% 
            semi_join(filtrado(), 
                      by = c("cod_mod" = "Código modular de la institución"))
    })
    
    output$reporte_individual <- renderDataTable({
        if (input$tipo_reporte == "Director"){
            data <- datos
        } else {
            data <- datos_GP
        }
        data %>% 
            filter(`Nombre de la institución` == input$institucion) %>% 
            pivot_longer(cols = everything(), 
                         names_to = "Pregunta", 
                         values_to = "Respuesta") %>% 
            filter(Respuesta != "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
