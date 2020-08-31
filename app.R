library(shiny)
library(tidyverse)
library(pals)

source("global.R")

datos <-
    read_csv("director.csv", col_types = cols(.default = "c")) %>%
    dplyr::filter(`Nombre de la institución` != "") %>%
    mutate(across(everything(), str_to_upper))

preguntas <-
    read_csv("preguntas.csv") %>%
    dplyr::filter(as.logical(listo)) %>%
    mutate(num = str_extract(columna, "^.{1,5} "))

muestra <-
    read_csv("muestra.csv", col_types = cols(.default = "c")) %>%
    mutate(across(everything(), str_to_upper))

datos_GP <-
    read_csv("GP.csv", col_types = cols(.default = "c")) %>%
    filter(`Nombre de la institución` != "") %>%
    mutate(across(everything(), str_to_upper))

director_limpio <- read_csv("director limpio.csv", col_types = cols(.default = "c")) %>%
    mutate(across(everything(), str_to_upper))

GP_limpio <- read_csv("GP limpio.csv", col_types = cols(.default = "c")) %>%
    mutate(across(everything(), str_to_upper))

regiones <- unique(datos$Region)[order(unique(datos$Region))]
instituciones <-
    unique(datos$`Nombre de la institución`)[order(unique(datos$`Nombre de la institución`))]

ui <- fluidPage(# Application title
    titlePanel("Reporte de Gestión Institucional"),
    
    tabsetPanel(
        tabPanel("Reporte Gráfico",
                 sidebarLayout(
                     sidebarPanel(
                         #Seleccionar cuestionario
                         selectInput(
                             inputId = "cuestionario",
                             label =  "Selecciona el cuestionario",
                             choices = c("Director", "Gestión Pedagógica"),
                             selected = "Director"
                         ),
                         # Seleccionar pregunta test
                         uiOutput("render_pregunta"),
                         # Seleccionar región
                         selectInput(
                             inputId = "region",
                             label = "Regiones",
                             choices = c("Todas", regiones),
                             selected = "Todas"
                         ),
                         # Seleccionar tipo de institución
                         selectInput(
                             inputId = "tipo_inst",
                             label = "Tipo de institución",
                             choices = c("IEST", "CETPRO", "Ambos"),
                             selected = "IEST"
                         ),
                         # Seleccionar ruralidad
                         selectInput(
                             inputId = "ruralidad",
                             label = "Ruralidad",
                             choices = c("Todos", "NO RURAL", "IEST RURAL", "RURAL 1", "RURAL 2", "RURAL 3"),
                             selected = "Todos"
                         ),
                         # Seleccionar otra priorización
                         selectInput(
                             inputId = "otra_priorizacion",
                             label = "Otra priorización",
                             choices = c("Todas incluídas", "VRAEM", "FRONTERA"),
                             selected = "Todas incluídas"
                         ),
                         # check de ver  como porcentaje
                         checkboxInput(inputId = "tipo_resumen",
                                       label = "Ver como porcentaje")
                     ),
                     
                     mainPanel(tabsetPanel(
                         tabPanel("Gráfico",
                                  plotOutput("grafico")),
                         tabPanel("Instituciones",
                                  tableOutput("instituciones_filtradas"))
                     ))
                 )),
        tabPanel(
            "Reporte Individual",
            sidebarLayout(
                sidebarPanel(
                    # seleccionar institución
                    selectInput(
                        inputId = "institucion",
                        label = "Selecciona una institución",
                        choices = instituciones,
                        selected = instituciones[1]
                    ),
                    selectInput(
                        inputId = "tipo_reporte",
                        label = "Seleccione el tipo de reporte",
                        choices = c("Director", "Responsable de GP"),
                        selected = "Director"
                    )
                ),
                mainPanel(dataTableOutput("reporte_individual"))
            )
        )
    ))

server <- function(input, output) {
    # filtrar preguntas según cuestionario
    mis_preguntas <- reactive({
        filter(preguntas, cuestionario == input$cuestionario)$columna
    })
    # renderizar preguntas test
    output$render_pregunta <- renderUI({
        selectInput(
            inputId = "pregunta",
            label = "Selecciona la pregunta",
            choices = mis_preguntas(),
            selected = mis_preguntas()[1]
        )
    })
    # reactive de argumentos para funciones
    mi_p <- reactive({
        filter(preguntas, columna == input$pregunta)
    })
    # filtrar columna que contiene codigo de pregunta
    filtrado <- reactive({
        if (input$cuestionario == "Director") {
            data <- datos
        } else {
            data <- datos_GP
        }
        
        data <- left_join(data,
                          muestra,
                          by = c("Código modular de la institución" = "cod_mod"))
        
        if (input$tipo_inst != "Ambos") {
            data <- filter(data, `Tipo de institución` == input$tipo_inst)
        }
        
        if (input$region != "Todas" | input$region == "") {
            data <- filter(data, Region %in% input$region)
        }
        
        if (input$ruralidad != "Todos") {
            data <- filter(data, ruralidad == input$ruralidad)
        }
        
        if (input$otra_priorizacion != "Todas incluídas") {
            data <- filter(data, otra_priorizacion == input$otra_priorizacion)
        }
        data
    })
    
    pre_data <- reactive({
        filtrado() %>%
            select(x = contains(mi_p()$num)) %>%
            filter(!is.na(x))
    })
    
    my_data <- reactive({
        data <- pre_data() %>%
            my_pipe(separate = mi_p()$separate,
                    usar_porc = input$tipo_resumen)
        
        if (mi_p()$num %in% c("A2.3A ", "D3.2 ")){
            data <- mutate(data, x = fct_relevel(x, sort))
        } else if (mi_p()$num == "E1.1 ") {
            data <- mutate(data, x = fct_relevel(x, c("MENOS DE 1 HORAS", 
                                                      "ENTRE 1 Y 2 HORAS",
                                                      "ENTRE 2 Y 4 HORAS",
                                                      "MÁS DE 4 HORAS")))
        } else if (mi_p()$num == "E2.1 ") {
            data <- mutate(data, x = fct_relevel(x, c("DOS VECES POR SEMANA O MÁS",
                                                      "UNA VEZ A LA SEMANA",
                                                      "UNA VEZ CADA QUINCE DÍAS",
                                                      "UNA VEZ AL MES",
                                                      "MENOS DE UNA VEZ AL MES")))
        }
        data
    })
    
    my_custom_heigth <- reactive({
        filas_data <- nrow(my_data())
        if (filas_data < 8) {400} else if (filas_data < 12) {600} else {800}
    })
    
    my_subtitle <- reactive({
        tipo_resp <- if_else(mi_p()$separate, "múltiple", "único")
        nivel <- if_else(
            input$region == "Todas",
            "todas las regiones de la muestra",
            str_to_title(input$region)
        )
        paste("Análisis de",
              nrow(pre_data()),
              "respuestas de tipo",
              tipo_resp,
              "en",
              nivel)
    })
    
    output$grafico <- renderPlot({
        my_labs <- labs(
            title = str_wrap(input$pregunta, width = 100),
            subtitle = my_subtitle(),
            x = "Instituciones educativas"
        )
        if (mi_p()$num %in% c("")) {
            p <- ggplot(my_data(), aes(y = x)) + geom_histogram()
        } else {
            p <-  my_plot(
                my_data(),
                paleta = input$paleta,
                usar_porc = input$tipo_resumen
            )
        }
        p + my_labs + my_theme
    },
    height = my_custom_heigth)
    
    output$instituciones_filtradas <- renderTable({
        muestra %>%
            semi_join(filtrado(),
                      by = c("cod_mod" = "Código modular de la institución")) %>% 
            mutate(fila = row_number()) %>% 
            relocate(fila)
    })
    
    output$reporte_individual <- renderDataTable({
        if (input$tipo_reporte == "Director") {
            data <- director_limpio
        } else {
            data <- GP_limpio
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
