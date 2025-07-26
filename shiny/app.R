library(shiny)
library(shinydashboard)
library(DT)
library(reshape2)

# Cargar modelo y funciones
modelo_completo <- readRDS("modelo_pricing_completo.rds")

# Cargar funciones (local o desde src)
if (file.exists("tablas_primas_puras.R")) {
  source("tablas_primas_puras.R")  # Para despliegue en shinyapps.io
} else {
  source("../src/utils/tablas_primas_puras.R")  # Para desarrollo local
}

ui <- dashboardPage(
  dashboardHeader(title = "Calculadora de Prima Pura - Seguros de Automóviles"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculadora", tabName = "calculadora", icon = icon("calculator")),
      menuItem("Tablas de Tarifas", tabName = "tablas", icon = icon("table")),
      menuItem("Información", tabName = "info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
        }
        .btn-primary {
          background-color: #3c8dbc;
          border-color: #357ca5;
        }
      "))
    ),
    
    tabItems(
      # Pestaña de calculadora
      tabItem(tabName = "calculadora",
        fluidRow(
          box(
            title = "Selección de Variables", status = "primary", solidHeader = TRUE,
            width = 6,
            
            h4("Características del Vehículo"),
            selectInput("modelo", "Modelo del Vehículo:",
                       choices = modelo_completo$niveles_factores$Modelo,
                       selected = modelo_completo$niveles_factores$Modelo[1]),
            
            selectInput("color", "Color:",
                       choices = modelo_completo$niveles_factores$Color,
                       selected = modelo_completo$niveles_factores$Color[1]),
            
            selectInput("carroceria", "Tipo de Carrocería:",
                       choices = modelo_completo$niveles_factores$Carroceria,
                       selected = modelo_completo$niveles_factores$Carroceria[1]),
            
            h4("Características del Servicio y Conductor"),
            selectInput("servicio", "Tipo de Servicio:",
                       choices = modelo_completo$niveles_factores$SERVICIO,
                       selected = modelo_completo$niveles_factores$SERVICIO[1]),
            
            selectInput("edad", "Edad del Conductor:",
                       choices = modelo_completo$niveles_factores$Edad,
                       selected = modelo_completo$niveles_factores$Edad[1]),
            
            numericInput("exposicion", "Exposición (años):",
                        value = 1, min = 0.1, max = 5, step = 0.1),
            
            br(),
            div(style = "padding: 10px 0;",
                actionButton("calcular", "Calcular Prima", 
                            class = "btn-primary btn-lg", 
                            style = "width: 100%; height: 50px; font-size: 16px;")
            )
          ),
          
          box(
            title = "Resultados del Cálculo", status = "success", solidHeader = TRUE,
            width = 6,
            
            div(style = "text-align: center; padding: 15px 0;",
                h3("Prima Pura Calculada", style = "margin-bottom: 10px;"),
                h1(textOutput("prima_pura"), style = "color: #00a65a; font-weight: bold; margin: 0;")
            ),
            
            hr(),
            
            h4("Desglose del Cálculo:", style = "margin-bottom: 15px;"),
            div(style = "margin-bottom: 20px;",
                tableOutput("desglose_calculo")
            ),
            
            h4("Interpretación:", style = "margin-bottom: 10px;"),
            div(
              style = "background-color: #f0f8ff; padding: 12px; border-radius: 5px; border-left: 4px solid #3c8dbc; font-size: 14px; line-height: 1.4;",
              textOutput("interpretacion")
            )
          )
        )
      ),
      
      # Pestaña de tablas de tarifas
      tabItem(tabName = "tablas",
        fluidRow(
          box(
            title = "Controles de Visualización", status = "primary", solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                selectInput("servicio_tabla", "Tipo de Servicio:",
                           choices = modelo_completo$niveles_factores$SERVICIO,
                           selected = modelo_completo$niveles_factores$SERVICIO[1])
              ),
              column(4,
                numericInput("exposicion_tabla", "Exposición (años):",
                            value = 1, min = 0.1, max = 5, step = 0.1)
              ),
              column(4,
                br(),
                actionButton("generar_tabla", "Generar Tabla", 
                            class = "btn-primary", style = "margin-top: 5px;"),
                br(), br(),
                downloadButton("descargar_excel", "Descargar Excel", 
                              class = "btn-success")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Tabla de Primas Puras", status = "success", solidHeader = TRUE,
            width = 12,
            
            div(style = "margin-bottom: 15px;",
                h4(textOutput("titulo_tabla"), style = "text-align: center; color: #00a65a;")
            ),
            
            div(style = "overflow-x: auto;",
                DT::dataTableOutput("tabla_primas")
            ),
            
            hr(),
            
            h4("Resumen Estadístico:"),
            fluidRow(
              column(6,
                h5("Estadísticas de la Tabla:"),
                tableOutput("estadisticas_tabla")
              ),
              column(6,
                h5("Interpretación:"),
                div(
                  style = "background-color: #f0f8ff; padding: 12px; border-radius: 5px; border-left: 4px solid #3c8dbc; font-size: 14px; line-height: 1.4;",
                  textOutput("interpretacion_tabla")
                )
              )
            )
          )
        )
      ),
      
      # Pestaña de información
      tabItem(tabName = "info",
        fluidRow(
          box(
            title = "Acerca de esta Aplicación", status = "primary", solidHeader = TRUE,
            width = 12,
            
            h3("Calculadora de Prima Pura para Seguros de Automóviles"),
            
            p("Esta aplicación utiliza modelos GLM (Modelos Lineales Generalizados) para calcular 
              la prima pura de seguros de automóviles basada en el enfoque frecuencia-severidad."),
            
            h4("Metodología:"),
            tags$ul(
              tags$li("Modelo de Frecuencia: Quasi-Poisson para predecir la tasa de siniestros"),
              tags$li("Modelo de Severidad: Log-normal para predecir el costo promedio por siniestro"),
              tags$li("Prima Pura = Frecuencia × Severidad × Exposición")
            ),
            
            h4("Variables del Modelo:"),
            tags$ul(
              tags$li("Modelo del Vehículo: Año de fabricación agrupado"),
              tags$li("Color: Color del vehículo"),
              tags$li("Carrocería: Tipo de carrocería del vehículo"),
              tags$li("Servicio: Uso del vehículo (Particular, Público, etc.)"),
              tags$li("Edad: Edad del conductor agrupada"),
              tags$li("Exposición: Tiempo de cobertura en años")
            ),
            
            h4("Desarrollado por:"),
            p("José Miguel Acuña Hernández - Maestría en Actuaría y Finanzas"),
            p("Universidad Nacional de Colombia"),
            p(paste("Fecha de actualización:", Sys.Date()))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Variables reactivas para las tablas
  tabla_completa_datos <- reactiveVal(NULL)
  tablas_por_servicio_datos <- reactiveVal(NULL)
  
  # Función para calcular prima pura
  calcular_resultado <- reactive({
    
    req(input$calcular)
    
    isolate({
      # Crear perfiles para predicción
      perfil_freq <- data.frame(
        Modelo = factor(input$modelo, levels = modelo_completo$niveles_factores$Modelo),
        Edad = factor(input$edad, levels = modelo_completo$niveles_factores$Edad),
        exposicion_total = input$exposicion
      )
      
      perfil_sev <- data.frame(
        Modelo = factor(input$modelo, levels = modelo_completo$niveles_factores$Modelo),
        Color = factor(input$color, levels = modelo_completo$niveles_factores$Color),
        Carroceria = factor(input$carroceria, levels = modelo_completo$niveles_factores$Carroceria),
        SERVICIO = factor(input$servicio, levels = modelo_completo$niveles_factores$SERVICIO),
        Edad = factor(input$edad, levels = modelo_completo$niveles_factores$Edad),
        n_siniestros = 1
      )
      
      # Predicciones
      freq_pred <- predict(modelo_completo$modelo_frecuencia, newdata = perfil_freq, type = "response")
      tasa_frecuencia <- freq_pred / input$exposicion
      
      sev_pred_log <- predict(modelo_completo$modelo_severidad, newdata = perfil_sev, type = "response")
      severidad_estimada <- exp(sev_pred_log)
      
      prima_pura <- tasa_frecuencia * severidad_estimada * input$exposicion
      
      list(
        frecuencia = as.numeric(tasa_frecuencia),
        severidad = as.numeric(severidad_estimada),
        prima = as.numeric(prima_pura),
        exposicion = input$exposicion
      )
    })
  })
  
  # Resultado principal
  output$prima_pura <- renderText({
    resultado <- calcular_resultado()
    paste("$", format(round(resultado$prima), big.mark = ","), "COP")
  })
  
  # Desglose del cálculo
  output$desglose_calculo <- renderTable({
    resultado <- calcular_resultado()
    
    data.frame(
      Componente = c("Frecuencia Anual", "Severidad Promedio", "Exposición", "Prima Pura"),
      Valor = c(
        round(resultado$frecuencia, 4),
        paste("$", format(round(resultado$severidad), big.mark = ","), "COP"),
        paste(resultado$exposicion, "años"),
        paste("$", format(round(resultado$prima), big.mark = ","), "COP")
      ),
      Descripción = c(
        "Siniestros esperados por año",
        "Costo promedio por siniestro",
        "Período de cobertura",
        "Prima pura total"
      )
    )
  }, bordered = TRUE, striped = TRUE)
  
  # Interpretación
  output$interpretacion <- renderText({
    resultado <- calcular_resultado()
    
    paste("Para el perfil seleccionado, se espera una frecuencia de",
          round(resultado$frecuencia, 4), "siniestros por año, con un costo promedio de",
          paste("$", format(round(resultado$severidad), big.mark = ","), "COP"),
          "por siniestro. La prima pura para", resultado$exposicion, 
          "año(s) de cobertura es de",
          paste("$", format(round(resultado$prima), big.mark = ","), "COP."))
  })
  
  # === LÓGICA PARA TABLAS DE TARIFAS ===
  
  # Generar tabla completa cuando se presiona el botón
  observeEvent(input$generar_tabla, {
    
    withProgress(message = 'Generando tabla completa...', value = 0, {
      
      incProgress(0.3, detail = "Calculando 768 primas puras...")
      tabla_completa <- generar_tabla_completa_primas(modelo_completo, input$exposicion_tabla)
      tabla_completa_datos(tabla_completa)
      
      incProgress(0.7, detail = "Organizando tablas por servicio...")
      tablas_por_servicio <- crear_tablas_por_servicio(tabla_completa)
      tablas_por_servicio_datos(tablas_por_servicio)
      
      incProgress(1, detail = "¡Completado!")
    })
  })
  
  # Título dinámico de la tabla
  output$titulo_tabla <- renderText({
    req(input$servicio_tabla, input$exposicion_tabla)
    paste("Primas Puras para Servicio:", input$servicio_tabla, 
          "- Exposición:", input$exposicion_tabla, "año(s)")
  })
  
  # Tabla principal
  output$tabla_primas <- DT::renderDataTable({
    
    req(tablas_por_servicio_datos())
    req(input$servicio_tabla)
    
    tabla_servicio <- tablas_por_servicio_datos()[[input$servicio_tabla]]
    
    if (is.null(tabla_servicio)) {
      return(data.frame(Mensaje = "Genere la tabla primero"))
    }
    
    # Formatear valores monetarios
    tabla_formateada <- tabla_servicio
    cols_numericas <- sapply(tabla_formateada, is.numeric)
    tabla_formateada[cols_numericas] <- lapply(tabla_formateada[cols_numericas], function(x) {
      paste("$", format(round(x), big.mark = ","))
    })
    
    DT::datatable(
      tabla_formateada,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv'),
        columnDefs = list(
          list(targets = "_all", className = "dt-center")
        )
      ),
      class = "cell-border stripe hover",
      rownames = FALSE
    )
  })
  
  # Estadísticas de la tabla
  output$estadisticas_tabla <- renderTable({
    
    req(tabla_completa_datos())
    req(input$servicio_tabla)
    
    datos_servicio <- tabla_completa_datos()[tabla_completa_datos()$SERVICIO == input$servicio_tabla, ]
    
    data.frame(
      Estadística = c("Combinaciones", "Prima Mínima", "Prima Máxima", "Prima Promedio"),
      Valor = c(
        nrow(datos_servicio),
        paste("$", format(round(min(datos_servicio$prima_pura)), big.mark = ","), "COP"),
        paste("$", format(round(max(datos_servicio$prima_pura)), big.mark = ","), "COP"),
        paste("$", format(round(mean(datos_servicio$prima_pura)), big.mark = ","), "COP")
      )
    )
  }, bordered = TRUE)
  
  # Interpretación de la tabla
  output$interpretacion_tabla <- renderText({
    
    req(tabla_completa_datos())
    req(input$servicio_tabla)
    
    datos_servicio <- tabla_completa_datos()[tabla_completa_datos()$SERVICIO == input$servicio_tabla, ]
    
    rango_prima <- max(datos_servicio$prima_pura) / min(datos_servicio$prima_pura)
    
    paste("La tabla muestra", nrow(datos_servicio), "combinaciones posibles para el servicio",
          input$servicio_tabla, ". El rango de variación de las primas es de",
          round(rango_prima, 1), "veces entre el perfil de menor y mayor riesgo.",
          "Las filas representan combinaciones de Modelo × Edad, y las columnas combinaciones de Color × Carrocería.")
  })
  
  # Descarga de Excel
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("tablas_primas_puras_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(tabla_completa_datos(), tablas_por_servicio_datos())
      exportar_tablas_excel(tablas_por_servicio_datos(), tabla_completa_datos(), file)
    }
  )
  
}

shinyApp(ui = ui, server = server)