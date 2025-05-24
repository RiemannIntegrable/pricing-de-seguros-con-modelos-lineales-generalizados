# Carga librerías
library(shiny)

# Verificar y cargar el modelo
if (file.exists("../models/modelo_simple.rds")) {
  source("../src/shiny_functions.R")
} else {
  stop("No se encontró el archivo del modelo en models/modelo_simple.rds")
}

# Definir UI directamente
ui <- fluidPage(
  titlePanel("Predictor Simple"),
  numericInput("valor_x", "Ingrese valor de X:", value = 0),
  textOutput("prediccion_y")
)

# Definir server directamente
server <- function(input, output, session) {
  output$prediccion_y <- renderText({
    predecir_y(input$valor_x)
  })
}

# Ejecuta la app
shinyApp(ui = ui, server = server)