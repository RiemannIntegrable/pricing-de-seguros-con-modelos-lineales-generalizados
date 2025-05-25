library(shiny)
source("shiny_functions.R")

ui <- fluidPage(
  titlePanel("Predictor Simple"),
  numericInput("valor_x", "Ingrese valor de X:", value = 0),
  textOutput("prediccion_y")
)

server <- function(input, output, session) {
  output$prediccion_y <- renderText({
    predecir_y(input$valor_x)
  })
}

shinyApp(ui = ui, server = server)