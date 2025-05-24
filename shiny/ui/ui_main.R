ui <- fluidPage(
  titlePanel("Predictor Simple"),
  numericInput("valor_x", "Ingrese valor de X:", value = 0),
  textOutput("prediccion_y")
)