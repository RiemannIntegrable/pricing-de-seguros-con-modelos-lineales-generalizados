# Toma el input, llama la función de predicción, muestra resultado
output$prediccion_y <- renderText({
  predecir_y(input$valor_x)
})