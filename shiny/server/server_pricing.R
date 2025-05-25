output$prediccion_y <- renderText({
  predecir_y(input$valor_x)
})