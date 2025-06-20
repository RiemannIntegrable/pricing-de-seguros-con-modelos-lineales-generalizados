dividir_polizas_por_anio <- function(df_input) {
  library(dplyr)
  library(lubridate)
  
  if (!all(c("Desde", "Hasta", "SumaDePagos") %in% names(df_input))) {
    stop("El dataframe debe contener las columnas 'Desde', 'Hasta' y 'SumaDePagos'")
  }
  
  df_input$Desde <- as.Date(df_input$Desde)
  df_input$Hasta <- as.Date(df_input$Hasta)
  
  if (any(df_input$Desde > df_input$Hasta)) {
    stop("Existen fechas 'Desde' posteriores a fechas 'Hasta'")
  }
  
  resultado <- data.frame()
  
  for (i in 1:nrow(df_input)) {
    fila <- df_input[i, ]
    fecha_desde <- fila$Desde
    fecha_hasta <- fila$Hasta
    suma_pagos <- fila$SumaDePagos
    
    anio_desde <- year(fecha_desde)
    anio_hasta <- year(fecha_hasta)
    
    if (anio_desde == anio_hasta) {
      resultado <- rbind(resultado, fila)
    } else {
      duracion_total <- as.numeric(fecha_hasta - fecha_desde + 1)
      
      for (anio in anio_desde:anio_hasta) {
        nueva_fila <- fila
        
        if (anio == anio_desde) {
          nueva_desde <- fecha_desde
          nueva_hasta <- as.Date(paste0(anio, "-12-31"))
        } else if (anio == anio_hasta) {
          nueva_desde <- as.Date(paste0(anio, "-01-01"))
          nueva_hasta <- fecha_hasta
        } else {
          nueva_desde <- as.Date(paste0(anio, "-01-01"))
          nueva_hasta <- as.Date(paste0(anio, "-12-31"))
        }
        
        if (nueva_hasta > fecha_hasta) nueva_hasta <- fecha_hasta
        if (nueva_desde < fecha_desde) nueva_desde <- fecha_desde
        
        duracion_anio <- as.numeric(nueva_hasta - nueva_desde + 1)
        proporcion <- duracion_anio / duracion_total
        
        nueva_fila$Desde <- nueva_desde
        nueva_fila$Hasta <- nueva_hasta
        nueva_fila$SumaDePagos <- suma_pagos * proporcion
        
        resultado <- rbind(resultado, nueva_fila)
      }
    }
  }
  
  rownames(resultado) <- NULL
  return(resultado)
}