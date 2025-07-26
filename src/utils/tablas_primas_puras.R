# Funciones para generar tablas completas de primas puras
# José Miguel Acuña Hernández - Maestría en Actuaría y Finanzas

generar_tabla_completa_primas <- function(modelo_completo, exposicion = 1) {
  
  niveles <- modelo_completo$niveles_factores
  
  # Crear todas las combinaciones posibles
  combinaciones <- expand.grid(
    Modelo = niveles$Modelo,
    Edad = niveles$Edad,
    Color = niveles$Color,
    Carroceria = niveles$Carroceria,
    SERVICIO = niveles$SERVICIO,
    stringsAsFactors = FALSE
  )
  
  # Vectores para almacenar resultados
  n_combinaciones <- nrow(combinaciones)
  frecuencias <- numeric(n_combinaciones)
  severidades <- numeric(n_combinaciones)
  primas_puras <- numeric(n_combinaciones)
  
  # Calcular cada prima pura
  for (i in 1:n_combinaciones) {
    
    # Perfil para frecuencia
    perfil_freq <- data.frame(
      Modelo = factor(combinaciones$Modelo[i], levels = niveles$Modelo),
      Edad = factor(combinaciones$Edad[i], levels = niveles$Edad),
      exposicion_total = exposicion
    )
    
    # Perfil para severidad
    perfil_sev <- data.frame(
      Modelo = factor(combinaciones$Modelo[i], levels = niveles$Modelo),
      Color = factor(combinaciones$Color[i], levels = niveles$Color),
      Carroceria = factor(combinaciones$Carroceria[i], levels = niveles$Carroceria),
      SERVICIO = factor(combinaciones$SERVICIO[i], levels = niveles$SERVICIO),
      Edad = factor(combinaciones$Edad[i], levels = niveles$Edad),
      n_siniestros = 1
    )
    
    # Predicciones
    freq_pred <- predict(modelo_completo$modelo_frecuencia, newdata = perfil_freq, type = "response")
    tasa_frecuencia <- freq_pred / exposicion
    
    sev_pred_log <- predict(modelo_completo$modelo_severidad, newdata = perfil_sev, type = "response")
    severidad_estimada <- exp(sev_pred_log)
    
    prima_pura <- tasa_frecuencia * severidad_estimada * exposicion
    
    # Almacenar resultados
    frecuencias[i] <- as.numeric(tasa_frecuencia)
    severidades[i] <- as.numeric(severidad_estimada)
    primas_puras[i] <- as.numeric(prima_pura)
  }
  
  # Agregar resultados al dataframe
  combinaciones$frecuencia <- frecuencias
  combinaciones$severidad <- severidades
  combinaciones$prima_pura <- primas_puras
  
  return(combinaciones)
}

crear_tablas_por_servicio <- function(tabla_completa) {
  
  # Dividir por tipo de servicio
  servicios <- unique(tabla_completa$SERVICIO)
  tablas_por_servicio <- list()
  
  for (servicio in servicios) {
    
    datos_servicio <- tabla_completa[tabla_completa$SERVICIO == servicio, ]
    
    # Crear tabla pivoteada: Modelo×Edad en filas, Color×Carrocería en columnas
    tabla_pivot <- reshape2::dcast(
      datos_servicio,
      Modelo + Edad ~ Color + Carroceria,
      value.var = "prima_pura",
      fun.aggregate = mean
    )
    
    # Crear nombres más legibles para filas
    tabla_pivot$Perfil <- paste(tabla_pivot$Modelo, tabla_pivot$Edad, sep = " | ")
    tabla_pivot <- tabla_pivot[, c("Perfil", setdiff(names(tabla_pivot), c("Modelo", "Edad", "Perfil")))]
    
    tablas_por_servicio[[servicio]] <- tabla_pivot
  }
  
  return(tablas_por_servicio)
}

crear_tabla_resumen_estadistico <- function(tabla_completa) {
  
  resumen <- data.frame(
    Variable = c("SERVICIO", "Modelo", "Edad", "Color", "Carroceria"),
    Niveles = c(
      length(unique(tabla_completa$SERVICIO)),
      length(unique(tabla_completa$Modelo)),
      length(unique(tabla_completa$Edad)),
      length(unique(tabla_completa$Color)),
      length(unique(tabla_completa$Carroceria))
    ),
    Detalle = c(
      paste(unique(tabla_completa$SERVICIO), collapse = ", "),
      paste(unique(tabla_completa$Modelo), collapse = ", "),
      paste(unique(tabla_completa$Edad), collapse = ", "),
      paste(unique(tabla_completa$Color), collapse = ", "),
      paste(unique(tabla_completa$Carroceria), collapse = ", ")
    )
  )
  
  estadisticas <- data.frame(
    Estadística = c("Total Combinaciones", "Prima Mínima", "Prima Máxima", "Prima Promedio", "Desviación Estándar"),
    Valor = c(
      nrow(tabla_completa),
      paste("$", format(round(min(tabla_completa$prima_pura)), big.mark = ","), "COP"),
      paste("$", format(round(max(tabla_completa$prima_pura)), big.mark = ","), "COP"),
      paste("$", format(round(mean(tabla_completa$prima_pura)), big.mark = ","), "COP"),
      paste("$", format(round(sd(tabla_completa$prima_pura)), big.mark = ","), "COP")
    )
  )
  
  return(list(variables = resumen, estadisticas = estadisticas))
}

exportar_tablas_excel <- function(tablas_por_servicio, tabla_completa, archivo = "tablas_primas_puras.xlsx") {
  
  require(openxlsx)
  
  wb <- createWorkbook()
  
  # Hoja con tabla completa
  addWorksheet(wb, "Tabla_Completa")
  writeData(wb, "Tabla_Completa", tabla_completa)
  
  # Hojas por servicio
  for (servicio in names(tablas_por_servicio)) {
    nombre_hoja <- paste0("Servicio_", gsub("[^A-Za-z0-9]", "_", servicio))
    addWorksheet(wb, nombre_hoja)
    writeData(wb, nombre_hoja, tablas_por_servicio[[servicio]])
  }
  
  # Hoja de resumen
  resumen <- crear_tabla_resumen_estadistico(tabla_completa)
  addWorksheet(wb, "Resumen")
  writeData(wb, "Resumen", resumen$variables, startRow = 1)
  writeData(wb, "Resumen", resumen$estadisticas, startRow = nrow(resumen$variables) + 3)
  
  saveWorkbook(wb, archivo, overwrite = TRUE)
  return(paste("Archivo guardado:", archivo))
}

formatear_prima_tabla <- function(valor) {
  paste("$", format(round(valor), big.mark = ","))
}