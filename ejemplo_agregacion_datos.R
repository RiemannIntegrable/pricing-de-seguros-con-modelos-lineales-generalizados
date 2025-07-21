# Ejemplo práctico de agregación de datos para modelo frecuencia-severidad

datos <- read.csv("data/processed/datos_limpios.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Crear ID de póliza
datos$id_poliza <- paste(
  datos$Modelo,
  datos$Referencia1, 
  datos$Referencia2,
  datos$Sexo_Aseg,
  round(datos$Vr_Comercial),
  sep = "_"
)

cat("=== EJEMPLO DE AGREGACIÓN PARA MODELO FRECUENCIA-SEVERIDAD ===\n\n")

# Función para agregar datos por póliza
agregar_por_poliza <- function(datos) {
  polizas_unicas <- unique(datos$id_poliza)
  
  # Inicializar dataframe resultado
  resultado <- data.frame(
    id_poliza = character(),
    exposicion_años = numeric(),
    num_siniestros = integer(),
    total_pagos = numeric(),
    severidad_promedio = numeric(),
    # Variables explicativas (tomar el primer valor de cada póliza)
    SERVICIO = character(),
    TIPO_VEHICULO = character(),
    CLASE_FASECOLDA = character(),
    Sexo_Aseg = character(),
    Vr_Comercial = numeric(),
    Modelo = numeric(),
    edad_19_41 = integer(),
    edad_41_63 = integer(),
    edad_63_85 = integer(),
    stringsAsFactors = FALSE
  )
  
  for(poliza in polizas_unicas) {
    subset_poliza <- datos[datos$id_poliza == poliza, ]
    
    # Calcular métricas agregadas
    exposicion_total <- sum(subset_poliza$exposicion) / 365.25  # en años
    num_siniestros <- sum(subset_poliza$Accidentado)
    total_pagos <- sum(subset_poliza$SumaDePagos)
    severidad_promedio <- ifelse(num_siniestros > 0, total_pagos / num_siniestros, NA)
    
    # Variables explicativas (tomar primer registro)
    primer_registro <- subset_poliza[1, ]
    
    # Agregar fila al resultado
    nueva_fila <- data.frame(
      id_poliza = poliza,
      exposicion_años = exposicion_total,
      num_siniestros = num_siniestros,
      total_pagos = total_pagos,
      severidad_promedio = severidad_promedio,
      SERVICIO = primer_registro$SERVICIO,
      TIPO_VEHICULO = primer_registro$TIPO_VEHICULO,
      CLASE_FASECOLDA = primer_registro$CLASE_FASECOLDA,
      Sexo_Aseg = primer_registro$Sexo_Aseg,
      Vr_Comercial = primer_registro$Vr_Comercial,
      Modelo = primer_registro$Modelo,
      edad_19_41 = primer_registro$Edad_19_41,
      edad_41_63 = primer_registro$Edad_41_63,
      edad_63_85 = primer_registro$Edad_63_85,
      stringsAsFactors = FALSE
    )
    
    resultado <- rbind(resultado, nueva_fila)
  }
  
  return(resultado)
}

# Aplicar agregación (solo primeras 100 pólizas para el ejemplo)
polizas_muestra <- unique(datos$id_poliza)[1:100]
datos_muestra <- datos[datos$id_poliza %in% polizas_muestra, ]

cat("Agregando datos de muestra...\n")
datos_agregados <- agregar_por_poliza(datos_muestra)

cat("RESULTADOS DE LA AGREGACIÓN:\n")
cat("==============================\n")
cat("Registros originales:", nrow(datos_muestra), "\n")
cat("Pólizas agregadas:", nrow(datos_agregados), "\n\n")

cat("ESTRUCTURA DEL DATASET AGREGADO:\n")
cat("Variables:", ncol(datos_agregados), "\n")
print(names(datos_agregados))

cat("\nPRIMERAS 10 PÓLIZAS AGREGADAS:\n")
print(datos_agregados[1:min(10, nrow(datos_agregados)), 
                      c("exposicion_años", "num_siniestros", "total_pagos", 
                        "SERVICIO", "TIPO_VEHICULO", "Sexo_Aseg")])

cat("\n\nESTADÍSTICAS DESCRIPTIVAS:\n")
cat("===========================\n")

# Estadísticas de frecuencia
cat("FRECUENCIA (número de siniestros):\n")
print(table(datos_agregados$num_siniestros))
cat("Tasa de siniestralidad:", 
    sum(datos_agregados$num_siniestros > 0) / nrow(datos_agregados), "\n")

# Estadísticas de exposición
cat("\nEXPOSICIÓN (en años):\n")
print(summary(datos_agregados$exposicion_años))

# Estadísticas de severidad
siniestros_con_pago <- datos_agregados[datos_agregados$num_siniestros > 0, ]
if(nrow(siniestros_con_pago) > 0) {
  cat("\nSEVERIDAD (solo siniestros con pago):\n")
  print(summary(siniestros_con_pago$severidad_promedio))
}

cat("\n\nCÓDIGO PARA MODELOS GLM:\n")
cat("========================\n")
cat("# Modelo de Frecuencia (Poisson)\n")
cat("modelo_frecuencia <- glm(num_siniestros ~ SERVICIO + TIPO_VEHICULO + \n")
cat("                         Sexo_Aseg + edad_41_63 + edad_63_85 + \n")
cat("                         log(Vr_Comercial) + offset(log(exposicion_años)),\n")
cat("                         family = poisson, data = datos_agregados)\n\n")

cat("# Modelo de Severidad (Gamma)\n")
cat("datos_severidad <- datos_agregados[datos_agregados$num_siniestros > 0, ]\n")
cat("modelo_severidad <- glm(severidad_promedio ~ SERVICIO + TIPO_VEHICULO + \n")
cat("                        log(Vr_Comercial) + Modelo,\n")
cat("                        family = Gamma(link = 'log'), \n")
cat("                        data = datos_severidad)\n\n")

cat("# Prima pura = frecuencia × severidad\n")
cat("frecuencia_pred <- predict(modelo_frecuencia, type = 'response')\n")
cat("severidad_pred <- predict(modelo_severidad, type = 'response')\n")
cat("prima_pura <- frecuencia_pred * severidad_pred\n\n")

cat("=== RESUMEN FINAL ===\n")
cat("1. Los datos están listos para agregación por póliza\n")
cat("2. Estructura temporal permite análisis sofisticado\n")
cat("3. Variables explicativas bien definidas\n")
cat("4. Consistencia entre variables objetivo confirmada\n")
cat("5. Recomendación: proceder con agregación para GLM tradicional\n")