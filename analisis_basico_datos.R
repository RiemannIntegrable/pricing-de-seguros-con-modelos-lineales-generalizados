# Análisis básico de estructura de datos para modelo frecuencia-severidad
# Sin dependencias adicionales

# Cargar datos
datos <- read.csv("data/processed/datos_limpios.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

cat("=== ANÁLISIS ACTUARIAL DE DATOS PARA MODELO FRECUENCIA-SEVERIDAD ===\n\n")

# 1. ESTRUCTURA GENERAL
cat("1. ESTRUCTURA GENERAL DEL DATASET\n")
cat("Dimensiones:", dim(datos)[1], "filas x", dim(datos)[2], "columnas\n")
cat("Variables:\n")
print(names(datos))
cat("\n")

# 2. ANÁLISIS DE VARIABLES CLAVE
cat("2. ANÁLISIS DE VARIABLES CLAVE\n\n")

# Variable Accidentado (frecuencia)
cat("a) Variable Accidentado (indicador de siniestro):\n")
freq_accidentado <- table(datos$Accidentado, useNA = "always")
print(freq_accidentado)
tasa_siniestralidad <- sum(datos$Accidentado, na.rm = TRUE) / nrow(datos)
cat("Tasa de siniestralidad:", round(tasa_siniestralidad, 4), "\n\n")

# Variable SumaDePagos (severidad)
cat("b) Variable SumaDePagos (severidad):\n")
cat("Resumen estadístico completo:\n")
print(summary(datos$SumaDePagos))

# Análisis solo para siniestros
siniestros <- datos[datos$Accidentado == 1, ]
cat("\nPara siniestros únicamente (Accidentado = 1):\n")
cat("Número de siniestros:", nrow(siniestros), "\n")
cat("Siniestros con pago > 0:", sum(siniestros$SumaDePagos > 0), "\n")
cat("Siniestros con pago = 0:", sum(siniestros$SumaDePagos == 0), "\n")
print(summary(siniestros$SumaDePagos))

# Variable Pago (indicador)
cat("\nc) Variable Pago (indicador texto):\n")
freq_pago <- table(datos$Pago, useNA = "always")
print(freq_pago)

# Variable exposicion
cat("\nd) Variable exposicion:\n")
print(summary(datos$exposicion))
cat("Exposición en años (días/365.25):\n")
exposicion_años <- datos$exposicion / 365.25
print(summary(exposicion_años))
cat("\n")

# 3. ANÁLISIS DE CONSISTENCIA
cat("3. ANÁLISIS DE CONSISTENCIA DE DATOS\n\n")

# Consistencia Accidentado vs SumaDePagos
cat("a) Consistencia Accidentado vs SumaDePagos:\n")
consistencia1 <- table(
  Accidentado = datos$Accidentado,
  TienePago = ifelse(datos$SumaDePagos > 0, "Con pago", "Sin pago")
)
print(consistencia1)

# Casos problemáticos
problemas1 <- sum(datos$Accidentado == 0 & datos$SumaDePagos > 0)
problemas2 <- sum(datos$Accidentado == 1 & datos$SumaDePagos == 0)
cat("Casos problemáticos:\n")
cat("- Sin accidente pero con pago:", problemas1, "\n")
cat("- Con accidente pero sin pago:", problemas2, "\n\n")

# 4. VARIABLES EXPLICATIVAS
cat("4. ANÁLISIS DE VARIABLES EXPLICATIVAS\n\n")

# Variables categóricas principales
cat("a) TIPO_VEHICULO:\n")
print(table(datos$TIPO_VEHICULO))
cat("\nb) SERVICIO:\n")
print(table(datos$SERVICIO))
cat("\nc) Sexo_Aseg:\n")
print(table(datos$Sexo_Aseg))
cat("\nd) CLASE_FASECOLDA:\n")
print(table(datos$CLASE_FASECOLDA))

# Variables de edad
cat("\ne) Variables de edad (dummies):\n")
suma_edad1 <- sum(datos$Edad_19_41)
suma_edad2 <- sum(datos$Edad_41_63)
suma_edad3 <- sum(datos$Edad_63_85)
cat("Edad_19_41:", suma_edad1, "\n")
cat("Edad_41_63:", suma_edad2, "\n")
cat("Edad_63_85:", suma_edad3, "\n")
total_edad <- suma_edad1 + suma_edad2 + suma_edad3
cat("Total asignaciones edad:", total_edad, "(de", nrow(datos), "registros)\n\n")

# 5. ANÁLISIS TEMPORAL
cat("5. ANÁLISIS TEMPORAL\n\n")
cat("Distribución por mes:\n")
print(table(datos$mes))
cat("\n")

# 6. DIAGNÓSTICO PARA MODELO FRECUENCIA-SEVERIDAD
cat("6. DIAGNÓSTICO PARA MODELO FRECUENCIA-SEVERIDAD\n\n")

# Identificar estructura de datos actual
cat("a) Estructura actual:\n")
cat("- Cada registro representa un período de exposición\n")
cat("- Exposición promedio:", round(mean(datos$exposicion), 2), "días\n")
cat("- Exposición promedio:", round(mean(datos$exposicion)/365.25, 4), "años\n")

# Crear identificador de póliza aproximado
id_poliza <- paste(datos$Modelo, datos$Referencia1, datos$Referencia2, 
                   datos$Sexo_Aseg, round(datos$Vr_Comercial), sep = "_")
polizas_unicas <- length(unique(id_poliza))
cat("- Pólizas únicas aproximadas:", polizas_unicas, "\n")
cat("- Registros por póliza promedio:", round(nrow(datos)/polizas_unicas, 2), "\n\n")

# Análisis de siniestralidad por variables clave
cat("b) Tasa de siniestralidad por variables:\n")

# Por tipo de servicio
for(servicio in unique(datos$SERVICIO)) {
  subset_serv <- datos[datos$SERVICIO == servicio, ]
  tasa <- mean(subset_serv$Accidentado)
  cat("SERVICIO", servicio, ":", round(tasa, 4), "\n")
}
cat("\n")

# Por tipo de vehículo
for(tipo in unique(datos$TIPO_VEHICULO)) {
  subset_tipo <- datos[datos$TIPO_VEHICULO == tipo, ]
  tasa <- mean(subset_tipo$Accidentado)
  cat("TIPO_VEHICULO", tipo, ":", round(tasa, 4), "\n")
}
cat("\n")

# 7. RECOMENDACIONES
cat("7. RECOMENDACIONES ACTUARIALES\n\n")

cat("a) ESTRUCTURA DE DATOS:\n")
cat("   - Los datos están desagregados por períodos de exposición\n")
cat("   - Para modelo frecuencia-severidad necesitamos agregación\n")
cat("   - Recomiendo agregar por póliza anual\n\n")

cat("b) VARIABLE OBJETIVO:\n")
cat("   - FRECUENCIA: usar 'Accidentado' (binaria 0/1)\n")
cat("   - SEVERIDAD: usar 'SumaDePagos' cuando > 0\n")
cat("   - Problemas de encoding en variable 'Pago' (corregir 'Sí' vs 'S�')\n\n")

cat("c) EXPOSICIÓN:\n")
cat("   - Variable 'exposicion' en días es correcta\n")
cat("   - Convertir a años para tasas anuales\n")
cat("   - Considerar exposición como offset en GLM\n\n")

cat("d) VARIABLES EXPLICATIVAS RECOMENDADAS:\n")
cat("   - SERVICIO (Particular/Público) - alta diferenciación\n")
cat("   - TIPO_VEHICULO - factor de riesgo importante\n")
cat("   - Variables de edad (ya como dummies)\n")
cat("   - Vr_Comercial (valor comercial) - factor de severidad\n")
cat("   - Modelo (año del vehículo)\n\n")

cat("e) TRATAMIENTO REQUERIDO:\n")
cat("   1. Corregir encoding de variable 'Pago'\n")
cat("   2. Agregar datos por póliza (suma exposición, cuenta siniestros)\n")
cat("   3. Separar modelo frecuencia (Poisson/NB) y severidad (Gamma)\n")
cat("   4. Validar consistencia Accidentado vs SumaDePagos\n")

cat("\n=== FIN DEL ANÁLISIS ===\n")