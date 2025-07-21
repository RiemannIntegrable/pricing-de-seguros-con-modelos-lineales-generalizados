library(dplyr)
library(readr)
library(ggplot2)
library(summarytools)

# Cargar datos
datos <- read_csv("data/processed/datos_limpios.csv", 
                  locale = locale(encoding = "UTF-8"),
                  col_types = cols(
                    Amparo = col_character(),
                    SumaDePagos = col_double(),
                    Modelo = col_double(),
                    Color = col_character(),
                    Carroceria = col_character(),
                    Referencia1 = col_character(),
                    Referencia2 = col_character(),
                    CLASE_FASECOLDA = col_character(),
                    TIPO_VEHICULO = col_character(),
                    SERVICIO = col_character(),
                    Sexo_Aseg = col_character(),
                    Vr_Comercial = col_double(),
                    Pago = col_character(),
                    Accidentado = col_integer(),
                    exposicion = col_double(),
                    mes = col_integer(),
                    Edad_19_41 = col_integer(),
                    Edad_41_63 = col_integer(),
                    Edad_63_85 = col_integer()
                  ))

# 1. ESTRUCTURA GENERAL DEL DATASET
cat("=== ESTRUCTURA GENERAL DEL DATASET ===\n")
cat("Dimensiones:", dim(datos), "\n")
cat("Total de registros:", nrow(datos), "\n")
cat("Total de variables:", ncol(datos), "\n\n")

# 2. ANÁLISIS DE VARIABLES CLAVE PARA MODELO FRECUENCIA-SEVERIDAD
cat("=== ANÁLISIS DE VARIABLES CLAVE ===\n")

# Variable Accidentado (frecuencia)
cat("Variable Accidentado (indicador de siniestro):\n")
table(datos$Accidentado, useNA = "always")
cat("Tasa de siniestralidad:", mean(datos$Accidentado, na.rm = TRUE), "\n\n")

# Variable Pago (indicador de pago)
cat("Variable Pago (indicador de pago):\n")
table(datos$Pago, useNA = "always")

# Variable SumaDePagos (severidad)
cat("\nVariable SumaDePagos (monto del siniestro):\n")
cat("Resumen estadístico:\n")
print(summary(datos$SumaDePagos))

# Análisis de severidad solo para siniestros con pago
siniestros_con_pago <- datos[datos$Accidentado == 1 & datos$SumaDePagos > 0, ]
cat("\nSeveridad para siniestros con pago > 0:\n")
cat("Número de siniestros con pago:", nrow(siniestros_con_pago), "\n")
print(summary(siniestros_con_pago$SumaDePagos))

# Variable exposicion
cat("\nVariable exposicion:\n")
print(summary(datos$exposicion))
cat("Distribución de exposición:\n")
print(table(cut(datos$exposicion, breaks = c(0, 30, 90, 180, 365))))

# 3. ANÁLISIS DE CONSISTENCIA DE DATOS
cat("\n=== ANÁLISIS DE CONSISTENCIA ===\n")

# Verificar consistencia entre Accidentado y SumaDePagos
cat("Consistencia Accidentado vs SumaDePagos:\n")
tabla_consistencia <- table(
  Accidentado = datos$Accidentado,
  TienePago = ifelse(datos$SumaDePagos > 0, "Sí", "No")
)
print(tabla_consistencia)

# Verificar consistencia entre Pago y SumaDePagos
cat("\nConsistencia Pago vs SumaDePagos:\n")
tabla_pago <- table(
  Pago = datos$Pago,
  TienePago = ifelse(datos$SumaDePagos > 0, "Sí", "No")
)
print(tabla_pago)

# 4. ANÁLISIS DE ESTRUCTURA TEMPORAL
cat("\n=== ANÁLISIS TEMPORAL ===\n")
cat("Distribución por mes:\n")
print(table(datos$mes))

# 5. ANÁLISIS DE VARIABLES EXPLICATIVAS
cat("\n=== VARIABLES EXPLICATIVAS PRINCIPALES ===\n")

# Variables categóricas clave
cat("TIPO_VEHICULO:\n")
print(table(datos$TIPO_VEHICULO))

cat("\nSERVICIO:\n")
print(table(datos$SERVICIO))

cat("\nSexo_Aseg:\n")
print(table(datos$Sexo_Aseg))

cat("\nCLASE_FASECOLDA:\n")
print(table(datos$CLASE_FASECOLDA))

# Variables de edad
cat("\nDistribución de edad (variables dummy):\n")
cat("Edad_19_41:", sum(datos$Edad_19_41), "\n")
cat("Edad_41_63:", sum(datos$Edad_41_63), "\n")
cat("Edad_63_85:", sum(datos$Edad_63_85), "\n")

# 6. ANÁLISIS PARA MODELO FRECUENCIA-SEVERIDAD
cat("\n=== DIAGNÓSTICO PARA MODELO FRECUENCIA-SEVERIDAD ===\n")

# Análisis por póliza
cat("Estructura actual de los datos:\n")
cat("- Cada fila representa: un período de exposición de una póliza\n")
cat("- Exposición promedio:", mean(datos$exposicion), "días\n")
cat("- Exposición en años promedio:", mean(datos$exposicion)/365.25, "\n")

# Identificar si hay múltiples registros por póliza
# Crear un ID único por las características del vehículo y asegurado
datos$id_poliza <- paste(datos$Modelo, datos$Color, datos$Carroceria, 
                        datos$Referencia1, datos$Referencia2, datos$Sexo_Aseg, 
                        datos$Vr_Comercial, sep = "_")

registros_por_poliza <- datos %>%
  group_by(id_poliza) %>%
  summarise(
    n_registros = n(),
    exposicion_total = sum(exposicion),
    siniestros_total = sum(Accidentado),
    pago_total = sum(SumaDePagos),
    .groups = "drop"
  )

cat("\nAnálisis por póliza identificada:\n")
cat("Pólizas únicas:", nrow(registros_por_poliza), "\n")
cat("Registros por póliza:\n")
print(table(registros_por_poliza$n_registros))

# 7. RECOMENDACIONES
cat("\n=== RECOMENDACIONES ===\n")
cat("1. Los datos están en formato desagregado (por período)\n")
cat("2. Para modelo frecuencia-severidad necesitamos agregar por póliza\n")
cat("3. Variables clave identificadas correctamente\n")
cat("4. Problema de encoding en variable 'Pago' que debe corregirse\n")