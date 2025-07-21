# Estructura recomendada para modelo Frecuencia-Severidad
# Ejemplo de agregación de datos

# Cargar datos originales
datos <- read.csv("data/processed/datos_limpios.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

cat("=== PROPUESTA DE ESTRUCTURA PARA MODELO FRECUENCIA-SEVERIDAD ===\n\n")

# Crear identificador único de póliza
# Combinamos características que identifican únicamente una póliza
datos$id_poliza <- paste(
  datos$Modelo,
  datos$Referencia1, 
  datos$Referencia2,
  datos$Sexo_Aseg,
  round(datos$Vr_Comercial),
  datos$Color,
  datos$Carroceria,
  sep = "_"
)

# OPCIÓN 1: Agregación por póliza (recomendada para GLM clásico)
cat("OPCIÓN 1: AGREGACIÓN POR PÓLIZA\n")
cat("========================================\n")

# Ejemplo de agregación manual para las primeras pólizas
polizas_unicas <- unique(datos$id_poliza)
cat("Total de pólizas únicas identificadas:", length(polizas_unicas), "\n")

# Mostrar ejemplo de agregación para las primeras 5 pólizas
cat("\nEjemplo de agregación para las primeras 5 pólizas:\n")
for(i in 1:min(5, length(polizas_unicas))) {
  poliza <- polizas_unicas[i]
  subset_poliza <- datos[datos$id_poliza == poliza, ]
  
  cat("\nPóliza", i, "- ID:", poliza, "\n")
  cat("  Registros individuales:", nrow(subset_poliza), "\n")
  cat("  Exposición total:", sum(subset_poliza$exposicion), "días (", 
      round(sum(subset_poliza$exposicion)/365.25, 3), "años)\n")
  cat("  Siniestros totales:", sum(subset_poliza$Accidentado), "\n")
  cat("  Suma de pagos:", sum(subset_poliza$SumaDePagos), "\n")
  cat("  Características fijas:\n")
  cat("    - SERVICIO:", subset_poliza$SERVICIO[1], "\n")
  cat("    - TIPO_VEHICULO:", subset_poliza$TIPO_VEHICULO[1], "\n")
  cat("    - Sexo_Aseg:", subset_poliza$Sexo_Aseg[1], "\n")
}

cat("\n\nESTRUCTURA DE DATOS AGREGADA RECOMENDADA:\n")
cat("==========================================\n")
cat("Cada fila = una póliza anual\n")
cat("Variables clave:\n")
cat("  - exposicion_años: suma de exposición en años\n")
cat("  - num_siniestros: total de siniestros (frecuencia)\n")
cat("  - total_pagos: suma de todos los pagos (para severidad promedio)\n")
cat("  - severidad_promedio: total_pagos / num_siniestros (cuando > 0)\n")
cat("  - variables_explicativas: características fijas de la póliza\n\n")

# OPCIÓN 2: Mantener datos desagregados (para modelos más sofisticados)
cat("OPCIÓN 2: DATOS DESAGREGADOS (ACTUAL)\n")
cat("=====================================\n")
cat("Cada fila = período de exposición\n")
cat("Ventajas:\n")
cat("  - Permite modelar variación temporal\n")
cat("  - Aprovecha información de períodos sin siniestro\n")
cat("  - Útil para modelos de machine learning\n")
cat("Desventajas:\n")
cat("  - Más complejo para GLM tradicional\n")
cat("  - Requiere ajustar por correlación intra-póliza\n\n")

# Análisis de correlación temporal dentro de pólizas
cat("ANÁLISIS DE ESTRUCTURA TEMPORAL:\n")
cat("=================================\n")

# Contar pólizas con múltiples períodos
polizas_multiples <- table(table(datos$id_poliza))
cat("Distribución de períodos por póliza:\n")
print(polizas_multiples)

# Ejemplos de pólizas con múltiples siniestros
polizas_con_multiples_siniestros <- aggregate(
  Accidentado ~ id_poliza, 
  data = datos, 
  sum
)
multiples_siniestros <- polizas_con_multiples_siniestros[
  polizas_con_multiples_siniestros$Accidentado > 1, 
]

cat("\nPólizas con múltiples siniestros:", nrow(multiples_siniestros), "\n")
if(nrow(multiples_siniestros) > 0) {
  cat("Distribución de número de siniestros:\n")
  print(table(multiples_siniestros$Accidentado))
}

cat("\n\nRECOMENDACIÓN FINAL PARA PRICING:\n")
cat("==================================\n")
cat("1. MODELO DE FRECUENCIA:\n")
cat("   - Variable dependiente: número de siniestros por póliza\n")
cat("   - Distribución: Poisson o Binomial Negativa\n")
cat("   - Offset: log(exposicion_años)\n")
cat("   - Predictores: SERVICIO + TIPO_VEHICULO + edad + valor_comercial\n\n")

cat("2. MODELO DE SEVERIDAD:\n")
cat("   - Variable dependiente: monto promedio por siniestro\n")
cat("   - Distribución: Gamma o Log-Normal\n")
cat("   - Solo observaciones con siniestros\n")
cat("   - Predictores: valor_comercial + tipo_vehiculo + características\n\n")

cat("3. PRIMA PURA = Frecuencia × Severidad\n")
cat("   Prima_anual = E[N] × E[X]\n")
cat("   donde N = número de siniestros, X = severidad promedio\n\n")

cat("4. ESTRUCTURA DE DATOS FINAL RECOMENDADA:\n")
cat("   - Agregar por póliza anual\n")
cat("   - Una fila por póliza-año\n")
cat("   - Variables: num_siniestros, exposicion_años, severidad_promedio\n")
cat("   - Variables explicativas: características de la póliza\n")

cat("\n=== FIN DE RECOMENDACIONES ===\n")