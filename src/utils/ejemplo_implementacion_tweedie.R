# =============================================================================
# EJEMPLO DE IMPLEMENTACIÓN COMPLETA - MODELO TWEEDIE
# =============================================================================
# Script de ejemplo que demuestra el uso completo de todas las funciones
# desarrolladas para modelado con distribución Tweedie aplicado a datos
# de seguros de automóviles.
# 
# Este script integra:
# - Carga y preparación de datos
# - Selección automática de variables
# - Ajuste del modelo Tweedie con optimización de parámetros
# - Análisis de coeficientes y estadísticas
# - Gráficos de diagnóstico completos
# - Suite completa de pruebas de bondad de ajuste
# 
# Autor: Guillermo Murillo
# Fecha: 2025
# =============================================================================

# =============================================================================
# CONFIGURACIÓN INICIAL
# =============================================================================

# Limpiar entorno
rm(list = ls())

# Configurar opciones de R
options(scipen = 999, digits = 4)
set.seed(12345)  # Para reproducibilidad

# Verificar e instalar paquetes necesarios
paquetes_necesarios <- c("dplyr", "ggplot2", "tweedie", "statmod", 
                        "gridExtra", "stringr", "moments")

for (paq in paquetes_necesarios) {
  if (!require(paq, character.only = TRUE)) {
    install.packages(paq)
    library(paq, character.only = TRUE)
  }
}

# Cargar funciones desarrolladas
source("src/utils/seleccion_variables_tweedie.R")
source("src/utils/ajuste_modelo_tweedie.R") 
source("src/utils/graficos_diagnosticos_tweedie.R")
source("src/utils/pruebas_ajuste_tweedie.R")

cat("=== ANÁLISIS COMPLETO CON MODELO TWEEDIE ===\n")
cat("Script de ejemplo para datos de seguros de automóviles\n\n")

# =============================================================================
# CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

cat("1. CARGA Y PREPARACIÓN DE DATOS\n")
cat("================================\n")

# Cargar datos limpios
datos <- read.csv("data/processed/datos_limpios.csv", stringsAsFactors = FALSE)

cat("Dimensiones originales:", nrow(datos), "filas,", ncol(datos), "columnas\n")

# Preparar datos para análisis
datos_preparados <- datos %>%
  # Convertir variables categóricas a factor
  mutate(
    Modelo = factor(Modelo),
    Color = factor(Color),
    Carroceria = factor(Carroceria),
    CLASE_FASECOLDA = factor(CLASE_FASECOLDA),
    TIPO_VEHICULO = factor(TIPO_VEHICULO),
    SERVICIO = factor(SERVICIO),
    Sexo_Aseg = factor(Sexo_Aseg),
    Accidentado = factor(Accidentado)
  ) %>%
  # Crear variable respuesta para modelo Tweedie (monto de siniestros)
  mutate(
    # Para Tweedie, necesitamos valores > 0, usar monto + pequeña constante
    SumaDePagos_tweedie = ifelse(SumaDePagos > 0, SumaDePagos, 0.01),
    # Variable de exposición (logaritmo para offset)
    log_exposicion = log(pmax(exposicion, 1))  # Evitar log(0)
  ) %>%
  # Filtrar datos válidos
  filter(
    !is.na(SumaDePagos_tweedie),
    !is.na(Edad),
    !is.na(Vr_Comercial),
    !is.na(exposicion),
    exposicion > 0,
    Edad > 0,
    Vr_Comercial > 0
  )

cat("Datos después de limpieza:", nrow(datos_preparados), "observaciones\n")

# Variables disponibles para el modelo
variables_respuesta <- "SumaDePagos_tweedie"
variables_predictoras <- c("Modelo", "Color", "Carroceria", "CLASE_FASECOLDA", 
                          "TIPO_VEHICULO", "SERVICIO", "Sexo_Aseg", "Edad", 
                          "Vr_Comercial")

# Resumen descriptivo
cat("\nRESUMEN DE VARIABLE RESPUESTA:\n")
cat("Media:", round(mean(datos_preparados$SumaDePagos_tweedie), 2), "\n")
cat("Mediana:", round(median(datos_preparados$SumaDePagos_tweedie), 2), "\n")
cat("Desviación estándar:", round(sd(datos_preparados$SumaDePagos_tweedie), 2), "\n")
cat("Mínimo:", round(min(datos_preparados$SumaDePagos_tweedie), 2), "\n")
cat("Máximo:", round(max(datos_preparados$SumaDePagos_tweedie), 2), "\n")
cat("% de ceros originales:", round(mean(datos$SumaDePagos == 0) * 100, 2), "%\n")

# =============================================================================
# SELECCIÓN AUTOMÁTICA DE VARIABLES
# =============================================================================

cat("\n\n2. SELECCIÓN AUTOMÁTICA DE VARIABLES\n")
cat("====================================\n")

# Realizar selección de variables usando AIC
resultado_seleccion <- seleccion_variables_aic(
  data = datos_preparados,
  response_var = variables_respuesta,
  predictor_vars = variables_predictoras,
  method = "both",  # Selección bidireccional
  p_tweedie = 1.6,  # Valor inicial para parámetro de potencia
  max_vars = 8,     # Máximo 8 variables en el modelo final
  verbose = TRUE
)

# Mostrar variables seleccionadas
cat("\nVARIABLES SELECCIONADAS:\n")
for (i in seq_along(resultado_seleccion$variables_seleccionadas)) {
  cat(i, ".", resultado_seleccion$variables_seleccionadas[i], "\n")
}

# Mostrar criterios de selección
cat("\nHISTORIAL DE SELECCIÓN:\n")
print(resultado_seleccion$criterios_seleccion)

# =============================================================================
# AJUSTE DEL MODELO TWEEDIE CON OPTIMIZACIÓN
# =============================================================================

cat("\n\n3. AJUSTE DEL MODELO TWEEDIE\n")
cat("============================\n")

# Crear fórmula con variables seleccionadas y offset de exposición
if (length(resultado_seleccion$variables_seleccionadas) > 0) {
  formula_modelo <- as.formula(paste(
    variables_respuesta, "~", 
    paste(resultado_seleccion$variables_seleccionadas, collapse = " + "),
    "+ offset(log_exposicion)"
  ))
} else {
  # Si no se seleccionaron variables, usar solo intercepto
  formula_modelo <- as.formula(paste(variables_respuesta, "~ 1 + offset(log_exposicion)"))
}

cat("Fórmula del modelo:\n")
print(formula_modelo)

# Ajustar modelo con optimización del parámetro p
resultado_ajuste <- ajuste_modelo_tweedie(
  data = datos_preparados,
  formula = formula_modelo,
  p_tweedie = 1.6,        # Valor inicial
  optimizar_p = TRUE,     # Optimizar parámetro p automáticamente
  rango_p = c(1.1, 1.9),  # Rango de búsqueda para p
  metodo_dispersion = "pearson",
  bootstrap_ic = TRUE,    # Calcular intervalos de confianza bootstrap
  n_bootstrap = 500,      # Número de replicaciones bootstrap
  verbose = TRUE
)

# Extraer modelo final
modelo_final <- resultado_ajuste$modelo

# =============================================================================
# ANÁLISIS DE COEFICIENTES Y ESTADÍSTICAS
# =============================================================================

cat("\n\n4. ANÁLISIS DETALLADO DE COEFICIENTES\n")
cat("=====================================\n")

# Mostrar coeficientes con intervalos de confianza
cat("COEFICIENTES DEL MODELO (Escala logarítmica):\n")
tabla_coeficientes <- data.frame(
  Variable = names(resultado_ajuste$coeficientes$coeficientes),
  Estimacion = resultado_ajuste$coeficientes$coeficientes,
  Error_Std = resultado_ajuste$coeficientes$errores_estandar,
  IC_Inferior = resultado_ajuste$coeficientes$intervalos_confianza[, 1],
  IC_Superior = resultado_ajuste$coeficientes$intervalos_confianza[, 2],
  P_Valor = resultado_ajuste$coeficientes$p_valores
)

print(round(tabla_coeficientes, 4))

# Coeficientes exponenciados (interpretación multiplicativa)
cat("\nCOEFICIENTES EXPONENCIADOS (Factores multiplicativos):\n")
coeficientes_exp <- exp(resultado_ajuste$coeficientes$coeficientes)
ic_exp <- exp(resultado_ajuste$coeficientes$intervalos_confianza)

tabla_exp <- data.frame(
  Variable = names(coeficientes_exp),
  Factor_Mult = coeficientes_exp,
  IC_Inf_Exp = ic_exp[, 1],
  IC_Sup_Exp = ic_exp[, 2]
)

print(round(tabla_exp, 4))

# Estadísticas del modelo
cat("\nESTADÍSTICAS DEL MODELO:\n")
cat("AIC:", round(resultado_ajuste$estadisticas$aic, 2), "\n")
cat("BIC:", round(resultado_ajuste$estadisticas$bic, 2), "\n")
cat("Log-verosimilitud:", round(resultado_ajuste$estadisticas$loglik, 2), "\n")
cat("Pseudo R²:", round(resultado_ajuste$estadisticas$pseudo_r2, 4), "\n")
cat("Parámetro p optimizado:", round(resultado_ajuste$parametros$p_tweedie, 4), "\n")
cat("Parámetro de dispersión:", round(resultado_ajuste$parametros$dispersion, 4), "\n")

# =============================================================================
# GRÁFICOS DE DIAGNÓSTICO
# =============================================================================

cat("\n\n5. GRÁFICOS DE DIAGNÓSTICO\n")
cat("==========================\n")

# Panel completo de diagnósticos
panel_diagnosticos <- panel_diagnosticos_tweedie(
  modelo = modelo_final,
  incluir_envelope = TRUE,
  n_sim_envelope = 100,  # Número de simulaciones para envelope
  guardar_archivo = "resultados/diagnosticos_modelo_tweedie.png",
  ancho_panel = 16,
  alto_panel = 12
)

# Gráfico de envelope detallado
envelope_detallado <- grafico_envelope_tweedie(
  modelo = modelo_final,
  tipo_residuo = "deviance",
  n_simulaciones = 200,
  nivel_confianza = 0.95,
  mostrar_outliers = TRUE,
  guardar_archivo = "resultados/envelope_tweedie_detallado.png",
  titulo = "Envelope Plot - Modelo Tweedie Final"
)

cat("Outliers detectados en envelope:", length(envelope_detallado$outliers_detectados), "\n")

# Gráfico de dispersión vs media
grafico_dispersion <- grafico_dispersion_media_tweedie(
  modelo = modelo_final,
  n_bins = 15,
  titulo = "Verificación Relación Varianza-Media Tweedie"
)

# Perfil de verosimilitud para p
grafico_perfil <- grafico_perfil_verosimilitud_p(
  data = datos_preparados,
  formula = formula_modelo,
  rango_p = c(1.1, 1.9),
  n_puntos = 30,
  p_actual = resultado_ajuste$parametros$p_tweedie
)

# Guardar gráficos adicionales
ggsave("resultados/dispersion_media_tweedie.png", grafico_dispersion, 
       width = 10, height = 8, dpi = 300)
ggsave("resultados/perfil_verosimilitud_p.png", grafico_perfil, 
       width = 10, height = 8, dpi = 300)

# =============================================================================
# SUITE COMPLETA DE PRUEBAS DE BONDAD DE AJUSTE
# =============================================================================

cat("\n\n6. PRUEBAS DE BONDAD DE AJUSTE\n")
cat("==============================\n")

# Ejecutar suite completa de pruebas
resultados_pruebas <- suite_pruebas_ajuste_tweedie(
  modelo = modelo_final,
  datos_originales = datos_preparados,
  alpha = 0.05,
  incluir_bootstrap = TRUE,
  n_bootstrap = 500,
  incluir_simulacion = TRUE,
  n_simulaciones = 300,
  verbose = TRUE
)

# =============================================================================
# VALIDACIÓN CRUZADA DEL MODELO
# =============================================================================

cat("\n\n7. VALIDACIÓN CRUZADA\n")
cat("=====================\n")

# Validación cruzada de la selección de variables
if (length(variables_predictoras) > 3) {  # Solo si hay suficientes variables
  validacion_cv <- validacion_cruzada_seleccion(
    data = datos_preparados,
    response_var = variables_respuesta,
    predictor_vars = variables_predictoras,
    k_folds = 5,
    p_tweedie = resultado_ajuste$parametros$p_tweedie,
    method = "both",
    seed = 12345
  )
} else {
  cat("Número insuficiente de variables para validación cruzada completa.\n")
}

# =============================================================================
# PREDICCIONES Y ANÁLISIS DE CASOS
# =============================================================================

cat("\n\n8. ANÁLISIS DE PREDICCIONES\n")
cat("===========================\n")

# Generar predicciones
predicciones <- predict(modelo_final, type = "response")
datos_preparados$predicciones <- predicciones

# Estadísticas de predicción
mae <- mean(abs(datos_preparados$SumaDePagos_tweedie - predicciones))
rmse <- sqrt(mean((datos_preparados$SumaDePagos_tweedie - predicciones)^2))
mape <- mean(abs((datos_preparados$SumaDePagos_tweedie - predicciones) / 
                datos_preparados$SumaDePagos_tweedie)) * 100

cat("MÉTRICAS DE PREDICCIÓN:\n")
cat("MAE (Error Absoluto Medio):", round(mae, 4), "\n")
cat("RMSE (Raíz del Error Cuadrático Medio):", round(rmse, 4), "\n")
cat("MAPE (Error Porcentual Absoluto Medio):", round(mape, 2), "%\n")

# Análisis de residuos por percentiles
residuos <- datos_preparados$SumaDePagos_tweedie - predicciones
percentiles_residuos <- quantile(residuos, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

cat("\nPERCENTILES DE RESIDUOS:\n")
print(round(percentiles_residuos, 4))

# Identificar casos extremos
casos_extremos <- which(abs(residuos) > quantile(abs(residuos), 0.95))

cat("\nCASOS CON RESIDUOS EXTREMOS (top 5%):", length(casos_extremos), "observaciones\n")

if (length(casos_extremos) > 0 && length(casos_extremos) <= 10) {
  cat("Índices de casos extremos:", paste(casos_extremos, collapse = ", "), "\n")
}

# =============================================================================
# RESUMEN EJECUTIVO Y CONCLUSIONES
# =============================================================================

cat("\n\n9. RESUMEN EJECUTIVO\n")
cat("====================\n")

cat("MODELO FINAL:\n")
cat("- Variables incluidas:", length(resultado_seleccion$variables_seleccionadas), "\n")
cat("- Parámetro p optimizado:", round(resultado_ajuste$parametros$p_tweedie, 4), "\n")
cat("- AIC:", round(resultado_ajuste$estadisticas$aic, 2), "\n")
cat("- Pseudo R²:", round(resultado_ajuste$estadisticas$pseudo_r2, 4), "\n")
cat("- Clasificación del ajuste:", resultados_pruebas$resumen_general$clasificacion_ajuste, "\n")

cat("\nCONCLUSIONES:\n")
for (i in seq_along(resultados_pruebas$resumen_general$recomendaciones)) {
  cat("  ", i, ". ", resultados_pruebas$resumen_general$recomendaciones[i], "\n")
}

cat("\nARCHIVOS GENERADOS:\n")
cat("- Diagnósticos completos: resultados/diagnosticos_modelo_tweedie.png\n")
cat("- Envelope detallado: resultados/envelope_tweedie_detallado.png\n")
cat("- Dispersión vs media: resultados/dispersion_media_tweedie.png\n")
cat("- Perfil de verosimilitud: resultados/perfil_verosimilitud_p.png\n")

# Guardar resultados en archivo RData para análisis posterior
save(
  resultado_seleccion,
  resultado_ajuste,
  resultados_pruebas,
  datos_preparados,
  modelo_final,
  file = "resultados/analisis_completo_tweedie.RData"
)

cat("\nResultados guardados en: resultados/analisis_completo_tweedie.RData\n")

cat("\n" + rep("=", 60), "\n")
cat("ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat(rep("=", 60), "\n")