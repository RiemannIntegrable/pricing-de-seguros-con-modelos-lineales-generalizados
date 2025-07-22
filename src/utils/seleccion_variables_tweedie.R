# =============================================================================
# SELECCIÓN DE VARIABLES PARA MODELO TWEEDIE
# =============================================================================
# Funciones para realizar selección de variables utilizando criterio de 
# información de Akaike (AIC) en modelos de distribución Tweedie.
# 
# Autor: Guillermo Murillo
# Fecha: 2025
# =============================================================================

# Librerías requeridas
if (!require("tweedie")) install.packages("tweedie")
if (!require("statmod")) install.packages("statmod")
if (!require("dplyr")) install.packages("dplyr")

library(tweedie)
library(statmod)
library(dplyr)

# =============================================================================
# FUNCIÓN PRINCIPAL DE SELECCIÓN DE VARIABLES
# =============================================================================

#' Selección de variables usando criterio AIC para modelo Tweedie
#' 
#' Esta función implementa un algoritmo de selección automática de variables
#' basado en el criterio de información de Akaike (AIC). Utiliza un enfoque
#' step-wise que puede ser forward, backward o both.
#' 
#' @param data Data frame con los datos de entrada
#' @param response_var Nombre de la variable respuesta (string)
#' @param predictor_vars Vector con nombres de variables predictoras
#' @param method Método de selección: "forward", "backward", "both"
#' @param p_tweedie Parámetro de potencia para distribución Tweedie
#' @param phi Parámetro de dispersión inicial (NULL para estimación automática)
#' @param max_vars Número máximo de variables en el modelo final
#' @param verbose Mostrar progreso del algoritmo (TRUE/FALSE)
#' 
#' @return Lista con:
#'   - modelo_final: Objeto glm con el modelo seleccionado
#'   - variables_seleccionadas: Vector con variables incluidas
#'   - criterios_seleccion: Data frame con AIC de cada paso
#'   - resumen_seleccion: Resumen del proceso de selección
seleccion_variables_aic <- function(data, 
                                   response_var, 
                                   predictor_vars, 
                                   method = "both",
                                   p_tweedie = 1.5,
                                   phi = NULL,
                                   max_vars = length(predictor_vars),
                                   verbose = TRUE) {
  
  # Validación de parámetros de entrada
  if (!is.data.frame(data)) {
    stop("El parámetro 'data' debe ser un data frame")
  }
  
  if (!response_var %in% names(data)) {
    stop("La variable respuesta no existe en los datos")
  }
  
  if (!all(predictor_vars %in% names(data))) {
    stop("Algunas variables predictoras no existen en los datos")
  }
  
  if (!method %in% c("forward", "backward", "both")) {
    stop("El método debe ser 'forward', 'backward' o 'both'")
  }
  
  if (p_tweedie < 1 || p_tweedie > 2) {
    stop("El parámetro p_tweedie debe estar entre 1 y 2")
  }
  
  # Preparación de datos - eliminar valores faltantes
  datos_completos <- data[complete.cases(data[c(response_var, predictor_vars)]), ]
  n_original <- nrow(data)
  n_completos <- nrow(datos_completos)
  
  if (verbose) {
    cat("Datos originales:", n_original, "observaciones\n")
    cat("Datos completos:", n_completos, "observaciones\n")
    cat("Observaciones eliminadas:", n_original - n_completos, "\n\n")
  }
  
  # Definir fórmulas base
  formula_nula <- as.formula(paste(response_var, "~ 1"))
  formula_completa <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " + ")))
  
  # Estimación inicial del parámetro de dispersión si no se proporciona
  if (is.null(phi)) {
    modelo_inicial <- glm(formula_completa, 
                         data = datos_completos, 
                         family = tweedie(var.power = p_tweedie, link.power = 0))
    phi <- summary(modelo_inicial)$dispersion
    if (verbose) {
      cat("Parámetro de dispersión estimado:", round(phi, 4), "\n\n")
    }
  }
  
  # Definir función de familia Tweedie con parámetros fijos
  familia_tweedie <- tweedie(var.power = p_tweedie, link.power = 0)
  
  # Proceso de selección según el método especificado
  if (method == "forward") {
    modelo_resultado <- seleccion_forward(datos_completos, formula_nula, 
                                        predictor_vars, familia_tweedie, 
                                        max_vars, verbose)
  } else if (method == "backward") {
    modelo_resultado <- seleccion_backward(datos_completos, formula_completa, 
                                         familia_tweedie, verbose)
  } else {
    # Método "both" - combinación de forward y backward
    modelo_forward <- seleccion_forward(datos_completos, formula_nula, 
                                      predictor_vars, familia_tweedie, 
                                      max_vars, verbose)
    
    if (verbose) cat("\n--- Iniciando refinamiento backward ---\n")
    
    variables_forward <- all.vars(formula(modelo_forward$modelo_final))[-1]
    formula_forward <- as.formula(paste(response_var, "~", 
                                      paste(variables_forward, collapse = " + ")))
    
    modelo_resultado <- seleccion_backward(datos_completos, formula_forward, 
                                         familia_tweedie, verbose)
  }
  
  # Compilar resultados finales
  variables_finales <- all.vars(formula(modelo_resultado$modelo_final))[-1]
  
  # Generar resumen del proceso
  resumen <- list(
    metodo_seleccion = method,
    variables_originales = length(predictor_vars),
    variables_finales = length(variables_finales),
    parametro_tweedie = p_tweedie,
    parametro_dispersion = phi,
    aic_final = AIC(modelo_resultado$modelo_final),
    observaciones_usadas = n_completos
  )
  
  if (verbose) {
    cat("\n=== RESUMEN DE SELECCIÓN ===\n")
    cat("Método:", method, "\n")
    cat("Variables originales:", length(predictor_vars), "\n")
    cat("Variables seleccionadas:", length(variables_finales), "\n")
    cat("AIC final:", round(AIC(modelo_resultado$modelo_final), 2), "\n")
    cat("Variables incluidas:", paste(variables_finales, collapse = ", "), "\n")
  }
  
  return(list(
    modelo_final = modelo_resultado$modelo_final,
    variables_seleccionadas = variables_finales,
    criterios_seleccion = modelo_resultado$historial_aic,
    resumen_seleccion = resumen
  ))
}

# =============================================================================
# FUNCIONES AUXILIARES DE SELECCIÓN
# =============================================================================

#' Selección forward de variables
#' 
#' Implementa el algoritmo de selección forward, agregando variables una por una
#' basándose en la mejora del criterio AIC.
#' 
#' @param datos Data frame con los datos
#' @param formula_base Fórmula base (usualmente solo intercepto)
#' @param vars_candidatas Variables candidatas para inclusión
#' @param familia Familia de distribución (Tweedie)
#' @param max_vars Número máximo de variables a incluir
#' @param verbose Mostrar progreso
#' 
#' @return Lista con modelo final e historial de selección
seleccion_forward <- function(datos, formula_base, vars_candidatas, 
                             familia, max_vars, verbose) {
  
  # Inicialización
  modelo_actual <- glm(formula_base, data = datos, family = familia)
  aic_actual <- AIC(modelo_actual)
  variables_incluidas <- character(0)
  vars_disponibles <- vars_candidatas
  historial <- data.frame(paso = 0, accion = "Base", variable = "Intercepto", 
                         aic = aic_actual, stringsAsFactors = FALSE)
  
  if (verbose) {
    cat("=== SELECCIÓN FORWARD ===\n")
    cat("AIC inicial:", round(aic_actual, 2), "\n")
  }
  
  paso <- 1
  mejora <- TRUE
  
  while (mejora && length(variables_incluidas) < max_vars && length(vars_disponibles) > 0) {
    
    mejor_aic <- Inf
    mejor_variable <- NULL
    
    # Evaluar cada variable disponible
    for (var in vars_disponibles) {
      vars_prueba <- c(variables_incluidas, var)
      formula_prueba <- as.formula(paste(all.vars(formula_base)[1], "~", 
                                       paste(vars_prueba, collapse = " + ")))
      
      tryCatch({
        modelo_prueba <- glm(formula_prueba, data = datos, family = familia)
        aic_prueba <- AIC(modelo_prueba)
        
        if (aic_prueba < mejor_aic) {
          mejor_aic <- aic_prueba
          mejor_variable <- var
        }
      }, error = function(e) {
        if (verbose) cat("Error al ajustar modelo con variable", var, "\n")
      })
    }
    
    # Decidir si incluir la mejor variable
    if (!is.null(mejor_variable) && mejor_aic < aic_actual) {
      variables_incluidas <- c(variables_incluidas, mejor_variable)
      vars_disponibles <- vars_disponibles[vars_disponibles != mejor_variable]
      aic_actual <- mejor_aic
      
      # Actualizar modelo actual
      formula_actual <- as.formula(paste(all.vars(formula_base)[1], "~", 
                                       paste(variables_incluidas, collapse = " + ")))
      modelo_actual <- glm(formula_actual, data = datos, family = familia)
      
      # Registrar en historial
      historial <- rbind(historial, 
                        data.frame(paso = paso, accion = "Agregar", 
                                 variable = mejor_variable, aic = aic_actual,
                                 stringsAsFactors = FALSE))
      
      if (verbose) {
        cat("Paso", paso, ": Agregada", mejor_variable, ", AIC =", round(aic_actual, 2), "\n")
      }
      
      paso <- paso + 1
    } else {
      mejora <- FALSE
      if (verbose) {
        cat("No hay más variables que mejoren el AIC\n")
      }
    }
  }
  
  return(list(modelo_final = modelo_actual, historial_aic = historial))
}

#' Selección backward de variables
#' 
#' Implementa el algoritmo de selección backward, eliminando variables una por una
#' basándose en la mejora del criterio AIC.
#' 
#' @param datos Data frame con los datos
#' @param formula_completa Fórmula con todas las variables
#' @param familia Familia de distribución (Tweedie)
#' @param verbose Mostrar progreso
#' 
#' @return Lista con modelo final e historial de selección
seleccion_backward <- function(datos, formula_completa, familia, verbose) {
  
  # Modelo inicial con todas las variables
  modelo_actual <- glm(formula_completa, data = datos, family = familia)
  aic_actual <- AIC(modelo_actual)
  variables_incluidas <- all.vars(formula_completa)[-1]  # Excluir variable respuesta
  historial <- data.frame(paso = 0, accion = "Completo", variable = "Todas", 
                         aic = aic_actual, stringsAsFactors = FALSE)
  
  if (verbose) {
    cat("=== SELECCIÓN BACKWARD ===\n")
    cat("AIC inicial:", round(aic_actual, 2), "\n")
  }
  
  paso <- 1
  mejora <- TRUE
  
  while (mejora && length(variables_incluidas) > 1) {
    
    mejor_aic <- Inf
    variable_eliminar <- NULL
    
    # Evaluar eliminación de cada variable
    for (var in variables_incluidas) {
      vars_restantes <- variables_incluidas[variables_incluidas != var]
      
      if (length(vars_restantes) > 0) {
        formula_prueba <- as.formula(paste(all.vars(formula_completa)[1], "~", 
                                         paste(vars_restantes, collapse = " + ")))
      } else {
        formula_prueba <- as.formula(paste(all.vars(formula_completa)[1], "~ 1"))
      }
      
      tryCatch({
        modelo_prueba <- glm(formula_prueba, data = datos, family = familia)
        aic_prueba <- AIC(modelo_prueba)
        
        if (aic_prueba < mejor_aic) {
          mejor_aic <- aic_prueba
          variable_eliminar <- var
        }
      }, error = function(e) {
        if (verbose) cat("Error al ajustar modelo sin variable", var, "\n")
      })
    }
    
    # Decidir si eliminar la variable
    if (!is.null(variable_eliminar) && mejor_aic < aic_actual) {
      variables_incluidas <- variables_incluidas[variables_incluidas != variable_eliminar]
      aic_actual <- mejor_aic
      
      # Actualizar modelo actual
      if (length(variables_incluidas) > 0) {
        formula_actual <- as.formula(paste(all.vars(formula_completa)[1], "~", 
                                         paste(variables_incluidas, collapse = " + ")))
      } else {
        formula_actual <- as.formula(paste(all.vars(formula_completa)[1], "~ 1"))
      }
      
      modelo_actual <- glm(formula_actual, data = datos, family = familia)
      
      # Registrar en historial
      historial <- rbind(historial, 
                        data.frame(paso = paso, accion = "Eliminar", 
                                 variable = variable_eliminar, aic = aic_actual,
                                 stringsAsFactors = FALSE))
      
      if (verbose) {
        cat("Paso", paso, ": Eliminada", variable_eliminar, ", AIC =", round(aic_actual, 2), "\n")
      }
      
      paso <- paso + 1
    } else {
      mejora <- FALSE
      if (verbose) {
        cat("No hay más variables que mejoren el AIC al eliminar\n")
      }
    }
  }
  
  return(list(modelo_final = modelo_actual, historial_aic = historial))
}

# =============================================================================
# FUNCIÓN DE VALIDACIÓN CRUZADA PARA SELECCIÓN
# =============================================================================

#' Validación cruzada para selección de variables
#' 
#' Evalúa la estabilidad de la selección de variables mediante validación
#' cruzada k-fold, calculando métricas de error de predicción.
#' 
#' @param data Data frame con los datos
#' @param response_var Nombre de la variable respuesta
#' @param predictor_vars Vector con variables predictoras
#' @param k_folds Número de folds para validación cruzada
#' @param p_tweedie Parámetro de potencia Tweedie
#' @param method Método de selección
#' @param seed Semilla para reproducibilidad
#' 
#' @return Lista con métricas de validación cruzada
validacion_cruzada_seleccion <- function(data, response_var, predictor_vars, 
                                        k_folds = 5, p_tweedie = 1.5, 
                                        method = "both", seed = 123) {
  
  set.seed(seed)
  n <- nrow(data)
  indices_fold <- sample(rep(1:k_folds, length.out = n))
  
  resultados_cv <- list()
  variables_seleccionadas <- list()
  
  cat("Ejecutando validación cruzada", k_folds, "-fold...\n")
  
  for (fold in 1:k_folds) {
    cat("Fold", fold, "de", k_folds, "\n")
    
    # División de datos
    datos_entrenamiento <- data[indices_fold != fold, ]
    datos_prueba <- data[indices_fold == fold, ]
    
    # Selección de variables en datos de entrenamiento
    resultado_seleccion <- seleccion_variables_aic(
      data = datos_entrenamiento,
      response_var = response_var,
      predictor_vars = predictor_vars,
      method = method,
      p_tweedie = p_tweedie,
      verbose = FALSE
    )
    
    variables_seleccionadas[[fold]] <- resultado_seleccion$variables_seleccionadas
    
    # Predicción en datos de prueba
    if (length(resultado_seleccion$variables_seleccionadas) > 0) {
      predicciones <- predict(resultado_seleccion$modelo_final, 
                            newdata = datos_prueba, type = "response")
      
      # Calcular métricas de error
      observados <- datos_prueba[[response_var]]
      mae <- mean(abs(observados - predicciones), na.rm = TRUE)
      rmse <- sqrt(mean((observados - predicciones)^2, na.rm = TRUE))
      
      resultados_cv[[fold]] <- list(
        variables = resultado_seleccion$variables_seleccionadas,
        mae = mae,
        rmse = rmse,
        aic = AIC(resultado_seleccion$modelo_final)
      )
    }
  }
  
  # Resumen de resultados
  mae_promedio <- mean(sapply(resultados_cv, function(x) x$mae), na.rm = TRUE)
  rmse_promedio <- mean(sapply(resultados_cv, function(x) x$rmse), na.rm = TRUE)
  
  # Frecuencia de selección de variables
  todas_variables <- unique(unlist(variables_seleccionadas))
  frecuencia_seleccion <- sapply(todas_variables, function(var) {
    sum(sapply(variables_seleccionadas, function(vars) var %in% vars))
  })
  
  cat("\n=== RESULTADOS DE VALIDACIÓN CRUZADA ===\n")
  cat("MAE promedio:", round(mae_promedio, 4), "\n")
  cat("RMSE promedio:", round(rmse_promedio, 4), "\n")
  cat("\nFrecuencia de selección de variables:\n")
  print(sort(frecuencia_seleccion, decreasing = TRUE))
  
  return(list(
    resultados_folds = resultados_cv,
    mae_promedio = mae_promedio,
    rmse_promedio = rmse_promedio,
    frecuencia_variables = frecuencia_seleccion,
    variables_por_fold = variables_seleccionadas
  ))
}