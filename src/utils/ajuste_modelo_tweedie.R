# =============================================================================
# AJUSTE Y ANÁLISIS DE MODELOS TWEEDIE
# =============================================================================
# Funciones para ajustar modelos de regresión con distribución Tweedie,
# calcular coeficientes, estadísticas de ajuste y realizar diagnósticos.
# 
# Autor: Guillermo Murillo
# Fecha: 2025
# =============================================================================

# Librerías requeridas
if (!require("tweedie")) install.packages("tweedie")
if (!require("statmod")) install.packages("statmod")
if (!require("boot")) install.packages("boot")
if (!require("MASS")) install.packages("MASS")

library(tweedie)
library(statmod)
library(boot)
library(MASS)

# =============================================================================
# FUNCIÓN PRINCIPAL DE AJUSTE
# =============================================================================

#' Ajuste completo de modelo Tweedie
#' 
#' Esta función ajusta un modelo de regresión con distribución Tweedie y
#' calcula estadísticas comprehensivas de ajuste, coeficientes e intervalos
#' de confianza.
#' 
#' @param data Data frame con los datos
#' @param formula Fórmula del modelo
#' @param p_tweedie Parámetro de potencia (entre 1 y 2)
#' @param link_power Parámetro de función de enlace (0 = log, 1 = identidad)
#' @param metodo_dispersion Método para estimar dispersión: "pearson", "deviance", "profile"
#' @param alfa Nivel de significancia para intervalos de confianza
#' @param bootstrap_ic Usar bootstrap para intervalos de confianza (TRUE/FALSE)
#' @param n_bootstrap Número de replicaciones bootstrap
#' @param optimizar_p Optimizar parámetro p automáticamente (TRUE/FALSE)
#' @param rango_p Vector con rango de búsqueda para p (c(min, max))
#' @param verbose Mostrar información detallada del ajuste
#' 
#' @return Lista con resultados completos del ajuste
ajuste_modelo_tweedie <- function(data, 
                                 formula, 
                                 p_tweedie = 1.5,
                                 link_power = 0,
                                 metodo_dispersion = "pearson",
                                 alfa = 0.05,
                                 bootstrap_ic = FALSE,
                                 n_bootstrap = 1000,
                                 optimizar_p = FALSE,
                                 rango_p = c(1.01, 1.99),
                                 verbose = TRUE) {
  
  # Validaciones iniciales
  if (!is.data.frame(data)) {
    stop("El parámetro 'data' debe ser un data frame")
  }
  
  if (!inherits(formula, "formula")) {
    stop("El parámetro 'formula' debe ser una fórmula válida")
  }
  
  if (p_tweedie <= 1 || p_tweedie >= 2) {
    stop("El parámetro p_tweedie debe estar estrictamente entre 1 y 2")
  }
  
  if (!metodo_dispersion %in% c("pearson", "deviance", "profile")) {
    stop("metodo_dispersion debe ser 'pearson', 'deviance' o 'profile'")
  }
  
  # Preparación de datos
  datos_completos <- data[complete.cases(model.frame(formula, data)), ]
  n_original <- nrow(data)
  n_completos <- nrow(datos_completos)
  
  if (verbose) {
    cat("=== AJUSTE DE MODELO TWEEDIE ===\n")
    cat("Observaciones originales:", n_original, "\n")
    cat("Observaciones completas:", n_completos, "\n")
    cat("Pérdida de datos:", round((n_original - n_completos)/n_original * 100, 2), "%\n\n")
  }
  
  # Optimización del parámetro p si se solicita
  if (optimizar_p) {
    if (verbose) cat("Optimizando parámetro de potencia p...\n")
    
    resultado_optimizacion <- optimizar_parametro_p(datos_completos, formula, 
                                                   rango_p, link_power, verbose)
    p_tweedie <- resultado_optimizacion$p_optimo
    
    if (verbose) {
      cat("Parámetro p optimizado:", round(p_tweedie, 4), "\n")
      cat("Log-verosimilitud máxima:", round(resultado_optimizacion$loglik_maximo, 2), "\n\n")
    }
  }
  
  # Definir familia Tweedie
  familia_tweedie <- tweedie(var.power = p_tweedie, link.power = link_power)
  
  # Ajuste del modelo principal
  if (verbose) cat("Ajustando modelo con p =", round(p_tweedie, 4), "...\n")
  
  modelo <- glm(formula, data = datos_completos, family = familia_tweedie)
  
  # Cálculo de parámetro de dispersión
  dispersion <- calcular_dispersion(modelo, metodo_dispersion, verbose)
  
  # Estadísticas del modelo
  estadisticas <- calcular_estadisticas_modelo(modelo, dispersion, verbose)
  
  # Coeficientes e intervalos de confianza
  coeficientes_resultado <- calcular_coeficientes_ic(modelo, alfa, bootstrap_ic, 
                                                    n_bootstrap, datos_completos, 
                                                    verbose)
  
  # Métricas de ajuste
  metricas_ajuste <- calcular_metricas_ajuste(modelo, datos_completos, verbose)
  
  # Diagnósticos residuales
  diagnosticos <- calcular_diagnosticos_residuales(modelo, p_tweedie, verbose)
  
  # Compilar resultados
  resultado_completo <- list(
    modelo = modelo,
    parametros = list(
      p_tweedie = p_tweedie,
      link_power = link_power,
      dispersion = dispersion,
      metodo_dispersion = metodo_dispersion
    ),
    estadisticas = estadisticas,
    coeficientes = coeficientes_resultado,
    metricas_ajuste = metricas_ajuste,
    diagnosticos = diagnosticos,
    datos_ajuste = list(
      n_observaciones = n_completos,
      n_predictores = length(coef(modelo)) - 1,
      formula = formula,
      perdida_datos = (n_original - n_completos)/n_original
    )
  )
  
  if (optimizar_p) {
    resultado_completo$optimizacion_p <- resultado_optimizacion
  }
  
  if (verbose) {
    imprimir_resumen_ajuste(resultado_completo)
  }
  
  return(resultado_completo)
}

# =============================================================================
# FUNCIONES AUXILIARES DE AJUSTE
# =============================================================================

#' Optimización del parámetro de potencia p
#' 
#' Encuentra el valor óptimo del parámetro p de la distribución Tweedie
#' maximizando la log-verosimilitud del modelo.
#' 
#' @param data Data frame con datos
#' @param formula Fórmula del modelo
#' @param rango_p Rango de búsqueda para p
#' @param link_power Parámetro de enlace
#' @param verbose Mostrar progreso
#' 
#' @return Lista con resultado de optimización
optimizar_parametro_p <- function(data, formula, rango_p, link_power, verbose) {
  
  # Función objetivo: log-verosimilitud negativa
  loglik_negativa <- function(p) {
    tryCatch({
      familia <- tweedie(var.power = p, link.power = link_power)
      modelo_temp <- glm(formula, data = data, family = familia)
      
      # Calcular log-verosimilitud usando función Tweedie
      y <- model.response(model.frame(formula, data))
      mu <- fitted(modelo_temp)
      phi <- summary(modelo_temp)$dispersion
      
      # Log-verosimilitud Tweedie
      loglik <- sum(dtweedie(y, mu = mu, phi = phi, power = p, log = TRUE))
      
      return(-loglik)  # Negativa para minimización
      
    }, error = function(e) {
      return(1e10)  # Valor alto si hay error
    })
  }
  
  # Optimización usando método Brent (unidimensional)
  resultado_optim <- optimize(loglik_negativa, interval = rango_p, 
                             maximum = FALSE, tol = 1e-6)
  
  p_optimo <- resultado_optim$minimum
  loglik_maximo <- -resultado_optim$objective
  
  # Evaluación en grid para diagnóstico
  if (verbose) {
    valores_p <- seq(rango_p[1], rango_p[2], length.out = 20)
    loglik_grid <- sapply(valores_p, function(p) -loglik_negativa(p))
    
    cat("Evaluación en grid de parámetros p:\n")
    resultado_grid <- data.frame(p = valores_p, loglik = loglik_grid)
    print(head(resultado_grid[order(resultado_grid$loglik, decreasing = TRUE), ], 5))
    cat("\n")
  }
  
  return(list(
    p_optimo = p_optimo,
    loglik_maximo = loglik_maximo,
    convergencia = resultado_optim$objective < 1e9
  ))
}

#' Cálculo del parámetro de dispersión
#' 
#' Calcula el parámetro de dispersión usando diferentes métodos.
#' 
#' @param modelo Modelo ajustado
#' @param metodo Método de cálculo
#' @param verbose Mostrar información
#' 
#' @return Valor del parámetro de dispersión
calcular_dispersion <- function(modelo, metodo, verbose) {
  
  if (metodo == "pearson") {
    # Estadístico de Pearson
    residuos_pearson <- residuals(modelo, type = "pearson")
    dispersion <- sum(residuos_pearson^2) / modelo$df.residual
    
  } else if (metodo == "deviance") {
    # Desvianza escalada
    dispersion <- modelo$deviance / modelo$df.residual
    
  } else if (metodo == "profile") {
    # Estimación por perfil de verosimilitud
    dispersion <- summary(modelo)$dispersion
  }
  
  if (verbose) {
    cat("Parámetro de dispersión (método", metodo, "):", round(dispersion, 4), "\n")
  }
  
  return(dispersion)
}

#' Cálculo de estadísticas del modelo
#' 
#' Calcula estadísticas comprehensivas de ajuste del modelo.
#' 
#' @param modelo Modelo ajustado
#' @param dispersion Parámetro de dispersión
#' @param verbose Mostrar información
#' 
#' @return Lista con estadísticas del modelo
calcular_estadisticas_modelo <- function(modelo, dispersion, verbose) {
  
  # Información básica
  n <- nobs(modelo)
  p <- length(coef(modelo))
  df_residual <- modelo$df.residual
  
  # Criterios de información
  aic <- AIC(modelo)
  bic <- BIC(modelo)
  
  # Log-verosimilitud
  loglik <- logLik(modelo)[1]
  
  # Desvianza y estadístico de Pearson
  deviance <- modelo$deviance
  pearson_stat <- sum(residuals(modelo, type = "pearson")^2)
  
  # Pseudo R-cuadrado (McFadden)
  modelo_nulo <- glm(update(formula(modelo), . ~ 1), 
                    data = modelo$data, family = modelo$family)
  pseudo_r2 <- 1 - (modelo$deviance / modelo_nulo$deviance)
  
  # Estadísticas de bondad de ajuste
  estadisticas <- list(
    n_observaciones = n,
    n_parametros = p,
    grados_libertad = df_residual,
    loglik = loglik,
    aic = aic,
    bic = bic,
    deviance = deviance,
    pearson_statistic = pearson_stat,
    dispersion = dispersion,
    pseudo_r2 = pseudo_r2,
    deviance_residual = deviance / df_residual,
    pearson_residual = pearson_stat / df_residual
  )
  
  if (verbose) {
    cat("\n=== ESTADÍSTICAS DEL MODELO ===\n")
    cat("Log-verosimilitud:", round(loglik, 2), "\n")
    cat("AIC:", round(aic, 2), "\n")
    cat("BIC:", round(bic, 2), "\n")
    cat("Desvianza:", round(deviance, 2), "\n")
    cat("Pseudo R²:", round(pseudo_r2, 4), "\n")
    cat("Dispersión:", round(dispersion, 4), "\n")
  }
  
  return(estadisticas)
}

#' Cálculo de coeficientes e intervalos de confianza
#' 
#' Calcula coeficientes del modelo con intervalos de confianza usando
#' métodos analíticos o bootstrap.
#' 
#' @param modelo Modelo ajustado
#' @param alfa Nivel de significancia
#' @param bootstrap_ic Usar bootstrap
#' @param n_bootstrap Número de replicaciones bootstrap
#' @param datos Data frame con datos
#' @param verbose Mostrar información
#' 
#' @return Lista con coeficientes e intervalos de confianza
calcular_coeficientes_ic <- function(modelo, alfa, bootstrap_ic, n_bootstrap, 
                                    datos, verbose) {
  
  # Coeficientes y errores estándar
  coef_estimados <- coef(modelo)
  se_coef <- summary(modelo)$coefficients[, "Std. Error"]
  t_valores <- summary(modelo)$coefficients[, "t value"]
  p_valores <- summary(modelo)$coefficients[, "Pr(>|t|)"]
  
  # Intervalos de confianza analíticos
  ic_analiticos <- confint(modelo, level = 1 - alfa)
  
  resultado <- list(
    coeficientes = coef_estimados,
    errores_estandar = se_coef,
    t_valores = t_valores,
    p_valores = p_valores,
    intervalos_confianza = ic_analiticos,
    nivel_confianza = 1 - alfa
  )
  
  # Intervalos de confianza por bootstrap si se solicita
  if (bootstrap_ic) {
    if (verbose) cat("Calculando intervalos de confianza por bootstrap...\n")
    
    # Función para bootstrap
    boot_coef <- function(data, indices) {
      datos_boot <- data[indices, ]
      tryCatch({
        modelo_boot <- glm(formula(modelo), data = datos_boot, family = modelo$family)
        return(coef(modelo_boot))
      }, error = function(e) {
        return(rep(NA, length(coef_estimados)))
      })
    }
    
    # Realizar bootstrap
    resultado_boot <- boot(datos, boot_coef, R = n_bootstrap)
    
    # Calcular intervalos de confianza bootstrap
    ic_bootstrap <- sapply(1:length(coef_estimados), function(i) {
      tryCatch({
        boot.ci(resultado_boot, index = i, conf = 1 - alfa, type = "perc")$percent[4:5]
      }, error = function(e) {
        return(c(NA, NA))
      })
    })
    
    resultado$intervalos_bootstrap <- t(ic_bootstrap)
    colnames(resultado$intervalos_bootstrap) <- c(paste0((alfa/2)*100, "%"), 
                                                  paste0((1-alfa/2)*100, "%"))
    rownames(resultado$intervalos_bootstrap) <- names(coef_estimados)
  }
  
  if (verbose) {
    cat("\n=== COEFICIENTES DEL MODELO ===\n")
    resumen_coef <- data.frame(
      Estimacion = coef_estimados,
      Error_Std = se_coef,
      t_valor = t_valores,
      p_valor = p_valores,
      IC_inf = ic_analiticos[, 1],
      IC_sup = ic_analiticos[, 2]
    )
    print(round(resumen_coef, 4))
  }
  
  return(resultado)
}

#' Cálculo de métricas de ajuste
#' 
#' Calcula métricas de evaluación del ajuste del modelo incluyendo
#' errores de predicción y medidas de bondad de ajuste.
#' 
#' @param modelo Modelo ajustado
#' @param datos Data frame con datos
#' @param verbose Mostrar información
#' 
#' @return Lista con métricas de ajuste
calcular_metricas_ajuste <- function(modelo, datos, verbose) {
  
  # Valores observados y predichos
  y_observado <- model.response(model.frame(modelo))
  y_predicho <- fitted(modelo)
  
  # Métricas de error
  mae <- mean(abs(y_observado - y_predicho))
  mse <- mean((y_observado - y_predicho)^2)
  rmse <- sqrt(mse)
  mape <- mean(abs((y_observado - y_predicho) / y_observado)) * 100
  
  # Correlación entre observados y predichos
  correlacion <- cor(y_observado, y_predicho)
  
  # Coeficiente de determinación lineal
  r2_lineal <- correlacion^2
  
  # Residuos
  residuos_deviance <- residuals(modelo, type = "deviance")
  residuos_pearson <- residuals(modelo, type = "pearson")
  
  # Estadísticos de los residuos
  residuos_stats <- list(
    deviance = list(
      media = mean(residuos_deviance),
      desv_std = sd(residuos_deviance),
      min = min(residuos_deviance),
      max = max(residuos_deviance)
    ),
    pearson = list(
      media = mean(residuos_pearson),
      desv_std = sd(residuos_pearson),
      min = min(residuos_pearson),
      max = max(residuos_pearson)
    )
  )
  
  metricas <- list(
    mae = mae,
    mse = mse,
    rmse = rmse,
    mape = mape,
    correlacion = correlacion,
    r2_lineal = r2_lineal,
    residuos_estadisticos = residuos_stats
  )
  
  if (verbose) {
    cat("\n=== MÉTRICAS DE AJUSTE ===\n")
    cat("MAE:", round(mae, 4), "\n")
    cat("RMSE:", round(rmse, 4), "\n")
    cat("MAPE:", round(mape, 2), "%\n")
    cat("Correlación obs-pred:", round(correlacion, 4), "\n")
    cat("R² lineal:", round(r2_lineal, 4), "\n")
  }
  
  return(metricas)
}

#' Cálculo de diagnósticos residuales
#' 
#' Calcula diagnósticos comprehensivos de los residuos del modelo
#' incluyendo detección de valores atípicos y puntos influyentes.
#' 
#' @param modelo Modelo ajustado
#' @param p_tweedie Parámetro de potencia
#' @param verbose Mostrar información
#' 
#' @return Lista con diagnósticos residuales
calcular_diagnosticos_residuales <- function(modelo, p_tweedie, verbose) {
  
  # Diferentes tipos de residuos
  residuos_deviance <- residuals(modelo, type = "deviance")
  residuos_pearson <- residuals(modelo, type = "pearson")
  residuos_working <- residuals(modelo, type = "working")
  
  # Valores ajustados
  valores_ajustados <- fitted(modelo)
  
  # Distancia de Cook
  cook_distance <- cooks.distance(modelo)
  
  # Leverage (palanca)
  leverage <- hatvalues(modelo)
  
  # Residuos estandarizados
  residuos_std <- rstandard(modelo)
  
  # Residuos estudentizados
  residuos_student <- rstudent(modelo)
  
  # Detección de valores atípicos
  n <- nobs(modelo)
  p <- length(coef(modelo))
  
  # Criterios de detección
  outliers_cook <- which(cook_distance > 4/(n-p))
  outliers_leverage <- which(leverage > 2*p/n)
  outliers_studentized <- which(abs(residuos_student) > 2)
  
  # DFFits
  dfbetas_vals <- dfbetas(modelo)
  dffits_vals <- dffits(modelo)
  outliers_dffits <- which(abs(dffits_vals) > 2*sqrt(p/n))
  
  diagnosticos <- list(
    residuos = list(
      deviance = residuos_deviance,
      pearson = residuos_pearson,
      working = residuos_working,
      estandarizados = residuos_std,
      estudentizados = residuos_student
    ),
    medidas_influencia = list(
      cook_distance = cook_distance,
      leverage = leverage,
      dffits = dffits_vals,
      dfbetas = dfbetas_vals
    ),
    valores_atipicos = list(
      cook = outliers_cook,
      leverage = outliers_leverage,
      studentized = outliers_studentized,
      dffits = outliers_dffits
    ),
    criterios_deteccion = list(
      cook_limite = 4/(n-p),
      leverage_limite = 2*p/n,
      studentized_limite = 2,
      dffits_limite = 2*sqrt(p/n)
    )
  )
  
  if (verbose) {
    cat("\n=== DIAGNÓSTICOS RESIDUALES ===\n")
    cat("Valores atípicos (Cook):", length(outliers_cook), "\n")
    cat("Valores con alta palanca:", length(outliers_leverage), "\n")
    cat("Residuos estudentizados extremos:", length(outliers_studentized), "\n")
    cat("DFFits extremos:", length(outliers_dffits), "\n")
    
    if (length(outliers_cook) > 0) {
      cat("Observaciones influyentes (Cook):", paste(outliers_cook, collapse = ", "), "\n")
    }
  }
  
  return(diagnosticos)
}

#' Imprimir resumen completo del ajuste
#' 
#' Imprime un resumen comprehensivo de los resultados del ajuste del modelo.
#' 
#' @param resultado Lista con resultados del ajuste
imprimir_resumen_ajuste <- function(resultado) {
  
  cat("\n" + rep("=", 60), "\n")
  cat("RESUMEN COMPLETO DEL MODELO TWEEDIE\n")
  cat(rep("=", 60), "\n")
  
  # Información del modelo
  cat("Parámetro de potencia (p):", round(resultado$parametros$p_tweedie, 4), "\n")
  cat("Parámetro de enlace:", resultado$parametros$link_power, "\n")
  cat("Observaciones utilizadas:", resultado$datos_ajuste$n_observaciones, "\n")
  cat("Número de predictores:", resultado$datos_ajuste$n_predictores, "\n")
  
  # Estadísticas principales
  cat("\nEstadísticas de ajuste:\n")
  cat("AIC:", round(resultado$estadisticas$aic, 2), "\n")
  cat("BIC:", round(resultado$estadisticas$bic, 2), "\n")
  cat("Log-verosimilitud:", round(resultado$estadisticas$loglik, 2), "\n")
  cat("Pseudo R²:", round(resultado$estadisticas$pseudo_r2, 4), "\n")
  
  # Métricas de error
  cat("\nMétricas de predicción:\n")
  cat("MAE:", round(resultado$metricas_ajuste$mae, 4), "\n")
  cat("RMSE:", round(resultado$metricas_ajuste$rmse, 4), "\n")
  cat("MAPE:", round(resultado$metricas_ajuste$mape, 2), "%\n")
  
  # Diagnósticos
  outliers_totales <- length(unique(c(
    resultado$diagnosticos$valores_atipicos$cook,
    resultado$diagnosticos$valores_atipicos$leverage,
    resultado$diagnosticos$valores_atipicos$studentized
  )))
  
  cat("\nDiagnósticos:\n")
  cat("Valores atípicos detectados:", outliers_totales, "\n")
  cat("Dispersión estimada:", round(resultado$parametros$dispersion, 4), "\n")
  
  cat(rep("=", 60), "\n")
}