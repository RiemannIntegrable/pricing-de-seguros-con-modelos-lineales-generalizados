# =============================================================================
# PRUEBAS DE BONDAD DE AJUSTE PARA MODELOS TWEEDIE
# =============================================================================
# Funciones para realizar pruebas estadísticas de bondad de ajuste, validación
# de supuestos y análisis de adecuación de modelos con distribución Tweedie.
# 
# Autor: Guillermo Murillo
# Fecha: 2025
# =============================================================================

# Librerías requeridas
if (!require("tweedie")) install.packages("tweedie")
if (!require("statmod")) install.packages("statmod")
if (!require("nortest")) install.packages("nortest")
if (!require("tseries")) install.packages("tseries")
if (!require("car")) install.packages("car")
if (!require("lmtest")) install.packages("lmtest")
if (!require("broom")) install.packages("broom")

library(tweedie)
library(statmod)
library(nortest)
library(tseries)
library(car)
library(lmtest)
library(broom)

# =============================================================================
# FUNCIÓN PRINCIPAL DE PRUEBAS DE AJUSTE
# =============================================================================

#' Suite completa de pruebas de bondad de ajuste para modelos Tweedie
#' 
#' Esta función ejecuta una batería comprehensiva de pruebas estadísticas
#' para evaluar la bondad de ajuste y validar los supuestos del modelo
#' Tweedie ajustado.
#' 
#' @param modelo Modelo ajustado con glm() usando familia Tweedie
#' @param datos_originales Data frame con los datos originales (opcional)
#' @param alpha Nivel de significancia para las pruebas (default: 0.05)
#' @param incluir_bootstrap Incluir pruebas bootstrap (TRUE/FALSE)
#' @param n_bootstrap Número de replicaciones bootstrap
#' @param incluir_simulacion Incluir pruebas basadas en simulación
#' @param n_simulaciones Número de simulaciones para pruebas
#' @param verbose Mostrar información detallada de cada prueba
#' @param semilla Semilla para reproducibilidad
#' 
#' @return Lista comprehensiva con resultados de todas las pruebas
suite_pruebas_ajuste_tweedie <- function(modelo, 
                                        datos_originales = NULL,
                                        alpha = 0.05,
                                        incluir_bootstrap = TRUE,
                                        n_bootstrap = 1000,
                                        incluir_simulacion = TRUE,
                                        n_simulaciones = 500,
                                        verbose = TRUE,
                                        semilla = 123) {
  
  # Validaciones iniciales
  if (!inherits(modelo, "glm")) {
    stop("El modelo debe ser un objeto glm")
  }
  
  if (modelo$family$family != "Tweedie") {
    stop("El modelo debe usar la familia Tweedie")
  }
  
  set.seed(semilla)
  
  if (verbose) {
    cat("=== SUITE DE PRUEBAS DE AJUSTE TWEEDIE ===\n")
    cat("Nivel de significancia:", alpha, "\n")
    cat("Bootstrap:", ifelse(incluir_bootstrap, "Sí", "No"), 
        ifelse(incluir_bootstrap, paste0(" (", n_bootstrap, " replicaciones)"), ""), "\n")
    cat("Simulación:", ifelse(incluir_simulacion, "Sí", "No"), 
        ifelse(incluir_simulacion, paste0(" (", n_simulaciones, " simulaciones)"), ""), "\n\n")
  }
  
  # Extraer información del modelo
  residuos_deviance <- residuals(modelo, type = "deviance")
  residuos_pearson <- residuals(modelo, type = "pearson")
  residuos_std <- rstandard(modelo)
  valores_ajustados <- fitted(modelo)
  y_observado <- model.response(model.frame(modelo))
  
  # Información del modelo
  n <- length(y_observado)
  p <- length(coef(modelo))
  df_residual <- modelo$df.residual
  
  # Parámetros Tweedie
  p_tweedie <- modelo$family$variance()$power
  phi <- summary(modelo)$dispersion
  
  # Inicializar lista de resultados
  resultados <- list(
    informacion_modelo = list(
      n_observaciones = n,
      n_parametros = p,
      grados_libertad = df_residual,
      parametro_p = p_tweedie,
      dispersion = phi
    )
  )
  
  # 1. PRUEBAS DE NORMALIDAD DE RESIDUOS
  if (verbose) cat("1. Pruebas de normalidad de residuos...\n")
  resultados$pruebas_normalidad <- pruebas_normalidad_residuos(
    residuos_deviance, residuos_pearson, residuos_std, alpha, verbose)
  
  # 2. PRUEBAS DE HOMOCEDASTICIDAD
  if (verbose) cat("\n2. Pruebas de homocedasticidad...\n")
  resultados$pruebas_homocedasticidad <- pruebas_homocedasticidad(
    modelo, residuos_pearson, valores_ajustados, alpha, verbose)
  
  # 3. PRUEBAS DE INDEPENDENCIA
  if (verbose) cat("\n3. Pruebas de independencia...\n")
  resultados$pruebas_independencia <- pruebas_independencia_residuos(
    residuos_deviance, residuos_pearson, alpha, verbose)
  
  # 4. PRUEBAS ESPECÍFICAS PARA TWEEDIE
  if (verbose) cat("\n4. Pruebas específicas para distribución Tweedie...\n")
  resultados$pruebas_tweedie <- pruebas_especificas_tweedie(
    modelo, y_observado, valores_ajustados, p_tweedie, phi, alpha, verbose)
  
  # 5. PRUEBAS DE FORMA FUNCIONAL
  if (verbose) cat("\n5. Pruebas de forma funcional...\n")
  resultados$pruebas_forma_funcional <- pruebas_forma_funcional(
    modelo, datos_originales, alpha, verbose)
  
  # 6. PRUEBAS BOOTSTRAP (si se solicita)
  if (incluir_bootstrap) {
    if (verbose) cat("\n6. Pruebas bootstrap...\n")
    resultados$pruebas_bootstrap <- pruebas_bootstrap_tweedie(
      modelo, datos_originales, n_bootstrap, alpha, verbose)
  }
  
  # 7. PRUEBAS POR SIMULACIÓN (si se solicita)
  if (incluir_simulacion) {
    if (verbose) cat("\n7. Pruebas basadas en simulación...\n")
    resultados$pruebas_simulacion <- pruebas_simulacion_tweedie(
      modelo, n_simulaciones, alpha, verbose)
  }
  
  # 8. RESUMEN GENERAL DE PRUEBAS
  if (verbose) cat("\n8. Generando resumen general...\n")
  resultados$resumen_general <- generar_resumen_pruebas(resultados, alpha, verbose)
  
  # Información final
  if (verbose) {
    cat("\n" + rep("=", 60), "\n")
    cat("SUITE DE PRUEBAS COMPLETADA\n")
    cat(rep("=", 60), "\n")
    imprimir_conclusion_general(resultados$resumen_general, alpha)
  }
  
  return(resultados)
}

# =============================================================================
# FUNCIONES DE PRUEBAS ESPECÍFICAS
# =============================================================================

#' Pruebas de normalidad de residuos
#' 
#' Ejecuta múltiples pruebas de normalidad sobre diferentes tipos de residuos.
#' 
#' @param res_dev Residuos deviance
#' @param res_pear Residuos Pearson
#' @param res_std Residuos estandarizados
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas de normalidad
pruebas_normalidad_residuos <- function(res_dev, res_pear, res_std, alpha, verbose) {
  
  # Función auxiliar para ejecutar prueba
  ejecutar_prueba_normalidad <- function(residuos, nombre_prueba, test_func) {
    tryCatch({
      resultado <- test_func(residuos)
      list(
        estadistico = as.numeric(resultado$statistic),
        p_valor = as.numeric(resultado$p.value),
        rechaza_h0 = resultado$p.value < alpha,
        nombre_estadistico = names(resultado$statistic)
      )
    }, error = function(e) {
      list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
    })
  }
  
  # Pruebas para cada tipo de residuo
  tipos_residuos <- list(
    "deviance" = res_dev,
    "pearson" = res_pear,
    "estandarizados" = res_std
  )
  
  resultados_normalidad <- list()
  
  for (tipo in names(tipos_residuos)) {
    residuos <- tipos_residuos[[tipo]]
    
    if (verbose) cat("  Probando normalidad de residuos", tipo, "...\n")
    
    # Shapiro-Wilk (si n <= 5000)
    if (length(residuos) <= 5000) {
      sw_resultado <- ejecutar_prueba_normalidad(residuos, "Shapiro-Wilk", shapiro.test)
    } else {
      sw_resultado <- list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, 
                          nota = "Muestra muy grande para Shapiro-Wilk")
    }
    
    # Kolmogorov-Smirnov
    ks_resultado <- tryCatch({
      resultado <- ks.test(residuos, "pnorm", mean(residuos), sd(residuos))
      list(
        estadistico = as.numeric(resultado$statistic),
        p_valor = as.numeric(resultado$p.value),
        rechaza_h0 = resultado$p.value < alpha,
        nombre_estadistico = "D"
      )
    }, error = function(e) {
      list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
    })
    
    # Anderson-Darling
    ad_resultado <- ejecutar_prueba_normalidad(residuos, "Anderson-Darling", ad.test)
    
    # Lilliefors
    lf_resultado <- ejecutar_prueba_normalidad(residuos, "Lilliefors", lillie.test)
    
    # Jarque-Bera
    jb_resultado <- ejecutar_prueba_normalidad(residuos, "Jarque-Bera", jarque.bera.test)
    
    resultados_normalidad[[tipo]] <- list(
      shapiro_wilk = sw_resultado,
      kolmogorov_smirnov = ks_resultado,
      anderson_darling = ad_resultado,
      lilliefors = lf_resultado,
      jarque_bera = jb_resultado
    )
    
    if (verbose) {
      cat("    Shapiro-Wilk: p =", round(sw_resultado$p_valor, 4), 
          ifelse(sw_resultado$rechaza_h0, "(Rechaza normalidad)", "(No rechaza)"), "\n")
      cat("    Anderson-Darling: p =", round(ad_resultado$p_valor, 4), 
          ifelse(ad_resultado$rechaza_h0, "(Rechaza normalidad)", "(No rechaza)"), "\n")
    }
  }
  
  return(resultados_normalidad)
}

#' Pruebas de homocedasticidad
#' 
#' Evalúa la constancia de la varianza en los residuos.
#' 
#' @param modelo Modelo ajustado
#' @param residuos_pearson Residuos Pearson
#' @param valores_ajustados Valores ajustados
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas de homocedasticidad
pruebas_homocedasticidad <- function(modelo, residuos_pearson, valores_ajustados, 
                                    alpha, verbose) {
  
  # Prueba de Breusch-Pagan
  bp_resultado <- tryCatch({
    resultado <- bptest(modelo)
    list(
      estadistico = as.numeric(resultado$statistic),
      p_valor = as.numeric(resultado$p.value),
      grados_libertad = resultado$parameter,
      rechaza_h0 = resultado$p.value < alpha
    )
  }, error = function(e) {
    list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
  })
  
  # Prueba de Goldfeld-Quandt (dividir muestra en dos)
  n <- length(residuos_pearson)
  idx_medio <- floor(n/2)
  
  gq_resultado <- tryCatch({
    # Ordenar por valores ajustados
    orden <- order(valores_ajustados)
    res_ordenados <- residuos_pearson[orden]
    
    # Dividir en dos grupos
    grupo1 <- res_ordenados[1:idx_medio]
    grupo2 <- res_ordenados[(idx_medio+1):n]
    
    # Prueba F de igualdad de varianzas
    f_test <- var.test(grupo1, grupo2)
    
    list(
      estadistico = as.numeric(f_test$statistic),
      p_valor = as.numeric(f_test$p.value),
      rechaza_h0 = f_test$p.value < alpha,
      razon_varianzas = var(grupo2) / var(grupo1)
    )
  }, error = function(e) {
    list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
  })
  
  # Prueba de White (heterocedasticidad general)
  white_resultado <- tryCatch({
    # Crear variables para prueba de White
    X <- model.matrix(modelo)
    res2 <- residuos_pearson^2
    
    # Regresión auxiliar: res^2 ~ X + X^2 + X*X (términos cruzados)
    if (ncol(X) > 1) {  # Solo si hay más de intercepto
      X_sin_intercepto <- X[, -1, drop = FALSE]
      
      # Términos cuadráticos
      X_cuad <- X_sin_intercepto^2
      colnames(X_cuad) <- paste0(colnames(X_sin_intercepto), "_sq")
      
      # Crear data frame para regresión auxiliar
      datos_aux <- data.frame(res2 = res2, X_sin_intercepto, X_cuad)
      
      # Regresión auxiliar
      modelo_aux <- lm(res2 ~ ., data = datos_aux)
      
      # Estadístico de White: n * R^2
      r_squared <- summary(modelo_aux)$r.squared
      estadistico_white <- n * r_squared
      
      # Grados de libertad: número de regresores (sin intercepto)
      df_white <- ncol(datos_aux) - 1
      
      # P-valor usando distribución chi-cuadrado
      p_valor_white <- 1 - pchisq(estadistico_white, df_white)
      
      list(
        estadistico = estadistico_white,
        p_valor = p_valor_white,
        grados_libertad = df_white,
        r_squared_auxiliar = r_squared,
        rechaza_h0 = p_valor_white < alpha
      )
    } else {
      list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, 
          nota = "No se puede realizar con solo intercepto")
    }
  }, error = function(e) {
    list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
  })
  
  if (verbose) {
    cat("  Breusch-Pagan: p =", round(bp_resultado$p_valor, 4), 
        ifelse(bp_resultado$rechaza_h0, "(Rechaza homocedasticidad)", "(No rechaza)"), "\n")
    cat("  Goldfeld-Quandt: p =", round(gq_resultado$p_valor, 4), 
        ifelse(gq_resultado$rechaza_h0, "(Rechaza homocedasticidad)", "(No rechaza)"), "\n")
    cat("  White: p =", round(white_resultado$p_valor, 4), 
        ifelse(white_resultado$rechaza_h0, "(Rechaza homocedasticidad)", "(No rechaza)"), "\n")
  }
  
  return(list(
    breusch_pagan = bp_resultado,
    goldfeld_quandt = gq_resultado,
    white = white_resultado
  ))
}

#' Pruebas de independencia de residuos
#' 
#' Evalúa la independencia temporal/espacial de los residuos.
#' 
#' @param res_dev Residuos deviance
#' @param res_pear Residuos Pearson
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas de independencia
pruebas_independencia_residuos <- function(res_dev, res_pear, alpha, verbose) {
  
  # Durbin-Watson para autocorrelación de primer orden
  dw_resultado <- tryCatch({
    # Calcular estadístico Durbin-Watson manualmente
    diff_res <- diff(res_dev)
    dw_stat <- sum(diff_res^2) / sum(res_dev^2)
    
    # Aproximación para p-valor (distribución exacta es compleja)
    # Valor cercano a 2 indica no autocorrelación
    list(
      estadistico = dw_stat,
      interpretacion = ifelse(abs(dw_stat - 2) < 0.5, "No autocorrelación aparente",
                            ifelse(dw_stat < 1.5, "Posible autocorrelación positiva",
                                  "Posible autocorrelación negativa")),
      distancia_de_2 = abs(dw_stat - 2)
    )
  }, error = function(e) {
    list(estadistico = NA, interpretacion = "Error en cálculo", error = e$message)
  })
  
  # Ljung-Box para autocorrelación de varios lags
  lb_resultado <- tryCatch({
    # Usar hasta 10 lags o n/4, lo que sea menor
    max_lag <- min(10, floor(length(res_dev)/4))
    resultado <- Box.test(res_dev, lag = max_lag, type = "Ljung-Box")
    
    list(
      estadistico = as.numeric(resultado$statistic),
      p_valor = as.numeric(resultado$p.value),
      grados_libertad = resultado$parameter,
      rechaza_h0 = resultado$p.value < alpha,
      lags_probados = max_lag
    )
  }, error = function(e) {
    list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
  })
  
  # Runs Test para aleatoriedad
  runs_resultado <- tryCatch({
    # Convertir residuos a secuencia binaria (positivo/negativo)
    secuencia_binaria <- ifelse(res_dev > median(res_dev), 1, 0)
    
    # Contar runs (secuencias consecutivas del mismo valor)
    runs <- rle(secuencia_binaria)
    n_runs <- length(runs$lengths)
    
    # Número de 1s y 0s
    n1 <- sum(secuencia_binaria)
    n0 <- length(secuencia_binaria) - n1
    
    # Estadístico de runs (aproximación normal)
    if (n1 > 0 && n0 > 0) {
      mu_runs <- (2 * n1 * n0) / (n1 + n0) + 1
      sigma2_runs <- (2 * n1 * n0 * (2 * n1 * n0 - n1 - n0)) / 
                     ((n1 + n0)^2 * (n1 + n0 - 1))
      
      if (sigma2_runs > 0) {
        z_stat <- (n_runs - mu_runs) / sqrt(sigma2_runs)
        p_valor <- 2 * (1 - pnorm(abs(z_stat)))
        
        list(
          estadistico = z_stat,
          p_valor = p_valor,
          n_runs_observado = n_runs,
          n_runs_esperado = mu_runs,
          rechaza_h0 = p_valor < alpha
        )
      } else {
        list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, 
            nota = "Varianza de runs es cero")
      }
    } else {
      list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, 
          nota = "Todos los residuos tienen el mismo signo")
    }
  }, error = function(e) {
    list(estadistico = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
  })
  
  if (verbose) {
    cat("  Durbin-Watson:", round(dw_resultado$estadistico, 4), 
        "(", dw_resultado$interpretacion, ")\n")
    cat("  Ljung-Box: p =", round(lb_resultado$p_valor, 4), 
        ifelse(lb_resultado$rechaza_h0, "(Rechaza independencia)", "(No rechaza)"), "\n")
    cat("  Runs Test: p =", round(runs_resultado$p_valor, 4), 
        ifelse(runs_resultado$rechaza_h0, "(Rechaza aleatoriedad)", "(No rechaza)"), "\n")
  }
  
  return(list(
    durbin_watson = dw_resultado,
    ljung_box = lb_resultado,
    runs_test = runs_resultado
  ))
}

#' Pruebas específicas para distribución Tweedie
#' 
#' Realiza pruebas específicas para validar la adecuación de la distribución
#' Tweedie y sus parámetros.
#' 
#' @param modelo Modelo ajustado
#' @param y_obs Valores observados
#' @param mu_pred Valores predichos
#' @param p_tweedie Parámetro de potencia
#' @param phi Parámetro de dispersión
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas específicas Tweedie
pruebas_especificas_tweedie <- function(modelo, y_obs, mu_pred, p_tweedie, 
                                       phi, alpha, verbose) {
  
  # Prueba de la relación varianza-media
  prueba_varianza_media <- tryCatch({
    # Dividir datos en bins basados en valores predichos
    n_bins <- min(20, length(unique(mu_pred)))
    bins <- cut(mu_pred, breaks = n_bins, include.lowest = TRUE)
    
    # Calcular media y varianza empírica por bin
    estadisticas_bin <- aggregate(cbind(y_obs, mu_pred), by = list(bin = bins), 
                                 FUN = function(x) c(media = mean(x), varianza = var(x)))
    
    media_bins <- estadisticas_bin$mu_pred[, "media"]
    var_obs_bins <- estadisticas_bin$y_obs[, "varianza"]
    
    # Varianza teórica Tweedie: φ * μ^p
    var_teo_bins <- phi * media_bins^p_tweedie
    
    # Correlación entre varianza observada y teórica
    if (length(var_obs_bins) > 2 && !any(is.na(var_obs_bins)) && 
        !any(is.na(var_teo_bins)) && var(var_obs_bins) > 0 && var(var_teo_bins) > 0) {
      
      correlacion <- cor(var_obs_bins, var_teo_bins, use = "complete.obs")
      
      # Prueba de correlación
      test_cor <- cor.test(var_obs_bins, var_teo_bins)
      
      # Regresión de var_obs ~ var_teo (pendiente debe ser cercana a 1)
      reg_var <- lm(var_obs_bins ~ var_teo_bins)
      pendiente <- coef(reg_var)[2]
      r_squared <- summary(reg_var)$r.squared
      
      list(
        correlacion = correlacion,
        p_valor_correlacion = test_cor$p.value,
        pendiente_regresion = pendiente,
        r_squared_regresion = r_squared,
        bins_utilizados = length(media_bins),
        adecuacion = ifelse(correlacion > 0.8 && abs(pendiente - 1) < 0.3, 
                          "Buena", "Cuestionable")
      )
    } else {
      list(correlacion = NA, p_valor_correlacion = NA, 
          nota = "Insuficientes bins o varianza constante")
    }
  }, error = function(e) {
    list(correlacion = NA, p_valor_correlacion = NA, error = e$message)
  })
  
  # Prueba de bondad de ajuste usando quantiles
  prueba_quantiles <- tryCatch({
    # Calcular quantiles teóricos y empíricos
    probs <- seq(0.1, 0.9, by = 0.1)
    
    quantiles_obs <- quantile(y_obs, probs = probs, na.rm = TRUE)
    
    # Para quantiles teóricos, usar aproximación con gamma para casos extremos
    quantiles_teo <- numeric(length(probs))
    for (i in seq_along(probs)) {
      # Usar quantil empírico de mu para cada probabilidad
      mu_quantil <- quantile(mu_pred, probs = probs[i], na.rm = TRUE)
      
      # Aproximar quantil Tweedie usando transformación
      if (p_tweedie > 1 && p_tweedie < 2) {
        # Usar aproximación basada en gamma
        shape_aprox <- (2 - p_tweedie) / (1 - p_tweedie)
        scale_aprox <- phi * mu_quantil^(p_tweedie - 1) / (2 - p_tweedie)
        quantiles_teo[i] <- qgamma(probs[i], shape = shape_aprox, scale = scale_aprox)
      } else {
        quantiles_teo[i] <- mu_quantil  # Aproximación simple
      }
    }
    
    # Correlación entre quantiles observados y teóricos
    if (!any(is.na(quantiles_obs)) && !any(is.na(quantiles_teo))) {
      cor_quantiles <- cor(quantiles_obs, quantiles_teo)
      
      # Prueba Kolmogorov-Smirnov aproximada
      max_diff <- max(abs(quantiles_obs - quantiles_teo)) / 
                  (max(quantiles_obs) - min(quantiles_obs))
      
      list(
        correlacion_quantiles = cor_quantiles,
        diferencia_maxima_normalizada = max_diff,
        adecuacion_quantiles = ifelse(cor_quantiles > 0.9 && max_diff < 0.2, 
                                    "Buena", "Regular")
      )
    } else {
      list(correlacion_quantiles = NA, diferencia_maxima_normalizada = NA,
          nota = "Quantiles contienen NA")
    }
  }, error = function(e) {
    list(correlacion_quantiles = NA, diferencia_maxima_normalizada = NA, 
        error = e$message)
  })
  
  # Prueba de dispersión
  prueba_dispersion <- tryCatch({
    # Comparar dispersión estimada con dispersión empírica
    residuos_pearson <- residuals(modelo, type = "pearson")
    dispersion_empirica <- sum(residuos_pearson^2) / modelo$df.residual
    
    # Prueba F para sobredispersión
    f_stat <- dispersion_empirica / 1
    p_valor_f <- 1 - pf(f_stat, modelo$df.residual, Inf)
    
    # Clasificación de dispersión
    clasificacion <- if (dispersion_empirica > 1.5) {
      "Sobredispersión"
    } else if (dispersion_empirica < 0.5) {
      "Subdispersión"  
    } else {
      "Dispersión adecuada"
    }
    
    list(
      dispersion_estimada = phi,
      dispersion_empirica = dispersion_empirica,
      ratio_dispersion = dispersion_empirica / phi,
      clasificacion = clasificacion,
      estadistico_f = f_stat,
      p_valor_f = p_valor_f
    )
  }, error = function(e) {
    list(dispersion_estimada = phi, error = e$message)
  })
  
  if (verbose) {
    cat("  Relación varianza-media: r =", round(prueba_varianza_media$correlacion, 3), 
        "(", prueba_varianza_media$adecuacion, ")\n")
    cat("  Dispersión:", round(prueba_dispersion$dispersion_empirica, 3), 
        "(", prueba_dispersion$clasificacion, ")\n")
    cat("  Quantiles: r =", round(prueba_quantiles$correlacion_quantiles, 3), 
        "(", prueba_quantiles$adecuacion_quantiles, ")\n")
  }
  
  return(list(
    varianza_media = prueba_varianza_media,
    quantiles = prueba_quantiles,
    dispersion = prueba_dispersion
  ))
}

#' Pruebas de forma funcional
#' 
#' Evalúa la adecuación de la forma funcional del modelo.
#' 
#' @param modelo Modelo ajustado
#' @param datos_originales Data frame con datos originales
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas de forma funcional
pruebas_forma_funcional <- function(modelo, datos_originales, alpha, verbose) {
  
  if (is.null(datos_originales)) {
    if (verbose) cat("  No se proporcionaron datos originales, omitiendo pruebas de forma funcional\n")
    return(list(nota = "Datos originales no disponibles"))
  }
  
  # Prueba RESET de Ramsey
  reset_resultado <- tryCatch({
    # Agregar potencias de valores ajustados
    y_hat <- fitted(modelo)
    y_hat2 <- y_hat^2
    y_hat3 <- y_hat^3
    
    # Modelo ampliado
    datos_ampliados <- modelo$data
    datos_ampliados$y_hat2 <- y_hat2
    datos_ampliados$y_hat3 <- y_hat3
    
    formula_original <- formula(modelo)
    formula_ampliada <- update(formula_original, . ~ . + y_hat2 + y_hat3)
    
    modelo_ampliado <- glm(formula_ampliada, data = datos_ampliados, 
                          family = modelo$family)
    
    # Prueba de razón de verosimilitudes
    lr_stat <- 2 * (logLik(modelo_ampliado) - logLik(modelo))
    df_diff <- length(coef(modelo_ampliado)) - length(coef(modelo))
    p_valor <- 1 - pchisq(lr_stat, df_diff)
    
    list(
      estadistico_lr = as.numeric(lr_stat),
      grados_libertad = df_diff,
      p_valor = p_valor,
      rechaza_h0 = p_valor < alpha
    )
  }, error = function(e) {
    list(estadistico_lr = NA, p_valor = NA, rechaza_h0 = NA, error = e$message)
  })
  
  # Prueba de linealidad (si hay variables continuas)
  linealidad_resultado <- tryCatch({
    # Identificar variables numéricas
    X <- model.matrix(modelo)
    vars_numericas <- sapply(datos_originales, is.numeric)
    
    if (any(vars_numericas) && ncol(X) > 1) {
      # Tomar primera variable numérica como ejemplo
      var_continua <- names(vars_numericas)[vars_numericas][1]
      
      if (!is.null(var_continua) && var_continua %in% names(datos_originales)) {
        x_var <- datos_originales[[var_continua]]
        
        # Crear splines o términos cuadráticos
        x_cuad <- x_var^2
        
        # Modelo con término cuadrático
        datos_cuad <- datos_originales
        datos_cuad$x_cuad <- x_cuad
        
        formula_cuad <- update(formula(modelo), 
                              as.formula(paste(". ~ . + I(", var_continua, "^2)")))
        
        modelo_cuad <- glm(formula_cuad, data = datos_cuad, family = modelo$family)
        
        # Prueba de razón de verosimilitudes
        lr_lin <- 2 * (logLik(modelo_cuad) - logLik(modelo))
        p_valor_lin <- 1 - pchisq(lr_lin, 1)
        
        list(
          variable_probada = var_continua,
          estadistico_lr = as.numeric(lr_lin),
          p_valor = p_valor_lin,
          rechaza_linealidad = p_valor_lin < alpha
        )
      } else {
        list(nota = "No se encontró variable continua adecuada")
      }
    } else {
      list(nota = "No hay variables continuas o solo hay intercepto")
    }
  }, error = function(e) {
    list(error = e$message)
  })
  
  if (verbose) {
    if (!is.na(reset_resultado$p_valor)) {
      cat("  RESET: p =", round(reset_resultado$p_valor, 4), 
          ifelse(reset_resultado$rechaza_h0, "(Rechaza forma funcional)", "(No rechaza)"), "\n")
    }
    if (!is.na(linealidad_resultado$p_valor)) {
      cat("  Linealidad (", linealidad_resultado$variable_probada, "): p =", 
          round(linealidad_resultado$p_valor, 4), 
          ifelse(linealidad_resultado$rechaza_linealidad, "(Rechaza linealidad)", "(No rechaza)"), "\n")
    }
  }
  
  return(list(
    reset = reset_resultado,
    linealidad = linealidad_resultado
  ))
}

#' Pruebas bootstrap
#' 
#' Realiza pruebas de bondad de ajuste usando remuestreo bootstrap.
#' 
#' @param modelo Modelo ajustado
#' @param datos_originales Data frame con datos
#' @param n_bootstrap Número de replicaciones
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas bootstrap
pruebas_bootstrap_tweedie <- function(modelo, datos_originales, n_bootstrap, 
                                     alpha, verbose) {
  
  if (is.null(datos_originales)) {
    return(list(nota = "Datos originales no disponibles"))
  }
  
  # Estadístico observado: deviance del modelo
  deviance_obs <- modelo$deviance
  
  # Bootstrap paramétrico
  if (verbose) cat("    Ejecutando bootstrap paramétrico...\n")
  
  deviances_bootstrap <- numeric(n_bootstrap)
  convergencias <- logical(n_bootstrap)
  
  # Parámetros del modelo
  mu_pred <- fitted(modelo)
  p_tweedie <- modelo$family$variance()$power
  phi <- summary(modelo)$dispersion
  
  for (b in 1:n_bootstrap) {
    tryCatch({
      # Generar nuevas observaciones
      y_boot <- rtweedie(length(mu_pred), mu = mu_pred, phi = phi, power = p_tweedie)
      
      # Ajustar modelo a datos bootstrap
      datos_boot <- datos_originales
      datos_boot[[all.vars(formula(modelo))[1]]] <- y_boot
      
      modelo_boot <- glm(formula(modelo), data = datos_boot, family = modelo$family)
      
      deviances_bootstrap[b] <- modelo_boot$deviance
      convergencias[b] <- modelo_boot$converged
      
    }, error = function(e) {
      deviances_bootstrap[b] <- NA
      convergencias[b] <- FALSE
    })
    
    if (b %% 100 == 0 && verbose) {
      cat("      Completadas", b, "de", n_bootstrap, "replicaciones\n")
    }
  }
  
  # Remover valores NA
  deviances_validas <- deviances_bootstrap[!is.na(deviances_bootstrap)]
  tasa_convergencia <- mean(convergencias, na.rm = TRUE)
  
  if (length(deviances_validas) > 10) {
    # P-valor bootstrap: proporción de deviances >= deviance observada
    p_valor_bootstrap <- mean(deviances_validas >= deviance_obs)
    
    # Intervalos de confianza para deviance
    ic_deviance <- quantile(deviances_validas, probs = c(alpha/2, 1 - alpha/2))
    
    resultado_bootstrap <- list(
      deviance_observada = deviance_obs,
      deviances_bootstrap = deviances_validas,
      p_valor = p_valor_bootstrap,
      intervalo_confianza = ic_deviance,
      tasa_convergencia = tasa_convergencia,
      rechaza_h0 = p_valor_bootstrap < alpha,
      n_replicaciones_exitosas = length(deviances_validas)
    )
  } else {
    resultado_bootstrap <- list(
      nota = "Muy pocas replicaciones bootstrap exitosas",
      tasa_convergencia = tasa_convergencia
    )
  }
  
  if (verbose) {
    cat("    Tasa de convergencia:", round(tasa_convergencia * 100, 1), "%\n")
    if (!is.null(resultado_bootstrap$p_valor)) {
      cat("    P-valor bootstrap:", round(resultado_bootstrap$p_valor, 4), 
          ifelse(resultado_bootstrap$rechaza_h0, "(Rechaza ajuste)", "(No rechaza)"), "\n")
    }
  }
  
  return(resultado_bootstrap)
}

#' Pruebas basadas en simulación
#' 
#' Realiza pruebas usando simulación Monte Carlo desde el modelo ajustado.
#' 
#' @param modelo Modelo ajustado
#' @param n_simulaciones Número de simulaciones
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resultados de pruebas de simulación
pruebas_simulacion_tweedie <- function(modelo, n_simulaciones, alpha, verbose) {
  
  if (verbose) cat("    Ejecutando pruebas por simulación...\n")
  
  # Valores observados y parámetros
  y_obs <- model.response(model.frame(modelo))
  mu_pred <- fitted(modelo)
  p_tweedie <- modelo$family$variance()$power
  phi <- summary(modelo)$dispersion
  
  # Estadísticos a evaluar
  estadisticos_obs <- list(
    media = mean(y_obs),
    varianza = var(y_obs),
    coef_variacion = sd(y_obs) / mean(y_obs),
    asimetria = moments::skewness(y_obs),
    curtosis = moments::kurtosis(y_obs) - 3,  # Exceso de curtosis
    quantil_90 = quantile(y_obs, 0.9),
    quantil_10 = quantile(y_obs, 0.1)
  )
  
  # Simulaciones
  estadisticos_sim <- matrix(NA, nrow = n_simulaciones, ncol = length(estadisticos_obs))
  colnames(estadisticos_sim) <- names(estadisticos_obs)
  
  for (s in 1:n_simulaciones) {
    tryCatch({
      # Simular nuevos datos
      y_sim <- rtweedie(length(mu_pred), mu = mu_pred, phi = phi, power = p_tweedie)
      
      # Calcular estadísticos
      if (all(is.finite(y_sim)) && length(y_sim) > 0) {
        estadisticos_sim[s, "media"] <- mean(y_sim)
        estadisticos_sim[s, "varianza"] <- var(y_sim)
        estadisticos_sim[s, "coef_variacion"] <- sd(y_sim) / mean(y_sim)
        estadisticos_sim[s, "asimetria"] <- moments::skewness(y_sim)
        estadisticos_sim[s, "curtosis"] <- moments::kurtosis(y_sim) - 3
        estadisticos_sim[s, "quantil_90"] <- quantile(y_sim, 0.9)
        estadisticos_sim[s, "quantil_10"] <- quantile(y_sim, 0.1)
      }
    }, error = function(e) {
      # Los valores NA ya están inicializados
    })
    
    if (s %% 100 == 0 && verbose) {
      cat("      Completadas", s, "de", n_simulaciones, "simulaciones\n")
    }
  }
  
  # Calcular p-valores para cada estadístico
  p_valores <- numeric(length(estadisticos_obs))
  names(p_valores) <- names(estadisticos_obs)
  
  for (stat in names(estadisticos_obs)) {
    valores_sim <- estadisticos_sim[, stat]
    valores_sim <- valores_sim[!is.na(valores_sim)]
    
    if (length(valores_sim) > 10) {
      # P-valor bilateral
      valor_obs <- estadisticos_obs[[stat]]
      p_valores[stat] <- 2 * min(
        mean(valores_sim <= valor_obs),
        mean(valores_sim >= valor_obs)
      )
    } else {
      p_valores[stat] <- NA
    }
  }
  
  # Resumen de resultados
  resultado_simulacion <- list(
    estadisticos_observados = estadisticos_obs,
    estadisticos_simulados = estadisticos_sim,
    p_valores = p_valores,
    rechazos = p_valores < alpha,
    n_simulaciones_exitosas = sum(complete.cases(estadisticos_sim))
  )
  
  if (verbose) {
    cat("    Simulaciones exitosas:", resultado_simulacion$n_simulaciones_exitosas, 
        "de", n_simulaciones, "\n")
    cat("    Estadísticos con p < ", alpha, ":\n")
    rechazados <- names(p_valores)[p_valores < alpha & !is.na(p_valores)]
    if (length(rechazados) > 0) {
      for (stat in rechazados) {
        cat("      ", stat, ": p =", round(p_valores[stat], 4), "\n")
      }
    } else {
      cat("      Ninguno\n")
    }
  }
  
  return(resultado_simulacion)
}

# =============================================================================
# FUNCIONES DE RESUMEN Y CONCLUSIONES
# =============================================================================

#' Generar resumen general de todas las pruebas
#' 
#' Compila un resumen comprehensivo de todos los resultados de las pruebas.
#' 
#' @param resultados Lista con todos los resultados
#' @param alpha Nivel de significancia
#' @param verbose Mostrar información
#' 
#' @return Lista con resumen general
generar_resumen_pruebas <- function(resultados, alpha, verbose) {
  
  # Contadores de pruebas
  total_pruebas <- 0
  pruebas_rechazadas <- 0
  pruebas_exitosas <- 0
  
  # Resumen por categoría
  resumen_categorias <- list()
  
  # 1. Normalidad
  if ("pruebas_normalidad" %in% names(resultados)) {
    rechazos_normalidad <- 0
    total_normalidad <- 0
    
    for (tipo in names(resultados$pruebas_normalidad)) {
      for (prueba in names(resultados$pruebas_normalidad[[tipo]])) {
        resultado <- resultados$pruebas_normalidad[[tipo]][[prueba]]
        if (!is.na(resultado$p_valor)) {
          total_normalidad <- total_normalidad + 1
          if (resultado$rechaza_h0) rechazos_normalidad <- rechazos_normalidad + 1
        }
      }
    }
    
    resumen_categorias$normalidad <- list(
      total = total_normalidad,
      rechazos = rechazos_normalidad,
      proporcion_rechazos = if (total_normalidad > 0) rechazos_normalidad / total_normalidad else 0
    )
  }
  
  # 2. Homocedasticidad
  if ("pruebas_homocedasticidad" %in% names(resultados)) {
    rechazos_homo <- 0
    total_homo <- 0
    
    for (prueba in names(resultados$pruebas_homocedasticidad)) {
      resultado <- resultados$pruebas_homocedasticidad[[prueba]]
      if (!is.na(resultado$p_valor)) {
        total_homo <- total_homo + 1
        if (resultado$rechaza_h0) rechazos_homo <- rechazos_homo + 1
      }
    }
    
    resumen_categorias$homocedasticidad <- list(
      total = total_homo,
      rechazos = rechazos_homo,
      proporcion_rechazos = if (total_homo > 0) rechazos_homo / total_homo else 0
    )
  }
  
  # 3. Independencia
  if ("pruebas_independencia" %in% names(resultados)) {
    rechazos_indep <- 0
    total_indep <- 0
    
    for (prueba in names(resultados$pruebas_independencia)) {
      resultado <- resultados$pruebas_independencia[[prueba]]
      if (!is.na(resultado$p_valor)) {
        total_indep <- total_indep + 1
        if (resultado$rechaza_h0) rechazos_indep <- rechazos_indep + 1
      }
    }
    
    resumen_categorias$independencia <- list(
      total = total_indep,
      rechazos = rechazos_indep,
      proporcion_rechazos = if (total_indep > 0) rechazos_indep / total_indep else 0
    )
  }
  
  # Totales generales
  total_general <- sum(sapply(resumen_categorias, function(x) x$total))
  rechazos_general <- sum(sapply(resumen_categorias, function(x) x$rechazos))
  
  # Clasificación del ajuste
  proporcion_rechazos_total <- if (total_general > 0) rechazos_general / total_general else 0
  
  clasificacion_ajuste <- if (proporcion_rechazos_total <= 0.1) {
    "Excelente"
  } else if (proporcion_rechazos_total <= 0.25) {
    "Bueno"
  } else if (proporcion_rechazos_total <= 0.5) {
    "Regular"
  } else {
    "Deficiente"
  }
  
  # Recomendaciones
  recomendaciones <- generar_recomendaciones(resultados, proporcion_rechazos_total)
  
  resumen <- list(
    resumen_por_categoria = resumen_categorias,
    total_pruebas = total_general,
    total_rechazos = rechazos_general,
    proporcion_rechazos = proporcion_rechazos_total,
    clasificacion_ajuste = clasificacion_ajuste,
    recomendaciones = recomendaciones,
    nivel_significancia = alpha
  )
  
  return(resumen)
}

#' Generar recomendaciones basadas en los resultados
#' 
#' Proporciona recomendaciones específicas basadas en los resultados de las pruebas.
#' 
#' @param resultados Lista con resultados de pruebas
#' @param prop_rechazos Proporción de pruebas rechazadas
#' 
#' @return Vector de recomendaciones
generar_recomendaciones <- function(resultados, prop_rechazos) {
  
  recomendaciones <- character(0)
  
  # Recomendaciones generales
  if (prop_rechazos > 0.5) {
    recomendaciones <- c(recomendaciones, 
      "El modelo presenta problemas serios de ajuste. Considere revisar la especificación.")
  } else if (prop_rechazos > 0.25) {
    recomendaciones <- c(recomendaciones,
      "El ajuste del modelo es cuestionable. Revise los diagnósticos específicos.")
  }
  
  # Recomendaciones específicas por normalidad
  if ("pruebas_normalidad" %in% names(resultados)) {
    rechazos_norm <- sum(sapply(resultados$pruebas_normalidad, function(tipo) {
      sum(sapply(tipo, function(prueba) ifelse(is.na(prueba$rechaza_h0), FALSE, prueba$rechaza_h0)))
    }))
    
    if (rechazos_norm > 5) {
      recomendaciones <- c(recomendaciones,
        "Los residuos muestran desviaciones significativas de la normalidad.")
    }
  }
  
  # Recomendaciones por homocedasticidad
  if ("pruebas_homocedasticidad" %in% names(resultados)) {
    rechazos_homo <- sum(sapply(resultados$pruebas_homocedasticidad, function(prueba) {
      ifelse(is.na(prueba$rechaza_h0), FALSE, prueba$rechaza_h0)
    }))
    
    if (rechazos_homo >= 2) {
      recomendaciones <- c(recomendaciones,
        "Se detectó heterocedasticidad. Considere transformaciones o modelos con varianza no constante.")
    }
  }
  
  # Recomendaciones Tweedie específicas
  if ("pruebas_tweedie" %in% names(resultados)) {
    if (!is.na(resultados$pruebas_tweedie$varianza_media$adecuacion)) {
      if (resultados$pruebas_tweedie$varianza_media$adecuacion == "Cuestionable") {
        recomendaciones <- c(recomendaciones,
          "La relación varianza-media no sigue el patrón Tweedie esperado. Revise el parámetro p.")
      }
    }
    
    if (!is.na(resultados$pruebas_tweedie$dispersion$clasificacion)) {
      if (resultados$pruebas_tweedie$dispersion$clasificacion %in% c("Sobredispersión", "Subdispersión")) {
        recomendaciones <- c(recomendaciones,
          paste("Se detectó", tolower(resultados$pruebas_tweedie$dispersion$clasificacion), 
                ". Considere ajustar el parámetro de dispersión."))
      }
    }
  }
  
  # Si no hay problemas específicos
  if (length(recomendaciones) == 0 && prop_rechazos <= 0.1) {
    recomendaciones <- c(recomendaciones,
      "El modelo presenta un ajuste satisfactorio según las pruebas realizadas.")
  }
  
  return(recomendaciones)
}

#' Imprimir conclusión general
#' 
#' Imprime un resumen ejecutivo de los resultados.
#' 
#' @param resumen_general Lista con resumen de pruebas
#' @param alpha Nivel de significancia
imprimir_conclusion_general <- function(resumen_general, alpha) {
  
  cat("CONCLUSIÓN GENERAL:\n")
  cat("==================\n")
  cat("Clasificación del ajuste:", resumen_general$clasificacion_ajuste, "\n")
  cat("Pruebas realizadas:", resumen_general$total_pruebas, "\n")
  cat("Pruebas rechazadas:", resumen_general$total_rechazos, 
      "(", round(resumen_general$proporcion_rechazos * 100, 1), "%)\n\n")
  
  cat("Resumen por categorías:\n")
  for (categoria in names(resumen_general$resumen_por_categoria)) {
    info <- resumen_general$resumen_por_categoria[[categoria]]
    cat("  ", stringr::str_to_title(categoria), ": ", info$rechazos, "/", info$total, 
        " rechazadas (", round(info$proporcion_rechazos * 100, 1), "%)\n")
  }
  
  cat("\nRecomendaciones:\n")
  for (i in seq_along(resumen_general$recomendaciones)) {
    cat("  ", i, ". ", resumen_general$recomendaciones[i], "\n")
  }
  
  cat("\nInterpretación:\n")
  cat("- Valores p < ", alpha, " indican rechazo del supuesto correspondiente\n")
  cat("- Un buen ajuste debe tener pocas pruebas rechazadas (< 25%)\n")
  cat("- Considere el contexto del problema al interpretar los resultados\n")
}