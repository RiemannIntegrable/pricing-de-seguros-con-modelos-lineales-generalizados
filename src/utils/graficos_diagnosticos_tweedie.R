# =============================================================================
# GRÁFICOS DE DIAGNÓSTICO PARA MODELOS TWEEDIE
# =============================================================================
# Funciones para crear gráficos de diagnóstico incluyendo gráficos de envelope,
# análisis de residuos, y visualizaciones de bondad de ajuste para modelos
# con distribución Tweedie.
# 
# Autor: Guillermo Murillo
# Fecha: 2025
# =============================================================================

# Librerías requeridas
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("tweedie")) install.packages("tweedie")
if (!require("statmod")) install.packages("statmod")
if (!require("MASS")) install.packages("MASS")
if (!require("car")) install.packages("car")

library(ggplot2)
library(gridExtra)
library(tweedie)
library(statmod)
library(MASS)
library(car)

# =============================================================================
# FUNCIÓN PRINCIPAL DE GRÁFICOS DE ENVELOPE
# =============================================================================

#' Gráficos de envelope para modelos Tweedie
#' 
#' Crea gráficos de envelope (half-normal plots) para evaluar la bondad de
#' ajuste de modelos con distribución Tweedie. Los gráficos de envelope
#' comparan los residuos observados con bandas de confianza generadas
#' mediante simulación.
#' 
#' @param modelo Modelo ajustado con glm() usando familia Tweedie
#' @param tipo_residuo Tipo de residuo: "deviance", "pearson", "standardized"
#' @param n_simulaciones Número de simulaciones para envelope
#' @param nivel_confianza Nivel de confianza para bandas (0-1)
#' @param semilla Semilla para reproducibilidad
#' @param titulo Título personalizado para el gráfico
#' @param mostrar_outliers Resaltar valores atípicos
#' @param umbral_outlier Umbral para detectar outliers (en desviaciones estándar)
#' @param guardar_archivo Ruta donde guardar el gráfico (NULL = no guardar)
#' @param ancho_grafico Ancho del gráfico en pulgadas
#' @param alto_grafico Alto del gráfico en pulgadas
#' @param dpi Resolución del gráfico
#' 
#' @return Lista con:
#'   - grafico: Objeto ggplot del gráfico de envelope
#'   - datos_envelope: Data frame con datos del envelope
#'   - outliers_detectados: Índices de valores atípicos
#'   - estadisticas_envelope: Estadísticas del análisis
grafico_envelope_tweedie <- function(modelo, 
                                    tipo_residuo = "deviance",
                                    n_simulaciones = 100,
                                    nivel_confianza = 0.95,
                                    semilla = 123,
                                    titulo = NULL,
                                    mostrar_outliers = TRUE,
                                    umbral_outlier = 2,
                                    guardar_archivo = NULL,
                                    ancho_grafico = 10,
                                    alto_grafico = 8,
                                    dpi = 300) {
  
  # Validaciones
  if (!inherits(modelo, "glm")) {
    stop("El modelo debe ser un objeto glm")
  }
  
  if (!tipo_residuo %in% c("deviance", "pearson", "standardized")) {
    stop("tipo_residuo debe ser 'deviance', 'pearson' o 'standardized'")
  }
  
  if (nivel_confianza <= 0 || nivel_confianza >= 1) {
    stop("nivel_confianza debe estar entre 0 y 1")
  }
  
  set.seed(semilla)
  
  # Extraer información del modelo
  y_observado <- model.response(model.frame(modelo))
  mu_ajustado <- fitted(modelo)
  n <- length(y_observado)
  
  # Obtener parámetros Tweedie
  familia <- modelo$family
  if (!inherits(familia, "family") || familia$family != "Tweedie") {
    stop("El modelo debe usar familia Tweedie")
  }
  
  p_tweedie <- familia$variance()$power
  phi <- summary(modelo)$dispersion
  
  # Calcular residuos observados
  residuos_obs <- obtener_residuos(modelo, tipo_residuo)
  residuos_abs_ordenados <- sort(abs(residuos_obs))
  
  # Simulaciones para envelope
  cat("Generando", n_simulaciones, "simulaciones para envelope...\n")
  
  residuos_simulados <- matrix(NA, nrow = n, ncol = n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    # Simular nuevas observaciones
    y_sim <- rtweedie(n, mu = mu_ajustado, phi = phi, power = p_tweedie)
    
    # Ajustar modelo con datos simulados
    datos_sim <- modelo$data
    datos_sim[[all.vars(formula(modelo))[1]]] <- y_sim
    
    tryCatch({
      modelo_sim <- glm(formula(modelo), data = datos_sim, family = familia)
      residuos_sim <- obtener_residuos(modelo_sim, tipo_residuo)
      residuos_simulados[, i] <- abs(residuos_sim)
    }, error = function(e) {
      # Si falla la simulación, usar NA
      residuos_simulados[, i] <- rep(NA, n)
    })
    
    if (i %% 20 == 0) cat("Completadas", i, "simulaciones\n")
  }
  
  # Calcular bandas de envelope
  residuos_sim_ordenados <- apply(residuos_simulados, 1, sort, na.rm = TRUE)
  
  alpha <- 1 - nivel_confianza
  banda_inferior <- apply(residuos_sim_ordenados, 1, quantile, 
                         probs = alpha/2, na.rm = TRUE)
  banda_superior <- apply(residuos_sim_ordenados, 1, quantile, 
                         probs = 1 - alpha/2, na.rm = TRUE)
  banda_mediana <- apply(residuos_sim_ordenados, 1, median, na.rm = TRUE)
  
  # Quantiles teóricos half-normal
  quantiles_teoricos <- qnorm((1:n + n - 0.125) / (2 * n + 0.5))
  
  # Crear data frame para gráfico
  datos_grafico <- data.frame(
    quantiles_teoricos = quantiles_teoricos,
    residuos_observados = residuos_abs_ordenados,
    banda_inferior = banda_inferior,
    banda_superior = banda_superior,
    banda_mediana = banda_mediana,
    indice = 1:n
  )
  
  # Detectar outliers
  outliers <- which(residuos_abs_ordenados < banda_inferior | 
                   residuos_abs_ordenados > banda_superior)
  
  if (length(outliers) > 0) {
    datos_grafico$es_outlier <- datos_grafico$indice %in% outliers
  } else {
    datos_grafico$es_outlier <- FALSE
  }
  
  # Crear título si no se proporciona
  if (is.null(titulo)) {
    titulo <- paste0("Gráfico de Envelope - Residuos ", 
                    stringr::str_to_title(tipo_residuo), 
                    "\n(p = ", round(p_tweedie, 3), 
                    ", φ = ", round(phi, 3), ")")
  }
  
  # Crear gráfico base
  p <- ggplot(datos_grafico, aes(x = quantiles_teoricos)) +
    
    # Banda de confianza
    geom_ribbon(aes(ymin = banda_inferior, ymax = banda_superior), 
                alpha = 0.3, fill = "lightblue", color = "blue", size = 0.5) +
    
    # Mediana teórica
    geom_line(aes(y = banda_mediana), color = "blue", linetype = "dashed", size = 0.8) +
    
    # Puntos observados
    geom_point(aes(y = residuos_observados, color = es_outlier), 
               size = 2, alpha = 0.7) +
    
    # Línea de referencia (ajuste perfecto)
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
    
    # Escalas y colores
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), 
                      name = "Outliers", 
                      labels = c("Normal", "Atípico")) +
    
    # Etiquetas y tema
    labs(
      title = titulo,
      subtitle = paste0("Nivel de confianza: ", nivel_confianza*100, 
                       "% | Simulaciones: ", n_simulaciones, 
                       " | Outliers: ", length(outliers)),
      x = "Quantiles Teóricos Half-Normal",
      y = paste("Residuos", stringr::str_to_title(tipo_residuo), "Absolutos"),
      caption = paste("Modelo Tweedie | n =", n)
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = if(mostrar_outliers && length(outliers) > 0) "bottom" else "none",
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Añadir etiquetas a outliers si se solicita
  if (mostrar_outliers && length(outliers) > 0) {
    datos_outliers <- datos_grafico[datos_grafico$es_outlier, ]
    
    p <- p + 
      geom_text(data = datos_outliers, 
                aes(x = quantiles_teoricos, y = residuos_observados, 
                    label = indice), 
                vjust = -0.5, hjust = 0.5, size = 3, color = "red")
  }
  
  # Guardar gráfico si se especifica archivo
  if (!is.null(guardar_archivo)) {
    ggsave(guardar_archivo, plot = p, 
           width = ancho_grafico, height = alto_grafico, dpi = dpi)
    cat("Gráfico guardado en:", guardar_archivo, "\n")
  }
  
  # Estadísticas del envelope
  prop_fuera_banda <- length(outliers) / n
  estadisticas <- list(
    n_observaciones = n,
    n_simulaciones = n_simulaciones,
    nivel_confianza = nivel_confianza,
    outliers_detectados = length(outliers),
    proporcion_outliers = prop_fuera_banda,
    prop_esperada_outliers = 1 - nivel_confianza,
    exceso_outliers = prop_fuera_banda - (1 - nivel_confianza)
  )
  
  cat("\n=== ESTADÍSTICAS ENVELOPE ===\n")
  cat("Outliers detectados:", length(outliers), "de", n, 
      "(", round(prop_fuera_banda*100, 2), "%)\n")
  cat("Proporción esperada:", round((1-nivel_confianza)*100, 2), "%\n")
  cat("Exceso de outliers:", round(estadisticas$exceso_outliers*100, 2), "%\n")
  
  return(list(
    grafico = p,
    datos_envelope = datos_grafico,
    outliers_detectados = outliers,
    estadisticas_envelope = estadisticas
  ))
}

# =============================================================================
# FUNCIONES AUXILIARES PARA GRÁFICOS
# =============================================================================

#' Obtener diferentes tipos de residuos
#' 
#' Función auxiliar para extraer diferentes tipos de residuos del modelo.
#' 
#' @param modelo Modelo glm ajustado
#' @param tipo Tipo de residuo deseado
#' 
#' @return Vector de residuos
obtener_residuos <- function(modelo, tipo) {
  
  switch(tipo,
    "deviance" = residuals(modelo, type = "deviance"),
    "pearson" = residuals(modelo, type = "pearson"),
    "standardized" = rstandard(modelo),
    stop("Tipo de residuo no reconocido")
  )
}

#' Panel de gráficos de diagnóstico
#' 
#' Crea un panel completo con múltiples gráficos de diagnóstico para
#' el modelo Tweedie ajustado.
#' 
#' @param modelo Modelo ajustado
#' @param incluir_envelope Incluir gráfico de envelope
#' @param n_sim_envelope Número de simulaciones para envelope
#' @param guardar_archivo Archivo donde guardar el panel
#' @param ancho_panel Ancho del panel completo
#' @param alto_panel Alto del panel completo
#' 
#' @return Lista con gráficos individuales y panel combinado
panel_diagnosticos_tweedie <- function(modelo, 
                                      incluir_envelope = TRUE,
                                      n_sim_envelope = 50,
                                      guardar_archivo = NULL,
                                      ancho_panel = 15,
                                      alto_panel = 12) {
  
  # Datos para gráficos
  residuos_deviance <- residuals(modelo, type = "deviance")
  residuos_pearson <- residuals(modelo, type = "pearson")
  valores_ajustados <- fitted(modelo)
  y_observado <- model.response(model.frame(modelo))
  leverage <- hatvalues(modelo)
  cook_dist <- cooks.distance(modelo)
  
  # 1. Residuos vs Valores Ajustados
  datos_res_ajust <- data.frame(
    ajustados = valores_ajustados,
    residuos = residuos_deviance
  )
  
  g1 <- ggplot(datos_res_ajust, aes(x = ajustados, y = residuos)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "loess", se = TRUE, color = "red", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    labs(title = "Residuos vs Valores Ajustados",
         x = "Valores Ajustados", 
         y = "Residuos Deviance") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
  
  # 2. Q-Q Plot de residuos
  datos_qq <- data.frame(
    teoricos = qnorm(ppoints(length(residuos_deviance))),
    observados = sort(residuos_deviance)
  )
  
  g2 <- ggplot(datos_qq, aes(x = teoricos, y = observados)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_qq_line(color = "red", size = 1) +
    labs(title = "Q-Q Plot Normal",
         x = "Quantiles Teóricos", 
         y = "Residuos Deviance") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
  
  # 3. Scale-Location Plot
  residuos_abs_sqrt <- sqrt(abs(residuos_deviance))
  
  datos_scale <- data.frame(
    ajustados = valores_ajustados,
    residuos_sqrt = residuos_abs_sqrt
  )
  
  g3 <- ggplot(datos_scale, aes(x = ajustados, y = residuos_sqrt)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_smooth(method = "loess", se = TRUE, color = "red", size = 1) +
    labs(title = "Scale-Location",
         x = "Valores Ajustados", 
         y = "√|Residuos Deviance|") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
  
  # 4. Leverage vs Cook's Distance
  datos_leverage <- data.frame(
    leverage = leverage,
    cook = cook_dist,
    indice = 1:length(leverage)
  )
  
  # Identificar puntos influyentes
  n <- length(leverage)
  p <- length(coef(modelo))
  limite_leverage <- 2*p/n
  limite_cook <- 4/(n-p)
  
  datos_leverage$influyente <- datos_leverage$leverage > limite_leverage | 
                              datos_leverage$cook > limite_cook
  
  g4 <- ggplot(datos_leverage, aes(x = leverage, y = cook, color = influyente)) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = limite_cook, linetype = "dashed", color = "red") +
    geom_vline(xintercept = limite_leverage, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    labs(title = "Leverage vs Distancia de Cook",
         x = "Leverage", 
         y = "Distancia de Cook") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"),
          legend.position = "none")
  
  # 5. Observados vs Predichos
  datos_obs_pred <- data.frame(
    observados = y_observado,
    predichos = valores_ajustados
  )
  
  correlacion <- cor(y_observado, valores_ajustados)
  
  g5 <- ggplot(datos_obs_pred, aes(x = predichos, y = observados)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
    labs(title = paste0("Observados vs Predichos (r = ", round(correlacion, 3), ")"),
         x = "Valores Predichos", 
         y = "Valores Observados") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
  
  # 6. Histograma de residuos
  g6 <- ggplot(data.frame(residuos = residuos_deviance), aes(x = residuos)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = ..density.. * length(residuos_deviance) * diff(range(residuos_deviance))/30), 
                color = "red", size = 1) +
    labs(title = "Distribución de Residuos",
         x = "Residuos Deviance", 
         y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
  
  # Lista de gráficos
  graficos <- list(
    residuos_ajustados = g1,
    qq_plot = g2,
    scale_location = g3,
    leverage_cook = g4,
    observados_predichos = g5,
    histograma_residuos = g6
  )
  
  # Agregar gráfico de envelope si se solicita
  if (incluir_envelope) {
    cat("Generando gráfico de envelope...\n")
    envelope_resultado <- grafico_envelope_tweedie(modelo, 
                                                  n_simulaciones = n_sim_envelope,
                                                  titulo = "Envelope Plot")
    graficos$envelope <- envelope_resultado$grafico
  }
  
  # Crear panel combinado
  if (incluir_envelope) {
    panel <- grid.arrange(
      g1, g2, g3,
      g4, g5, g6,
      graficos$envelope,
      ncol = 3, nrow = 3,
      top = "Panel de Diagnósticos - Modelo Tweedie"
    )
  } else {
    panel <- grid.arrange(
      g1, g2, g3,
      g4, g5, g6,
      ncol = 3, nrow = 2,
      top = "Panel de Diagnósticos - Modelo Tweedie"
    )
  }
  
  # Guardar panel si se especifica
  if (!is.null(guardar_archivo)) {
    ggsave(guardar_archivo, plot = panel, 
           width = ancho_panel, height = alto_panel, dpi = 300)
    cat("Panel de diagnósticos guardado en:", guardar_archivo, "\n")
  }
  
  return(list(
    graficos_individuales = graficos,
    panel_completo = panel
  ))
}

# =============================================================================
# GRÁFICOS ESPECÍFICOS PARA TWEEDIE
# =============================================================================

#' Gráfico de dispersión vs media
#' 
#' Visualiza la relación varianza-media característica de la distribución
#' Tweedie, útil para verificar la idoneidad del parámetro p.
#' 
#' @param modelo Modelo Tweedie ajustado
#' @param n_bins Número de bins para agrupar observaciones
#' @param titulo Título del gráfico
#' 
#' @return Objeto ggplot
grafico_dispersion_media_tweedie <- function(modelo, n_bins = 20, titulo = NULL) {
  
  # Extraer datos
  y <- model.response(model.frame(modelo))
  mu <- fitted(modelo)
  
  # Obtener parámetro p
  p_tweedie <- modelo$family$variance()$power
  
  # Crear bins basados en valores ajustados
  bins <- cut(mu, breaks = n_bins, include.lowest = TRUE)
  
  # Calcular media y varianza por bin
  estadisticas_bin <- aggregate(cbind(y, mu), by = list(bin = bins), 
                               FUN = function(x) c(media = mean(x), varianza = var(x)))
  
  datos_dispersion <- data.frame(
    media = estadisticas_bin$mu[, "media"],
    varianza_obs = estadisticas_bin$y[, "varianza"],
    varianza_teorica = estadisticas_bin$mu[, "media"]^p_tweedie * 
                      summary(modelo)$dispersion
  )
  
  # Crear título si no se proporciona
  if (is.null(titulo)) {
    titulo <- paste0("Relación Varianza-Media\n(p = ", round(p_tweedie, 3), ")")
  }
  
  # Crear gráfico
  ggplot(datos_dispersion, aes(x = media)) +
    geom_point(aes(y = varianza_obs, color = "Observada"), size = 3, alpha = 0.7) +
    geom_line(aes(y = varianza_teorica, color = "Teórica"), size = 1.2) +
    scale_color_manual(values = c("Observada" = "blue", "Teórica" = "red"),
                      name = "Varianza") +
    labs(title = titulo,
         x = "Media por Bin",
         y = "Varianza",
         caption = paste("Distribución Tweedie con p =", round(p_tweedie, 3))) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "bottom")
}

#' Gráfico de perfil de verosimilitud para p
#' 
#' Crea un gráfico del perfil de log-verosimilitud para diferentes valores
#' del parámetro p, útil para evaluar la idoneidad del valor elegido.
#' 
#' @param data Data frame con los datos
#' @param formula Fórmula del modelo
#' @param rango_p Rango de valores p a evaluar
#' @param n_puntos Número de puntos a evaluar
#' @param p_actual Valor actual de p (para marcar en el gráfico)
#' 
#' @return Objeto ggplot con el perfil de verosimilitud
grafico_perfil_verosimilitud_p <- function(data, formula, 
                                          rango_p = c(1.01, 1.99),
                                          n_puntos = 50,
                                          p_actual = NULL) {
  
  # Vector de valores p a evaluar
  valores_p <- seq(rango_p[1], rango_p[2], length.out = n_puntos)
  loglik_valores <- numeric(n_puntos)
  
  cat("Calculando perfil de verosimilitud...\n")
  
  # Calcular log-verosimilitud para cada valor de p
  for (i in 1:n_puntos) {
    tryCatch({
      familia <- tweedie(var.power = valores_p[i], link.power = 0)
      modelo_temp <- glm(formula, data = data, family = familia)
      
      # Log-verosimilitud
      y <- model.response(model.frame(formula, data))
      mu <- fitted(modelo_temp)
      phi <- summary(modelo_temp)$dispersion
      
      loglik_valores[i] <- sum(dtweedie(y, mu = mu, phi = phi, 
                                       power = valores_p[i], log = TRUE))
      
    }, error = function(e) {
      loglik_valores[i] <- NA
    })
    
    if (i %% 10 == 0) cat("Progreso:", round(i/n_puntos*100), "%\n")
  }
  
  # Crear data frame para gráfico
  datos_perfil <- data.frame(
    p = valores_p,
    loglik = loglik_valores
  )
  
  # Eliminar valores NA
  datos_perfil <- datos_perfil[!is.na(datos_perfil$loglik), ]
  
  # Encontrar máximo
  idx_max <- which.max(datos_perfil$loglik)
  p_optimo <- datos_perfil$p[idx_max]
  loglik_max <- datos_perfil$loglik[idx_max]
  
  # Crear gráfico
  p <- ggplot(datos_perfil, aes(x = p, y = loglik)) +
    geom_line(size = 1.2, color = "blue") +
    geom_point(size = 2, alpha = 0.6) +
    geom_vline(xintercept = p_optimo, linetype = "dashed", color = "red", size = 1) +
    labs(title = "Perfil de Log-Verosimilitud para Parámetro p",
         subtitle = paste0("Máximo en p = ", round(p_optimo, 4), 
                          " (LogLik = ", round(loglik_max, 2), ")"),
         x = "Parámetro de Potencia (p)",
         y = "Log-Verosimilitud",
         caption = "Línea roja indica el máximo") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5))
  
  # Marcar valor actual si se proporciona
  if (!is.null(p_actual)) {
    # Interpolar log-verosimilitud para p_actual
    loglik_actual <- approx(datos_perfil$p, datos_perfil$loglik, 
                           xout = p_actual)$y
    
    p <- p + 
      geom_vline(xintercept = p_actual, linetype = "dotted", 
                color = "green", size = 1) +
      annotate("text", x = p_actual, y = max(datos_perfil$loglik) * 0.95,
              label = paste0("p actual = ", round(p_actual, 3)),
              angle = 90, vjust = -0.5, color = "green")
  }
  
  cat("Parámetro p óptimo:", round(p_optimo, 4), "\n")
  cat("Log-verosimilitud máxima:", round(loglik_max, 2), "\n")
  
  return(p)
}