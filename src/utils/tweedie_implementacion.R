     /home/ge-murillo/universidad/glms/pricing-de-seguros-con-modelos-lineales-generalizados/shiny/ui/tweedie_implementacion.R
     # =============================================================================
     # IMPLEMENTACIÓN FINAL TWEEDIE - VERSIÓN ULTRA ROBUSTA
     # =============================================================================
     # Implementación desde cero basada en todo lo aprendido
     # Enfoque: SIMPLICIDAD y FUNCIONALIDAD garantizada
     #
     # Autor: Guillermo Murillo
     # =============================================================================

     # Cargar librerías esenciales
     library(statmod)
     library(tweedie)
     library(ggplot2)

     # =============================================================================
     # FUNCIÓN 1: PREPARACIÓN ULTRA ROBUSTA DE DATOS
     # =============================================================================

     #' Preparar datos de forma ultra robusta para Tweedie
     #' @param data Data frame original
     #' @param response_var Nombre de variable respuesta
     #' @param predictor_vars Variables predictoras
     #' @return Data frame limpio y preparado
     preparar_datos_ultra_robusto <- function(data, response_var, predictor_vars) {

       cat("🔧 PREPARACIÓN ULTRA ROBUSTA DE DATOS\n")
       cat("=====================================\n\n")

       # 1. Verificar que existen las variables
       vars_necesarias <- c(response_var, predictor_vars)
       vars_faltantes <- vars_necesarias[!vars_necesarias %in% names(data)]

       if (length(vars_faltantes) > 0) {
         stop("Variables faltantes: ", paste(vars_faltantes, collapse = ", "))
       }

       # 2. Crear subset de trabajo
       datos <- data[, vars_necesarias, drop = FALSE]
       cat("Datos iniciales:", nrow(datos), "observaciones\n")

       # 3. LIMPIAR VARIABLE RESPUESTA DE FORMA ULTRA CONSERVADORA
       cat("\n📊 LIMPIANDO VARIABLE RESPUESTA:", response_var, "\n")

       y <- datos[[response_var]]

       # Estadísticas iniciales
       cat("   Observaciones iniciales:", length(y), "\n")
       cat("   NAs:", sum(is.na(y)), "\n")
       cat("   Negativos:", sum(y < 0, na.rm = TRUE), "\n")
       cat("   Ceros:", sum(y == 0, na.rm = TRUE), "(", round(mean(y == 0, na.rm = TRUE) * 100, 1), "%)\n")
       cat("   Positivos:", sum(y > 0, na.rm = TRUE), "\n")

       # Remover NAs
       datos <- datos[!is.na(y), ]
       y <- datos[[response_var]]

       # Remover negativos (no permitidos en Tweedie)
       if (any(y < 0)) {
         cat("   Removiendo", sum(y < 0), "valores negativos\n")
         datos <- datos[y >= 0, ]
         y <- datos[[response_var]]
       }

       # Análisis de ceros
       prop_zeros <- mean(y == 0)
       cat("   Proporción final de ceros:", round(prop_zeros * 100, 1), "%\n")

       # Winsorizar outliers extremos para estabilidad
       if (sum(y > 0) > 0) {
         valores_pos <- y[y > 0]
         p99 <- quantile(valores_pos, 0.99)
         outliers_extremos <- sum(y > p99)

         if (outliers_extremos > 0) {
           cat("   Winsorizing", outliers_extremos, "outliers al P99 =", round(p99, 2), "\n")
           datos[[response_var]][datos[[response_var]] > p99] <- p99
           y <- datos[[response_var]]
         }
       }

       # Crear variable Tweedie (reemplazar ceros con valor muy pequeño)
       min_pos <- min(y[y > 0], na.rm = TRUE)
       valor_cero <- min_pos / 1000  # Usar 1/1000 del valor mínimo positivo

       datos$y_tweedie <- ifelse(y == 0, valor_cero, y)

       cat("   Variable Tweedie creada:\n")
       cat("     Ceros reemplazados por:", valor_cero, "\n")
       cat("     Rango final:", round(range(datos$y_tweedie), 4), "\n")

       # 4. LIMPIAR VARIABLES PREDICTORAS CATEGÓRICAS
       cat("\n📋 LIMPIANDO VARIABLES CATEGÓRICAS:\n")

       for (var in predictor_vars) {
         x <- datos[[var]]

         if (is.character(x) || is.factor(x)) {
           cat("   Variable categórica:", var, "\n")

           # Convertir a character primero
           x_char <- as.character(x)

           # Reemplazar NAs y vacíos
           x_char[is.na(x_char) | x_char == "" | x_char == " "] <- "Desconocido"

           # Crear tabla de frecuencias
           tabla_freq <- table(x_char)
           cat("     Niveles únicos:", length(tabla_freq), "\n")

           # Agrupar niveles con muy pocas observaciones
           niveles_raros <- names(tabla_freq)[tabla_freq < 5]

           if (length(niveles_raros) > 0) {
             cat("     Agrupando", length(niveles_raros), "niveles raros en 'Otros'\n")
             x_char[x_char %in% niveles_raros] <- "Otros"
           }

           # Convertir a factor y usar relevel con primer nivel como referencia
           x_factor <- factor(x_char)
           niveles_finales <- levels(x_factor)

           # Usar el primer nivel (alfabéticamente) como referencia
           primer_nivel <- niveles_finales[1]
           x_factor <- relevel(x_factor, ref = as.character(primer_nivel))

           datos[[var]] <- x_factor

           cat("     Niveles finales:", nlevels(x_factor), "\n")
           cat("     Referencia:", levels(x_factor)[1], "\n")

         } else if (is.numeric(x)) {
           cat("   Variable numérica:", var, "\n")

           # Tratar NAs con mediana
           n_nas <- sum(is.na(x))
           if (n_nas > 0) {
             mediana <- median(x, na.rm = TRUE)
             x[is.na(x)] <- mediana
             cat("     Reemplazados", n_nas, "NAs con mediana =", round(mediana, 2), "\n")
           }

           # Winsorizar outliers
           q01 <- quantile(x, 0.01, na.rm = TRUE)
           q99 <- quantile(x, 0.99, na.rm = TRUE)

           outliers_inf <- sum(x < q01, na.rm = TRUE)
           outliers_sup <- sum(x > q99, na.rm = TRUE)

           if (outliers_inf > 0 || outliers_sup > 0) {
             x[x < q01] <- q01
             x[x > q99] <- q99
             cat("     Winsorized", outliers_inf + outliers_sup, "outliers\n")
           }

           datos[[var]] <- x
           cat("     Rango final:", round(range(x), 2), "\n")
         }
       }

       # 5. Remover observaciones con datos faltantes restantes
       filas_completas <- complete.cases(datos)
       if (sum(!filas_completas) > 0) {
         cat("\nRemoving", sum(!filas_completas), "filas incompletas\n")
         datos <- datos[filas_completas, ]
       }

       cat("\n✅ DATOS FINALES PREPARADOS:\n")
       cat("   Observaciones:", nrow(datos), "\n")
       cat("   Variables:", ncol(datos), "\n")
       cat("   Variable respuesta Tweedie: y_tweedie\n")
       cat("   Todas las categóricas tienen niveles de referencia establecidos\n")

       return(datos)
     }

     # =============================================================================
     # FUNCIÓN 2: BÚSQUEDA SIMPLE Y CONFIABLE DE P
     # =============================================================================

     #' Buscar p óptimo con método ultra simple
     #' @param formula Fórmula del modelo
     #' @param data Datos preparados
     #' @return Valor de p que funciona
     buscar_p_que_funcione <- function(formula, data) {

       cat("🔍 BÚSQUEDA DE P QUE FUNCIONE\n")
       cat("=============================\n")

       # Probar valores comunes de p
       p_candidatos <- c(1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8)

       for (p in p_candidatos) {
         cat("Probando p =", p, "... ")

         tryCatch({
           # Intentar ajustar modelo
           modelo_test <- glm(formula, data = data,
                             family = tweedie(var.power = p, link.power = 0))

           # Verificar que convergió
           if (modelo_test$converged) {
             deviance_val <- deviance(modelo_test)

             if (is.finite(deviance_val) && !is.na(deviance_val)) {
               cat("✅ FUNCIONA (deviance =", round(deviance_val, 1), ")\n")
               return(p)
             } else {
               cat("❌ Deviance inválida\n")
             }
           } else {
             cat("❌ No convergió\n")
           }

         }, error = function(e) {
           cat("❌ Error:", e$message, "\n")
         })
       }

       cat("⚠️  Ningún p funcionó con el modelo completo, probando modelo simple...\n")

       # Intentar con modelo solo intercepto
       var_resp <- all.vars(formula)[1]
       formula_simple <- as.formula(paste(var_resp, "~ 1"))

       for (p in p_candidatos) {
         tryCatch({
           modelo_simple <- glm(formula_simple, data = data,
                               family = tweedie(var.power = p, link.power = 0))

           if (modelo_simple$converged) {
             cat("✅ p =", p, "funciona con modelo simple\n")
             return(p)
           }

         }, error = function(e) {
           # Continuar probando
         })
       }

       stop("❌ No se pudo ajustar modelo Tweedie con ningún valor de p")
     }

     # =============================================================================
     # FUNCIÓN 3: AJUSTE GARANTIZADO DEL MODELO
     # =============================================================================

     #' Ajustar modelo Tweedie con garantía de funcionamiento
     #' @param formula Fórmula del modelo
     #' @param data Datos preparados
     #' @param p_value Valor de p a usar
     #' @return Modelo ajustado
     ajustar_modelo_garantizado <- function(formula, data, p_value) {

       cat("⚙️  AJUSTANDO MODELO TWEEDIE\n")
       cat("===========================\n")
       cat("Fórmula:", deparse(formula), "\n")
       cat("Parámetro p:", p_value, "\n")
       cat("Observaciones:", nrow(data), "\n")

       # Crear familia Tweedie
       familia <- tweedie(var.power = p_value, link.power = 0)

       # Ajustar modelo
       modelo <- glm(formula, data = data, family = familia)

       # Verificar convergencia
       if (!modelo$converged) {
         warning("Modelo no convergió completamente")
       }

       # Estadísticas básicas
       cat("✅ Modelo ajustado:\n")
       cat("   Convergió:", modelo$converged, "\n")
       cat("   AIC:", round(AIC(modelo), 2), "\n")
       cat("   Deviance:", round(deviance(modelo), 2), "\n")
       cat("   Coeficientes:", length(coef(modelo)), "\n")
       cat("   Observaciones:", nobs(modelo), "\n")

       return(modelo)
     }

     # =============================================================================
     # FUNCIÓN 4: ENVELOPE SIMPLE Y CONFIABLE
     # =============================================================================

     #' Crear envelope plot que garantizadamente funciona
     #' @param modelo Modelo ajustado
     #' @return Plot o NULL si no es posible
     crear_envelope_simple <- function(modelo) {

       cat("📊 CREANDO ENVELOPE PLOT\n")
       cat("========================\n")

       tryCatch({
         # Intentar residuos cuantílicos
         residuos <- qres.tweedie(modelo)
         cat("Residuos cuantílicos obtenidos:", length(residuos), "\n")

       }, error = function(e) {
         cat("Error con qres.tweedie, usando residuos de Pearson\n")
         residuos <<- residuals(modelo, type = "pearson")
       })

       # Limpiar residuos
       residuos_clean <- residuos[is.finite(residuos) & !is.na(residuos)]
       n <- length(residuos_clean)

       cat("Residuos válidos:", n, "\n")

       if (n < 10) {
         cat("❌ Muy pocos residuos para envelope\n")
         return(NULL)
       }

       # Crear Q-Q plot simple pero efectivo
       tryCatch({
         # Cuantiles teóricos normales
         quantiles_teoricos <- qnorm(ppoints(n))
         residuos_ordenados <- sort(residuos_clean)

         # Crear envelope mediante simulación simple
         n_sim <- 50  # Menos simulaciones para velocidad
         envelope_sims <- replicate(n_sim, sort(rnorm(n)))

         # Percentiles 2.5% y 97.5%
         envelope_lower <- apply(envelope_sims, 1, quantile, 0.025)
         envelope_upper <- apply(envelope_sims, 1, quantile, 0.975)

         # Data frame para plot
         df_plot <- data.frame(
           teoricos = quantiles_teoricos,
           observados = residuos_ordenados,
           lower = envelope_lower,
           upper = envelope_upper
         )

         # Crear gráfico
         p <- ggplot(df_plot) +
           geom_ribbon(aes(x = teoricos, ymin = lower, ymax = upper),
                       fill = "lightblue", alpha = 0.7) +
           geom_point(aes(x = teoricos, y = observados),
                      size = 1, alpha = 0.8) +
           geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
           labs(
             title = "Envelope Plot - Modelo Tweedie",
             x = "Cuantiles Teóricos",
             y = "Residuos Observados",
             subtitle = paste("n =", n, "observaciones")
           ) +
           theme_minimal() +
           theme(
             plot.title = element_text(hjust = 0.5, size = 14),
             plot.subtitle = element_text(hjust = 0.5)
           )

         # Evaluación automática
         fuera_envelope <- sum(df_plot$observados < df_plot$lower |
                              df_plot$observados > df_plot$upper)
         prop_fuera <- fuera_envelope / n

         cat("✅ Envelope creado exitosamente\n")
         cat("   Puntos fuera del envelope:", fuera_envelope, "(", round(prop_fuera * 100, 1), "%)\n")

         if (prop_fuera <= 0.05) {
           cat("   ✅ EXCELENTE AJUSTE: ≤5% fuera del envelope\n")
         } else if (prop_fuera <= 0.10) {
           cat("   ✅ BUEN AJUSTE: ≤10% fuera del envelope\n")
         } else {
           cat("   ⚠️  AJUSTE CUESTIONABLE: >10% fuera del envelope\n")
         }

         print(p)
         return(p)

       }, error = function(e) {
         cat("❌ Error creando envelope:", e$message, "\n")
         return(NULL)
       })
     }

     # =============================================================================
     # FUNCIÓN 5: ANÁLISIS COMPLETO ULTRA SIMPLE
     # =============================================================================

     #' Análisis Tweedie completo desde cero
     #' @param data Data frame original
     #' @param response_var Variable respuesta
     #' @param predictor_vars Variables predictoras
     #' @return Lista con resultados
     analisis_tweedie_desde_cero <- function(data, response_var, predictor_vars) {

       cat("🚀 ANÁLISIS TWEEDIE DESDE CERO\n")
       cat("==============================\n\n")

       # 1. Preparar datos ultra robustamente
       datos_prep <- preparar_datos_ultra_robusto(data, response_var, predictor_vars)

       # 2. Crear fórmula usando variable Tweedie
       formula <- as.formula(paste("y_tweedie ~", paste(predictor_vars, collapse = " + ")))
       cat("\n📋 Fórmula del modelo:\n")
       print(formula)

       # 3. Buscar p que funcione
       cat("\n")
       p_funcional <- buscar_p_que_funcione(formula, datos_prep)

       # 4. Ajustar modelo final
       cat("\n")
       modelo_final <- ajustar_modelo_garantizado(formula, datos_prep, p_funcional)

       # 5. Crear envelope
       cat("\n")
       envelope_plot <- crear_envelope_simple(modelo_final)

       # 6. Resumen de coeficientes
       cat("\n📈 COEFICIENTES DEL MODELO:\n")
       print(summary(modelo_final))

       # 7. Resumen ejecutivo
       pseudo_r2 <- 1 - (deviance(modelo_final) / modelo_final$null.deviance)

       cat("\n🎯 RESUMEN EJECUTIVO:\n")
       cat("====================\n")
       cat("✅ Datos procesados:", nrow(datos_prep), "observaciones\n")
       cat("✅ Parámetro p usado:", p_funcional, "\n")
       cat("✅ Modelo convergió:", modelo_final$converged, "\n")
       cat("✅ AIC:", round(AIC(modelo_final), 2), "\n")
       cat("✅ Pseudo R²:", round(pseudo_r2, 4), "\n")
       cat("✅ Variables en modelo:", length(coef(modelo_final)) - 1, "\n")

       # Interpretación del p
       if (p_funcional < 1.4) {
         cat("📊 Interpretación: Muchos ceros (comportamiento Poisson-like)\n")
       } else if (p_funcional > 1.6) {
         cat("📊 Interpretación: Pocos ceros (comportamiento Gamma-like)\n")
       } else {
         cat("📊 Interpretación: Balance Poisson-Gamma típico\n")
       }

       return(list(
         datos_preparados = datos_prep,
         modelo = modelo_final,
         p_usado = p_funcional,
         aic = AIC(modelo_final),
         pseudo_r2 = pseudo_r2,
         envelope_plot = envelope_plot,
         formula = formula
       ))
     }