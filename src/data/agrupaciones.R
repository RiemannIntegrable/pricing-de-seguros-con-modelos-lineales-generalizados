Freedman_Diaconis_Modelo <- function(data, n_categories = 3) {
  if (!is.numeric(data)) {
    stop("Los datos deben ser numéricos")
  }
  
  data <- data[!is.na(data)]
  
  if (length(data) == 0) {
    stop("No hay datos válidos")
  }
  
  iqr_val <- IQR(data)
  
  if (iqr_val == 0) {
    iqr_val <- mad(data, constant = 2)
  }
  
  if (iqr_val == 0) {
    warning("Los datos no tienen variabilidad. Usando división por cuantiles.")
    breaks <- quantile(data, probs = seq(0, 1, length.out = n_categories + 1))
  } else {
    bin_width_fd <- 2 * iqr_val / length(data)^(1/3)
    
    min_val <- min(data)
    max_val <- max(data)
    
    optimal_bins <- ceiling((max_val - min_val) / bin_width_fd)
    
    if (optimal_bins < n_categories) {
      optimal_bins <- n_categories
    }
    
    breaks <- seq(min_val, max_val, length.out = n_categories + 1)
    
    if (n_categories > optimal_bins) {
      warning(paste("El número de categorías solicitado (", n_categories, 
                   ") es mayor que el óptimo según Freedman-Diaconis (", 
                   optimal_bins, "). Usando división por cuantiles."))
      breaks <- quantile(data, probs = seq(0, 1, length.out = n_categories + 1))
    }
  }
  
  breaks[1] <- breaks[1] - 0.001
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.001
  
  category_labels <- character(n_categories)
  for (i in 1:n_categories) {
    min_modelo <- ceiling(breaks[i])
    max_modelo <- floor(breaks[i + 1])
    
    if (i == n_categories) {
      max_modelo <- floor(breaks[i + 1] - 0.001)
    }
    
    category_labels[i] <- paste0(min_modelo, "_", max_modelo)
  }
  
  categories <- cut(data, 
                   breaks = breaks, 
                   labels = category_labels,
                   include.lowest = TRUE)
  
  list(
    categories = categories,
    breaks = breaks,
    category_labels = category_labels,
    summary = table(categories)
  )
}

Freedman_Diaconis <- function(data, n_categories = 3) {
  if (!is.numeric(data)) {
    stop("Los datos deben ser numéricos")
  }
  
  data <- data[!is.na(data)]
  
  if (length(data) == 0) {
    stop("No hay datos válidos")
  }
  
  iqr_val <- IQR(data)
  
  if (iqr_val == 0) {
    iqr_val <- mad(data, constant = 2)
  }
  
  if (iqr_val == 0) {
    warning("Los datos no tienen variabilidad. Usando división por cuantiles.")
    breaks <- quantile(data, probs = seq(0, 1, length.out = n_categories + 1))
  } else {
    bin_width_fd <- 2 * iqr_val / length(data)^(1/3)
    
    min_val <- min(data)
    max_val <- max(data)
    
    optimal_bins <- ceiling((max_val - min_val) / bin_width_fd)
    
    if (optimal_bins < n_categories) {
      optimal_bins <- n_categories
    }
    
    breaks <- seq(min_val, max_val, length.out = n_categories + 1)
    
    if (n_categories > optimal_bins) {
      warning(paste("El número de categorías solicitado (", n_categories, 
                   ") es mayor que el óptimo según Freedman-Diaconis (", 
                   optimal_bins, "). Usando división por cuantiles."))
      breaks <- quantile(data, probs = seq(0, 1, length.out = n_categories + 1))
    }
  }
  
  breaks[1] <- breaks[1] - 0.001
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.001
  
  category_labels <- character(n_categories)
  for (i in 1:n_categories) {
    min_age <- ceiling(breaks[i])
    max_age <- floor(breaks[i + 1])
    
    if (i == n_categories) {
      max_age <- floor(breaks[i + 1] - 0.001)
    }
    
    category_labels[i] <- paste0(min_age, "_", max_age)
  }
  
  categories <- cut(data, 
                   breaks = breaks, 
                   labels = category_labels,
                   include.lowest = TRUE)
  
  list(
    categories = categories,
    breaks = breaks,
    category_labels = category_labels,
    summary = table(categories)
  )
}

agrupaciones <- function(df) {
  if (!is.data.frame(df)) {
    stop("El input debe ser un data.frame")
  }
  
  # Primero filtrar Vr_Comercial < 4 millones antes de hacer agrupaciones
  if ("Vr_Comercial" %in% names(df)) {
    filas_inicial <- nrow(df)
    cat("Filas iniciales:", filas_inicial, "\n")
    cat("Rango inicial Vr_Comercial: $", format(min(df$Vr_Comercial, na.rm=TRUE), big.mark=","), 
        " - $", format(max(df$Vr_Comercial, na.rm=TRUE), big.mark=","), "\n")
    
    # Filtrar valores válidos y >= 4M
    df <- df[!is.na(df$Vr_Comercial) & df$Vr_Comercial >= 4000000, ]
    filas_final <- nrow(df)
    
    cat("Filas después del filtro >= 4M:", filas_final, "\n")
    cat("Filas eliminadas:", filas_inicial - filas_final, "\n")
    cat("Rango final Vr_Comercial: $", format(min(df$Vr_Comercial, na.rm=TRUE), big.mark=","), 
        " - $", format(max(df$Vr_Comercial, na.rm=TRUE), big.mark=","), "\n")
  }
  
  df_result <- df
  
  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    
    if (col_name == "Edad") {
      if (is.numeric(col_data)) {
        # Crear variable resultado inicializada
        edad_resultado <- character(length(col_data))
        
        # Asignar "Otros" a edades extremas
        edad_resultado[col_data < 18 | col_data > 90] <- "Otros"
        
        # Filtrar edades en rango [18,90] para aplicar Freedman-Diaconis
        edades_validas <- col_data[col_data >= 18 & col_data <= 90]
        
        if (length(edades_validas) > 0) {
          # Aplicar Freedman-Diaconis solo a edades en rango válido
          edad_categorizada <- Freedman_Diaconis(edades_validas, n_categories = 3)$categories
          
          # Asignar las categorías a las posiciones correspondientes
          edad_resultado[col_data >= 18 & col_data <= 90] <- as.character(edad_categorizada)
        }
        
        df_result[[col_name]] <- as.factor(edad_resultado)
      }
    } else if (col_name == "MODELO" || col_name == "Modelo") {
      if (is.numeric(col_data)) {
        modelo_categorizado <- Freedman_Diaconis_Modelo(col_data, n_categories = 3)$categories
        df_result[[col_name]] <- modelo_categorizado
      } else {
        col_numeric <- as.numeric(as.character(col_data))
        if (!all(is.na(col_numeric))) {
          modelo_categorizado <- Freedman_Diaconis_Modelo(col_numeric, n_categories = 3)$categories
          df_result[[col_name]] <- modelo_categorizado
        }
      }
    } else if (col_name == "Vr_Comercial") {
      if (is.numeric(col_data)) {
        cat("\n=== AGRUPACIÓN DE Vr_Comercial → Gama ===\n")
        cat("Datos para agrupar - N:", length(col_data), "\n")
        cat("Rango: $", format(min(col_data, na.rm=TRUE), big.mark=","), 
            " - $", format(max(col_data, na.rm=TRUE), big.mark=","), "\n")
        
        # Crear categorías de valor comercial usando terciles
        terciles <- quantile(col_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
        
        # Verificar que los terciles son diferentes
        if (length(unique(terciles)) < 4) {
          warning("Los terciles no son únicos. Usando cuantiles alternativos.")
          terciles <- quantile(col_data, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
        }
        
        gama_categorizada <- cut(col_data,
                                breaks = terciles,
                                labels = c("Baja", "Media", "Media_Alta"),
                                include.lowest = TRUE)
        
        # Cambiar el nombre de la columna de Vr_Comercial a Gama
        df_result[["Gama"]] <- gama_categorizada
        df_result[[col_name]] <- NULL  # Eliminar la columna original
        
        cat("Puntos de corte (terciles):\n")
        cat("Baja: $", format(terciles[1], big.mark=","), " - $", format(terciles[2], big.mark=","), "\n")
        cat("Media: $", format(terciles[2], big.mark=","), " - $", format(terciles[3], big.mark=","), "\n") 
        cat("Media_Alta: $", format(terciles[3], big.mark=","), " - $", format(terciles[4], big.mark=","), "\n")
        cat("Distribución por categoría:\n")
        print(table(gama_categorizada, useNA = "ifany"))
      }
    } else if (col_name == "Amparo") {
      # Ignorar la variable Amparo - no aplicar agrupaciones
      next
    } else if (is.factor(col_data) || is.character(col_data)) {
      niveles_freq <- table(col_data)
      n_niveles <- length(niveles_freq)
      
      if (n_niveles > 4) {
        # Calcular exposición por nivel si existen las columnas Hasta y Desde
        if ("Hasta" %in% names(df) && "Desde" %in% names(df)) {
          # Calcular exposición como diferencia entre Hasta y Desde
          exposicion <- as.numeric(as.Date(df$Hasta) - as.Date(df$Desde))
          
          # Crear tabla de exposición por nivel
          exposicion_por_nivel <- aggregate(exposicion, 
                                           by = list(col_data), 
                                           FUN = sum, na.rm = TRUE)
          names(exposicion_por_nivel) <- c("nivel", "exposicion_total")
          
          # Ordenar por exposición y tomar los top 3
          exposicion_ordenada <- exposicion_por_nivel[order(exposicion_por_nivel$exposicion_total, decreasing = TRUE), ]
          top_3 <- exposicion_ordenada$nivel[1:3]
        } else {
          # Si no hay columnas de fecha, usar frecuencia como fallback
          top_3 <- names(sort(niveles_freq, decreasing = TRUE)[1:3])
        }
        
        nueva_variable <- as.character(col_data)
        nueva_variable[!nueva_variable %in% top_3] <- "Otros"
        df_result[[col_name]] <- as.factor(nueva_variable)
      }
    }
  }
  
  return(df_result)
}