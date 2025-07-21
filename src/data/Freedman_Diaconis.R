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
    
    category_labels[i] <- paste0("Edad_", min_age, "_", max_age)
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