ajustar_inflacion_2015 <- function(df_input) {
  library(dplyr)
  library(lubridate)
  
  columnas_requeridas <- c("Desde", "Hasta")
  columnas_monetarias <- c("SumaDePagos", "Vr_Comercial")
  
  if (!all(columnas_requeridas %in% names(df_input))) {
    stop("El dataframe debe contener las columnas 'Desde' y 'Hasta'")
  }
  
  columnas_presentes <- intersect(columnas_monetarias, names(df_input))
  if (length(columnas_presentes) == 0) {
    stop("El dataframe debe contener al menos una de las columnas: 'SumaDePagos' o 'Vr_Comercial'")
  }
  
  df_input$Desde <- as.Date(df_input$Desde)
  df_input$Hasta <- as.Date(df_input$Hasta)
  
  inflaciones <- data.frame(
    anio = c(2011, 2012, 2013, 2014),
    tasa = c(3.73, 2.44, 1.94, 3.66) / 100
  )
  
  calcular_factor_inflacion <- function(anio_poliza) {
    if (anio_poliza >= 2015) {
      return(1)
    }
    
    if (anio_poliza < 2011) {
      stop(paste("Año", anio_poliza, "no tiene datos de inflación disponibles"))
    }
    
    anios_ajuste <- anio_poliza:2014
    tasas_aplicar <- inflaciones$tasa[inflaciones$anio %in% anios_ajuste]
    
    factor_acumulado <- 1
    for (tasa in tasas_aplicar) {
      factor_acumulado <- factor_acumulado * (1 + tasa)
    }
    
    return(factor_acumulado)
  }
  
  df_resultado <- df_input %>%
    mutate(
      anio_poliza = year(Desde),
      factor_inflacion = sapply(anio_poliza, calcular_factor_inflacion)
    )
  
  for (columna in columnas_presentes) {
    df_resultado[[columna]] <- df_resultado[[columna]] * df_resultado$factor_inflacion
  }
  
  df_resultado <- df_resultado %>%
    select(-anio_poliza, -factor_inflacion)
  
  return(df_resultado)
}