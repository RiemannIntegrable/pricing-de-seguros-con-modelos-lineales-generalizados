source("../data/ajustar_inflacion.R")

test_ajustar_inflacion <- function() {
  library(dplyr)
  library(lubridate)
  
  cat("=== PRUEBAS UNITARIAS: ajustar_inflacion_2015 ===\n\n")
  
  # Caso 1: Póliza de 2014 (un año de inflación)
  cat("Caso 1: Póliza de 2014 - ajuste por un año\n")
  df_caso1 <- data.frame(
    ID = 1,
    Desde = as.Date("2014-01-01"),
    Hasta = as.Date("2014-12-31"),
    SumaDePagos = 1000,
    Producto = "Seguro A"
  )
  
  resultado1 <- ajustar_inflacion_2015(df_caso1)
  cat("Input:\n")
  print(df_caso1)
  cat("Output:\n")
  print(resultado1)
  
  # 2014 a 2015: 1000 * (1 + 0.0366) = 1036.6
  esperado1 <- 1000 * 1.0366
  stopifnot(abs(resultado1$SumaDePagos - esperado1) < 0.01)
  cat("✓ Caso 1 PASÓ\n\n")
  
  # Caso 2: Póliza de 2013 (dos años de inflación)
  cat("Caso 2: Póliza de 2013 - ajuste por dos años\n")
  df_caso2 <- data.frame(
    ID = 2,
    Desde = as.Date("2013-06-01"),
    Hasta = as.Date("2013-11-30"),
    SumaDePagos = 2000,
    Producto = "Seguro B"
  )
  
  resultado2 <- ajustar_inflacion_2015(df_caso2)
  cat("Input:\n")
  print(df_caso2)
  cat("Output:\n")
  print(resultado2)
  
  # 2013 a 2015: 2000 * (1 + 0.0194) * (1 + 0.0366) = 2000 * 1.0194 * 1.0366
  esperado2 <- 2000 * 1.0194 * 1.0366
  stopifnot(abs(resultado2$SumaDePagos - esperado2) < 0.01)
  cat("✓ Caso 2 PASÓ\n\n")
  
  # Caso 3: Póliza de 2012 (tres años de inflación)
  cat("Caso 3: Póliza de 2012 - ajuste por tres años\n")
  df_caso3 <- data.frame(
    ID = 3,
    Desde = as.Date("2012-03-15"),
    Hasta = as.Date("2012-08-20"),
    SumaDePagos = 1500,
    Producto = "Seguro C"
  )
  
  resultado3 <- ajustar_inflacion_2015(df_caso3)
  cat("Input:\n")
  print(df_caso3)
  cat("Output:\n")
  print(resultado3)
  
  # 2012 a 2015: 1500 * (1 + 0.0244) * (1 + 0.0194) * (1 + 0.0366)
  esperado3 <- 1500 * 1.0244 * 1.0194 * 1.0366
  stopifnot(abs(resultado3$SumaDePagos - esperado3) < 0.01)
  cat("✓ Caso 3 PASÓ\n\n")
  
  # Caso 4: Póliza de 2011 (cuatro años de inflación)
  cat("Caso 4: Póliza de 2011 - ajuste por cuatro años\n")
  df_caso4 <- data.frame(
    ID = 4,
    Desde = as.Date("2011-01-01"),
    Hasta = as.Date("2011-12-31"),
    SumaDePagos = 3000,
    Producto = "Seguro D"
  )
  
  resultado4 <- ajustar_inflacion_2015(df_caso4)
  cat("Input:\n")
  print(df_caso4)
  cat("Output:\n")
  print(resultado4)
  
  # 2011 a 2015: 3000 * (1 + 0.0373) * (1 + 0.0244) * (1 + 0.0194) * (1 + 0.0366)
  esperado4 <- 3000 * 1.0373 * 1.0244 * 1.0194 * 1.0366
  stopifnot(abs(resultado4$SumaDePagos - esperado4) < 0.01)
  cat("✓ Caso 4 PASÓ\n\n")
  
  # Caso 5: Póliza de 2015 (sin ajuste)
  cat("Caso 5: Póliza de 2015 - sin ajuste\n")
  df_caso5 <- data.frame(
    ID = 5,
    Desde = as.Date("2015-01-01"),
    Hasta = as.Date("2015-12-31"),
    SumaDePagos = 5000,
    Producto = "Seguro E"
  )
  
  resultado5 <- ajustar_inflacion_2015(df_caso5)
  cat("Input:\n")
  print(df_caso5)
  cat("Output:\n")
  print(resultado5)
  
  stopifnot(resultado5$SumaDePagos == 5000)
  cat("✓ Caso 5 PASÓ\n\n")
  
  # Caso 6: Múltiples pólizas de diferentes años
  cat("Caso 6: Múltiples pólizas de diferentes años\n")
  df_caso6 <- data.frame(
    ID = c(6, 7, 8, 9),
    Desde = as.Date(c("2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01")),
    Hasta = as.Date(c("2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31")),
    SumaDePagos = c(1000, 1000, 1000, 1000),
    Producto = c("Seguro F", "Seguro G", "Seguro H", "Seguro I")
  )
  
  resultado6 <- ajustar_inflacion_2015(df_caso6)
  cat("Input:\n")
  print(df_caso6)
  cat("Output:\n")
  print(resultado6)
  
  # Verificar que todos los montos son diferentes y mayores al original
  stopifnot(all(resultado6$SumaDePagos > 1000))
  stopifnot(all(diff(resultado6$SumaDePagos) < 0))  # Decreciente (más ajuste para años anteriores)
  cat("✓ Caso 6 PASÓ\n\n")
  
  # Caso 7: Con ambas columnas monetarias (SumaDePagos y Vr_Comercial)
  cat("Caso 7: Ajuste de SumaDePagos y Vr_Comercial simultáneamente\n")
  df_caso7 <- data.frame(
    ID = c(10, 11),
    Desde = as.Date(c("2013-01-01", "2014-01-01")),
    Hasta = as.Date(c("2013-12-31", "2014-12-31")),
    SumaDePagos = c(2000, 3000),
    Vr_Comercial = c(50000, 75000),
    Producto = c("Seguro J", "Seguro K")
  )
  
  resultado7 <- ajustar_inflacion_2015(df_caso7)
  cat("Input:\n")
  print(df_caso7)
  cat("Output:\n")
  print(resultado7)
  
  # Verificar que ambas columnas fueron ajustadas
  esperado_suma_2013 <- 2000 * 1.0194 * 1.0366
  esperado_suma_2014 <- 3000 * 1.0366
  esperado_vr_2013 <- 50000 * 1.0194 * 1.0366
  esperado_vr_2014 <- 75000 * 1.0366
  
  stopifnot(abs(resultado7$SumaDePagos[1] - esperado_suma_2013) < 0.01)
  stopifnot(abs(resultado7$SumaDePagos[2] - esperado_suma_2014) < 0.01)
  stopifnot(abs(resultado7$Vr_Comercial[1] - esperado_vr_2013) < 0.01)
  stopifnot(abs(resultado7$Vr_Comercial[2] - esperado_vr_2014) < 0.01)
  cat("✓ Caso 7 PASÓ\n\n")
  
  # Caso 8: Solo con Vr_Comercial (sin SumaDePagos)
  cat("Caso 8: Solo con Vr_Comercial\n")
  df_caso8 <- data.frame(
    ID = 12,
    Desde = as.Date("2012-06-01"),
    Hasta = as.Date("2012-11-30"),
    Vr_Comercial = 100000,
    Producto = "Seguro L"
  )
  
  resultado8 <- ajustar_inflacion_2015(df_caso8)
  cat("Input:\n")
  print(df_caso8)
  cat("Output:\n")
  print(resultado8)
  
  esperado_vr_2012 <- 100000 * 1.0244 * 1.0194 * 1.0366
  stopifnot(abs(resultado8$Vr_Comercial - esperado_vr_2012) < 0.01)
  cat("✓ Caso 8 PASÓ\n\n")
  
  cat("=== TODAS LAS PRUEBAS PASARON EXITOSAMENTE ===\n")
  cat("Factores de inflación aplicados:\n")
  cat("2011 → 2015:", round(1.0373 * 1.0244 * 1.0194 * 1.0366, 4), "\n")
  cat("2012 → 2015:", round(1.0244 * 1.0194 * 1.0366, 4), "\n")
  cat("2013 → 2015:", round(1.0194 * 1.0366, 4), "\n")
  cat("2014 → 2015:", round(1.0366, 4), "\n")
  cat("2015 → 2015:", 1.0000, "\n")
  
  return(list(
    caso1 = resultado1,
    caso2 = resultado2,
    caso3 = resultado3,
    caso4 = resultado4,
    caso5 = resultado5,
    caso6 = resultado6,
    caso7 = resultado7,
    caso8 = resultado8
  ))
}

if (sys.nframe() == 0) {
  resultados_test <- test_ajustar_inflacion()
}