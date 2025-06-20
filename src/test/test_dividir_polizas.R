source("../data/dividir_polizas_por_anio.R")

test_dividir_polizas <- function() {
  library(dplyr)
  library(lubridate)
  
  cat("=== PRUEBAS UNITARIAS: dividir_polizas_por_anio ===\n\n")
  
  # Caso 1: Póliza dentro del mismo año
  cat("Caso 1: Póliza dentro del mismo año\n")
  df_caso1 <- data.frame(
    ID = 1,
    Desde = as.Date("2023-03-15"),
    Hasta = as.Date("2023-08-20"),
    SumaDePagos = 1000,
    Producto = "Seguro A"
  )
  
  resultado1 <- dividir_polizas_por_anio(df_caso1)
  cat("Input:\n")
  print(df_caso1)
  cat("Output:\n")
  print(resultado1)
  
  stopifnot(nrow(resultado1) == 1)
  stopifnot(resultado1$SumaDePagos == 1000)
  cat("✓ Caso 1 PASÓ\n\n")
  
  # Caso 2: Póliza que cruza dos años
  cat("Caso 2: Póliza que cruza dos años\n")
  df_caso2 <- data.frame(
    ID = 2,
    Desde = as.Date("2023-10-01"),
    Hasta = as.Date("2024-03-31"),
    SumaDePagos = 1200,
    Producto = "Seguro B"
  )
  
  resultado2 <- dividir_polizas_por_anio(df_caso2)
  cat("Input:\n")
  print(df_caso2)
  cat("Output:\n")
  print(resultado2)
  
  stopifnot(nrow(resultado2) == 2)
  
  # Verificar proporciones
  dias_2023 <- as.numeric(as.Date("2023-12-31") - as.Date("2023-10-01") + 1)  # 92 días
  dias_2024 <- as.numeric(as.Date("2024-03-31") - as.Date("2024-01-01") + 1)  # 91 días
  dias_total <- dias_2023 + dias_2024  # 183 días
  
  proporcion_2023 <- dias_2023 / dias_total
  proporcion_2024 <- dias_2024 / dias_total
  
  stopifnot(abs(resultado2$SumaDePagos[1] - 1200 * proporcion_2023) < 0.01)
  stopifnot(abs(resultado2$SumaDePagos[2] - 1200 * proporcion_2024) < 0.01)
  stopifnot(abs(sum(resultado2$SumaDePagos) - 1200) < 0.01)
  cat("✓ Caso 2 PASÓ\n\n")
  
  # Caso 3: Póliza que cruza tres años
  cat("Caso 3: Póliza que cruza tres años\n")
  df_caso3 <- data.frame(
    ID = 3,
    Desde = as.Date("2022-11-15"),
    Hasta = as.Date("2024-02-28"),
    SumaDePagos = 2400,
    Producto = "Seguro C"
  )
  
  resultado3 <- dividir_polizas_por_anio(df_caso3)
  cat("Input:\n")
  print(df_caso3)
  cat("Output:\n")
  print(resultado3)
  
  stopifnot(nrow(resultado3) == 3)
  stopifnot(abs(sum(resultado3$SumaDePagos) - 2400) < 0.01)
  cat("✓ Caso 3 PASÓ\n\n")
  
  # Caso 4: Múltiples pólizas con diferentes casos
  cat("Caso 4: Múltiples pólizas con diferentes casos\n")
  df_caso4 <- data.frame(
    ID = c(4, 5, 6),
    Desde = as.Date(c("2023-06-01", "2023-11-01", "2022-12-01")),
    Hasta = as.Date(c("2023-09-30", "2024-01-31", "2024-01-15")),
    SumaDePagos = c(800, 1500, 2000),
    Producto = c("Seguro D", "Seguro E", "Seguro F")
  )
  
  resultado4 <- dividir_polizas_por_anio(df_caso4)
  cat("Input:\n")
  print(df_caso4)
  cat("Output:\n")
  print(resultado4)
  
  stopifnot(abs(sum(resultado4$SumaDePagos) - 4300) < 0.01)
  cat("✓ Caso 4 PASÓ\n\n")
  
  # Caso 5: Póliza de un solo día
  cat("Caso 5: Póliza de un solo día\n")
  df_caso5 <- data.frame(
    ID = 7,
    Desde = as.Date("2023-12-31"),
    Hasta = as.Date("2023-12-31"),
    SumaDePagos = 500,
    Producto = "Seguro G"
  )
  
  resultado5 <- dividir_polizas_por_anio(df_caso5)
  cat("Input:\n")
  print(df_caso5)
  cat("Output:\n")
  print(resultado5)
  
  stopifnot(nrow(resultado5) == 1)
  stopifnot(resultado5$SumaDePagos == 500)
  cat("✓ Caso 5 PASÓ\n\n")
  
  # Caso 6: Póliza que cruza de 31 dic a 1 ene
  cat("Caso 6: Póliza que cruza de 31 dic a 1 ene\n")
  df_caso6 <- data.frame(
    ID = 8,
    Desde = as.Date("2023-12-31"),
    Hasta = as.Date("2024-01-01"),
    SumaDePagos = 1000,
    Producto = "Seguro H"
  )
  
  resultado6 <- dividir_polizas_por_anio(df_caso6)
  cat("Input:\n")
  print(df_caso6)
  cat("Output:\n")
  print(resultado6)
  
  stopifnot(nrow(resultado6) == 2)
  stopifnot(resultado6$SumaDePagos[1] == 500)  # 1 día en cada año
  stopifnot(resultado6$SumaDePagos[2] == 500)
  cat("✓ Caso 6 PASÓ\n\n")
  
  cat("=== TODAS LAS PRUEBAS PASARON EXITOSAMENTE ===\n")
  
  return(list(
    caso1 = resultado1,
    caso2 = resultado2,
    caso3 = resultado3,
    caso4 = resultado4,
    caso5 = resultado5,
    caso6 = resultado6
  ))
}

if (sys.nframe() == 0) {
  resultados_test <- test_dividir_polizas()
}