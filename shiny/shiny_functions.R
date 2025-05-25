modelo <- readRDS("modelo.rds")

predecir_y <- function(nuevo_x) {
  prediccion <- predict(modelo, newdata = data.frame(x = nuevo_x))
  paste("Y predicho:", round(prediccion, 2))
}