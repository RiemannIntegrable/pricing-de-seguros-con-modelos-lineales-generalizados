# Carga librerías
library(shiny)
# Carga el modelo entrenado
source("../src/shiny_functions.R")
# Conecta UI y server
source("ui/ui_main.R")
source("server/server_main.R")
# Ejecuta la app
shinyApp(ui = ui, server = server)