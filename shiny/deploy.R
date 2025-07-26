library(rsconnect)

# Configuración de la aplicación
app_name <- "calculadora-prima-pura-seguros"

cat("🚀 Iniciando despliegue de la aplicación Shiny...\n")
cat("📱 Aplicación:", app_name, "\n")

# Verificar que existen los archivos necesarios
if (!file.exists("app.R")) {
  stop("❌ Error: No se encontró app.R")
}

if (!file.exists("modelo_pricing_completo.rds")) {
  stop("❌ Error: No se encontró el modelo modelo_pricing_completo.rds en la carpeta shiny")
}

# Verificar que existe el archivo de funciones
if (!file.exists("../src/utils/tablas_primas_puras.R")) {
  cat("⚠️  Advertencia: No se encontró tablas_primas_puras.R\n")
  cat("📁 Copiando archivo de funciones...\n")
  file.copy("../src/utils/tablas_primas_puras.R", "tablas_primas_puras.R")
}

cat("✅ Archivos verificados\n")

# Verificar paquetes necesarios
required_packages <- c("shiny", "shinydashboard", "DT", "reshape2", "openxlsx")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("📦 Instalando paquetes faltantes:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "http://cran.rstudio.com/")
}

cat("✅ Dependencias verificadas\n")

# Limpiar despliegues previos si existen
tryCatch({
  rsconnect::forgetDeployment()
  cat("🧹 Limpiando despliegues previos\n")
}, error = function(e) {
  cat("ℹ️  No hay despliegues previos que limpiar\n")
})

# Asegurar que tenemos todos los archivos necesarios
if (!file.exists("tablas_primas_puras.R")) {
  if (file.exists("../src/utils/tablas_primas_puras.R")) {
    file.copy("../src/utils/tablas_primas_puras.R", "tablas_primas_puras.R")
    cat("📁 Archivo de funciones copiado\n")
  } else {
    stop("❌ Error: No se encontró el archivo de funciones tablas_primas_puras.R")
  }
}

# Desplegar aplicación
cat("📦 Desplegando aplicación...\n")
rsconnect::deployApp(
  appName = app_name,
  launch.browser = FALSE,
  forceUpdate = TRUE,
  logLevel = "normal",
  appFiles = c("app.R", "modelo_pricing_completo.rds", "tablas_primas_puras.R")
)

cat("✅ Despliegue completado exitosamente!\n")
cat("🌐 URL: https://riemannintegrable.shinyapps.io/", app_name, "/\n", sep="")
cat("📊 Calculadora de Prima Pura para Seguros de Automóviles\n")
cat("🎯 Aplicación lista para uso actuarial\n")