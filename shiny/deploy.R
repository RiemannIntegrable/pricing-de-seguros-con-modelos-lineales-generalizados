library(rsconnect)
app_name <- "glm-seguros-pricing"
tryCatch(rsconnect::forgetDeployment(), error = function(e) {})
rsconnect::deployApp(appName = app_name, launch.browser = FALSE, forceUpdate = TRUE)
cat("✅ URL: https://riemannintegrable.shinyapps.io/", app_name, "/\n", sep="")