# La estructura visual básica - solo una página
ui <- fluidPage(
  source("ui/ui_pricing.R", local = TRUE)$value
)