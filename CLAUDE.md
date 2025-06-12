# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Proyecto de Modelos Lineales Generalizados para Seguros

### Descripción
Proyecto académico de Modelos Lineales Generalizados aplicados al sector asegurador, desarrollado como trabajo final para la maestría en Actuaría y Finanzas. Incluye análisis exploratorio de datos de siniestros, modelación actuarial y aplicación interactiva para pricing de seguros.

### Stack Tecnológico
- **Lenguajes**: R (principal), Python (soporte)
- **Entorno**: Conda environment `Renv`
- **Notebooks**: Jupyter con kernels de R y Python
- **Aplicaciones**: Shiny para dashboards interactivos
- **Documentación**: LaTeX para reportes académicos
- **Despliegue**: shinyapps.io

### Estructura del Proyecto
```
proyecto-glms/
├── data/
│   ├── input/          # Datos de siniestros (CSV)
│   ├── output/         # Resultados del análisis
│   └── processed/      # Datos procesados
├── notebooks/          # Análisis exploratorio (Jupyter + R)
├── shiny/              # Aplicación web interactiva
│   ├── app.R           # Aplicación principal
│   ├── ui/             # Componentes de interfaz
│   ├── server/         # Lógica del servidor
│   └── deploy.R        # Script de despliegue
├── models/             # Modelos GLM entrenados (.rds)
├── src/                # Código fuente organizado
├── docs/               # Documentación LaTeX
└── images/             # Recursos gráficos
```

## Configuración del Entorno
```bash
# Activar entorno conda
conda activate Renv
```

## Comandos de Desarrollo

### Aplicación Shiny
```bash
# Ejecutar aplicación localmente
cd shiny/
R -e "shiny::runApp('app.R')"

# Desplegar en shinyapps.io
R -e "source('deploy.R')"
```

## Flujo de Trabajo

### 1. Análisis de Datos
