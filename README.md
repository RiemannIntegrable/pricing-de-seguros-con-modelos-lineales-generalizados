<div align="center">

# 🚗 **Modelos Lineales Generalizados para Seguros**

*Pricing actuarial con metodología frecuencia-severidad*

[![R](https://img.shields.io/badge/R-4.2.3-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-brightgreen.svg)](https://shiny.rstudio.com/)
[![Conda](https://img.shields.io/badge/Conda-Environment-green.svg)](https://conda.io/)
[![Jupyter](https://img.shields.io/badge/Jupyter-Notebooks-orange.svg)](https://jupyter.org/)
[![LaTeX](https://img.shields.io/badge/LaTeX-Document-red.svg)](https://www.latex-project.org/)
[![Universidad Nacional](https://img.shields.io/badge/Universidad-Nacional%20de%20Colombia-yellow.svg)](https://unal.edu.co/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](#licencia)

🌐 **[Aplicación Web en Vivo](https://riemannintegrable.shinyapps.io/calculadora-prima-pura-seguros/)**

</div>

---

## 👥 **Autoría**

### 🎓 **Estudiantes**
| Nombre | Email | GitHub |
|--------|-------|---------|
| José Miguel Acuña Hernández | jacunah@unal.edu.co | [@RiemannIntegrable](https://github.com/RiemannIntegrable) |
| Guillermo Murillo Tirado | gmurillot@unal.edu.co | - |

### 👩‍🏫 **Supervisión Académica**
**Docente:** Luz Mery González G.  
**Email:** lgonzalezg@unal.edu.co  
**Materia:** Modelos Lineales Generalizados para Seguros 2025-I  
**Universidad:** Universidad Nacional de Colombia  
**Facultad:** Ciencias - Departamento de Matemáticas

---

## 🎯 **Descripción del Proyecto**

Este proyecto implementa un **sistema completo de tarificación actuarial** para seguros de automóviles utilizando **Modelos Lineales Generalizados (GLM)** con metodología **frecuencia-severidad**. Incluye análisis exploratorio, modelación estadística, y una aplicación web interactiva para cálculo de primas puras.

### 🔬 **Metodología Actuarial**
- **Modelo de Frecuencia**: Quasi-Poisson con offset de exposición
- **Modelo de Severidad**: Log-normal con ponderación por número de siniestros  
- **Prima Pura**: Frecuencia × Severidad × Exposición
- **Variables**: Modelo de vehículo, Color, Carrocería, Servicio, Edad del conductor

### 📊 **Dataset**
- **6,750 observaciones** de siniestros de seguros de automóviles
- **Variables categóricas**: 5 variables con 19 niveles totales
- **768 combinaciones posibles** de primas puras
- **Período**: Datos históricos de siniestros vehiculares

---

## 🛠️ **Estructura del Proyecto**

```
📦 proyecto-glms/
├── 📁 data/                         # 📊 Datos del proyecto
│   ├── 📁 input/                    # Datos originales (CSV)
│   ├── 📁 processed/                # Datos procesados y agrupados
│   └── 📁 output/                   # Resultados del análisis
├── 📁 src/                          # 🔧 Código fuente
│   ├── 📁 data/                     # Funciones de procesamiento
│   └── 📁 utils/                    # Algoritmos y utilidades GLM
├── 📁 models/                       # 🤖 Modelos GLM entrenados (.rds)
├── 📁 notebooks/                    # 📓 Análisis en Jupyter + R
│   ├── limpieza.ipynb              # Limpieza y procesamiento
│   ├── analisis_descriptivo.ipynb  # EDA y visualizaciones
│   ├── frecuencia.ipynb            # Modelado de frecuencia
│   ├── severidad.ipynb             # Modelado de severidad
│   └── tablas_contingencia.ipynb   # Análisis de dependencias
├── 📁 shiny/                        # 🌐 Aplicación web interactiva
│   ├── app.R                       # Aplicación principal
│   ├── deploy.R                    # Script de despliegue
│   └── modelo_pricing_completo.rds # Modelo para producción
├── 📁 docs/                         # 📄 Documentación LaTeX
│   ├── 📁 config/                   # Configuración LaTeX
│   ├── 📁 content/                  # Secciones del documento
│   └── main.tex                    # Documento principal
├── 📁 images/                       # 📊 Gráficos y visualizaciones
├── 📄 CLAUDE.md                     # Instrucciones para Claude Code
├── 📄 environment.yml               # 🐍 Configuración entorno Conda
└── 📄 README.md                     # 📖 Este archivo
```

---

## 🚀 **Instalación y Uso**

### 📋 **Prerequisitos**

- 🐧 **Sistema Operativo**: Linux (Ubuntu, WSL, etc.)
- 🐍 **Conda/Miniconda**: Para gestión de entornos
- 📝 **Editor de Código**: VSCode, Cursor o RStudio
- 📊 **R**: >= 4.2.3 (se instala automáticamente)
- 📄 **TeXLive Full**: Para compilación de documentos LaTeX

### ⚡ **Setup Rápido**

```bash
# 1. Clonar repositorio
git clone https://github.com/RiemannIntegrable/proyecto-glms.git
cd proyecto-glms

# 2. Crear entorno conda (IMPORTANTE: usar conda, no mamba)
conda env create -f environment.yml
conda activate Renv

# 3. Verificar instalación
R --version
jupyter --version

# 4. Iniciar Jupyter Lab
jupyter lab
```

### 📓 **Orden de Ejecución de Notebooks**

**⚠️ IMPORTANTE**: Los notebooks deben ejecutarse en este orden específico:

#### 🧹 **Fase 1: Preparación de Datos**
```
1. 📋 limpieza.ipynb                 # Limpieza y estructuración de datos
2. 📊 analisis_descriptivo.ipynb     # Análisis exploratorio de datos
3. 📈 tablas_contingencia.ipynb      # Análisis de dependencias entre variables
```

#### 🎯 **Fase 2: Modelación GLM**
```
4. 📊 frecuencia.ipynb               # Modelo GLM de frecuencia (Quasi-Poisson)
5. 📈 severidad.ipynb                # Modelo GLM de severidad (Log-normal)
6. 🔄 agrupaciones.ipynb             # Combinación frecuencia-severidad
```

### 🌐 **Aplicación Shiny**

La aplicación web incluye:

- **🧮 Calculadora Individual**: Cálculo de prima pura para perfiles específicos
- **📋 Tablas de Tarifas**: Visualización completa de las 768 primas puras
- **📊 Análisis Estadístico**: Resúmenes y comparaciones por tipo de servicio
- **💾 Exportación Excel**: Descarga de tablas completas para uso comercial

#### **Ejecutar Localmente**
```bash
cd shiny/
R -e "shiny::runApp('app.R')"
```

#### **Desplegar en shinyapps.io**
```bash
cd shiny/
R -e "source('deploy.R')"
```

### 🔧 **Funciones Principales**

```r
# Cargar modelo completo de pricing
modelo_completo <- readRDS("models/modelo_pricing_completo.rds")

# Cargar funciones de tablas de primas
source("src/utils/tablas_primas_puras.R")

# Generar tabla completa de 768 primas puras
tabla_completa <- generar_tabla_completa_primas(
  modelo_completo = modelo_completo,
  exposicion = 1
)

# Crear tablas organizadas por servicio
tablas_por_servicio <- crear_tablas_por_servicio(tabla_completa)

# Exportar a Excel
exportar_tablas_excel(
  tablas_por_servicio = tablas_por_servicio,
  tabla_completa = tabla_completa,
  archivo = "primas_puras_seguros.xlsx"
)

# Calcular prima para perfil específico
perfil <- data.frame(
  Modelo = "2007_2013",
  Color = "BLANCO", 
  Carroceria = "SEDAN",
  SERVICIO = "Particular",
  Edad = "41_63",
  exposicion = 1
)

prima_calculada <- modelo_completo$funcion_prediccion(perfil)
```

### 🎯 **Configuración del Entorno**

El archivo `environment.yml` incluye:

- **R 4.2.3**: Lenguaje principal para modelación actuarial
- **Paquetes GLM**: MASS, car, broom para modelación estadística
- **Paquetes Shiny**: shiny, shinydashboard, DT para aplicación web
- **Análisis de datos**: tidyverse, ggplot2, dplyr, reshape2
- **Jupyter**: Para notebooks interactivos con kernel R
- **Exportación**: openxlsx para generación de tablas Excel

### 📄 **Configuración de LaTeX**

**Instalación de TeXLive Full**:
```bash
# Ubuntu/Debian
sudo apt-get install texlive-full

# Verificar XeLaTeX
xelatex --version
```

**Compilar documentación**:
```bash
cd docs/
xelatex main.tex
xelatex main.tex  # Ejecutar dos veces para referencias cruzadas
```

---

## 📈 **Resultados y Aplicaciones**

### 🎯 **Modelo de Frecuencia**
- **Distribución**: Quasi-Poisson con overdispersion
- **Variables significativas**: Modelo del vehículo, Edad del conductor
- **Métrica**: Tasa de siniestros por año de exposición

### 💰 **Modelo de Severidad** 
- **Distribución**: Log-normal 
- **Variables significativas**: Todas las categóricas (Modelo, Color, Carrocería, Servicio, Edad)
- **Métrica**: Costo promedio por siniestro en COP

### 📊 **Tablas de Primas Puras**
- **768 combinaciones** organizadas por tipo de servicio
- **Rango de variación**: Hasta 5x entre perfiles de menor y mayor riesgo
- **Formato profesional**: Listas para uso comercial y regulatorio

### 🌐 **Aplicación Web**
- **Interfaz intuitiva** para productores y suscriptores
- **Cálculos en tiempo real** con validación actuarial
- **Exportación completa** para análisis externos
- **Deployed**: Disponible 24/7 en shinyapps.io

---

## 🧪 **Pruebas y Validación**

### **Ejecutar Script de Pruebas**
```bash
R -e "source('test_tablas_primas.R')"
```

El script valida:
- ✅ Integridad de los modelos GLM
- ✅ Cálculo correcto de las 768 primas
- ✅ Consistencia de tablas por servicio
- ✅ Funcionalidad de exportación Excel
- ✅ Rangos actuariales coherentes

---

## 📚 **Referencias Técnicas**

- **CAS Monograph**: "Generalized Linear Models for Insurance Rating"
- **Metodología**: Frecuencia-Severidad con GLMs
- **Distribuciones**: Quasi-Poisson, Log-normal
- **Software**: R, Shiny, Jupyter, LaTeX

---

## 📄 **Licencia**

Este proyecto está bajo la Licencia MIT - ver el archivo [LICENSE](LICENSE) para detalles.

---

## 🙏 **Agradecimientos**

- **Universidad Nacional de Colombia** - Facultad de Ciencias
- **Departamento de Matemáticas** - Programa de Actuaría y Finanzas
- **Docente Luz Mery González G.** - Supervisión académica
- **Claude Code** - Asistencia en desarrollo y mejores prácticas actuariales

---

<div align="center">

**🎓 Trabajo Final - Modelos Lineales Generalizados para Seguros**  
*Universidad Nacional de Colombia - 2025*

[![Universidad Nacional](https://img.shields.io/badge/🎓-Universidad%20Nacional%20de%20Colombia-yellow.svg)](https://unal.edu.co/)

</div>