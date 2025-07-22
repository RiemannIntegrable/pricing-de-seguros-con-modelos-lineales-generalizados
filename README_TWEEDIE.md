# Implementación de Modelos Tweedie para Seguros de Automóviles

Este repositorio contiene una implementación completa y documentada de modelos de regresión con distribución Tweedie, específicamente diseñada para el análisis de datos de seguros de automóviles y pricing de pólizas.

## Descripción General

Los modelos Tweedie son una familia de distribuciones exponenciales que resultan especialmente útiles en el contexto actuarial y de seguros, ya que pueden manejar datos con una gran proporción de ceros (no siniestros) y valores positivos continuos (montos de siniestros) en una sola distribución.

### Características Principales

- **Distribución Tweedie**: Parametrizada por el parámetro de potencia `p` ∈ (1,2)
- **Función de enlace**: Logarítmica (link.power = 0)
- **Aplicación**: Modelado de severidad y frecuencia conjunta de siniestros
- **Variables offset**: Manejo de exposición temporal en pólizas de seguro

## Estructura del Proyecto

```
src/utils/
├── seleccion_variables_tweedie.R      # Selección automática de variables con AIC
├── ajuste_modelo_tweedie.R            # Ajuste y análisis del modelo principal
├── graficos_diagnosticos_tweedie.R    # Gráficos de envelope y diagnósticos
├── pruebas_ajuste_tweedie.R           # Suite completa de pruebas de bondad
└── ejemplo_implementacion_tweedie.R   # Script de ejemplo completo
```

## Módulos Principales

### 1. Selección de Variables (`seleccion_variables_tweedie.R`)

Implementa algoritmos de selección automática de variables basados en el criterio de información de Akaike (AIC):

#### Funciones Principales:
- `seleccion_variables_aic()`: Selección principal con métodos forward, backward y both
- `seleccion_forward()`: Algoritmo de selección hacia adelante
- `seleccion_backward()`: Algoritmo de selección hacia atrás
- `validacion_cruzada_seleccion()`: Validación cruzada k-fold para estabilidad

#### Parámetros Clave:
- `method`: Tipo de selección ("forward", "backward", "both")
- `p_tweedie`: Parámetro de potencia de la distribución
- `max_vars`: Número máximo de variables a incluir
- `phi`: Parámetro de dispersión (estimado automáticamente si es NULL)

#### Ejemplo de Uso:
```r
resultado_seleccion <- seleccion_variables_aic(
  data = datos_seguros,
  response_var = "SumaDePagos",
  predictor_vars = c("Edad", "Sexo", "TipoVehiculo", "Marca"),
  method = "both",
  p_tweedie = 1.6,
  verbose = TRUE
)
```

### 2. Ajuste del Modelo (`ajuste_modelo_tweedie.R`)

Proporciona funcionalidades completas para el ajuste y análisis de modelos Tweedie:

#### Funciones Principales:
- `ajuste_modelo_tweedie()`: Función principal de ajuste con optimización
- `optimizar_parametro_p()`: Optimización del parámetro de potencia mediante MLE
- `calcular_dispersion()`: Cálculo del parámetro de dispersión
- `calcular_estadisticas_modelo()`: Estadísticas comprehensivas de ajuste
- `calcular_coeficientes_ic()`: Coeficientes con intervalos de confianza
- `calcular_metricas_ajuste()`: Métricas de error y bondad de ajuste

#### Características Avanzadas:
- **Optimización automática de p**: Encuentra el valor óptimo del parámetro de potencia
- **Intervalos de confianza bootstrap**: Métodos robustos para estimación de incertidumbre
- **Múltiples métodos de dispersión**: Pearson, deviance, y profile likelihood
- **Diagnósticos residuales**: Detección de outliers y puntos influyentes

#### Ejemplo de Uso:
```r
resultado_ajuste <- ajuste_modelo_tweedie(
  data = datos_seguros,
  formula = SumaDePagos ~ Edad + Sexo + TipoVehiculo + offset(log(Exposicion)),
  p_tweedie = 1.5,
  optimizar_p = TRUE,
  bootstrap_ic = TRUE,
  n_bootstrap = 1000,
  verbose = TRUE
)
```

### 3. Gráficos de Diagnóstico (`graficos_diagnosticos_tweedie.R`)

Implementa visualizaciones especializadas para modelos Tweedie:

#### Funciones Principales:
- `grafico_envelope_tweedie()`: Gráficos de envelope con simulación Monte Carlo
- `panel_diagnosticos_tweedie()`: Panel completo de 6-7 gráficos de diagnóstico
- `grafico_dispersion_media_tweedie()`: Verificación de la relación varianza-media
- `grafico_perfil_verosimilitud_p()`: Perfil de log-verosimilitud para parámetro p

#### Gráficos de Envelope:
Los gráficos de envelope son fundamentales para evaluar la bondad de ajuste:
- **Simulación Monte Carlo**: Genera bandas de confianza mediante simulación
- **Detección de outliers**: Identifica automáticamente valores atípicos
- **Múltiples tipos de residuos**: Deviance, Pearson, y estandarizados

#### Panel de Diagnósticos Incluye:
1. Residuos vs Valores Ajustados
2. Q-Q Plot Normal
3. Scale-Location Plot
4. Leverage vs Cook's Distance
5. Observados vs Predichos
6. Histograma de Residuos
7. Gráfico de Envelope (opcional)

#### Ejemplo de Uso:
```r
# Gráfico de envelope
envelope_resultado <- grafico_envelope_tweedie(
  modelo = modelo_tweedie,
  tipo_residuo = "deviance",
  n_simulaciones = 200,
  nivel_confianza = 0.95,
  mostrar_outliers = TRUE
)

# Panel completo
panel_completo <- panel_diagnosticos_tweedie(
  modelo = modelo_tweedie,
  incluir_envelope = TRUE,
  guardar_archivo = "diagnosticos_tweedie.png"
)
```

### 4. Pruebas de Bondad de Ajuste (`pruebas_ajuste_tweedie.R`)

Suite comprehensiva de pruebas estadísticas para validar el modelo:

#### Funciones Principales:
- `suite_pruebas_ajuste_tweedie()`: Batería completa de pruebas
- `pruebas_normalidad_residuos()`: Múltiples pruebas de normalidad
- `pruebas_homocedasticidad()`: Pruebas de constancia de varianza
- `pruebas_independencia_residuos()`: Pruebas de autocorrelación
- `pruebas_especificas_tweedie()`: Pruebas específicas para Tweedie
- `pruebas_bootstrap_tweedie()`: Pruebas bootstrap paramétricas
- `pruebas_simulacion_tweedie()`: Pruebas basadas en simulación Monte Carlo

#### Categorías de Pruebas:

**1. Normalidad de Residuos:**
- Shapiro-Wilk (n ≤ 5000)
- Kolmogorov-Smirnov
- Anderson-Darling
- Lilliefors
- Jarque-Bera

**2. Homocedasticidad:**
- Breusch-Pagan
- Goldfeld-Quandt
- White (heterocedasticidad general)

**3. Independencia:**
- Durbin-Watson (autocorrelación)
- Ljung-Box (múltiples lags)
- Runs Test (aleatoriedad)

**4. Específicas para Tweedie:**
- Verificación relación varianza-media
- Pruebas de quantiles
- Análisis de dispersión

**5. Bootstrap y Simulación:**
- Bootstrap paramétrico
- Simulación Monte Carlo de estadísticos

#### Ejemplo de Uso:
```r
resultados_pruebas <- suite_pruebas_ajuste_tweedie(
  modelo = modelo_tweedie,
  datos_originales = datos_seguros,
  alpha = 0.05,
  incluir_bootstrap = TRUE,
  n_bootstrap = 1000,
  incluir_simulacion = TRUE,
  n_simulaciones = 500,
  verbose = TRUE
)

# Interpretación automática
print(resultados_pruebas$resumen_general$clasificacion_ajuste)
print(resultados_pruebas$resumen_general$recomendaciones)
```

### 5. Ejemplo Completo (`ejemplo_implementacion_tweedie.R`)

Script de demostración que integra todos los módulos:

#### Flujo Completo:
1. **Carga y preparación de datos**
2. **Selección automática de variables**
3. **Ajuste con optimización de parámetros**
4. **Análisis de coeficientes**
5. **Gráficos de diagnóstico**
6. **Pruebas de bondad de ajuste**
7. **Validación cruzada**
8. **Análisis de predicciones**
9. **Resumen ejecutivo**

## Requisitos y Dependencias

### Paquetes de R Necesarios:
```r
# Paquetes principales
install.packages(c(
  "tweedie",      # Distribución Tweedie
  "statmod",      # Funciones estadísticas
  "dplyr",        # Manipulación de datos
  "ggplot2",      # Gráficos
  "gridExtra",    # Arreglo de gráficos
  "boot",         # Bootstrap
  "MASS",         # Funciones estadísticas adicionales
  "car",          # Diagnósticos de regresión
  "lmtest",       # Pruebas de modelos lineales
  "nortest",      # Pruebas de normalidad
  "tseries",      # Series temporales
  "broom",        # Limpieza de resultados
  "moments",      # Momentos estadísticos
  "stringr"       # Manipulación de strings
))
```

## Uso Básico

### Ejemplo Rápido:

```r
# 1. Cargar funciones
source("src/utils/seleccion_variables_tweedie.R")
source("src/utils/ajuste_modelo_tweedie.R")
source("src/utils/graficos_diagnosticos_tweedie.R")
source("src/utils/pruebas_ajuste_tweedie.R")

# 2. Cargar datos
datos <- read.csv("data/processed/datos_limpios.csv")

# 3. Preparar datos
datos_prep <- datos %>%
  mutate(
    SumaDePagos_tweedie = ifelse(SumaDePagos > 0, SumaDePagos, 0.01),
    log_exposicion = log(pmax(exposicion, 1))
  )

# 4. Selección de variables
variables_seleccionadas <- seleccion_variables_aic(
  data = datos_prep,
  response_var = "SumaDePagos_tweedie",
  predictor_vars = c("Edad", "Sexo_Aseg", "TIPO_VEHICULO", "Vr_Comercial"),
  method = "both",
  p_tweedie = 1.6
)

# 5. Ajuste del modelo
modelo_ajustado <- ajuste_modelo_tweedie(
  data = datos_prep,
  formula = SumaDePagos_tweedie ~ Edad + Sexo_Aseg + offset(log_exposicion),
  optimizar_p = TRUE,
  verbose = TRUE
)

# 6. Diagnósticos
diagnosticos <- panel_diagnosticos_tweedie(
  modelo = modelo_ajustado$modelo,
  incluir_envelope = TRUE
)

# 7. Pruebas de ajuste
pruebas <- suite_pruebas_ajuste_tweedie(
  modelo = modelo_ajustado$modelo,
  datos_originales = datos_prep,
  verbose = TRUE
)
```

## Interpretación de Resultados

### Parámetros del Modelo Tweedie:

- **p ∈ (1,2)**: Parámetro de potencia
  - p → 1: Se aproxima a Poisson
  - p = 1.5: Caso especial (compound Poisson-gamma)
  - p → 2: Se aproxima a Gamma

- **φ > 0**: Parámetro de dispersión
  - φ = 1: Dispersión nominal
  - φ > 1: Sobredispersión
  - φ < 1: Subdispersión

### Interpretación de Coeficientes:

Con enlace logarítmico:
- **β₀**: Log del monto esperado de referencia
- **βᵢ**: Cambio logarítmico en el monto esperado por unidad de Xᵢ
- **exp(βᵢ)**: Factor multiplicativo en el monto esperado

### Métricas de Ajuste:

- **AIC/BIC**: Criterios de información (menor es mejor)
- **Pseudo R²**: Proporción de deviance explicada
- **MAE/RMSE**: Errores de predicción
- **MAPE**: Error porcentual absoluto medio

## Casos de Uso Específicos

### 1. Pricing de Seguros de Automóviles:
```r
# Modelo de pricing básico
formula_pricing <- SiniestroTotal ~ 
  Edad + Sexo + TipoVehiculo + Zona + 
  offset(log(Exposicion))

modelo_pricing <- ajuste_modelo_tweedie(
  data = datos_polizas,
  formula = formula_pricing,
  optimizar_p = TRUE
)
```

### 2. Análisis de Rentabilidad:
```r
# Incluir variables de rentabilidad
variables_rentabilidad <- c(
  "PrimaDevengada", "CostoSiniestros", "GastosOperativos"
)

seleccion_rentabilidad <- seleccion_variables_aic(
  data = datos_rentabilidad,
  response_var = "UtilidadNeta",
  predictor_vars = variables_rentabilidad,
  method = "forward"
)
```

### 3. Segmentación de Riesgos:
```r
# Modelo por segmentos
segmentos <- split(datos, datos$SegmentoRiesgo)

modelos_segmentos <- lapply(segmentos, function(seg) {
  ajuste_modelo_tweedie(
    data = seg,
    formula = Siniestro ~ Edad + Sexo + offset(log(Exposicion)),
    optimizar_p = TRUE,
    verbose = FALSE
  )
})
```

## Diagnósticos y Validación

### Indicadores de Buen Ajuste:

1. **Gráficos de Envelope**: 
   - Puntos dentro de las bandas de confianza
   - Pocos outliers (< 5% para nivel 95%)

2. **Relación Varianza-Media**:
   - Correlación alta entre varianza observada y teórica (r > 0.8)
   - Pendiente de regresión cercana a 1

3. **Residuos**:
   - Distribución aproximadamente normal
   - Homocedasticidad (varianza constante)
   - Independencia (no autocorrelación)

4. **Pruebas Estadísticas**:
   - Proporción de rechazos < 25%
   - Clasificación "Bueno" o "Excelente"

### Solución de Problemas Comunes:

**Problema**: Sobredispersión excesiva
**Solución**: 
- Verificar variables omitidas
- Considerar efectos de interacción
- Revisar transformaciones

**Problema**: Parámetro p fuera del rango válido
**Solución**:
- Verificar datos (valores negativos, ceros)
- Considerar otras distribuciones
- Revisar especificación del modelo

**Problema**: Convergencia fallida
**Solución**:
- Revisar multicolinealidad
- Simplificar el modelo
- Verificar valores extremos

## Extensiones y Mejoras Futuras

### Funcionalidades Planeadas:
1. **Modelos jerárquicos**: Efectos aleatorios por región/compañía
2. **Regularización**: Ridge y Lasso para selección automática
3. **Modelos mixtos**: Zero-inflated Tweedie
4. **Análisis temporal**: Incorporación de tendencias temporales
5. **Interface web**: Dashboard interactivo para análisis

### Validaciones Adicionales:
1. **Validación externa**: Holdout samples
2. **Backtesting**: Validación temporal
3. **Stress testing**: Escenarios extremos
4. **Benchmarking**: Comparación con otros modelos

## Documentación Técnica

### Referencias Matemáticas:

**Función de densidad Tweedie:**
```
f(y; μ, φ, p) = a(y, φ, p) × exp((yθ - κ(θ))/φ)
```

**Relación varianza-media:**
```
Var(Y) = φ × μ^p
```

**Log-verosimilitud:**
```
ℓ(β, φ, p) = Σᵢ [yᵢθᵢ - κ(θᵢ)]/φ + Σᵢ log a(yᵢ, φ, p)
```

### Algoritmos Implementados:

1. **Optimización de p**: Método de Brent unidimensional
2. **Selección de variables**: Forward/Backward stepwise con AIC
3. **Bootstrap**: Bootstrap paramétrico con remuestreo
4. **Simulación**: Monte Carlo con generación Tweedie

## Contribuciones y Desarrollo

### Estructura de Desarrollo:
- **Pruebas unitarias**: Tests para cada función principal
- **Documentación**: Roxygen2 para documentación automática  
- **Control de versiones**: Git con branches por funcionalidad
- **Integración continua**: Validación automática de código

### Estándares de Código:
- **Nomenclatura**: snake_case para funciones y variables
- **Documentación**: Comentarios extensivos y ejemplos
- **Validación**: Verificación de parámetros de entrada
- **Manejo de errores**: try-catch comprehensivo

---

**Autor**: Guillermo Murillo  
**Fecha**: 2025  
**Versión**: 1.0  
**Licencia**: Uso académico y educativo