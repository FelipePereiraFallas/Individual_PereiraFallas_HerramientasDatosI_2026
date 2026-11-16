# Predicción de ventas diarias de Carnes Tío Juan

**Universidad de Costa Rica**  
**Curso:** Herramientas de Datos I  
**Estudiante:** Felipe Pereira Fallas  
**Año:** 2026

## Descripción del proyecto

Este proyecto desarrolla un análisis estadístico y predictivo de las ventas diarias de Carnes Tío Juan, establecimiento comercial ubicado en Guápiles, Costa Rica.

El propósito es identificar patrones en las ventas relacionados con los días de la semana, los períodos de pago y la evolución temporal, utilizando herramientas de análisis de datos en R.

Se comparan distintos modelos estadísticos para seleccionar el que ofrece una mayor precisión predictiva y generar estimaciones de ventas futuras que puedan apoyar la planificación comercial.

## Objetivo

Desarrollar y evaluar un modelo estadístico para predecir las ventas diarias de Carnes Tío Juan mediante el análisis de registros históricos y variables de calendario.

## Herramientas utilizadas

- R y RStudio.
- Quarto para la elaboración del reporte académico.
- tidyverse para manipulación, limpieza y visualización de datos.
- lubridate para el tratamiento de fechas.
- ggplot2 para la creación de gráficos.
- Git y GitHub para el control de versiones.

## Estructura del proyecto

```text
Individual_PereiraFallas_HerramientasDatosI_2026/
│
├── cod/
│   ├── 01_limpieza.R
│   ├── 02_exploracion.R
│   ├── 03_modelado.R
│   └── 04_predicciones.R
│
├── data/
│   ├── Ventas_Guapiles.csv
│   └── procesados/
│
├── info/
│   ├── auditoria/
│   └── figuras/
│
├── Individual_PereiraFallas_HerramientasDatosI_2026.qmd
├── Individual_PereiraFallas_HerramientasDatosI_2026.pdf
├── Individual_PereiraFallas_HerramientasDatosI_2026.Rproj
├── .gitignore
└── README.md
```

## Metodología

El análisis se desarrolló en cuatro etapas.

### 1. Limpieza y preparación

Se importaron los registros originales de ventas, se verificaron las fechas y se identificaron valores faltantes, duplicados y posibles errores de digitación.

Después de la depuración, se obtuvo una base de 699 observaciones válidas correspondientes al período comprendido entre el 2 de octubre de 2024 y el 11 de noviembre de 2026.

Las fechas sin registros se interpretaron como días de cierre y no como ventas iguales a cero.

### 2. Análisis exploratorio

Se estudiaron las diferencias de ventas según:

- Día de la semana.
- Mes del año.
- Períodos anteriores a quincena.
- Períodos de quincena.
- Días anteriores al fin de mes.
- Períodos de fin de mes.

Se elaboraron gráficos y tablas descriptivas para identificar patrones relevantes.

### 3. Construcción y evaluación de modelos

Se compararon tres modelos:

- M1: promedio histórico de ventas.
- M2: regresión lineal utilizando el día de la semana.
- M3: regresión lineal utilizando día de la semana, período de pago, mes y tendencia temporal.

Los datos se dividieron cronológicamente en 80 % para entrenamiento y 20 % para prueba.

Se utilizaron las métricas MAE y RMSE para comparar el desempeño predictivo.

### 4. Predicción de ventas futuras

Se seleccionó el modelo con menor MAE en el conjunto de prueba y se reestimó utilizando todas las observaciones válidas.

Posteriormente, se generaron pronósticos para los siguientes 30 días calendario, excluyendo los domingos debido al horario actual del establecimiento.

## Principales resultados

El modelo M3 presentó el mejor desempeño predictivo, con los siguientes resultados:

| Indicador | Resultado |
|---|---:|
| Observaciones válidas | 699 |
| Observaciones de entrenamiento | 559 |
| Observaciones de prueba | 140 |
| MAE del modelo seleccionado | ₡74.948 |
| RMSE del modelo seleccionado | ₡93.034 |
| Reducción del MAE frente a M1 | 17,1 % |
| Días futuros proyectados | 26 |
| Ventas totales pronosticadas | Aproximadamente ₡10,06 millones |

Entre los principales hallazgos se identificaron mayores ventas durante los sábados y una asociación positiva entre las ventas y el período de fin de mes.

No se encontró evidencia estadística suficiente para afirmar que las ventas disminuyen sistemáticamente antes de las fechas de pago.

Los resultados son asociaciones estadísticas y no deben interpretarse como relaciones causales.

## Reproducibilidad

Para reproducir el análisis:

1. Clonar o descargar este repositorio.
2. Abrir el archivo `.Rproj` en RStudio.
3. Instalar los paquetes necesarios de R.
4. Ejecutar los scripts en el siguiente orden:

```r
source("cod/01_limpieza.R")
source("cod/02_exploracion.R")
source("cod/03_modelado.R")
source("cod/04_predicciones.R")
```

5. Abrir el archivo `.qmd` y seleccionar **Render** para generar el reporte PDF mediante Quarto y LuaLaTeX.

Los scripts generan los archivos procesados, las tablas de resultados y las visualizaciones utilizadas en el reporte.

## Consideraciones

Las predicciones representan estimaciones estadísticas y pueden diferir de las ventas efectivamente realizadas.

El modelo no incorpora directamente factores como promociones, cambios de precios, feriados, condiciones climáticas o modificaciones extraordinarias en el horario de atención.

Se recomienda actualizar el modelo conforme se incorporen nuevos registros de ventas.

## Autor

Felipe Pereira Fallas  
Universidad de Costa Rica  
Herramientas de Datos I — 2026

## Uso académico

Proyecto desarrollado con fines académicos. Los registros de ventas corresponden a información interna del establecimiento y deben utilizarse únicamente con la