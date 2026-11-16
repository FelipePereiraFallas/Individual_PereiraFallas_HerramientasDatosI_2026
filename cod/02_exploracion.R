# Este script se utiliza para el análisis exploratorio de los dato 

library(tidyverse)
library(lubridate)
library(here)

# se importan los datos limpios

ventas <- readr::read_csv(
  here("data", "procesados", "ventas_limpias.csv"),
  col_types = readr::cols(
    fecha = readr::col_date(),
    venta = readr::col_double(),
    valor_atipico = readr::col_logical()
  ),
  show_col_types = FALSE
)
# verificación de datos con formato correcto
stopifnot(
  !anyNA(ventas$fecha),
  !anyNA(ventas$venta),
  !anyDuplicated(ventas$fecha)
)

# Se utiliza week_start = 1 para que lunes sea
# el primer dia de la semana.

ventas <- ventas |>
  mutate(
    numero_dia = wday(fecha, week_start = 1),
    
    dia_semana = factor(
      numero_dia,
      levels = 1:7,
      labels = c(
        "Lunes", "Martes", "Miercoles",
        "Jueves", "Viernes", "Sabado",
        "Domingo"
      )
    ),
    
    es_sabado = as.integer(numero_dia == 6),
    
    es_domingo = as.integer(numero_dia == 7),
    
    es_fin_semana = as.integer(numero_dia >= 6)
  )

ventas <- ventas |>
  mutate(
    mes = month(fecha),
    anio = year(fecha),
    dia_mes = day(fecha),
    
    # Tiempo transcurrido desde el inicio del estudio.
    tendencia = as.integer(
      fecha - min(fecha)
    )
  )


# para las fechas de pago
# Se supone que las fechas de referencia son:
# - Dia 15 de cada mes.
# - Ultimo dia de cada mes.
#
# Estas fechas son aproximaciones de calendario,

ventas <- ventas |>
  mutate(
    inicio_mes = floor_date(fecha, "month"),
    
    fin_mes_fecha = ceiling_date(
      fecha, "month"
    ) - days(1),
    
    # Ultimo dia del mes anterior.
    fin_mes_anterior = inicio_mes - days(1),
    
    # Fecha de pago quincenal.
    fecha_quincena = inicio_mes + days(14)
  )


# buscamos los días de interes de antes y después del pago de quincenas
ventas <- ventas |>
  mutate(
    # Tres dias antes del 15.
    prequincena = as.integer(
      fecha >= fecha_quincena - days(3) &
        fecha < fecha_quincena
    ),
    
    # Dia 15 y los dos dias posteriores.
    postquincena = as.integer(
      fecha >= fecha_quincena &
        fecha <= fecha_quincena + days(2)
    ),
    
    # Tres dias anteriores al ultimo dia del mes.
    prefin_mes = as.integer(
      fecha >= fin_mes_fecha - days(3) &
        fecha < fin_mes_fecha
    ),
    
    # Ultimo dia del mes y los dos siguientes.
    # Se considera tambien el cierre del mes anterior.
    postfin_mes = as.integer(
      (
        fecha >= fin_mes_fecha &
          fecha <= fin_mes_fecha + days(2)
      ) |
        (
          fecha >= fin_mes_anterior &
            fecha <= fin_mes_anterior + days(2)
        )
    )
  )


# clasificamos estos periodos

ventas <- ventas |>
  mutate(
    periodo_pago = case_when(
      prequincena == 1 ~ "Antes de quincena",
      postquincena == 1 ~ "Quincena",
      prefin_mes == 1 ~ "Antes de fin de mes",
      postfin_mes == 1 ~ "Fin de mes",
      TRUE ~ "Ordinario"
    ),
    
    periodo_pago = factor(
      periodo_pago,
      levels = c(
        "Ordinario",
        "Antes de quincena",
        "Quincena",
        "Antes de fin de mes",
        "Fin de mes"
      )
    )
  )


# resumen de ventas por día de la semana

resumen_dias <- ventas |>
  group_by(dia_semana) |>
  summarise(
    observaciones = n(),
    venta_promedio = mean(venta),
    venta_mediana = median(venta),
    desviacion = sd(venta),
    .groups = "drop"
  )

print(resumen_dias)


# resumen de vetnas por el periodo de pago, antes y después de quincenas

resumen_pagos <- ventas |>
  group_by(periodo_pago) |>
  summarise(
    observaciones = n(),
    venta_promedio = mean(venta),
    venta_mediana = median(venta),
    desviacion = sd(venta),
    .groups = "drop"
  )

print(resumen_pagos)


# gráfico de ventas diarias


grafico_dias <- ggplot(
  ventas,
  aes(x = dia_semana, y = venta)
) +
  geom_boxplot(
    fill = "#377EB8",
    alpha = 0.7,
    outlier.alpha = 0.4
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₡",
      big.mark = "."
    )
  ) +
  labs(
    title = "Ventas segun dia de la semana",
    x = "Dia de la semana",
    y = "Ventas diarias (CRC)",
    caption = "Fuente: Carnes Tio Juan."
  ) +
  theme_minimal(base_size = 12)

print(grafico_dias)

# grafico ventas por periodo de pago

grafico_pagos <- ggplot(
  resumen_pagos,
  aes(
    x = periodo_pago,
    y = venta_promedio
  )
) +
  geom_col(
    fill = "#2A9D8F",
    width = 0.7
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₡",
      big.mark = "."
    )
  ) +
  labs(
    title = "Ventas promedio por periodo de pago",
    x = "Periodo de pago",
    y = "Venta promedio (CRC)",
    caption = "Fuente: Carnes Tio Juan."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

print(grafico_pagos)


#exportamos los resultados

ruta_datos <- here("data", "procesados")
ruta_figuras <- here("info", "figuras")

dir.create(
  ruta_figuras,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  ventas,
  file.path(
    ruta_datos,
    "ventas_modelo.csv"
  )
)

readr::write_csv(
  resumen_dias,
  file.path(
    ruta_datos,
    "resumen_dias.csv"
  )
)

readr::write_csv(
  resumen_pagos,
  file.path(
    ruta_datos,
    "resumen_pagos.csv"
  )
)

ggsave(
  filename = file.path(
    ruta_figuras,
    "ventas_por_dia.png"
  ),
  plot = grafico_dias,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(
    ruta_figuras,
    "ventas_por_periodo_pago.png"
  ),
  plot = grafico_pagos,
  width = 9,
  height = 5,
  dpi = 300
)

cat(
  "\nVariables de calendario creadas correctamente.\n"
)
