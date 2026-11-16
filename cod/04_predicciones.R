# en este script se realiza

library(tidyverse)
library(lubridate)
library(here)


# definimos rutas

ruta_datos <- here("data", "procesados")
ruta_figuras <- here("info", "figuras")

dir.create(
  ruta_datos,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  ruta_figuras,
  recursive = TRUE,
  showWarnings = FALSE
)


# importamos los datos a utilizar

ventas <- readr::read_csv(
  file.path(ruta_datos, "ventas_modelo.csv"),
  show_col_types = FALSE
) |>
  mutate(
    fecha = as.Date(fecha),
    
    dia_semana = factor(
      dia_semana,
      levels = c(
        "Lunes", "Martes", "Miercoles",
        "Jueves", "Viernes", "Sabado",
        "Domingo"
      )
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
    ),
    
    mes = factor(mes, levels = 1:12)
  ) |>
  arrange(fecha)

stopifnot(
  nrow(ventas) > 0,
  !anyDuplicated(ventas$fecha),
  !anyNA(ventas$venta)
)

# entrenamos al modelo
# Se utiliza la especificacion del Modelo 3,
# seleccionado previamente mediante el conjunto
# de prueba.

modelo_final <- lm(
  venta ~ dia_semana +
    periodo_pago +
    mes +
    tendencia,
  data = ventas
)

print(summary(modelo_final))


fecha_inicio <- min(ventas$fecha)
fecha_final <- max(ventas$fecha)

horizonte <- 30

# calendario de predicciones
calendario_futuro <- tibble(
  fecha = seq.Date(
    from = fecha_final + days(1),
    by = "day",
    length.out = horizonte
  )
)


calendario_futuro <- calendario_futuro |>
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
    )
  )


# ------------------------------------------------------------
# 7. EXCLUSION DE DOMINGOS
# ------------------------------------------------------------

# El establecimiento actualmente abre de lunes
# a sabado. No se pronostican ventas los domingos.

calendario_futuro <- calendario_futuro |>
  filter(numero_dia != 7)


# ------------------------------------------------------------
# 8. VARIABLES GENERALES DE CALENDARIO
# ------------------------------------------------------------

calendario_futuro <- calendario_futuro |>
  mutate(
    mes = factor(
      month(fecha),
      levels = 1:12
    ),
    
    tendencia = as.integer(
      fecha - fecha_inicio
    ),
    
    inicio_mes = floor_date(fecha, "month"),
    
    fin_mes_fecha = ceiling_date(
      fecha, "month"
    ) - days(1),
    
    fin_mes_anterior = inicio_mes - days(1),
    
    fecha_quincena = inicio_mes + days(14)
  )

# Se aplican las mismas reglas utilizadas
# durante la construccion de la base historica.

calendario_futuro <- calendario_futuro |>
  mutate(
    prequincena = as.integer(
      fecha >= fecha_quincena - days(3) &
        fecha < fecha_quincena
    ),
    
    postquincena = as.integer(
      fecha >= fecha_quincena &
        fecha <= fecha_quincena + days(2)
    ),
    
    prefin_mes = as.integer(
      fecha >= fin_mes_fecha - days(3) &
        fecha < fin_mes_fecha
    ),
    
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


# clasificamos periodo de pago

calendario_futuro <- calendario_futuro |>
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

# Se calculan intervalos de prediccion del 95%.
# Los limites negativos se ajustan a cero porque
# las ventas no pueden ser negativas.

predicciones <- predict(
  modelo_final,
  newdata = calendario_futuro,
  interval = "prediction",
  level = 0.95
)

ventas_futuras <- calendario_futuro |>
  mutate(
    venta_predicha = pmax(
      0,
      as.numeric(predicciones[, "fit"])
    ),
    
    limite_inferior = pmax(
      0,
      as.numeric(predicciones[, "lwr"])
    ),
    
    limite_superior = pmax(
      0,
      as.numeric(predicciones[, "upr"])
    )
  ) |>
  select(
    fecha,
    dia_semana,
    periodo_pago,
    venta_predicha,
    limite_inferior,
    limite_superior
  )

stopifnot(
  !anyNA(ventas_futuras$venta_predicha),
  all(ventas_futuras$venta_predicha >= 0)
)

resumen_predicciones <- ventas_futuras |>
  summarise(
    dias_proyectados = n(),
    
    venta_promedio_predicha = mean(
      venta_predicha
    ),
    
    venta_total_predicha = sum(
      venta_predicha
    ),
    
    venta_minima_predicha = min(
      venta_predicha
    ),
    
    venta_maxima_predicha = max(
      venta_predicha
    )
  )

print(resumen_predicciones)


#graficos

grafico_predicciones <- ggplot(
  ventas_futuras,
  aes(
    x = fecha,
    y = venta_predicha
  )
) +
  geom_ribbon(
    aes(
      ymin = limite_inferior,
      ymax = limite_superior
    ),
    fill = "#377EB8",
    alpha = 0.20
  ) +
  geom_line(
    color = "#1D4E89",
    linewidth = 1
  ) +
  geom_point(
    color = "#1D4E89",
    size = 1.5
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₡",
      big.mark = "."
    )
  ) +
  labs(
    title = "Prediccion de ventas diarias",
    subtitle = "Proximos 30 dias calendario",
    x = "Fecha",
    y = "Ventas estimadas (CRC)",
    caption = paste(
      "Banda: intervalo de prediccion del 95%.",
      "Fuente: elaboracion propia."
    )
  ) +
  theme_minimal(base_size = 12)

print(grafico_predicciones)


# ------------------------------------------------------------
# 14. EXPORTACION DE RESULTADOS
# ------------------------------------------------------------

readr::write_csv(
  ventas_futuras,
  file.path(
    ruta_datos,
    "predicciones_futuras.csv"
  )
)

readr::write_csv(
  resumen_predicciones,
  file.path(
    ruta_datos,
    "resumen_predicciones.csv"
  )
)

ggsave(
  filename = file.path(
    ruta_figuras,
    "predicciones_futuras.png"
  ),
  plot = grafico_predicciones,
  width = 10,
  height = 5,
  dpi = 300
)

cat("\nPredicciones generadas correctamente.\n")
cat("Ultima fecha historica:", as.character(fecha_final), "\n")
cat("Dias proyectados:", nrow(ventas_futuras), "\n")
