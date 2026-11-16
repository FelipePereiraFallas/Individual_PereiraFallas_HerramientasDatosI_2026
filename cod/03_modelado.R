# acá se comparan los modelos predictivivos para las ventas
library(tidyverse)
library(here)
library(broom)

ruta_datos <- here("data", "procesados")

dir.create(
  ruta_datos,
  recursive = TRUE,
  showWarnings = FALSE
)

# importamos los datos

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
  nrow(ventas) == 699,
  !anyDuplicated(ventas$fecha),
  !anyNA(ventas$venta),
  all(ventas$venta >= 0)
)

# definimos las etapas para entrenamiento del modelo y para comparación

n_total <- nrow(ventas)
n_entrenamiento <- floor(n_total * 0.80)

entrenamiento <- ventas |>
  slice_head(n = n_entrenamiento)

prueba <- ventas |>
  slice_tail(n = n_total - n_entrenamiento)

stopifnot(
  max(entrenamiento$fecha) < min(prueba$fecha)
)

cat("Entrenamiento:", nrow(entrenamiento), "\n")
cat("Prueba:", nrow(prueba), "\n")

# para este primero modelo utilizamos únicamente el promedio histórico
# es lo que le llaman modelo ingenuo

promedio_historico <- mean(entrenamiento$venta)

pred_m1 <- rep(
  promedio_historico,
  nrow(prueba)
)

# para este modelo se implementa la variable de ajuste de día de la semana

modelo_2 <- lm(
  venta ~ dia_semana,
  data = entrenamiento
)

pred_m2 <- predict(
  modelo_2,
  newdata = prueba
)

# para el tercer modelo se utiliza las variables de calendario completo, dia
# de la semana, periodo de pago, mes del año y la tendencia histórica. 

modelo_3 <- lm(
  venta ~ dia_semana +
    periodo_pago +
    mes +
    tendencia,
  data = entrenamiento
)

pred_m3 <- predict(
  modelo_3,
  newdata = prueba
)

# tabla de predicciones

comparacion <- prueba |>
  transmute(
    fecha,
    venta_real = venta,
    modelo_1 = pmax(0, pred_m1),
    modelo_2 = pmax(0, as.numeric(pred_m2)),
    modelo_3 = pmax(0, as.numeric(pred_m3))
  )

#funciones para comprar los errores de los modelos en comparación al periodo 
# de referencia

calcular_mae <- function(real, predicho) {
  mean(abs(real - predicho))
}

calcular_rmse <- function(real, predicho) {
  sqrt(mean((real - predicho)^2))
}

# comparamos los modelos
resultados <- comparacion |>
  pivot_longer(
    cols = starts_with("modelo_"),
    names_to = "modelo",
    values_to = "prediccion"
  ) |>
  group_by(modelo) |>
  summarise(
    MAE = calcular_mae(venta_real, prediccion),
    RMSE = calcular_rmse(venta_real, prediccion),
    .groups = "drop"
  ) |>
  arrange(MAE)

print(resultados)

# buscamos el mejor modelo

mejor_modelo <- resultados |>
  slice_min(MAE, n = 1, with_ties = FALSE)

cat(
  "\nMejor modelo segun MAE:",
  mejor_modelo$modelo,
  "\n"
)

# gráfico para comparar los modelos

grafico_modelos <- resultados |>
  pivot_longer(
    cols = c(MAE, RMSE),
    names_to = "metrica",
    values_to = "error"
  ) |>
  ggplot(
    aes(
      x = modelo,
      y = error,
      fill = metrica
    )
  ) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₡",
      big.mark = "."
    )
  ) +
  labs(
    title = "Comparacion de errores predictivos",
    x = "Modelo",
    y = "Error en colones",
    fill = "Metrica",
    caption = "Fuente: elaboracion propia."
  ) +
  theme_minimal(base_size = 12)

print(grafico_modelos)

# exportamos los datos

readr::write_csv(
  comparacion,
  file.path(ruta_datos, "comparacion_predicciones.csv")
)

readr::write_csv(
  resultados,
  file.path(ruta_datos, "resultados_modelos.csv")
)

readr::write_csv(
  mejor_modelo,
  file.path(ruta_datos, "mejor_modelo.csv")
)

readr::write_csv(
  broom::tidy(modelo_3),
  file.path(ruta_datos, "coeficientes_modelo_3.csv")
)

ruta_figuras <- here("info", "figuras")

dir.create(
  ruta_figuras,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  file.path(ruta_figuras, "comparacion_modelos.png"),
  plot = grafico_modelos,
  width = 9,
  height = 5,
  dpi = 300
)

cat("\nModelado finalizado correctamente.\n")
