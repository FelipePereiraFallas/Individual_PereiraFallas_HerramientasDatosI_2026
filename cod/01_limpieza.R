#acá se integra la limpieza de los datos de las ventas

library(tidyverse)
library(lubridate)
library(here)


#Definimos rutas
ruta_entrada <- here("data", "Venta_Guapiles.csv")

ruta_salida <- here("data", "procesados")

ruta_auditoria <- here("info", "auditoria")

dir.create(
  ruta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  ruta_auditoria,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(ruta_entrada)) {
  stop("No se encontro data/Ventas_Guapiles.csv")
}


# Se importan ambas columnas como texto para evitar
# conversiones automaticas que oculten errores de transcripción.

ventas_originales <- readr::read_csv(
  ruta_entrada,
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  show_col_types = FALSE
)

# Verificar las columnas requeridas

columnas_requeridas <- c("Fecha", "Venta")

if (!all(columnas_requeridas %in%
         names(ventas_originales))) {
  stop("El archivo no contiene Fecha y Venta.")
}

# Agregar identificador de fila original.

ventas_originales <- ventas_originales |>
  mutate(
    fila_origen = row_number() + 1L,
    .before = 1
  )

cat(
  "Filas originales:",
  nrow(ventas_originales),
  "\n"
)

ventas_base <- ventas_originales |>
  mutate(
    Fecha = str_trim(Fecha),
    Venta = str_trim(Venta)
  ) |>
  filter(
    !(
      (is.na(Fecha) | Fecha == "") &
        (is.na(Venta) | Venta == "")
    )
  )

# leemos las fechas

ventas_base <- ventas_base |>
  mutate(
    fecha_texto = stringr::str_remove(
      Fecha,
      "^[A-Za-z]+,\\s*"
    ),
    fecha = lubridate::mdy(
      fecha_texto,
      locale = "C"
    )
  ) |>
  select(-fecha_texto)


#convertimos la columna de ventas en datos numéricos

ventas_base <- ventas_base |>
  mutate(
    venta = readr::parse_double(
      Venta,
      na = c("", "NA")
    )
  )


#buscamos errores de transcripción


registros_invalidos <- ventas_base |>
  filter(
    is.na(fecha) |
      is.na(venta) |
      venta < 0
  ) |>
  mutate(
    motivo = case_when(
      is.na(fecha) ~ "Fecha invalida",
      is.na(venta) ~ "Venta no numerica",
      venta < 0 ~ "Venta negativa",
      TRUE ~ "Otro problema"
    )
  )

readr::write_csv(
  registros_invalidos,
  file.path(
    ruta_auditoria,
    "registros_invalidos.csv"
  )
)

#acá buscamos a ver si se encuentran ventas duplicadas

ventas_validas <- ventas_base |>
  filter(
    !is.na(fecha),
    !is.na(venta),
    venta >= 0
  )

registros_duplicados <- ventas_validas |>
  add_count(
    fecha,
    name = "cantidad_registros"
  ) |>
  filter(cantidad_registros > 1) |>
  arrange(fecha, fila_origen)

readr::write_csv(
  registros_duplicados,
  file.path(
    ruta_auditoria,
    "registros_duplicados.csv"
  )
)

#vamos a solucionar los duplicaods
# Regla:
# Si los montos son identicos, conservar una fila.
# Si los montos distintos, excluir temporalmente
# la fecha hasta verificar el registro original.

resumen_duplicados <- ventas_validas |>
  group_by(fecha) |>
  summarise(
    cantidad = n(),
    montos_distintos = n_distinct(venta),
    .groups = "drop"
  ) |>
  filter(cantidad > 1)

fechas_conflictivas <- resumen_duplicados |>
  filter(montos_distintos > 1) |>
  pull(fecha)

# Guardar todos los registros de fechas conflictivas.

duplicados_conflictivos <- ventas_validas |>
  filter(fecha %in% fechas_conflictivas)

readr::write_csv(
  duplicados_conflictivos,
  file.path(
    ruta_auditoria,
    "duplicados_conflictivos.csv"
  )
)

# Excluir fechas conflictivas y conservar una fila
# cuando todos los montos duplicados coinciden.

ventas_sin_duplicados <- ventas_validas |>
  filter(!fecha %in% fechas_conflictivas) |>
  arrange(fecha, fila_origen) |>
  distinct(fecha, .keep_all = TRUE)



# El periodo principal inicia el 02/10/2024.
# Se separan los registros anteriores para revision.
#
# Esta fecha se basa en la inspeccion inicial del CSV,
# no en una regla general de limpieza.

fecha_inicio <- as.Date("2024-10-02")

registros_fuera_periodo <- ventas_sin_duplicados |>
  filter(fecha < fecha_inicio)

readr::write_csv(
  registros_fuera_periodo,
  file.path(
    ruta_auditoria,
    "registros_fuera_periodo.csv"
  )
)

ventas_periodo <- ventas_sin_duplicados |>
  filter(fecha >= fecha_inicio)

# Se aplica el criterio exploratorio del rango
# intercuartil (IQR).
#
# Los valores atipicos se marcan, pero no se eliminan. Pueden haber sido simplemente
# ventas muy altas.

cuartiles <- quantile(
  ventas_periodo$venta,
  probs = c(0.25, 0.75),
  na.rm = TRUE
)

q1 <- unname(cuartiles[1])
q3 <- unname(cuartiles[2])

rango_iqr <- q3 - q1

limite_inferior <- q1 - 1.5 * rango_iqr
limite_superior <- q3 + 1.5 * rango_iqr

ventas_periodo <- ventas_periodo |>
  mutate(
    valor_atipico = (
      venta < limite_inferior |
        venta > limite_superior
    )
  )

ventas_atipicas <- ventas_periodo |>
  filter(valor_atipico) |>
  arrange(desc(venta))

readr::write_csv(
  ventas_atipicas,
  file.path(
    ruta_auditoria,
    "ventas_atipicas.csv"
  )
)

# Se identifican fechas ausentes para documentarlas.
# No se agregan a la base de entrenamiento.
#
# ya que corresponden a dias de cierre.

calendario <- tibble(
  fecha = seq.Date(
    from = min(ventas_periodo$fecha),
    to = max(ventas_periodo$fecha),
    by = "day"
  )
)

fechas_sin_registro <- calendario |>
  anti_join(
    ventas_base |>
      filter(!is.na(fecha)) |>
      distinct(fecha),
    by = "fecha"
  )

readr::write_csv(
  fechas_sin_registro,
  file.path(
    ruta_auditoria,
    "fechas_sin_registro.csv"
  )
)

# Se conservan solamente las variables necesarias
# para el analisis posterior.

ventas_limpias <- ventas_periodo |>
  transmute(
    fecha,
    venta,
    valor_atipico
  ) |>
  arrange(fecha)


#vemos que no hayan fechas duplicadas y que su formato sea el correcto

stopifnot(
  !anyDuplicated(ventas_limpias$fecha)
)

stopifnot(
  is.numeric(ventas_limpias$venta),
  !anyNA(ventas_limpias$fecha),
  !anyNA(ventas_limpias$venta),
  all(ventas_limpias$venta >= 0)
)
stopifnot(
  !is.unsorted(ventas_limpias$fecha)
)

#finalmente exportamos los datos


readr::write_csv(
  ventas_limpias,
  file.path(
    ruta_salida,
    "ventas_limpias.csv"
  )
)

#verificación de la limpiez 

resumen_limpieza <- tibble(
  indicador = c(
    "Filas originales",
    "Registros no vacios",
    "Registros invalidos",
    "Fechas duplicadas",
    "Fechas con montos conflictivos",
    "Registros fuera del periodo",
    "Registros limpios",
    "Valores atipicos identificados",
    "Fechas sin registro en calendario"
  ),
  cantidad = c(
    nrow(ventas_originales),
    nrow(ventas_base),
    nrow(registros_invalidos),
    nrow(resumen_duplicados),
    length(fechas_conflictivas),
    nrow(registros_fuera_periodo),
    nrow(ventas_limpias),
    nrow(ventas_atipicas),
    nrow(fechas_sin_registro)
  )
)

readr::write_csv(
  resumen_limpieza,
  file.path(
    ruta_auditoria,
    "resumen_limpieza.csv"
  )
)

print(resumen_limpieza, n = Inf)

cat("\nLimpieza finalizada correctamente.\n")
cat("Base limpia guardada en data/procesados/\n")
cat("Archivos de auditoria guardados en info/auditoria/\n")
