# ============================================================
# PASO 1: LIBRERÍAS Y CONFIGURACIÓN
# ============================================================
# (Tu código - sin cambios)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("fs")) install.packages("fs")
library(fs)
if (!require("arrow")) install.packages("arrow")
library(arrow)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if(!require("stringr")) install.packages("stringr")
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

## ============================================================
# PASO 1: LIBRERÍAS Y CONFIGURACIÓN
# ============================================================
# (Tu código - sin cambios)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("fs")) install.packages("fs")
library(fs)
if (!require("arrow")) install.packages("arrow")
library(arrow)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if(!require("stringr")) install.packages("stringr")
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

# ============================================================
# PASO 2: FUNCIÓN DE CARGA DE DATOS SMN (FWF)
# ============================================================
# Esta es la función que usa anchos fijos (fwf) con las
# posiciones que dedujimos de tu primer ejemplo.
leer_archivo_fwf <- function(ruta_archivo) {
  
  # Definimos las posiciones EXACTAS basadas en tu ejemplo
  posiciones_columnas <- fwf_cols(
    Fecha  = c(1, 8),
    Hora   = c(9, 14),   # 6 caracteres
    Temp   = c(15, 20),  # 6 caracteres
    Hum    = c(21, 26),  # 6 caracteres
    PNM    = c(27, 33),  # 7 caracteres
    DD     = c(34, 39),  # 6 caracteres
    FF     = c(40, 45),  # 6 caracteres
    Nombre = c(46, NA)   # El resto
  )
  
  tryCatch({
    datos_extraidos <- read_fwf(
      file = ruta_archivo,
      col_positions = posiciones_columnas,
      skip = 2, # Saltamos las 2 cabeceras
      locale = locale(encoding = "latin1"),
      col_types = cols(.default = "c") # Leemos todo como texto
    )
    
    # IMPORTANTE: Quitar filas donde 'Fecha' sea NA.
    datos_extraidos <- datos_extraidos |>
      filter(!is.na(Fecha))
    
    return(datos_extraidos)
    
  }, error = function(e) {
    warning(paste("Error en archivo:", ruta_archivo, "-", e$message))
    return(NULL)
  })
}

# ============================================================
# PASO 3: CARGA Y LIMPIEZA INICIAL
# ============================================================
ruta_a_los_datos <- "../data/raw/smn-data"
lista_de_archivos <- list.files(path = ruta_a_los_datos,
                                pattern = "\\.txt$",
                                full.names = TRUE,
                                ignore.case = TRUE)

cat("Iniciando lectura de", length(lista_de_archivos), "archivos (con lógica 'fwf' correcta)...\n")

# 1. Usamos map() para aplicar la función a cada archivo.
lista_datos <- map(
  lista_de_archivos,
  leer_archivo_fwf
)

# 2. Asignamos los nombres de los archivos a la lista
names(lista_datos) <- lista_de_archivos

# 3. Usamos bind_rows() para unificar todo en un gran data.frame.
datos_unificados <- bind_rows(lista_datos, .id = "archivo_origen")

cat("Archivos leídos correctamente. Total filas:", nrow(datos_unificados), "\n")

cat("Limpiando la columna 'Hora' y 'Nombre'...\n")

datos_unificados <- datos_unificados |>
  mutate(
    # Quita cualquier cosa que NO sea un número de la Hora (ej. " 0 " -> "0")
    Hora = str_remove_all(Hora, "\\D"),
    
    # Quita espacios extra al inicio/fin del Nombre
    Nombre = str_trim(Nombre) 
  ) |>
  # Filtra filas que quedaron vacías o con datos NA
  filter(!is.na(Fecha), Hora != "") 

cat("Limpieza básica completada.\n")

# ============================================================
# PASO 4: CONVERSIÓN DE TIPOS
# ============================================================
# Convertimos las columnas de character a sus tipos correctos
# (Fecha a Date, el resto a numeric) en un solo paso.
cat("Convirtiendo tipos de datos (Fecha a Date, resto a Numeric)...\n")
datos_unificados <- datos_unificados |> 
  mutate(
    Fecha = dmy(Fecha), # Convertimos la Fecha (ej: "01012018")
    Temp = as.numeric(Temp),
    Hum = as.numeric(Hum),
    PNM = as.numeric(PNM),
    DD = as.numeric(DD),
    FF = as.numeric(FF)
  )

# ============================================================
# PASO 5: LIMPIEZA DE NOMBRES DE ESTACIONES
# ============================================================
cat("Iniciando limpieza de nombres de estaciones...\n")

# 1. Definimos la BASURA REAL (valores que NO se pueden reparar)
valores_basura_real <- c(NA, "29/7/2018", "}", "28")

datos_limpios <- datos_unificados |>
  
  # 2. Filtramos ÚNICAMENTE la basura real
  filter(!Nombre %in% valores_basura_real) |>
  
  # 3. REPARAMOS las pocas inconsistencias que quedan
  mutate(
    Nombre = str_replace_all(Nombre, regex("OBS\\.", ignore_case = TRUE), "OBSERVATORIO"),
    
    Nombre = case_when(
      # --- Unificar duplicados (para que coincidan con el diccionario) ---
      Nombre == "BUENOS AIRES" ~ "BUENOS AIRES OBSERVATORIO",
      Nombre == "SAN FERNANDO" ~ "SAN FERNANDO AERO",
      Nombre == "VENADO TUERTO" ~ "VENADO TUERTO AERO",
      Nombre == "LAS FLORES AERO" ~ "LAS FLORES",
      Nombre == "OBERA AERO" ~ "OBERA",
      Nombre %in% c("PCIA. ROQUE SAENZ PEÑA AER", "PRESIDENCIA ROQUE SAENZ PE") ~ "PRESIDENCIA ROQUE SAENZ PEÑA AERO",
      Nombre %in% c("ESC.AVIACION MILITAR AERO", "ESCUELA DE AVIACION MILITA") ~ "ESCUELA DE AVIACION MILITAR AERO",
      Nombre %in% c("VILLA MARIA DEL RIO SECO", "VILLA DE MARIA DEL RIO SEC") ~ "VILLA DE MARIA DEL RIO SECO",
      TRUE ~ Nombre
    )
  )

cat("Limpieza de nombres de estaciones completada.\n")


# Guardar datos limpios intermedios como parquet
write_parquet(datos_limpios, "../data/processed/datos_climaticos_limpios.parquet")


# ============================================================
# Cargar datos desde parquet

datos_limpios <- read_parquet("../data/processed/datos_climaticos_limpios.parquet")
# ============================================================

# ============================================================
# PASO 6: CARGA DE DATOS ADICIONALES (PRECIPITACIÓN Y ESTACIONES)
# ============================================================
cat("Cargando datos de precipitación y estaciones...\n")

datos_precipitacion <- read_csv("../data/raw/smn_precipitaciones-1991-2024.txt") |> 
  rename(Precipitacion_mm = "Precipitacion (mm)") |> 
  mutate(
    Precipitacion_mm = na_if(Precipitacion_mm, "\\N"),
    Precipitacion_mm = as.numeric(Precipitacion_mm)
  )

datos_estaciones <- read_csv("../data/raw/smn_estaciones.csv")

# ============================================================
# PASO 7: UNIFICACIÓN FINAL (JOINS)
# ============================================================
cat("Uniendo todos los dataframes...\n")

# 1. Agregamos el Nro de estación a los datos climáticos
datos_finales <- datos_limpios |> 
  left_join(datos_estaciones |> select(Nombre, Nro), by = "Nombre")

# --- VERIFICACIÓN DE TIPOS (para estar 100% seguros) ---
# Nos aseguramos que AMBAS columnas 'Fecha' sean 'Date'
if (class(datos_finales$Fecha) != "Date") {
  warning("La fecha de datos_finales no es 'Date'!")
}
if (class(datos_precipitacion$Fecha) != "Date") {
  warning("La fecha de datos_precipitacion no es 'Date'!")
}
# --- FIN VERIFICACIÓN ---


# 2. Agregamos la precipitación (¡AHORA DEBE FUNCIONAR!)
#    Se une por "Nro" (numérico) y "Fecha" (Date)
datos_finales <- datos_finales |>
  left_join(datos_precipitacion |> select(Estacion, Fecha, Precipitacion_mm),
            by = c("Nro" = "Estacion", "Fecha" = "Fecha"))

# 3. Agregamos el resto de la info de las estaciones
datos_finales <- datos_finales |>
  left_join(datos_estaciones |> select(Nro, Latitud, Longitud, Altura, Provincia), by = "Nro")

# 🔍 Resumen después de los joins (Ahora precipitación debe tener datos)
cat("--- Resumen post-join (Precipitación no debería ser todo NA) ---\n")
summary(datos_finales$Precipitacion_mm)

# ============================================================
# PASO 8: ANÁLISIS Y LIMPIEZA DE OUTLIERS
# ============================================================
cat("Iniciando análisis de valores atípicos (outliers)...\n")

# --- 1. Definir los rangos lógicos ---
rango_temp <- c(-40, 50)     # °C
rango_pnm <- c(850, 1100)    # hPa (Presión a Nivel del Mar)
rango_dd <- c(0, 360)       # Grados
rango_ff <- c(0, 250)       # km/h
rango_precip <- c(0, 500)   # mm

# --- 2. Calcular y reportar la proporción de Outliers ---
cat("--- Reporte de Proporción de Outliers ---\n")

# Calculamos las proporciones en un solo paso
outlier_summary <- datos_finales |>
  summarise(
    # Contar filas no-NA para cada variable
    total_temp = sum(!is.na(Temp)),
    total_pnm = sum(!is.na(PNM)),
    total_dd = sum(!is.na(DD)),
    total_ff = sum(!is.na(FF)),
    total_precip = sum(!is.na(Precipitacion_mm)),
    
    # Contar los outliers
    outliers_temp = sum(Temp < rango_temp[1] | Temp > rango_temp[2], na.rm = TRUE),
    outliers_pnm = sum(PNM < rango_pnm[1] | PNM > rango_pnm[2], na.rm = TRUE),
    outliers_dd = sum(DD < rango_dd[1] | DD > rango_dd[2], na.rm = TRUE),
    outliers_ff = sum(FF < rango_ff[1] | FF > rango_ff[2], na.rm = TRUE),
    outliers_precip = sum(Precipitacion_mm < rango_precip[1] | Precipitacion_mm > rango_precip[2], na.rm = TRUE)
  ) |>
  # Calcular el porcentaje
  mutate(
    prop_temp = (outliers_temp / total_temp) * 100,
    prop_pnm = (outliers_pnm / total_pnm) * 100,
    prop_dd = (outliers_dd / total_dd) * 100,
    prop_ff = (outliers_ff / total_ff) * 100,
    prop_precip = (outliers_precip / total_precip) * 100
  )

# Imprimir el reporte
cat(sprintf(
  "Temperatura (rango: %.0f a %.0f°C): %.2f%% de los datos son outliers (%d / %d filas)\n",
  rango_temp[1], rango_temp[2],
  outlier_summary$prop_temp,
  outlier_summary$outliers_temp,
  outlier_summary$total_temp
))
cat(sprintf(
  "Presión (PNM) (rango: %.0f a %.0f hPa): %.2f%% de los datos son outliers (%d / %d filas)\n",
  rango_pnm[1], rango_pnm[2],
  outlier_summary$prop_pnm,
  outlier_summary$outliers_pnm,
  outlier_summary$total_pnm
))
cat(sprintf(
  "Dir. Viento (DD) (rango: %.0f a %.0f°): %.2f%% de los datos son outliers (%d / %d filas)\n",
  rango_dd[1], rango_dd[2],
  outlier_summary$prop_dd,
  outlier_summary$outliers_dd,
  outlier_summary$total_dd
))
cat(sprintf(
  "Vel. Viento (FF) (rango: %.0f a %.0f km/h): %.2f%% de los datos son outliers (%d / %d filas)\n",
  rango_ff[1], rango_ff[2],
  outlier_summary$prop_ff,
  outlier_summary$outliers_ff,
  outlier_summary$total_ff
))
cat(sprintf(
  "Precipitación (rango: %.0f a %.0f mm): %.2f%% de los datos son outliers (%d / %d filas)\n",
  rango_precip[1], rango_precip[2],
  outlier_summary$prop_precip,
  outlier_summary$outliers_precip,
  outlier_summary$total_precip
))
cat("---------------------------------------------\n")

# Diagnostico de Precipitacion - Buscamos explicacion a la alta cantidad de outliers

# --- CÓDIGO DE DIAGNÓSTICO PARA PNM ---
cat("--- Investigando Outliers de PNM ---\n")

# Filtramos para ver solo las filas problemáticas
outliers_pnm_df <- datos_finales |>
  filter(PNM < 850 | PNM > 1100, !is.na(PNM))

# 1. ¿Cuáles son los valores? (Quizás todos son "999" o algo así)
cat("\nValores más comunes del 'outlier':\n")
print(head(count(outliers_pnm_df, PNM, sort = TRUE), 20))

# 2. ¿Qué estaciones tienen el problema?
cat("\nEstaciones con más outliers de PNM:\n")
print(head(count(outliers_pnm_df, Nombre, sort = TRUE), 20))

# 3. ¿En qué fechas ocurren estos outliers?
cat("\nFechas con más outliers de PNM:\n")
print(head(count(outliers_pnm_df, Fecha, sort = TRUE), 20))


# No encontramos relacion entre los outliers y las estaciones o fechas específicas.

# --- Convertir outliers a NA (para imputación) ---
cat("Convirtiendo outliers a NA para que sean imputados en el próximo paso...\n")

datos_finales <- datos_finales |>
  mutate(
    # Usamos los rangos que definimos arriba
    Temp = if_else(Temp < rango_temp[1] | Temp > rango_temp[2], NA_real_, Temp),
    PNM = if_else(PNM < rango_pnm[1] | PNM > rango_pnm[2], NA_real_, PNM),
    DD = if_else(DD < rango_dd[1] | DD > rango_dd[2], NA_real_, DD),
    FF = if_else(FF < rango_ff[1] | FF > rango_ff[2], NA_real_, FF),
    Precipitacion_mm = if_else(Precipitacion_mm < rango_precip[1] | Precipitacion_mm > rango_precip[2], NA_real_, Precipitacion_mm)
  )

cat("Limpieza de outliers finalizada (convertidos a NA).\n")
# ============================================================
# PASO 9: IMPUTACIÓN DE VALORES NA
# ============================================================
cat("Iniciando imputación de NAs...\n")

# Variables a imputar (excluimos Hum porque ya no existe)
vars_imputar <- c("Temp", "Precipitacion_mm", "PNM")

# --- 1. Imputación por Estación y Mes ---
datos_imputados <- datos_finales |>
  mutate(Mes = month(Fecha)) |>  
  group_by(Nro, Mes) |> 
  mutate(across(
    all_of(vars_imputar),
    ~ if_else(is.na(.), median(., na.rm = TRUE), .)
  )) |> 
  ungroup()

# --- 2. Imputación por Estación (backup) ---
datos_imputados <- datos_imputados |>
  group_by(Nro) |>
  mutate(across(
    all_of(vars_imputar),
    ~ if_else(is.na(.), median(., na.rm = TRUE), .)
  )) |>
  ungroup()

# # --- 3. Eliminación de filas con NAs restantes (Menos Antartida) ---
# datos_imputados <- datos_imputados |>
#   filter(!(Provincia != "ANTARTIDA" & (
#     is.na(Temp) | is.na(Precipitacion_mm) | is.na(PNM)
#   )))
# 
# cat("Imputación finalizada.\n")

# ============================================================
# PASO 10: EXPORTAR Y GUARDAR
# ============================================================
write_parquet(datos_imputados, "../data/processed/datos_climaticos_unificados_imputados.parquet")
cat("Archivo final creado: datos_climaticos_unificados_imputados.parquet\n")

# --- Resumen general ---
cat("\nResumen general del dataset final:\n")
print(summary(datos_imputados))

# Limpieza de la Memoria - menos datos_imputados
rm(list = setdiff(ls(), "datos_imputados"))
gc()
cat("Memoria limpiada, solo queda 'datos_imputados'.\n")