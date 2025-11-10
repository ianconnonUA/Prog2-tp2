# ============================================================
# PASO 1: LIBRERÍAS Y CONFIGURACIÓN
# ============================================================
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

# ============================================================
# PASO 2: CARGA DE ARCHIVOS DE TEXTO DEL SMN
# ============================================================
# ruta_a_los_datos <- "../data/raw/smn-data"
# lista_de_archivos <- dir_ls(path = ruta_a_los_datos, regexp = "\\.txt$")
# 
# anchos_de_columna <- c(FECHA = 10, HORA = 6, TEMP = 6, HUM = 6, PNM = 6, DD = 6, FF = 6, NOMBRE = 54)
# nombres_de_columna <- c("Fecha", "Hora", "Temp", "Hum", "PNM", "DD", "FF", "Nombre")
# 
# cat("Iniciando lectura de archivos...\n")
# 
# datos_unificados <- map_dfr(
#   lista_de_archivos,
#   ~ read_fwf(
#     file = .x,
#     col_positions = fwf_widths(widths = anchos_de_columna, col_names = nombres_de_columna),
#     skip = 3,
#     col_types = cols(.default = "c")
#   )
# ) |> 
#   filter(!is.na(Fecha))
# 
# cat("Archivos leídos correctamente. Total filas:", nrow(datos_unificados), "\n")
# 
# # 🔍 Resumen inicial (datos crudos)
# summary(datos_unificados)
# # Guardar datos unificados como parquet intermedio
# write_parquet(datos_unificados, "datos_climaticos_unificados_crudo.parquet")

#===========================================================
# PASO 2.5: Cargar datos unificados desde parquet intermedio
#===========================================================
datos_unificados <- read_parquet("../data/processed/datos_climaticos_unificados_crudo.parquet")

# ============================================================
# PASO 3: LIMPIEZA DE LA COLUMNA 'Nombre'
# ============================================================
datos_unificados <- datos_unificados |> 
  filter(!Nombre %in% c("29/7/2018", "}", "28")) |> 
  mutate(
    Nombre = case_when(
      Nombre == "ESCUELA DE AVIACION MILITA" ~ "ESCUELA DE AVIACION MILITAR AERO",
      Nombre == "PRESIDENCIA ROQUE SAENZ PE" ~ "PRESIDENCIA ROQUE SAENZ PEÑA AERO",
      Nombre == "PCIA. ROQUE SAENZ PE\xd1A AER" ~ "PRESIDENCIA ROQUE SAENZ PEÑA AERO",
      Nombre == "VILLA MARIA DEL RIO SECO" ~ "VILLA DE MARIA DEL RIO SECO",
      Nombre == "VILLA DE MARIA DEL RIO SEC" ~ "VILLA DE MARIA DEL RIO SECO",
      TRUE ~ Nombre
    ),
    Nombre = str_replace_all(Nombre, regex("OBS\\.", ignore_case = TRUE), "OBSERVATORIO"),
    Nombre = str_replace_all(Nombre, regex("ESC\\.AVIACION MILITAR AERO", ignore_case = TRUE), "ESCUELA DE AVIACION MILITAR AERO"),
    Nombre = case_when(
      Nombre == "LAS FLORES AERO" ~ "LAS FLORES",
      Nombre == "OBERA AERO" ~ "OBERA",
      Nombre == "VENADO TUERTO" ~ "VENADO TUERTO AERO",
      Nombre == "SAN FERNANDO" ~ "SAN FERNANDO AERO",
      Nombre == "BUENOS AIRES" ~ "BUENOS AIRES OBSERVATORIO",
      TRUE ~ Nombre
    )
  )

# ============================================================
# PASO 4: CARGA DE DATOS ADICIONALES
# ============================================================
datos_precipitacion <- read_csv("../data/raw/smn_precipitaciones-1991-2024.txt") |> 
  rename(Precipitacion_mm = "Precipitacion (mm)")

summary(datos_precipitacion)

# Ver tipos de valores en PRecipitacion_mm
unique(datos_precipitacion$Precipitacion_mm)

#Cambiar los \\N por NA y convertir a numérico
datos_precipitacion <- datos_precipitacion |> 
  mutate(Precipitacion_mm = na_if(Precipitacion_mm, "\\N")) |> 
  mutate(Precipitacion_mm = as.numeric(Precipitacion_mm))

datos_estaciones <- read_csv("../data/raw/smn_estaciones.csv")

summary(datos_estaciones)

# ============================================================
# PASO 5: JOINS Y UNIFICACIÓN FINAL
# ============================================================
datos_unificados <- datos_unificados |> 
  left_join(datos_estaciones |> select(Nombre, Nro), by = "Nombre") |> 
  mutate(Fecha = dmy(Fecha)) |> 
  left_join(datos_precipitacion |> select(Estacion, Fecha, Precipitacion_mm),
            by = c("Nro" = "Estacion", "Fecha" = "Fecha")) |> 
  left_join(datos_estaciones |> select(Nro, Latitud, Longitud, Altura, Provincia), by = "Nro")

# 🔍 Resumen después de los joins
summary(datos_unificados)

# ============================================================
# PASO 6: CONVERSIÓN DE TIPOS
# ============================================================
datos_unificados <- datos_unificados |> 
  mutate(across(c(Temp, Hum, PNM, DD, FF, Precipitacion_mm, Latitud, Longitud, Altura), as.numeric))

# 🔍 Resumen tras conversión de tipos
summary(datos_unificados)

#Variables que se encuentran en rango
#Precipitación: 0 a 500 mm (valores mayores son errores)
#Temperatura: -30 a 50 °C (valores fuera de este rango son errores)
#FF (Velocidad Viento)
#PNM (Presión): Los datos de la presion son horribles y totalmente fuera de rango, por lo tanto solo lo limpiaremos en el caso de graficar algo

# ============================================================
# PASO 6.5: LIMPIEZA DE OUTLIERS (¡ANTES DE IMPUTAR!)
# ============================================================
cat("Iniciando limpieza de valores atípicos (outliers)...\n")

datos_unificados <- datos_unificados |>
  mutate(
    #Volvemos NA los outliers de DD
    DD = if_else(DD < 0 | DD > 360, NA_real_, DD)
  )

cat("Limpieza de outliers finalizada. Variable 'Hum' eliminada.\n")

print(summary(datos_unificados))



# ============================================================
# PASO 7: IMPUTACIÓN DE VALORES NA (AHORA SÍ)
# ============================================================
cat("Iniciando imputación de NAs...\n")

# Variables a imputar con mediana (incluimos Precipitacion_mm)
vars_imputar <- c("Temp", "DD", "FF", "Precipitacion_mm")

# --- 1. Imputación por Estación y Mes ---
datos_unificados <- datos_unificados |>
  mutate(Mes = month(Fecha)) |>  
  group_by(Nro, Mes) |> 
  mutate(across(
    all_of(vars_imputar),
    ~ if_else(is.na(.), median(., na.rm = TRUE), .)
  )) |> 
  ungroup()

# --- 2. Imputación por Estación (backup si faltaba todo el mes) ---
datos_unificados <- datos_unificados |>
  group_by(Nro) |>
  mutate(across(
    all_of(vars_imputar),
    ~ if_else(is.na(.), median(., na.rm = TRUE), .)
  )) |>
  ungroup()
# --- 3. Eliminación de filas con NAs restantes (Menos de la Antartida) ---
datos_unificados <- datos_unificados |>
  filter(!(Provincia != "ANTARTIDA" & (is.na(Temp) | is.na(DD) | is.na(FF) | is.na(Precipitacion_mm))))


cat("Imputación finalizada")

# --- Resumen general ---
cat("\nResumen general de variables (post-limpieza y post-imputación):\n")
print(summary(datos_unificados))

# Imputación y depuración de valores faltantes:
#   Se imputaron los valores faltantes mediante la mediana por estación y mes, manteniendo la coherencia local en cada variable climática.
# Si una estación no tenía datos válidos en un mes, se aplicó una imputación por mediana total de la estación.
# Finalmente, se eliminaron los registros restantes con valores faltantes, para evitar introducir información artificial y asegurar la consistencia del dataset final.
# Se decidio no eliminar los datos de la Antartida ya que tenian muchos NA y podrian ser utiles para analisis futuros.

# ============================================================
# PASO 8: EXPORTAR Y GUARDAR
# ============================================================
write_parquet(datos_unificados, "../data/processed/datos_climaticos_unificados_imputados.parquet")
cat("Archivo final creado: datos_climaticos_unificados_imputados.parquet\n")

# ============================================================
# Liberar memoria
# ============================================================
# 
# rm(list = ls())
# gc()
# cat("Memoria liberada.\n")
