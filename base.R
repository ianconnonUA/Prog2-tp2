# PASO 1: INSTALAR Y CARGAR LIBRERÍAS
# Si no tienes tidyverse instalado, descomenta y ejecuta la siguiente línea:
# install.packages("tidyverse")

library(tidyverse)
library(fs)
library(arrow)
library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# PASO 2: LISTAR TODOS LOS ARCHIVOS DE DATOS
# Actualizamos la ruta para que apunte a tu carpeta "smn-data".
# El script asume que la carpeta "smn-data" está en el mismo lugar que tu archivo de R.
# Si no es así, reemplázala por la ruta completa.
ruta_a_los_datos <- "smn-data" 

# Creamos una lista con las rutas completas a todos los archivos .txt dentro de esa carpeta.
lista_de_archivos <- dir_ls(path = ruta_a_los_datos, regexp = "\\.txt$")

# Opcional: verifica que encontró los archivos.
# print(head(lista_de_archivos))

# PASO 3: DEFINIR LA ESTRUCTURA EXACTA DEL ARCHIVO
# Basado en el análisis de tu archivo, estos son los anchos correctos.
# La suma total es 100.
anchos_de_columna <- c(
  FECHA = 10,
  HORA = 6,
  TEMP = 6,
  HUM = 6,
  PNM = 6,
  DD = 6,
  FF = 6,
  NOMBRE = 54
)
# Los nombres de las columnas que usaremos
nombres_de_columna <- c("Fecha", "Hora", "Temp", "Hum", "PNM", "DD", "FF", "Nombre")


# PASO 4: LEER Y COMBINAR TODOS LOS ARCHIVOS
cat("Iniciando la lectura de", length(lista_de_archivos), "archivos... Esto puede tardar unos minutos.\n")

datos_unificados <- map_dfr(
  lista_de_archivos,
  ~ read_fwf(
    file = .x,
    # Usamos fwf_widths para definir la estructura
    col_positions = fwf_widths(widths = anchos_de_columna, col_names = nombres_de_columna),
    # ¡IMPORTANTE! Saltamos las primeras 3 líneas de cada archivo (encabezado)
    skip = 3,
    # Leemos todo como texto para máxima compatibilidad con datos sucios
    col_types = cols(.default = "c")
  )
)

cat("¡Lectura completada! Se han procesado", nrow(datos_unificados), "filas.\n")


#Ahora Eliminamos los registros con fecha NA
datos_unificados <- datos_unificados |>
  filter(!is.na(Fecha))
cat("¡Proceso finalizado!\n")

# Opcional: revisa cómo quedaron los datos finales
glimpse(datos_unificados)
summary(datos_unificados)

#Ver valores unicos de la columna NOMBRE
unique_nombres <- unique(datos_unificados$Nombre)
unique_nombres

cat("Iniciando limpieza de la columna 'Nombre'...\n")

#
# LA LIMPIEZA DE LOS NOMBRES SE LOGRO YA QUE AL REVISAR LOS VALORES VIMOS QUE ALGUNOS NO COINCIDIAN CON DATOS_ESTACIONES
#

# Se encontraron nombres anormales en la columna Nombre
lista_nombres_borrar = c("29/7/2018", "}", "28")
datos_unificados <- datos_unificados |>
  filter(!Nombre %in% lista_nombres_borrar)

# Corrijo nombres con case_when
datos_unificados <- datos_unificados |>
  mutate(
    Nombre = case_when(
      Nombre == "ESCUELA DE AVIACION MILITA" ~ "ESCUELA DE AVIACION MILITAR AERO",
      Nombre == "PRESIDENCIA ROQUE SAENZ PE" ~ "PRESIDENCIA ROQUE SAENZ PEÑA AERO",
      Nombre == "PCIA. ROQUE SAENZ PE\xd1A AER" ~ "PRESIDENCIA ROQUE SAENZ PEÑA AERO",
      Nombre == "VILLA MARIA DEL RIO SECO" ~ "VILLA DE MARIA DEL RIO SECO",
      Nombre == "VILLA DE MARIA DEL RIO SEC" ~ "VILLA DE MARIA DEL RIO SECO",
      TRUE ~ Nombre
    )
  )

# Corrijo todos los que tengan en el nombre obs.
datos_unificados <- datos_unificados |>
  mutate(
    Nombre = str_replace_all(Nombre, regex("OBS\\.", ignore_case = TRUE), "OBSERVATORIO")
  )

# Ahora cambio "ESC.AVIACION MILITAR AERO"
datos_unificados <- datos_unificados |>
  mutate(
    Nombre = str_replace_all(Nombre, regex("ESC\\.AVIACION MILITAR AERO", ignore_case = TRUE), "ESCUELA DE AVIACION MILITAR AERO")
  )

#Sustituimos las flores aero por las flores, obera aero por obera, venado tuerto por venado tuerto aero, san fernando por san fernando aero
datos_unificados <- datos_unificados |>
  mutate(
    Nombre = case_when(
      Nombre == "LAS FLORES AERO" ~ "LAS FLORES",
      Nombre == "OBERA AERO" ~ "OBERA",
      Nombre == "VENADO TUERTO" ~ "VENADO TUERTO AERO",
      Nombre == "SAN FERNANDO" ~ "SAN FERNANDO AERO",
      Nombre == "BUENOS AIRES" ~ "BUENOS AIRES OBSERVATORIO",  
      TRUE ~ Nombre
    )
  )

cat("Limpieza de 'Nombre' finalizada.\n")


# 2. LEVANTAMOS LOS DATASETS ADICIONALES
ruta_precipitacion <- "smn_precipitaciones-1991-2024/smn_precipitaciones-1991-2024.txt"
datos_precipitacion <- read_csv(ruta_precipitacion)

datos_precipitacion <- datos_precipitacion |>
  rename(Precipitacion_mm = "Precipitacion (mm)")

ruta_estaciones <- "smn_estaciones.csv"
datos_estaciones <- read_csv(ruta_estaciones)

#Ver datos preciciptaciones glimpse(datos_precipitacion)
glimpse(datos_precipitacion)

#Ver rangos de Precipitacion (mm)
summary(datos_precipitacion$`Precipitacion_mm`)

# Borrar datos con /n de precipitacion
datos_precipitacion <- datos_precipitacion |>
  filter(`Precipitacion_mm` != "\\N")

# 3. HACEMOS LOS JOINS

# Agrego columna Nro a datos_unificados
datos_unificados <- datos_unificados |>
  left_join(
    datos_estaciones |> select(Nombre, Nro),
    by = c("Nombre" = "Nombre")
  )

# 4. VERIFICAMOS EL RESULTADO FINAL
cat("Verificación final de nombres no relacionados:\n")
nombres_no_relacionados <- datos_unificados |>
  filter(is.na(Nro)) |>
  distinct(Nombre)

# Imprimimos el resultado final
print(nombres_no_relacionados)

#Hacemos una limpieza de las variables que no vamos a volver a usar
rm(nombres_no_relacionados)
rm(anchos_de_columna)
rm(lista_de_archivos)
rm(ruta_a_los_datos)
rm(ruta_estaciones)
rm(ruta_precipitacion)
rm(unique_nombres)
rm(lista_nombres_borrar)
rm(nombres_de_columna)

# Nos queda corregir las fechas en datos_unificados
datos_unificados <- datos_unificados |>
  mutate(Fecha = dmy(Fecha))

# Voy a hacer un join de datos_precipictacion, datos_estaciones y los voy a agregar a datos unificados
datos_unificados <- datos_unificados |>
  left_join(
    datos_precipitacion |> select(Estacion, Fecha, Precipitacion_mm),
    # CORRECCIÓN: "Nro" (de la izq.) debe unirse con "Estacion" (de la der.)
    by = c("Nro" = "Estacion", "Fecha" = "Fecha")
  ) |>
  left_join(
    datos_estaciones |> select(Nro, Latitud, Longitud, Altura, Provincia),
    # Esta parte estaba bien, ya que ambos tienen la columna "Nro"
    by = c("Nro" = "Nro")
  )

#Pasar todos los datos a su tipo de dato predominante y los que son numericos a numericos
datos_unificados <- datos_unificados |>
  mutate(
    Temp = as.numeric(Temp),
    Hum = as.numeric(Hum),
    PNM = as.numeric(PNM),
    DD = as.numeric(DD),
    FF = as.numeric(FF),
    Precipitacion_mm = as.numeric(Precipitacion_mm),
    Latitud = as.numeric(Latitud),
    Longitud = as.numeric(Longitud),
    Altura = as.numeric(Altura)
  )

#No se encontraron datos de precipitacion para Antartida

write_parquet(datos_unificados, "datos_climaticos_unificados.parquet")

#Limpieza del entorno para mejor manejo de memoria
rm(datos_estaciones)
rm(datos_precipitacion)
rm(datos_unificados)
cat("Archivo 'datos_climaticos_unificados.parquet' creado exitosamente.\n")


# Carga del archivo Parquet para verificación
datos_climaticos <- read_parquet("datos_climaticos_unificados.parquet")
glimpse(datos_climaticos)

#Valores unicos en precipitacion
unique_precipitacion <- unique(datos_climaticos$Precipitacion_mm)
# Contar NA y \\N en precipitaciones
na_count <- sum(is.na(datos_climaticos$Precipitacion_mm))
backslash_n_count <- sum(datos_climaticos$Precipitacion_mm == "\\N", na.rm = TRUE)

