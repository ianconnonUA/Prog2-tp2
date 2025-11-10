# PASO 1: INSTALAR Y CARGAR LIBRERÍAS
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("fs")) install.packages("fs")
library(fs)
if (!require("arrow")) install.packages("arrow")
library(arrow)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("leaflet")) install.packages("leaflet")
library(leaflet)
if (!require("ggalluvial")) install.packages("ggalluvial")
library(ggalluvial)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# PASO 2: LISTAR TODOS LOS ARCHIVOS DE DATOS
ruta_a_los_datos <- "../data/raw/smn-data" 

# Creamos una lista con las rutas completas a todos los archivos .txt dentro de esa carpeta.
lista_de_archivos <- dir_ls(path = ruta_a_los_datos, regexp = "\\.txt$")

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
    col_positions = fwf_widths(widths = anchos_de_columna, col_names = nombres_de_columna),
    skip = 3,
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
ruta_precipitacion <- "../data/raw/smn_precipitaciones-1991-2024/smn_precipitaciones-1991-2024.txt"
datos_precipitacion <- read_csv(ruta_precipitacion)

datos_precipitacion <- datos_precipitacion |>
  rename(Precipitacion_mm = "Precipitacion (mm)")

ruta_estaciones <- "../data/raw/smn_estaciones.csv"
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

summary(datos_climaticos)

#-------------------------------------------------------------------------------
# ANALISIS DE LA VARIABLE PRESION ATMOSFERICA Y SU DISTRIBUCION
head(unique(datos_climaticos$PNM), 20)

mi_tabla <- table(datos_climaticos$PNM)
print(mi_tabla)

# conversión a número 
datos_climaticos$PNM <- as.numeric(datos_climaticos$PNM)

datos_climaticos <- datos_climaticos %>%
  mutate(
    PNM = trimws(PNM),
    PNM = if_else(PNM == "NA" | PNM == "", NA_character_, PNM),
    PNM = as.numeric(PNM)
  )

# resumen de tus datos numéricos
summary(datos_climaticos$PNM)

#grafico de distribucion de la presion atmosferica
disitribucion_pnm <- datos_climaticos %>%
  ggplot(aes(x = PNM)) +
  geom_histogram(
    binwidth = 10,  
    fill = "skyblue",
    color = "white"
  ) +
  labs(
    title = "Frecuencia de la Columna PNM",
    subtitle = "Datos climaticos",
    x = "PNM", 
    y = "Frecuencia (Número de Observaciones)"
  ) +
  theme_minimal()
disitribucion_pnm

# vemos si se distribuye en rangos normales
# 1. Definimos los rangos normales de presión (en hPa)
rango_minimo <- 950
rango_maximo <- 1050

# 2. Creamos un NUEVO dataframe,"filtrar" los parámetros normales
datos_pnm_normales <- datos_climaticos %>%
  filter(PNM >= rango_minimo & PNM <= rango_maximo)

# 3. Contamos cuántos valores están FUERA de ese rango
conteo_fuera_de_rango <- datos_climaticos %>%
  filter(PNM < rango_minimo | PNM > rango_maximo | is.na(PNM)) %>%
  nrow() # nrow() cuenta el número de filas

print(paste("Número de observaciones DENTRO del rango normal (950-1050 hPa):", nrow(datos_pnm_normales)))
print(paste("Número de observaciones FUERA del rango normal:", conteo_fuera_de_rango))


#------------------------------------------------------------------------------
# LIMPIEZA Y ANALISIS
table(datos_climaticos$Hum)

datos_limpios <- datos_climaticos %>%
  mutate(
    across(
      .cols = c(Temp, DD, FF, Precipitacion_mm, PNM), # Quitamos Hum de aquí
      .fns = ~ as.numeric(
        na_if(
          trimws(gsub(",", ".", .)), 
          ""
        )
      )
    ),
    
    # 3. Lógica ESPECIAL para Hum
    #    Primero, extraemos solo el primer número (antes del primer espacio)
    #    ej: "61 1 61 3" se convierte en "61"
    Hum_extraido = sub(" .*", "", Hum),
    
    #    Ahora, convertimos ese número extraído
    Hum = as.numeric(Hum_extraido)
  )
  

summary(datos_limpios)

# TEMPERATURAS DISTRIBUIDAS EN MAPA Y POR ESTACION
datos_por_estacion <- datos_limpios %>%
  mutate(
    Latitud = as.numeric(gsub(",", ".", Latitud)),
    Longitud = as.numeric(gsub(",", ".", Longitud))
  ) %>%
  group_by(Nombre, Latitud, Longitud) %>%
  summarise(
    Temp_media = mean(Temp, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(Latitud) & !is.na(Longitud)) 

paleta_color <- colorNumeric(
  palette = "YlOrRd",
  domain = datos_por_estacion$Temp_media # El rango de valores (de tu min a tu max)
)

leaflet(data = datos_por_estacion) %>%
  addTiles() %>% 
  addCircleMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    color = ~paleta_color(Temp_media),
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~paste(
      Nombre, " | ", 
      "Temp. Media:", round(Temp_media, 1), "°C"
    ),
    popup = ~paste(
      "<b>Estación:</b>", Nombre, "<br>",
      "<b>Temp. Media:</b>", round(Temp_media, 1), "°C"
    )
  ) %>%
  addLegend(
    pal = paleta_color,
    values = ~Temp_media,
    title = "Temp. Media (°C)",
    position = "bottomright"
  )


# ROSA DE LOS VIENTOS
datos_viento <- datos_limpios %>%
  filter(!is.na(DD) & !is.na(FF)) %>% # Quitar NAs de viento
  mutate(
    # --- Bin de Velocidad (FF) ---
    # Usamos "1.", "2." al inicio para que se ordenen solos
    FF_rango = case_when(
      FF < 5 ~ "1. 0-5", 
      FF < 15 ~ "2. 5-15",
      FF < 25 ~ "3. 15-25",
      TRUE ~ "4. 25+"
    ),
    DD_rango = case_when(
      (DD > 337.5 | DD <= 22.5) ~ "N",
      (DD > 22.5 & DD <= 67.5) ~ "NE",
      (DD > 67.5 & DD <= 112.5) ~ "E",
      (DD > 112.5 & DD <= 157.5) ~ "SE",
      (DD > 157.5 & DD <= 202.5) ~ "S",
      (DD > 202.5 & DD <= 247.5) ~ "SO",
      (DD > 247.5 & DD <= 292.5) ~ "O",
      (DD > 292.5 & DD <= 337.5) ~ "NO",
      TRUE ~ "NA"
    )
  ) %>%
  filter(DD_rango != "NA") 

datos_viento_summary <- datos_viento %>%
  group_by(DD_rango, FF_rango) %>%
  summarise(count = n(), .groups = 'drop')

plot_ly(datos_viento_summary,
        r = ~count,           # 'r' es el radio (la frecuencia)
        theta = ~DD_rango,      # 'theta' es el ángulo (dirección)
        color = ~FF_rango,      # Apilamos por rango de velocidad
        type = 'barpolar',      # Tipo de gráfico: barras polares
        barmode = 'stack') %>%  # Modo apilado
  plotly::layout(
    title = "Rosa de los Vientos Interactiva",
    # Ordenamos los ejes para que tengan sentido (N, NE, E...)
    polar = list(
      angularaxis = list(
        categoryarray = c("N", "NE", "E", "SE", "S", "SO", "O", "NO")
      )
    )
  )

# PRECIPITACION TOTAL POR MES
precipitacion_mensual <- datos_limpios %>%
  filter(!is.na(Precipitacion_mm) & Precipitacion_mm > 0) %>%
  mutate(Mes = month(Fecha, label = TRUE, abbr = FALSE)) %>%
  group_by(Mes) %>%
  summarise(Precipitacion_Total = sum(Precipitacion_mm, na.rm = TRUE))

plot_ly(precipitacion_mensual,
        x = ~Mes,
        y = ~Precipitacion_Total,
        type = 'bar') %>%
  plotly::layout(
    title = "Precipitación Total por Mes",
    xaxis = list(title = "Mes"),
    yaxis = list(title = "Precipitación Total (mm)")
  )

# PRECIPITACION TOTAL POR MES Y REGION
datos_limpios <- datos_limpios %>%
  mutate(
    Latitud = as.numeric(gsub(",", ".", Latitud)),
    Longitud = as.numeric(gsub(",", ".", Longitud))
  )

medianas <- datos_limpios %>%
  filter(!is.na(Latitud) & !is.na(Longitud)) %>%
  distinct(Nombre, Latitud, Longitud) %>% # Usamos solo estaciones únicas
  summarise(
    lat_mediana = median(Latitud, na.rm = TRUE),
    lon_mediana = median(Longitud, na.rm = TRUE)
  )

datos_con_region <- datos_limpios %>%
  mutate(
    Region = case_when(
      Latitud > medianas$lat_mediana & Longitud > medianas$lon_mediana ~ "Norte-Este",
      Latitud > medianas$lat_mediana & Longitud <= medianas$lon_mediana ~ "Norte-Oeste",
      Latitud <= medianas$lat_mediana & Longitud > medianas$lon_mediana ~ "Sur-Este",
      Latitud <= medianas$lat_mediana & Longitud <= medianas$lon_mediana ~ "Sur-Oeste",
      TRUE ~ "Ubicación Desconocida" # Para estaciones sin Lat/Lon
    )
  )

precipitacion_regional_mensual <- datos_con_region %>%
  filter(!is.na(Precipitacion_mm) & Precipitacion_mm > 0) %>%
  mutate(Mes = month(Fecha, label = TRUE, abbr = FALSE)) %>%
  group_by(Region, Mes) %>% # <-- ¡Agrupamos por Región Y Mes!
  summarise(Precipitacion_Total = sum(Precipitacion_mm, na.rm = TRUE), .groups = 'drop') %>%
  filter(Region != "Ubicación Desconocida") # Quitamos estaciones sin región

plot_ly(precipitacion_regional_mensual,
        x = ~Mes,
        y = ~Precipitacion_Total,
        color = ~Region,  # <-- ¡La clave! Separa por color
        type = 'bar') %>%
  plotly::layout(
    title = "Precipitación Total por Mes y Región",
    xaxis = list(title = "Mes"),
    yaxis = list(title = "Precipitación Total (mm)"),
    barmode = 'group' # 'group' = barras lado a lado
    # 'stack' = barras apiladas
  )

# -----------------------------------------------------------------------------
# RELACONADO CON AGRO

# NIVEL DE HUMEDAD
# se buscan zonas con buena ventilación (viento) y menos horas de rocío.

UMBRAL_PELIGRO <- 90

ranking_riesgo_humedad <- datos_limpios %>%
  filter(!is.na(Hum) & !is.na(Nombre)) %>% # Quitar NAs
  group_by(Nombre) %>%
  summarise(
    Total_Horas = n(), 
    Horas_Riesgo_Alto = sum(Hum >= UMBRAL_PELIGRO, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(
    Porcentaje_Riesgo = (Horas_Riesgo_Alto / Total_Horas) * 100
  ) %>%
  arrange(Porcentaje_Riesgo)

datos_grafico <- ranking_riesgo_humedad %>%
  head(10)

g <- ggplot(datos_grafico, 
            aes(x = reorder(Nombre, Porcentaje_Riesgo), 
                y = Porcentaje_Riesgo,
                fill = Nombre)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Top 10 Estaciones con MENOR Riesgo por humedad",
    subtitle = "Estaciones con el menor porcentaje de horas > 90% de humedad",
    x = "Estación", 
    y = "Porcentaje de Horas de Riesgo (Hum > 90%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Porcentaje_Riesgo, 1), "%")), 
            vjust = -0.5, size = 3) +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank() 
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)) 
  )
ggplotly(g)


# RIESGO CLIMATICO 

amplitud_diaria_completa <- datos_limpios %>%
  filter(!is.na(Temp)) %>%
  # Usamos Datetime, que es más robusto que Fecha
  mutate(Dia = floor_date(Fecha, "day")) %>% 
  group_by(Nombre, Dia) %>%
  summarise(
    Temp_min = min(Temp, na.rm = TRUE),
    Temp_max = max(Temp, na.rm = TRUE), 
    .groups = 'drop'
  )

MESES_PRIMAVERA <- c(9, 10, 11) 

todas_las_estaciones <- amplitud_diaria_completa %>%
  distinct(Nombre)

estaciones_con_heladas <- amplitud_diaria_completa %>%
  filter(
    Temp_min <= 0 & 
      month(Dia) %in% MESES_PRIMAVERA
  ) %>%
  group_by(Nombre) %>%
  summarise(
    Total_Dias_Helada_Tardia = n()
  )

ranking_seguridad <- todas_las_estaciones %>%
  left_join(estaciones_con_heladas, by = "Nombre") %>%
  mutate(
    Total_Dias_Helada_Tardia = ifelse(is.na(Total_Dias_Helada_Tardia), 0, Total_Dias_Helada_Tardia)
  )

UMBRAL_CALOR <- 35 

estaciones_con_calor <- amplitud_diaria_completa %>%
  filter(Temp_max >= UMBRAL_CALOR) %>% 
  group_by(Nombre) %>%
  summarise(
    Total_Dias_Calor_Extremo = n()
  )

ranking_calor_completo <- todas_las_estaciones %>%
  left_join(estaciones_con_calor, by = "Nombre") %>%
  mutate(
    Total_Dias_Calor_Extremo = ifelse(is.na(Total_Dias_Calor_Extremo), 0, Total_Dias_Calor_Extremo)
  )

ranking_total_agro <- ranking_seguridad %>%
  left_join(ranking_calor_completo, by = "Nombre")

UMBRAL_MAX_HELADAS = 5  
UMBRAL_MAX_CALOR = 15 

estaciones_ideales <- ranking_total_agro %>%
  filter(
    Total_Dias_Helada_Tardia <= UMBRAL_MAX_HELADAS &
      Total_Dias_Calor_Extremo <= UMBRAL_MAX_CALOR
  ) %>%
  arrange(Total_Dias_Helada_Tardia)

g <- ggplot(estaciones_ideales,
                  aes(
                    x = reorder(Nombre, Total_Dias_Helada_Tardia),
                    y = Total_Dias_Helada_Tardia,
                    text = paste0(
                      "<b>📍 Estación:</b> ", Nombre,
                      "<br><b> Días con Helada:</b> ", Total_Dias_Helada_Tardia,
                      "<br><b>️ Días con Calor Extremo:</b> ", Total_Dias_Calor_Extremo
                    )
                  )) +
  geom_segment(
    aes(xend = Nombre, yend = 0),
    color = "gray70",
    linewidth = 0.7
  ) +
  geom_point(
    aes(color = Total_Dias_Calor_Extremo, size = Total_Dias_Calor_Extremo),
    alpha = 0.9
  ) +
  scale_color_gradient(
    low = "#4C9BE8", high = "#E87D1F",
    name = "Días de Calor Extremo"
  ) +
  scale_size(range = c(3, 8), guide = "none") +
  labs(
    title = "🌾 Estaciones Ideales para el Agro",
    subtitle = paste0(
      "Filtradas por < ", UMBRAL_MAX_HELADAS, " días de helada y < ",
      UMBRAL_MAX_CALOR, " días de calor extremo"
    ),
    x = "Estación Meteorológica",
    y = "Total de Días de Helada (Septiembre–Noviembre)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2E4057"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.title = element_text(size = 11, color = "#444444"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right"
  ) +
  coord_flip()

ggplotly(g, tooltip = "text") %>%
  layout(
    hoverlabel = list(bgcolor = "white", font = list(color = "#1B263B")),
    title = list(
      text = paste0(
        "<b>🌾 Estaciones Ideales para el Agro</b><br>"
      )
    )
  )
