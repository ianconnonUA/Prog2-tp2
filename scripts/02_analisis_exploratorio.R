## -----------------------------------------------------------------------------
## SECCIÓN 0: Librerías Esenciales
## -----------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("arrow")) install.packages("arrow")
library(arrow)
if (!require("leaflet")) install.packages("leaflet")
library(leaflet)
if (!require("plotly")) install.packages("plotly")
library(plotly)   
if (!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)

options(scipen = 999)

## -----------------------------------------------------------------------------
## SECCIÓN 1: Carga de Datos Crudos
## -----------------------------------------------------------------------------
cat("Paso 1: Cargando datos crudos...\n")
datos_climaticos <- read_parquet("../data/processed/datos_climaticos_unificados_imputados.parquet")
cat("-> Datos crudos cargados.\n")

## -----------------------------------------------------------------------------
## SECCIÓN 2: Limpieza de Datos (Robusta)
## -----------------------------------------------------------------------------
cat("Paso 2: Limpiando y transformando datos...\n")

datos_climaticos_limpios <- datos_climaticos |>
  filter(
    !is.na(Precipitacion_mm), 
    !is.na(Temp),
    !is.na(Hum)
  ) |>
  mutate(
    Precipitacion_mm_Num = ifelse(
      grepl("\\.", Precipitacion_mm, fixed = TRUE), 
      as.numeric(Precipitacion_mm),
      as.numeric(Precipitacion_mm) / 10
    ),
    Temp_Num = as.numeric(Temp),
    Hum_Num = as.numeric(Hum),
    Año = year(Fecha),
    Mes_Num = month(Fecha),
    Mes = floor_date(Fecha, "month")
  ) |>
  filter(
    !is.na(Precipitacion_mm_Num),
    !is.na(Temp_Num),
    !is.na(Hum_Num)
  )

cat("-> Datos limpios y listos para usar.\n")

## -----------------------------------------------------------------------------
## SECCIÓN 3: Preparación de Resúmenes para Gráficos
## -----------------------------------------------------------------------------
cat("Paso 3: Creando resúmenes para los gráficos...\n")

# --- Resumen 1: Totales anuales por estación (necesario para promedios) ---
datos_anuales_estacion <- datos_climaticos_limpios |>
  group_by(Nro, Provincia, Latitud, Longitud, Año) |>
  summarise(Precip_Total_Anual = sum(Precipitacion_mm_Num, na.rm = TRUE),
            .groups = 'drop')

# --- Resumen 2: Promedio climatológico por Estación (Temp, Hum, Precip) ---
# (Para los MAPAS)
datos_estacion_promedio <- datos_climaticos_limpios |>
  group_by(Nro, Provincia, Latitud, Longitud) |>
  summarise(
    Temp_Promedio = mean(Temp_Num, na.rm = TRUE),
    Hum_Promedio = mean(Hum_Num, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  left_join(
    datos_anuales_estacion |>
      group_by(Nro) |>
      summarise(Precip_Promedio_Anual_Est = mean(Precip_Total_Anual, na.rm = TRUE),
                .groups = 'drop'),
    by = "Nro"
  )

# --- Resumen 3: Promedio climatológico por Provincia ---
# (Para el GRÁFICO DE HUMEDAD)
datos_provinciales_promedio <- datos_estacion_promedio |>
  filter(!is.na(Precip_Promedio_Anual_Est)) |> 
  group_by(Provincia) |>
  summarise(
    Hum_Mediana_Prov = median(Hum_Promedio, na.rm = TRUE)
  )

# --- ¡¡CORRECCIÓN AQUÍ!! ---
# --- Resumen 4: Promedio por Provincia SÓLO PARA 2024 ---
# (Para el GRÁFICO DE BARRAS DE PRECIPITACIÓN 2024)
datos_provinciales_2024 <- datos_anuales_estacion |>
  filter(Año == 2024) |>
  group_by(Provincia) |>
  summarise(
    Precip_Promedio_Anual_2024 = mean(Precip_Total_Anual, na.rm = TRUE)
  ) |>
  ungroup()

# --- Resumen 5: Promedio por Día (Temp) ---
datos_diarios_pais <- datos_climaticos_limpios |>
  group_by(Fecha) |>
  summarise(Temp_Media_Pais = mean(Temp_Num, na.rm = TRUE),
            .groups = 'drop')

# --- Resumen 6: Total por Mes (Precip) ---
datos_mensuales_pais <- datos_climaticos_limpios |>
  group_by(Mes) |>
  summarise(Precip_Total_Mensual = sum(Precipitacion_mm_Num, na.rm = TRUE),
            .groups = 'drop')

cat("-> Resúmenes creados (incluyendo el de Precipitación 2024).\n")


## -----------------------------------------------------------------------------
## SECCIÓN 4: Generación de 6 Gráficos Interactivos (HTML)
## -----------------------------------------------------------------------------
cat("Paso 4: Generando 6 archivos HTML...\n")

# --- GRÁFICO 1: Mapa Interactivo de Temperatura (Climatológico) ---
cat("Generando 1/6: Mapa de Temperaturas (Climatológico)...\n")
# (Usa datos_estacion_promedio)
pal_temp <- colorNumeric(palette = "RdBu", domain = datos_estacion_promedio$Temp_Promedio, reverse = TRUE)
mapa_temp <- leaflet(data = datos_estacion_promedio) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 4,
    color = ~pal_temp(Temp_Promedio), stroke = FALSE, fillOpacity = 0.7,
    popup = ~paste("<strong>Estación:</strong>", Nro, "<br>",
                   "<strong>Provincia:</strong>", Provincia, "<br>",
                   "<strong>Temp. Promedio:</strong>", round(Temp_Promedio, 1), "°C")
  ) |>
  addLegend(pal = pal_temp, values = ~Temp_Promedio, opacity = 1,
            title = "Temp. (°C)", position = "bottomright") |>
  fitBounds(lng1 = -73.5, lat1 = -55.5, lng2 = -53.5, lat2 = -21.5)
saveWidget(mapa_temp, "1_mapa_temperatura_climatologico.html")
cat("-> '1_mapa_temperatura_climatologico.html' guardado.\n")


# --- GRÁFICO 2: Mapa Interactivo de Precipitación (Climatológico) ---
cat("Generando 2/6: Mapa de Precipitaciones (Climatológico)...\n")
# (Usa datos_estacion_promedio)
pal_precip <- colorNumeric(palette = "YlGnBu", domain = datos_estacion_promedio$Precip_Promedio_Anual_Est)
mapa_precip <- leaflet(data = datos_estacion_promedio) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 4,
    color = ~pal_precip(Precip_Promedio_Anual_Est), stroke = FALSE, fillOpacity = 0.7,
    popup = ~paste("<strong>Estación:</strong>", Nro, "<br>",
                   "<strong>Provincia:</strong>", Provincia, "<br>",
                   "<strong>Precip. Promedio:</strong>", round(Precip_Promedio_Anual_Est, 0), " mm/año")
  ) |>
  addLegend(pal = pal_precip, values = ~Precip_Promedio_Anual_Est, opacity = 1,
            title = "Precip. Anual (mm)", position = "bottomright") |>
  fitBounds(lng1 = -73.5, lat1 = -55.5, lng2 = -53.5, lat2 = -21.5)
saveWidget(mapa_precip, "2_mapa_precipitacion_climatologico.html")
cat("-> '2_mapa_precipitacion_climatologico.html' guardado.\n")


# --- GRÁFICO 3: Barras Interactivas de Humedad (Climatológico) ---
cat("Generando 3/6: Gráfico de Humedad (Climatológico)...\n")
# (Usa datos_provinciales_promedio)
fig_humedad <- plot_ly(
  data = datos_provinciales_promedio,
  x = ~reorder(Provincia, -Hum_Mediana_Prov),
  y = ~Hum_Mediana_Prov,
  type = 'bar', marker = list(color = ~Hum_Mediana_Prov, colorscale = 'Blues'),
  hovertemplate = ~paste("<b>%{x}</b><br>Humedad: %{y:.1f}%<extra></extra>")
) |>
  layout(title = "Humedad Relativa Mediana por Provincia (Climatológico)",
         xaxis = list(title = "Provincia"), yaxis = list(title = "Humedad Mediana (%)"))
saveWidget(fig_humedad, "3_grafico_humedad_climatologico.html")
cat("-> '3_grafico_humedad_climatologico.html' guardado.\n")


# --- ¡¡GRÁFICO CORREGIDO!! ---
# --- GRÁFICO 4: Barras Interactivas de Precipitación (SOLO 2024) ---
cat("Generando 4/6: Gráfico de Precipitación (SOLO 2024)...\n")
# (Usa el nuevo resumen datos_provinciales_2024)
fig_precip_prov_2024 <- plot_ly(
  data = datos_provinciales_2024,
  x = ~reorder(Provincia, -Precip_Promedio_Anual_2024),
  y = ~Precip_Promedio_Anual_2024,
  type = 'bar', marker = list(color = ~Precip_Promedio_Anual_2024, colorscale = 'YlGnBu'),
  hovertemplate = ~paste("<b>%{x}</b><br>Precipitación 2024: %{y:.0f} mm<extra></extra>")
) |>
  layout(title = "Precipitación Promedio por Provincia (Año 2024)",
         xaxis = list(title = "Provincia"), yaxis = list(title = "Precipitación Promedio (mm)"))
saveWidget(fig_precip_prov_2024, "4_grafico_precipitacion_provincias_2024.html")
cat("-> '4_grafico_precipitacion_provincias_2024.html' guardado.\n")


# --- GRÁFICO 5: Línea Interactiva de Temperatura Diaria ---
cat("Generando 5/6: Gráfico de Temperatura Diaria...\n")
# (Usa datos_diarios_pais)
fig_temp_diaria <- plot_ly(
  data = datos_diarios_pais, x = ~Fecha, y = ~Temp_Media_Pais,
  type = 'scatter', mode = 'lines', line = list(color = 'darkred', width = 1),
  hovertemplate = ~paste("<b>%{x}</b><br>Temp. Media: %{y:.1f}°C<extra></extra>")
) |>
  layout(title = "Temperatura Media Diaria en Argentina (Promedio de estaciones)",
         xaxis = list(title = "Fecha"), yaxis = list(title = "Temperatura Media (°C)"))
saveWidget(fig_temp_diaria, "5_grafico_temperatura_diaria.html")
cat("-> '5_grafico_temperatura_diaria.html' guardado.\n")


# --- GRÁFICO 6: Barras Interactivas de Precipitación Mensual ---
cat("Generando 6/6: Gráfico de Precipitación Mensual...\n")
# (Usa datos_mensuales_pais)
fig_precip_mensual <- plot_ly(
  data = datos_mensuales_pais, x = ~Mes, y = ~Precip_Total_Mensual,
  type = 'bar', marker = list(color = 'steelblue'),
  hovertemplate = ~paste("<b>%{x|%B %Y}</b><br>Precip. Total: %{y:.0f} mm<extra></extra>")
) |>
  layout(title = "Precipitación Mensual Total en Argentina (Suma de estaciones)",
         xaxis = list(title = "Mes", dtick = "M3"), 
         yaxis = list(title = "Precipitación Total (mm)"))
saveWidget(fig_precip_mensual, "6_grafico_precipitacion_mensual.html")
cat("-> '6_grafico_precipitacion_mensual.html' guardado.\n")


cat("\n--- ¡PROCESO COMPLETADO! Revisa tu carpeta por los 6 archivos HTML. ---\n")