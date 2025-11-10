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
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

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
    Latitud = as.numeric(gsub(",", ".", Latitud)),   # <--- IMPORTANTE
    Longitud = as.numeric(gsub(",", ".", Longitud)), # <--- IMPORTANTE
    Precipitacion_mm_Num = ifelse(
      grepl(".", Precipitacion_mm, fixed = TRUE),    # <--- CORREGIDO
      as.numeric(Precipitacion_mm),
      as.numeric(Precipitacion_mm) / 10
    ),
    Temp_Num = as.numeric(Temp),
    Hum_Num = as.numeric(sub(" .*", "", Hum)),       
    DD_Num = as.numeric(DD),
    FF_Num = as.numeric(FF)
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
  group_by(Nro, Nombre, Provincia, Latitud, Longitud, Año = year(Fecha)) |>      # <--- Se crea 'Año' solo para este agrupamiento
  summarise(Precip_Total_Anual = sum(Precipitacion_mm, na.rm = TRUE),
            .groups = 'drop')

# --- Resumen 2: Promedio climatológico por Estación (Temp, Hum, Precip) ---
# (Para los MAPAS)
datos_estacion_promedio <- datos_climaticos_limpios |>
  group_by(Nro, Nombre, Provincia, Latitud, Longitud) |>
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

## --------------------------------------------------------------------------
## SECCIÓN 3.2
## --------------------------------------------------------------------------

# Viento
datos_viento_summary <- datos_climaticos_limpios |>
  filter(!is.na(DD_Num), !is.na(FF_Num)) |>
  mutate(
    FF_rango = case_when(
      FF_Num < 5 ~ "1. Calma (0-5 km/h)",
      FF_Num < 15 ~ "2. Leve (5-15 km/h)",
      FF_Num < 25 ~ "3. Moderado (15-25 km/h)",
      TRUE ~ "4. Fuerte (>25 km/h)"
    ),
    DD_rango = case_when(
      (DD_Num > 337.5 | DD_Num <= 22.5) ~ "N",
      (DD_Num > 22.5 & DD_Num <= 67.5) ~ "NE",
      (DD_Num > 67.5 & DD_Num <= 112.5) ~ "E",
      (DD_Num > 112.5 & DD_Num <= 157.5) ~ "SE",
      (DD_Num > 157.5 & DD_Num <= 202.5) ~ "S",
      (DD_Num > 202.5 & DD_Num <= 247.5) ~ "SO",
      (DD_Num > 247.5 & DD_Num <= 292.5) ~ "O",
      (DD_Num > 292.5 & DD_Num <= 337.5) ~ "NO",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(DD_rango)) |>
  count(DD_rango, FF_rango)

# Agro - Riesgo Heladas/Calor
MESES_PRIMAVERA <- c(9, 10, 11)
UMBRAL_CALOR <- 35

agro_diario <- datos_climaticos_limpios |>
  group_by(Nro,Nombre, Dia = floor_date(Fecha, "day")) |>
  summarise(
    Temp_min = min(Temp_Num, na.rm = TRUE),
    Temp_max = max(Temp_Num, na.rm = TRUE),
    .groups = 'drop'
  )

ranking_agro <- agro_diario |>
  group_by(Nro, Nombre) |>
  summarise(
    # Días con helada (Tmin <= 0) en primavera
    Dias_Helada_Tardia = sum(Temp_min <= 0 & month(Dia) %in% MESES_PRIMAVERA, na.rm = TRUE),
    # Días con calor extremo (Tmax >= 35) todo el año
    Dias_Calor_Extremo = sum(Temp_max >= UMBRAL_CALOR, na.rm = TRUE)
  ) |>
  # Filtro para "Estaciones Ideales" (ajustable)
  filter(Dias_Helada_Tardia <= 5, Dias_Calor_Extremo <= 20) |>
  arrange(Dias_Helada_Tardia)

cat("-> Todos los resúmenes creados.\n")

# --- Resumen 7: Índice de Sequía (Días consecutivos sin lluvia) ---
# (Aproximación simple para identificar zonas propensas a sequía)
datos_sequia <- datos_climaticos_limpios |>
  arrange(Nro, Fecha) |>
  group_by(Nro, Nombre, Provincia, Latitud, Longitud) |>
  mutate(
    Dia_Sin_Lluvia = ifelse(Precipitacion_mm_Num < 1, 1, 0),
    # Truco para contar rachas: agrupa por cada vez que llueve
    Racha_ID = cumsum(Dia_Sin_Lluvia == 0)
  ) |>
  filter(Dia_Sin_Lluvia == 1) |> # Quedarse solo con días secos
  count(Racha_ID, name = "Dias_Consecutivos_Secos") |>
  summarise(
    Max_Dias_Sin_Lluvia = max(Dias_Consecutivos_Secos, na.rm = TRUE),
    Promedio_Rachas_Secas = mean(Dias_Consecutivos_Secos, na.rm = TRUE),
    .groups = 'drop'
  )

cat("-> Resumen de Sequía creado.\n")

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
saveWidget(mapa_temp, "../outputs/graficos/mapas/1_mapa_temperatura_climatologico.html")
cat("-> '1_mapa_temperatura_climatologico.html' guardado.\n")

mapa_temp


# --- GRÁFICO 2: Mapa Interactivo de Precipitación (Climatológico) ---
cat("Generando Mapa de Precipitaciones (Climatológico)...\n")
# (Usa datos_estacion_promedio)
# Cambiamos la paleta a "Blues" para tonos más oscuros, o "BuGn"
pal_precip <- colorNumeric(palette = c("#ADD8E6", "#00008B"), domain = datos_estacion_promedio$Precip_Promedio_Anual_Est) # O "Blues"
mapa_precip <- leaflet(data = datos_estacion_promedio) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 4,
    color = "black",            # Color del borde (por ejemplo, negro)
    weight = 0.5,                 # Grosor del borde
    stroke = TRUE, 
    fillColor = ~pal_precip(Precip_Promedio_Anual_Est), # Color de relleno (usamos fillColor en lugar de color para el relleno)
    fillOpacity = 0.7,
    popup = ~paste("<strong>Estación:</strong>", Nro, "<br>",
                   "<strong>Provincia:</strong>", Provincia, "<br>",
                   "<strong>Precip. Promedio:</strong>", round(Precip_Promedio_Anual_Est, 0), " mm/año")
  ) |>
  addLegend(pal = pal_precip, values = ~Precip_Promedio_Anual_Est, opacity = 1,
            title = "Precip. Anual (mm)", position = "bottomright") |>
  fitBounds(lng1 = -73.5, lat1 = -55.5, lng2 = -53.5, lat2 = -21.5)
saveWidget(mapa_precip, "../outputs/graficos/mapas/2_mapa_precipitacion_climatologico.html")
cat("-> '2_mapa_precipitacion_climatologico.html' guardado.\n")

mapa_precip


# --- GRÁFICO: Mapa Combinado (Temp/Precip) --- (quedarse con este o con los otros dos por separado)
cat("Generando Mapa Combinado (Temp/Precip)...\n")
pal_temp <- colorNumeric(palette = "RdBu", domain = datos_estacion_promedio$Temp_Promedio, reverse = TRUE)
pal_precip <- colorNumeric(palette = c("#ADD8E6", "#00008B"), domain = datos_estacion_promedio$Precip_Promedio_Anual_Est)

mapa_combinado <- leaflet(data = datos_estacion_promedio) |>
  addProviderTiles(providers$CartoDB.Positron, group = "Mapa Base") |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 4,
    color = ~pal_temp(Temp_Promedio), stroke = FALSE, fillOpacity = 0.7,
    group = "Temperatura",  # Nombre de la capa
    popup = ~paste("<strong>Estación:</strong>", Nro, "<br>",
                   "<strong>Provincia:</strong>", Provincia, "<br>",
                   "<strong>Temp. Promedio:</strong>", round(Temp_Promedio, 1), "°C")
  ) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud,
    radius = 4, color = "black", weight = 0.5, stroke = TRUE, # Tus estilos de precipitación
    fillColor = ~pal_precip(Precip_Promedio_Anual_Est), fillOpacity = 0.7,
    group = "Precipitación", # Nombre de la capa
    popup = ~paste("<strong>Estación:</strong>", Nro, "<br>",
                   "<strong>Provincia:</strong>", Provincia, "<br>",
                   "<strong>Precip. Promedio:</strong>", round(Precip_Promedio_Anual_Est, 0), " mm/año")
  ) |>
  addLayersControl(
    baseGroups = c("Temperatura", "Precipitación"), # Permite alternar entre una y otra
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  ) |>
  addLegend(pal = pal_temp, values = ~Temp_Promedio, opacity = 1,
            title = "Temp. (°C)", position = "bottomright", group = "Temperatura") |>
  addLegend(pal = pal_precip, values = ~Precip_Promedio_Anual_Est, opacity = 1,
            title = "Precip. (mm)", position = "bottomleft", group = "Precipitación") |> # En la otra esquina
  
  fitBounds(lng1 = -73.5, lat1 = -55.5, lng2 = -53.5, lat2 = -21.5)

saveWidget(mapa_combinado, "../outputs/graficos/mapas/mapa_combinado_temp_precip.html")
cat("-> 'mapa_combinado_temp_precip.html' guardado.\n")

mapa_combinado


# --- GRÁFICO 3: Barras Interactivas de Humedad (Climatológico) ---
cat("Generando Gráfico de Humedad (Climatológico)...\n")
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
saveWidget(fig_humedad, "../outputs/graficos/agroclimaticos/3_grafico_humedad_climatologico.html")
cat("-> '3_grafico_humedad_climatologico.html' guardado.\n")

fig_humedad

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
saveWidget(fig_precip_prov_2024, "../outputs/graficos/agroclimaticos/4_grafico_precipitacion_provincias_2024.html")
cat("-> '4_grafico_precipitacion_provincias_2024.html' guardado.\n")

fig_precip_prov_2024

# --- GRÁFICO 5: Gráfico de Temperatura Diaria ---
cat("Generando Gráfico de Temperatura Diaria...\n")
# (Usa datos_diarios_pais)
fig_temp_diaria <- plot_ly(
  data = datos_diarios_pais, x = ~Fecha, y = ~Temp_Media_Pais,
  type = 'scatter', mode = 'lines', line = list(color = 'darkred', width = 1),
  hovertemplate = ~paste("<b>%{x}</b><br>Temp. Media: %{y:.1f}°C<extra></extra>")
) |>
  layout(title = "Temperatura Media Diaria en Argentina (Promedio de estaciones)",
         xaxis = list(title = "Fecha"), yaxis = list(title = "Temperatura Media (°C)"))
saveWidget(fig_temp_diaria, "../outputs/graficos/series_temporales/5_grafico_temperatura_diaria.html")
cat("-> '5_grafico_temperatura_diaria.html' guardado.\n")

fig_temp_diaria

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
saveWidget(fig_precip_mensual, "../outputs/graficos/series_temporales/6_grafico_precipitacion_mensual.html")
cat("-> '6_grafico_precipitacion_mensual.html' guardado.\n")

fig_precip_mensual

# --- GRÁFICO 7 (AGRO): Mapa de Riesgo de Heladas Tardías (Primavera) ---
cat("Generando 7/9: Mapa Agro - Heladas Tardías (Colores mejorados)...\n")
ranking_agro_mapa <- ranking_agro |>
  left_join(distinct(datos_climaticos_limpios, Nombre, Latitud, Longitud), by = "Nombre")

pal_heladas <- colorNumeric(palette = c("#ADD8E6", "#00008B"), domain = ranking_agro_mapa$Dias_Helada_Tardia)

mapa_heladas <- leaflet(data = ranking_agro_mapa) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 5,
    fillColor = ~pal_heladas(Dias_Helada_Tardia), # Usamos fillColor para el relleno
    color = "#444444",  # Color del borde (gris oscuro)
    weight = 1,         # Grosor del borde (fino)
    stroke = TRUE,      # Activamos el borde
    fillOpacity = 0.9,
    popup = ~paste("<b>Estación:</b>", Nombre, "<br>",
                   "<b>Días Helada (Sep-Nov):</b>", Dias_Helada_Tardia)
  ) |>
  addLegend(pal = pal_heladas, values = ~Dias_Helada_Tardia,
            title = "Días Helada Tardía", position = "bottomright")
saveWidget(mapa_heladas, "../outputs/graficos/mapas/7_mapa_agro_heladas_tardias.html")

mapa_heladas

# --- GRÁFICO 8 (AGRO): Mapa de Riesgo de Sequía (Máxima racha sin lluvia) ---
cat("Generando: Mapa Agro - Riesgo Sequía...\n")
pal_sequia <- colorNumeric(palette = c("#FFC300", "#D35400", "#8D3A00", "#5C3317"), domain = datos_sequia$Max_Dias_Sin_Lluvia)

mapa_sequia <- leaflet(data = datos_sequia) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 5,
    fillColor = ~pal_sequia(Max_Dias_Sin_Lluvia),# Usamos fillColor para el relleno
    color = "#444444",  # Color del borde (gris oscuro)
    weight = 0.5,         # Grosor del borde (fino)
    stroke = TRUE,      # Activamos el borde
    fillOpacity = 0.6,
    popup = ~paste("<b>Estación:</b>", Nombre, "<br>",
                   "<b>Máx. Días Sin Lluvia:</b>", Max_Dias_Sin_Lluvia, "<br>",
                   "<b>Promedio Racha Seca:</b>", round(Promedio_Rachas_Secas, 1), "días")
  ) |>
  addLegend(pal = pal_sequia, values = ~Max_Dias_Sin_Lluvia,
            title = "Máx. Días Consec. Sin Lluvia", position = "bottomright")
saveWidget(mapa_sequia, "../outputs/graficos/mapas/8_mapa_agro_riesgo_sequia.html")

mapa_sequia

# --- GRÁFICO 9 (AGRO): Temperatura vs Precipitación ---
## Objetivo: identificar las zonas con balance hídrico óptimo (ni muy calurosas ni muy secas).
fig_disp_provincia <- plot_ly(
  data = datos_estacion_promedio,
  x = ~Temp_Promedio,
  y = ~Precip_Promedio_Anual_Est,
  color = ~Provincia,    # <--- Agrupamos por Provincia aquí
  colors = "Paired",     # Paleta categórica (buena para muchos grupos)
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10, opacity = 0.7, line = list(width = 1, color = '#333333')),
  text = ~paste("Estación:", Nombre, "<br>Provincia:", Provincia),
  hovertemplate = ~paste("<b>%{text}</b><br>Temp: %{x:.1f}°C<br>Precip: %{y:.0f} mm<extra></extra>")
) |>
  layout(
    title = "Relación Temp. vs Precipitación (Agrupado por Provincia)",
    xaxis = list(title = "Temperatura Promedio (°C)"),
    yaxis = list(title = "Precipitación Anual (mm)"),
    legend = list(title = list(text = "<b>Provincia</b>"))
  )

fig_disp_provincia

saveWidget(fig_disp_provincia, "../outputs/graficos/agroclimaticos/9_dispersion_provincia.html")

# --- GRÁFICO 10 (AGRO): Rosa de Vientos Nacional o Regional ---
ggplot(datos_viento_summary, aes(x = DD_rango, y = n, fill = FF_rango)) +
  geom_bar(stat = "identity") +
  coord_polar(start = -pi/8) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  labs(title = "Rosa de Vientos - Argentina",
       x = "Dirección del viento", y = "Frecuencia",
       fill = "Intensidad del viento") 

# --- GRÁFICO 10 (AGRO): Mapa combinado de Heladas + Sequía (riesgo agroclimático) ---
riesgo_agro <- ranking_agro_mapa |>
  left_join(select(datos_sequia, Provincia, Nombre, Max_Dias_Sin_Lluvia), by = "Nombre") |>
  mutate(Riesgo_Total = Dias_Helada_Tardia + (Max_Dias_Sin_Lluvia / 10))

pal_riesgo <- colorNumeric(palette = c("#ADD8E6", "#00008B", "#8D3A00", "#5C3317"), domain = riesgo_agro$Riesgo_Total)

mapa_riesgo <- leaflet(data = riesgo_agro) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud,
    color = ~pal_riesgo(Riesgo_Total),
    fillOpacity = 0.8, radius = 6,
    popup = ~paste("<b>Estación:</b>", Nombre, "<br>",
                   "<b>Riesgo Total:</b>", round(Riesgo_Total,1))
  ) |>
  addLegend(pal = pal_riesgo, values = ~Riesgo_Total,
            title = "Índice de Riesgo Climático", position = "bottomright")

saveWidget(mapa_riesgo, "../outputs/graficos/mapas/10_mapa_riesgo_agroclimatico.html")
mapa_riesgo

# --- GRÁFICO 11 (AGRO): Agroclimáticos y de Riesgo---
ranking_total <- riesgo_agro |>
  group_by(Provincia) |>
  summarise(Riesgo_Medio = mean(Riesgo_Total, na.rm = TRUE)) |>
  arrange(desc(Riesgo_Medio))

ggplot(ranking_total, aes(x = reorder(Provincia, Riesgo_Medio), y = Riesgo_Medio, fill = Riesgo_Medio)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#D35431", high = "#5C3317") +
  theme_minimal() +
  labs(title = "Ranking Provincial de Riesgo Agroclimático",
       x = "Provincia", y = "Índice de Riesgo Medio")

## ---------------------------------------------------------------------------
## BLOQUE FINAL: “Conclusiones Agroclimáticas”
## --------------------------------------------------------------------------
# --- PREPARACIÓN: Índice de Aptitud Agronómica (0 a 100) ---
# Normalizamos las variables para poder sumarlas/restarlas en una misma escala
datos_aptitud <- datos_estacion_promedio |>
  left_join(ranking_agro, by = c("Nro", "Nombre")) |>
  left_join(datos_sequia, by = c("Nro", "Nombre", "Provincia", "Latitud", "Longitud")) |>
  ungroup() |>
  mutate(
    # 1. Normalizamos cada variable de 0 a 1
    Score_Precip = scales::rescale(Precip_Promedio_Anual_Est, to = c(0, 1)), # Más lluvia = Mejor (generalmente)
    Score_Temp = 1 - abs(scales::rescale(Temp_Promedio, to = c(-1, 1))),    # Temp media ideal (ni muy frío ni muy calor)
    Score_Helada = 1 - scales::rescale(Dias_Helada_Tardia, to = c(0, 1)),   # Menos heladas = Mejor
    Score_Sequia = 1 - scales::rescale(Max_Dias_Sin_Lluvia, to = c(0, 1)),  # Menos sequía = Mejor
    
    # 2. Índice Final Ponderado (puedes ajustar los pesos según importancia)
    Aptitud_Final = (Score_Precip * 0.3 + Score_Temp * 0.2 + Score_Helada * 0.25 + Score_Sequia * 0.25) * 100
  ) |>
  filter(!is.na(Aptitud_Final))

# --- AMPLITUD AGRONOMICA ---
pal_aptitud <- colorNumeric(palette = "RdYlGn", domain = datos_aptitud$Aptitud_Final)

mapa_aptitud <- leaflet(data = datos_aptitud) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~Longitud, lat = ~Latitud, radius = 6,
    fillColor = ~pal_aptitud(Aptitud_Final),
    color = "#333333", weight = 1, opacity = 1, fillOpacity = 0.9,
    popup = ~paste("<b>ESTACIÓN:</b>", Nombre, "<br>",
                   "<b>Provincia:</b>", Provincia, "<br>",
                   "<b>APTITUD FINAL:</b>", round(Aptitud_Final, 1), "/ 100")
  ) |>
  addLegend(pal = pal_aptitud, values = ~Aptitud_Final,
            title = "Aptitud Agroclimática", position = "bottomright")
saveWidget(mapa_aptitud, "../outputs/graficos/mapas/CONCLUSION_mapa_aptitud.html")
mapa_aptitud

# --- TOP 15 MEJORES ESTACIONES ---
top_estaciones <- datos_aptitud |>
  arrange(desc(Aptitud_Final)) |>
  head(15)

fig_top15 <- plot_ly(
  data = top_estaciones,
  x = ~Aptitud_Final,
  y = ~reorder(Nombre, Aptitud_Final),
  type = 'bar',
  orientation = 'h',
  marker = list(color = ~Aptitud_Final, colorscale = 'RdYlGn'),
  text = ~paste("Provincia:", Provincia),
  hovertemplate = ~paste("<b>%{y}</b><br>%{text}<br>Aptitud: %{x:.1f}<extra></extra>")
) |>
  layout(
    title = "Top 15 Lugares con Mejor Balance Agroclimático",
    xaxis = list(title = "Índice de Aptitud (0-100)"),
    yaxis = list(title = "")
  )
saveWidget(fig_top15, "../outputs/graficos/agroclimaticos/CONCLUSION_ranking_top15.html")
fig_top15


# --- PERFIL DE RIESGO POR PROVINCIA (Normalizado y CERRADO) ---
perfil_riesgo_prov <- datos_aptitud |>
  group_by(Provincia) |>
  summarise(
    Riesgo_Helada = mean(1 - Score_Helada, na.rm = TRUE),
    Riesgo_Sequia = mean(1 - Score_Sequia, na.rm = TRUE),
    Riesgo_Calor = mean(scales::rescale(Dias_Calor_Extremo, to = c(0, 1)), na.rm = TRUE),
    .groups = 'drop'
  ) |>
  pivot_longer(cols = starts_with("Riesgo"), names_to = "Tipo_Riesgo", values_to = "Valor") |>
  mutate(Tipo_Riesgo = factor(Tipo_Riesgo, levels = c("Riesgo_Helada", "Riesgo_Sequia", "Riesgo_Calor"))) |>
  arrange(Provincia, Tipo_Riesgo) |>
  group_by(Provincia) |>
  slice(c(1:n(), 1)) |>
  ungroup()

fig_radar <- plot_ly(
  type = 'scatterpolar',
  mode = 'lines+markers',
  fill = 'toself'
)

provincias_top <- unique(perfil_riesgo_prov$Provincia)[1:5]

for (prov in provincias_top) {
  datos_prov <- perfil_riesgo_prov |> filter(Provincia == prov)
  
  fig_radar <- fig_radar |>
    add_trace(
      r = datos_prov$Valor,
      theta = datos_prov$Tipo_Riesgo,
      name = prov,
      opacity = 0.6,
      line = list(width = 2)
    )
}

fig_radar <- fig_radar |>
  layout(
    title = "Perfil de Riesgo Climático Regional",
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0, 1),
        title = "Intensidad Riesgo (0=Bajo, 1=Alto)"
      )
    ),
    # Ajustamos la leyenda para que no se superponga
    legend = list(x = 1, y = 1)
  )

saveWidget(fig_radar, "../outputs/graficos/agroclimaticos/CONCLUSION_radar_riesgos.html")
fig_radar

## CONCLUSIÓN FINAL
#Para máxima seguridad climática: Misiones (específicamente zonas como Bernardo de Irigoyen).
#Para gran escala con riesgo moderado: El centro y sur de Buenos Aires siguen siendo la apuesta más segura por extensión y calidad de suelo (aunque este análisis es puramente climático).
#Zonas a evitar para cultivos sensibles: Chubut (por heladas) y zonas centrales muy específicas con alto riesgo de calor si no hay agua asegurada.
