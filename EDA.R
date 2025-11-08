## -----------------------------------------------------------------------------
## SECCIÓN 0: Librerías (Añadimos sf y rnaturalearth)
## -----------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(arrow)
library(leaflet)
library(htmlwidgets)
library(sf)               # ¡Nueva! Para manejar datos espaciales (mapas)
library(rnaturalearth)    # ¡Nueva! Para descargar los mapas base

# (Instálalas si no las tienes con: install.packages(c("sf", "rnaturalearth")))

# Paso 1: Instalar el paquete de herramientas 'devtools' desde CRAN
# install.packages("devtools")

# Paso 2: Usar 'devtools' para instalar el paquete de mapas en alta resolución desde GitHub
# devtools::install_github("ropensci/rnaturalearthhires")

options(scipen = 999)

## -----------------------------------------------------------------------------
## SECCIÓN 1: Carga de Datos Crudos
## -----------------------------------------------------------------------------
datos_climaticos <- read_parquet("datos_climaticos_unificados.parquet")

## -----------------------------------------------------------------------------
## SECCIÓN 2: Limpieza de Datos (¡Ahora incluimos la Temperatura!)
## -----------------------------------------------------------------------------
# Limpiamos precipitación Y temperatura
datos_climaticos_limpios <- datos_climaticos |>
  filter(!is.na(Precipitacion_mm), !is.na(Temp)) |> # Filtramos NAs
  mutate(
    # Corregimos Precipitación (la que ya teníamos)
    Precipitacion_mm_Num = ifelse(
      grepl("\\.", Precipitacion_mm, fixed = TRUE), 
      as.numeric(Precipitacion_mm),
      as.numeric(Precipitacion_mm) / 10
    ),
    
    # ¡NUEVO! Corregimos Temperatura (as.numeric suele ser suficiente)
    Temp_Num = as.numeric(Temp),
    
    Año = year(Fecha)
  ) |>
  # Filtramos NAs creados en la conversión
  filter(!is.na(Precipitacion_mm_Num), !is.na(Temp_Num))

cat("Datos limpios con Precipitación y Temperatura numéricas.\n")


## -----------------------------------------------------------------------------
## SECCIÓN 3: Resumen Anual por ESTACIÓN (¡Ahora incluimos Temp!)
## -----------------------------------------------------------------------------
datos_anuales_estacion <- datos_climaticos_limpios |>
  group_by(Nro, Año, Provincia, Latitud, Longitud, Altura) |>
  summarise(
    Precipitacion_Anual_mm = sum(Precipitacion_mm_Num, na.rm = TRUE),
    # ¡NUEVO! Calculamos la temperatura media de esa estación para ese año
    Temperatura_Media_Anual = mean(Temp_Num, na.rm = TRUE),
    .groups = 'drop' # Buena práctica para evitar problemas
  )

glimpse(datos_anuales_estacion)


## -----------------------------------------------------------------------------
## SECCIÓN 4: Resumen Climatológico por PROVINCIA
## -----------------------------------------------------------------------------
# Ahora creamos el promedio de TODAS las estaciones y AÑOS para cada provincia
# Esto nos da un solo valor de temperatura promedio por provincia

datos_provinciales_promedio <- datos_anuales_estacion |>
  group_by(Provincia) |>
  summarise(
    # Usamos la mediana, es más robusta a outliers que el promedio (mean)
    Temp_Mediana_Prov = median(Temperatura_Media_Anual, na.rm = TRUE)
  ) |>
  ungroup()

# ¡OJO! Paso clave: Estandarizar nombres
# Tus datos (ej. "CAPITAL FEDERAL") deben coincidir con los del mapa (ej. "Ciudad Autónoma de Buenos Aires")
datos_provinciales_promedio <- datos_provinciales_promedio |>
  mutate(
    # Creamos una nueva columna 'Provincia_Join' para la unión
    Provincia_Join = recode(
      Provincia,
      "CAPITAL FEDERAL" = "Ciudad Autónoma de Buenos Aires",
      "BUENOS AIRES" = "Buenos Aires",
      "TIERRA DEL FUEGO" = "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
      "SANTA FE" = "Santa Fe",
      "ENTRE RIOS" = "Entre Ríos",
      "CORDOBA" = "Córdoba",
      "TUCUMAN" = "Tucumán",
      "NEUQUEN" = "Neuquén",
      "RIO NEGRO" = "Río Negro",
      .default = Provincia # Deja el resto como están (ej: "Jujuy", "Salta")
    )
  )

## -----------------------------------------------------------------------------
## SECCIÓN 5: Cargar el Mapa de Argentina y Unir Datos
## -----------------------------------------------------------------------------

# 1. Descargamos las formas de las provincias de Argentina
# ne_states descarga "states" (provincias)
argentina_map_sf <- ne_states(country = "Argentina", returnclass = "sf")

# 2. Unimos nuestros datos de temperatura al mapa
# Usamos la columna 'name' del mapa y nuestra 'Provincia_Join'
mapa_con_datos <- argentina_map_sf |>
  left_join(datos_provinciales_promedio, by = c("name" = "Provincia_Join"))


## -----------------------------------------------------------------------------
## SECCIÓN 6: Crear y Guardar el Mapa de Coropletas
## -----------------------------------------------------------------------------

# 1. Crear paleta de colores (Azul=Frío, Rojo=Caliente)
pal_temp <- colorNumeric(
  palette = "RdBu",
  domain = mapa_con_datos$Temp_Mediana_Prov,
  reverse = TRUE # Invertimos para que el rojo sea caliente
)

# 2. Crear el mapa con polígonos
mapa_coropletas <- leaflet(data = mapa_con_datos) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  
  # ¡NUEVO! Usamos addPolygons en lugar de addCircleMarkers
  addPolygons(
    fillColor = ~pal_temp(Temp_Mediana_Prov), # Color de relleno según la temp
    fillOpacity = 0.8,         # Transparencia
    color = "white",           # Color del borde de la provincia
    weight = 1,                # Ancho del borde
    
    # Popup (lo que ves al hacer clic)
    popup = ~paste(
      "<strong>Provincia:</strong>", name, "<br>",
      "<strong>Temp. Mediana:</strong>", round(Temp_Mediana_Prov, 1), " °C"
    ),
    
    # Highlight (lo que pasa al pasar el mouse)
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 1
    )
  ) |>
  
  # 3. Agregar la leyenda
  addLegend(
    pal = pal_temp,
    values = ~Temp_Mediana_Prov,
    opacity = 1,
    title = "Temp. Mediana (°C)",
    position = "bottomright"
  ) |>
  
  # 4. Centrar el mapa en Argentina
  fitBounds(
    lng1 = -73.5, lat1 = -55.5, # Esquina SW
    lng2 = -53.5, lat2 = -21.5  # Esquina NE
  )

# 3. Mostrar el mapa
mapa_coropletas

# 4. Guardar en HTML
saveWidget(mapa_coropletas, "mapa_temperatura_provincias.html")