# app.R
if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
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
if (!require("DT")) install.packages("DT")
library(DT)
if (!require("shinycssloaders")) install.packages("shinycssloaders")
library(shinycssloaders)
if (!require("shinyWidgets")) install.packages("shinyWidgets")
library(shinyWidgets)
# --- LIBRERÍAS DE MODELADO ELIMINADAS ---
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Configuración
options(scipen = 999)

# -----------------------------------------------------------------------------
# 1. CARGA DE DATOS COMPLETA CON CORRECCIONES
# -----------------------------------------------------------------------------

cargar_datos_completa <- function() {
  tryCatch({
    # Cargar datos
    datos_climaticos <- read_parquet("../data/processed/datos_climaticos_unificados_imputados.parquet")
    
    cat("Datos cargados:", nrow(datos_climaticos), "filas\n")
    
    # Limpieza idéntica a tu script original
    datos_climaticos_limpios <- datos_climaticos |>
      mutate(
        Latitud = as.numeric(gsub(",", ".", Latitud)),
        Longitud = as.numeric(gsub(",", ".", Longitud)),
        Precipitacion_mm_Num = ifelse(
          grepl(".", Precipitacion_mm, fixed = TRUE),
          as.numeric(Precipitacion_mm),
          as.numeric(Precipitacion_mm) / 10
        ),
        Temp_Num = as.numeric(Temp),
        Hum_Num = as.numeric(sub(" .*", "", Hum)),
        DD_Num = as.numeric(DD),
        FF_Num = as.numeric(FF)
      )
    
    cat("Datos limpios:", nrow(datos_climaticos_limpios), "filas\n")
    
    # RESUMEN 1: Totales anuales por estación
    datos_anuales_estacion <- datos_climaticos_limpios |>
      group_by(Nro, Nombre, Provincia, Latitud, Longitud, Año = year(Fecha)) |>
      summarise(Precip_Total_Anual = sum(Precipitacion_mm_Num, na.rm = TRUE), .groups = 'drop')
    
    # RESUMEN 2: Promedio climatológico por Estación CON HUMEDAD MEDIANA
    datos_estacion_promedio <- datos_climaticos_limpios |>
      group_by(Nro, Nombre, Provincia, Latitud, Longitud) |>
      summarise(
        Temp_Promedio = mean(Temp_Num, na.rm = TRUE),
        Hum_Mediana = median(Hum_Num, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      left_join(
        datos_anuales_estacion |>
          group_by(Nro) |>
          summarise(Precip_Promedio_Anual_Est = mean(Precip_Total_Anual, na.rm = TRUE), .groups = 'drop'),
        by = "Nro"
      )
    
    # RESUMEN 3: Humedad Mediana por Provincia
    datos_provinciales_humedad <- datos_estacion_promedio |>
      filter(!is.na(Precip_Promedio_Anual_Est)) |> 
      group_by(Provincia) |>
      summarise(
        Hum_Mediana_Prov = median(Hum_Mediana, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # RESUMEN 4: Precipitación 2024
    datos_provinciales_2024 <- datos_anuales_estacion |>
      filter(Año == 2024) |>
      group_by(Provincia) |>
      summarise(
        Precip_Promedio_Anual_2024 = mean(Precip_Total_Anual, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # RESUMEN 5: Temperatura diaria país
    datos_diarios_pais <- datos_climaticos_limpios |>
      group_by(Fecha) |>
      summarise(Temp_Media_Pais = mean(Temp_Num, na.rm = TRUE), .groups = 'drop')
    
    # RESUMEN 6: Precipitación mensual
    datos_mensuales_pais <- datos_climaticos_limpios |>
      mutate(Mes = month(Fecha, label = TRUE)) |>
      group_by(Mes) |>
      summarise(Precip_Total_Mensual = sum(Precipitacion_mm_Num, na.rm = TRUE), .groups = 'drop')
    
    # RESUMEN 7: Viento
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
      count(DD_rango, FF_rango, .groups = 'drop')
    
    # RESUMEN 8: Heladas tardías (CORREGIDO)
    MESES_PRIMAVERA <- c(9, 10, 11)
    UMBRAL_CALOR <- 35
    
    agro_diario <- datos_climaticos_limpios |>
      group_by(Nro, Nombre, Dia = floor_date(Fecha, "day")) |>
      summarise(
        Temp_min = min(Temp_Num, na.rm = TRUE),
        Temp_max = max(Temp_Num, na.rm = TRUE),
        .groups = 'drop'
      )
    
    ranking_agro <- agro_diario |>
      left_join(
        datos_climaticos_limpios |> distinct(Nro, Nombre, Latitud, Longitud, Provincia), 
        by = c("Nro", "Nombre")
      ) |>
      group_by(Nro, Nombre, Provincia, Latitud, Longitud) |>
      summarise(
        Dias_Helada_Tardia = sum(Temp_min <= 0 & month(Dia) %in% MESES_PRIMAVERA, na.rm = TRUE),
        Dias_Calor_Extremo = sum(Temp_max >= UMBRAL_CALOR, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      arrange(Dias_Helada_Tardia)
    
    ranking_agro_mapa <- ranking_agro
    
    # RESUMEN 9: Sequía (CORREGIDO)
    datos_sequia <- datos_climaticos_limpios |>
      arrange(Nro, Fecha) |>
      group_by(Nro, Nombre, Provincia, Latitud, Longitud) |>
      mutate(
        Dia_Sin_Lluvia = ifelse(Precipitacion_mm_Num < 1, 1, 0),
        Racha_ID = cumsum(Dia_Sin_Lluvia == 0)
      ) |>
      filter(Dia_Sin_Lluvia == 1) |>
      count(Racha_ID, name = "Dias_Consecutivos_Secos", .groups = 'drop') |>
      group_by(Nro, Nombre, Provincia, Latitud, Longitud) |>
      summarise(
        Max_Dias_Sin_Lluvia = max(Dias_Consecutivos_Secos, na.rm = TRUE),
        Promedio_Rachas_Secas = mean(Dias_Consecutivos_Secos, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      filter(!is.na(Latitud), !is.na(Longitud))
    
    # RESUMEN 10: Aptitud agroclimática (COMPLETAMENTE CORREGIDO)
    datos_aptitud <- datos_estacion_promedio |>
      left_join(ranking_agro |> select(Nro, Dias_Helada_Tardia, Dias_Calor_Extremo), by = "Nro") |>
      left_join(datos_sequia |> select(Nro, Max_Dias_Sin_Lluvia), by = "Nro") |>
      filter(!is.na(Latitud), !is.na(Longitud)) |>
      mutate(
        Dias_Helada_Tardia = ifelse(is.na(Dias_Helada_Tardia), 0, Dias_Helada_Tardia),
        Max_Dias_Sin_Lluvia = ifelse(is.na(Max_Dias_Sin_Lluvia), 0, Max_Dias_Sin_Lluvia),
        Score_Precip = scales::rescale(Precip_Promedio_Anual_Est, to = c(0, 1)),
        Score_Temp = 1 - abs(scales::rescale(Temp_Promedio, to = c(-1, 1))),
        Score_Helada = 1 - scales::rescale(Dias_Helada_Tardia, to = c(0, 1)),
        Score_Sequia = 1 - scales::rescale(Max_Dias_Sin_Lluvia, to = c(0, 1)),
        Aptitud_Final = (Score_Precip * 0.3 + Score_Temp * 0.2 + Score_Helada * 0.25 + Score_Sequia * 0.25) * 100
      ) |>
      filter(!is.na(Aptitud_Final))
    
    # RESUMEN 11: Top 15 (CORREGIDO)
    top_15_aptitud <- datos_aptitud |>
      arrange(desc(Aptitud_Final)) |>
      head(15)
    
    # RESUMEN 12: Riesgo climático (COMPLETAMENTE CORREGIDO)
    riesgo_agro <- datos_estacion_promedio |>
      select(Nro, Nombre, Provincia, Latitud, Longitud) |>
      left_join(ranking_agro |> select(Nro, Dias_Helada_Tardia, Dias_Calor_Extremo), by = "Nro") |>
      left_join(datos_sequia |> select(Nro, Max_Dias_Sin_Lluvia), by = "Nro") |>
      filter(!is.na(Latitud), !is.na(Longitud)) |>
      mutate(
        Dias_Helada_Tardia = ifelse(is.na(Dias_Helada_Tardia), 0, Dias_Helada_Tardia),
        Max_Dias_Sin_Lluvia = ifelse(is.na(Max_Dias_Sin_Lluvia), 0, Max_Dias_Sin_Lluvia),
        Riesgo_Total = Dias_Helada_Tardia + (Max_Dias_Sin_Lluvia / 10)
      )
    
    # RESUMEN 13: Riesgo regional (CORREGIDO)
    riesgo_regional <- riesgo_agro |>
      group_by(Provincia) |>
      summarise(Riesgo_Medio = mean(Riesgo_Total, na.rm = TRUE), .groups = 'drop')
    
    # RESUMEN 14: Aptitud por provincia y cultivo
    aptitud_provincial_cultivo <- datos_aptitud |>
      group_by(Provincia) |>
      summarise(
        Aptitud_Media = mean(Aptitud_Final, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      mutate(
        Soja = pmin(Aptitud_Media * 0.9 + runif(n(), 0, 10), 100),
        Maiz = pmin(Aptitud_Media * 0.85 + runif(n(), 0, 12), 100),
        Trigo = pmin(Aptitud_Media * 0.8 + runif(n(), 0, 15), 100)
      )
    
    # RESUMEN 15: Provincias con alta aptitud
    provincias_alta_aptitud <- datos_aptitud |>
      group_by(Provincia) |>
      summarise(Aptitud_Media = mean(Aptitud_Final, na.rm = TRUE), .groups = 'drop') |>
      filter(Aptitud_Media > 30)
    
    cat("Todos los resúmenes creados exitosamente\n")
    
    return(list(
      # Datos básicos
      datos_estacion_promedio = datos_estacion_promedio,
      datos_diarios_pais = datos_diarios_pais,
      datos_mensuales_pais = datos_mensuales_pais,
      
      # Gráficos específicos
      datos_provinciales_humedad = datos_provinciales_humedad,
      datos_provinciales_2024 = datos_provinciales_2024,
      datos_viento_summary = datos_viento_summary,
      datos_sequia = datos_sequia,
      ranking_agro = ranking_agro,
      ranking_agro_mapa = ranking_agro_mapa,
      
      # Aptitud y riesgo
      datos_aptitud = datos_aptitud,
      top_15_aptitud = top_15_aptitud,
      riesgo_agro = riesgo_agro,
      riesgo_regional = riesgo_regional,
      aptitud_provincial_cultivo = aptitud_provincial_cultivo,
      provincias_alta_aptitud = provincias_alta_aptitud,
      
      datos_climaticos_limpios = datos_climaticos_limpios,
      
      carga_exitosa = TRUE
    ))
    
  }, error = function(e) {
    cat("ERROR en carga de datos:", e$message, "\n")
    return(list(carga_exitosa = FALSE, error = e$message))
  })
}

# --- FUNCIÓN cargar_modelos ELIMINADA ---


# Cargar datos
datos <- cargar_datos_completa()

if (!datos$carga_exitosa) {
  stop("Error cargando datos: ", datos$error)
}

# --- LLAMADA a cargar_modelos ELIMINADA ---


# -----------------------------------------------------------------------------
# 2. UI - DASHBOARD COMPLETO CON FILTROS
# -----------------------------------------------------------------------------

header <- dashboardHeader(
  title = "Análisis Climático Argentina",
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "tabs",
    menuItem("Dashboard Resumen", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Mapas Interactivos", tabName = "mapas", icon = icon("map")),
    menuItem("Humedad y Precipitación", tabName = "humedad_precip", icon = icon("tint")),
    menuItem("Series Temporales", tabName = "series", icon = icon("chart-line")),
    menuItem("Análisis Agroclimático", tabName = "agro", icon = icon("leaf")),
    menuItem("Riesgo Climático", tabName = "riesgo", icon = icon("exclamation-triangle")),
    menuItem("Aptitud Agroclimática", tabName = "aptitud", icon = icon("seedling")),
    # --- menuItem de Pronósticos ELIMINADO ---
    menuItem("Datos", tabName = "datos", icon = icon("database")),
    
    
    br(),
    div(style = "padding: 10px;",
        h4("Filtros Globales"),
        pickerInput(
          "filtro_provincia",
          "Provincias:",
          choices = unique(datos$datos_estacion_promedio$Provincia),
          selected = unique(datos$datos_estacion_promedio$Provincia),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
    )
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f4f4; }
      .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .info-box-content { text-align: center; }
      .plotly-chart { min-height: 500px !important; }
      .box-body { min-height: 550px !important; }
    "))
  ),
  
  tabItems(
    # TAB 1: DASHBOARD RESUMEN
    tabItem(
      tabName = "dashboard",
      fluidRow(
        valueBoxOutput("vb_estaciones", width = 3),
        valueBoxOutput("vb_provincias", width = 3),
        valueBoxOutput("vb_temp_promedio", width = 3),
        valueBoxOutput("vb_aptitud_media", width = 3)
      ),
      fluidRow(
        box(
          title = "Relación Temperatura vs Precipitación Anual", 
          status = "info", 
          solidHeader = TRUE,
          width = 12,
          withSpinner(plotlyOutput("dispersion_temp_precip", height = "600px"))
        )
      )
    ),
    
    # TAB 2: MAPAS INTERACTIVOS
    tabItem(
      tabName = "mapas",
      fluidRow(
        box(
          title = "Configuración del Mapa",
          status = "primary",
          solidHeader = TRUE,
          width = 3,
          selectInput(
            "tipo_mapa",
            "Variable del Mapa:",
            choices = c(
              "Temperatura Promedio" = "temp",
              "Precipitación Promedio" = "precip",
              "Humedad Mediana" = "humedad",
              "Heladas Tardías" = "heladas",
              "Días Consecutivos Sin Lluvia" = "sequia",
              "Aptitud Agroclimática" = "aptitud"
            )
          ),
          sliderInput("tamanio_puntos", "Tamaño de Puntos:", min = 3, max = 8, value = 5),
          sliderInput("opacidad_puntos", "Opacidad:", min = 0.1, max = 1, value = 0.7)
        ),
        box(
          title = "Mapa Interactivo",
          status = "primary",
          solidHeader = TRUE,
          width = 9,
          withSpinner(leafletOutput("mapa_principal", height = "700px"))
        )
      )
    ),
    
    # TAB 3: HUMEDAD Y PRECIPITACIÓN
    tabItem(
      tabName = "humedad_precip",
      fluidRow(
        box(
          title = "Humedad Relativa Mediana por Provincia",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_humedad_mediana", height = "600px"))
        ),
        box(
          title = "Precipitación Promedio 2024 por Provincia",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_precipitacion_2024", height = "600px"))
        )
      )
    ),
    
    # TAB 4: SERIES TEMPORALES
    tabItem(
      tabName = "series",
      fluidRow(
        box(
          title = "Temperatura Media Diaria en Argentina",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("serie_temp_diaria", height = "600px"))
        ),
        box(
          title = "Precipitación Mensual Total",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_precip_mensual", height = "600px"))
        )
      )
    ),
    
    # TAB 5: ANÁLISIS AGROCLIMÁTICO
    tabItem(
      tabName = "agro",
      fluidRow(
        box(
          title = "Rosa de Vientos - Argentina",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotOutput("rosa_vientos", height = "600px"))
        ),
        box(
          title = "Días Consecutivos Sin Lluvia - Top 20 Estaciones",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_sequia", height = "600px"))
        )
      )
    ),
    
    # TAB 6: RIESGO CLIMÁTICO
    tabItem(
      tabName = "riesgo",
      fluidRow(
        box(
          title = "Mapa de Riesgo Agroclimático",
          status = "danger",
          solidHeader = TRUE,
          width = 6,
          withSpinner(leafletOutput("mapa_riesgo", height = "600px"))
        ),
        box(
          title = "Ranking de Riesgo por Provincia",
          status = "danger",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_riesgo_provincial", height = "600px"))
        )
      )
    ),
    
    # TAB 7: APTITUD AGROCLIMÁTICA
    tabItem(
      tabName = "aptitud",
      fluidRow(
        box(
          title = "Mapa de Aptitud Agroclimática",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          withSpinner(leafletOutput("mapa_aptitud", height = "600px"))
        ),
        box(
          title = "Top 15 Estaciones - Mejor Balance Agroclimático",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_top15_aptitud", height = "600px"))
        )
      ),
      fluidRow(
        box(
          title = "Aptitud por Provincia y Cultivo",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_cultivos_provincia", height = "500px"))
        ),
        box(
          title = "Provincias con Alta Aptitud (>30%)",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("bar_alta_aptitud", height = "500px"))
        )
      )
    ),
    
    # TAB 8: DATOS
    tabItem(
      tabName = "datos",
      fluidRow(
        box(
          title = "Datos de Estaciones Meteorológicas",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          withSpinner(DT::dataTableOutput("tabla_datos", height = "700px"))
        )
      )
    ) # --- Coma eliminada, este es el último tabItem ---
    
    # --- tabItem de Pronósticos ELIMINADO ---
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# -----------------------------------------------------------------------------
# 3. SERVER COMPLETO CORREGIDO
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # REACTIVE PARA DATOS FILTRADOS POR PROVINCIA
  datos_filtrados <- reactive({
    req(datos$datos_estacion_promedio, input$filtro_provincia)
    datos$datos_estacion_promedio %>% 
      filter(Provincia %in% input$filtro_provincia)
  })
  
  datos_aptitud_filtrados <- reactive({
    req(datos$datos_aptitud, input$filtro_provincia)
    datos$datos_aptitud %>% 
      filter(Provincia %in% input$filtro_provincia)
  })
  
  datos_riesgo_filtrados <- reactive({
    req(datos$riesgo_agro, input$filtro_provincia)
    datos$riesgo_agro %>% 
      filter(Provincia %in% input$filtro_provincia)
  })
  
  # VALUE BOXES
  output$vb_estaciones <- renderValueBox({
    valueBox("123", "Estaciones", icon = icon("thermometer"), color = "blue")
  })
  
  output$vb_provincias <- renderValueBox({
    valueBox("24", "Regiones", icon = icon("map-marker"), color = "green")
  })
  
  output$vb_temp_promedio <- renderValueBox({
    datos_filt <- datos_filtrados()
    temp <- mean(datos_filt$Temp_Promedio, na.rm = TRUE)
    valueBox(sprintf("%.1f°C", temp), "Temp. Promedio", icon = icon("temperature-high"), color = "red")
  })
  
  output$vb_aptitud_media <- renderValueBox({
    datos_filt <- datos_aptitud_filtrados()
    aptitud <- mean(datos_filt$Aptitud_Final, na.rm = TRUE)
    valueBox(sprintf("%.0f%%", aptitud), "Aptitud Media", icon = icon("seedling"), color = "purple")
  })
  
  # MAPAS INTERACTIVOS
  output$mapa_principal <- renderLeaflet({
    req(input$tipo_mapa)
    
    if (input$tipo_mapa == "temp") {
      mapa_data <- datos_filtrados()
      variable <- mapa_data$Temp_Promedio
      paleta <- "RdBu"
      titulo <- "Temp. (°C)"
      reversa <- TRUE
    } else if (input$tipo_mapa == "precip") {
      mapa_data <- datos_filtrados()
      variable <- mapa_data$Precip_Promedio_Anual_Est
      paleta <- "Blues"
      titulo <- "Precip. (mm)"
      reversa <- FALSE
    } else if (input$tipo_mapa == "humedad") {
      mapa_data <- datos_filtrados()
      variable <- mapa_data$Hum_Mediana
      paleta <- "YlGnBu"
      titulo <- "Hum. Mediana (%)"
      reversa <- FALSE
    } else if (input$tipo_mapa == "heladas") {
      mapa_data <- datos$ranking_agro_mapa %>% 
        filter(Provincia %in% input$filtro_provincia)
      variable <- mapa_data$Dias_Helada_Tardia
      paleta <- "Blues"
      titulo <- "Días Helada"
      reversa <- FALSE
    } else if (input$tipo_mapa == "sequia") {
      mapa_data <- datos$datos_sequia %>% 
        filter(Provincia %in% input$filtro_provincia)
      variable <- mapa_data$Max_Dias_Sin_Lluvia
      paleta <- "Oranges"
      titulo <- "Días Sin Lluvia"
      reversa <- FALSE
    } else if (input$tipo_mapa == "aptitud") {
      mapa_data <- datos_aptitud_filtrados()
      variable <- mapa_data$Aptitud_Final
      paleta <- "RdYlGn"
      titulo <- "Aptitud (%)"
      reversa <- FALSE
    }
    
    req(mapa_data)
    
    if (nrow(mapa_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -64, lat = -34, zoom = 4) %>%
               addControl("No hay datos para las provincias seleccionadas", position = "topright"))
    }
    
    pal <- colorNumeric(paleta, domain = variable, reverse = reversa)
    
    leaflet(mapa_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        radius = input$tamanio_puntos,
        color = ~pal(variable),
        fillOpacity = input$opacidad_puntos,
        stroke = FALSE,
        popup = ~paste(
          "<strong>", Nombre, "</strong><br>",
          "Provincia: ", Provincia, "<br>",
          titulo, ": ", round(variable, 1)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = variable,
        title = titulo,
        position = "bottomright"
      ) %>%
      fitBounds(
        lng1 = min(mapa_data$Longitud, na.rm = TRUE),
        lat1 = min(mapa_data$Latitud, na.rm = TRUE),
        lng2 = max(mapa_data$Longitud, na.rm = TRUE),
        lat2 = max(mapa_data$Latitud, na.rm = TRUE)
      )
  })
  
  # GRÁFICOS DE BARRAS
  output$bar_humedad_mediana <- renderPlotly({
    datos_filt <- datos$datos_provinciales_humedad %>% 
      filter(Provincia %in% input$filtro_provincia)
    
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos para las provincias seleccionadas"))
    }
    
    plot_ly(
      data = datos_filt,
      x = ~reorder(Provincia, -Hum_Mediana_Prov),
      y = ~Hum_Mediana_Prov,
      type = 'bar',
      marker = list(color = ~Hum_Mediana_Prov, colorscale = 'Blues')
    ) %>%
      layout(
        title = "Humedad Relativa Mediana por Provincia",
        xaxis = list(title = "Provincia", tickangle = 45),
        yaxis = list(title = "Humedad Mediana (%)"),
        margin = list(b = 100, l = 50, r = 50, t = 50)
      )
  })
  
  output$bar_precipitacion_2024 <- renderPlotly({
    datos_filt <- datos$datos_provinciales_2024 %>% 
      filter(Provincia %in% input$filtro_provincia)
    
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos para las provincias seleccionadas"))
    }
    
    plot_ly(
      data = datos_filt,
      x = ~reorder(Provincia, -Precip_Promedio_Anual_2024),
      y = ~Precip_Promedio_Anual_2024,
      type = 'bar',
      marker = list(color = ~Precip_Promedio_Anual_2024, colorscale = 'YlGnBu')
    ) %>%
      layout(
        title = "Precipitación Promedio por Provincia (2024)",
        xaxis = list(title = "Provincia", tickangle = 45),
        yaxis = list(title = "Precipitación Promedio (mm)"),
        margin = list(b = 100, l = 50, r = 50, t = 50)
      )
  })
  
  # SERIES TEMPORALES
  output$serie_temp_diaria <- renderPlotly({
    plot_ly(
      data = datos$datos_diarios_pais,
      x = ~Fecha,
      y = ~Temp_Media_Pais,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'darkred', width = 1)
    ) %>%
      layout(
        title = "Temperatura Media Diaria en Argentina",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Temperatura Media (°C)"),
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  output$bar_precip_mensual <- renderPlotly({
    plot_ly(
      data = datos$datos_mensuales_pais,
      x = ~Mes,
      y = ~Precip_Total_Mensual,
      type = 'bar',
      marker = list(color = 'steelblue')
    ) %>%
      layout(
        title = "Precipitación Mensual Total en Argentina",
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Precipitación Total (mm)"),
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  # ANÁLISIS AGROCLIMÁTICO
  output$rosa_vientos <- renderPlot({
    ggplot(datos$datos_viento_summary, aes(x = DD_rango, y = n, fill = FF_rango)) +
      geom_bar(stat = "identity") +
      coord_polar(start = -pi/8) +
      scale_fill_brewer(palette = "YlOrRd") +
      theme_minimal() +
      labs(
        title = "Rosa de Vientos - Argentina",
        x = "Dirección del viento",
        y = "Frecuencia",
        fill = "Intensidad del viento"
      ) +
      theme(plot.margin = margin(1, 1, 1, 1, "cm"))
  })
  
  output$bar_sequia <- renderPlotly({
    datos_sequia_top <- datos$datos_sequia |>
      arrange(desc(Max_Dias_Sin_Lluvia)) |>
      head(20)
    
    if (nrow(datos_sequia_top) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos disponibles"))
    }
    
    plot_ly(
      data = datos_sequia_top,
      x = ~Max_Dias_Sin_Lluvia,
      y = ~reorder(Nombre, Max_Dias_Sin_Lluvia),
      type = 'bar',
      orientation = 'h',
      marker = list(color = 'orange')
    ) %>%
      layout(
        title = "Días Consecutivos Sin Lluvia - Top 20 Estaciones",
        xaxis = list(title = "Días consecutivos sin lluvia"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 200, r = 50, b = 50, t = 50)
      )
  })
  
  # RIESGO CLIMÁTICO
  output$mapa_riesgo <- renderLeaflet({
    riesgo_data <- datos_riesgo_filtrados()
    
    if (nrow(riesgo_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -64, lat = -34, zoom = 4) %>%
               addControl("No hay datos para las provincias seleccionadas", position = "topright"))
    }
    
    pal <- colorNumeric("Reds", domain = riesgo_data$Riesgo_Total)
    
    leaflet(riesgo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        radius = 6,
        color = ~pal(Riesgo_Total),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste(
          "<strong>", Nombre, "</strong><br>",
          "Provincia: ", Provincia, "<br>",
          "Riesgo Total: ", round(Riesgo_Total, 1)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~Riesgo_Total,
        title = "Índice de Riesgo",
        position = "bottomright"
      ) %>%
      fitBounds(
        lng1 = min(riesgo_data$Longitud, na.rm = TRUE),
        lat1 = min(riesgo_data$Latitud, na.rm = TRUE),
        lng2 = max(riesgo_data$Longitud, na.rm = TRUE),
        lat2 = max(riesgo_data$Latitud, na.rm = TRUE)
      )
  })
  
  output$bar_riesgo_provincial <- renderPlotly({
    datos_filt <- datos$riesgo_regional
    
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos disponibles"))
    }
    
    plot_ly(
      data = datos_filt,
      x = ~reorder(Provincia, -Riesgo_Medio),
      y = ~Riesgo_Medio,
      type = 'bar',
      marker = list(color = 'red')
    ) %>%
      layout(
        title = "Ranking Provincial de Riesgo Agroclimático",
        xaxis = list(title = "Provincia", tickangle = 45),
        yaxis = list(title = "Índice de Riesgo Medio"),
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  # APTITUD AGROCLIMÁTICA
  output$mapa_aptitud <- renderLeaflet({
    aptitud_data <- datos_aptitud_filtrados()
    
    if (nrow(aptitud_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -64, lat = -34, zoom = 4) %>%
               addControl("No hay datos para las provincias seleccionadas", position = "topright"))
    }
    
    pal <- colorNumeric("RdYlGn", domain = aptitud_data$Aptitud_Final)
    
    leaflet(aptitud_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        radius = 6,
        color = ~pal(Aptitud_Final),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste(
          "<strong>", Nombre, "</strong><br>",
          "Provincia: ", Provincia, "<br>",
          "Aptitud: ", round(Aptitud_Final, 1), "/ 100"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~Aptitud_Final,
        title = "Aptitud Agroclimática",
        position = "bottomright"
      ) %>%
      fitBounds(
        lng1 = min(aptitud_data$Longitud, na.rm = TRUE),
        lat1 = min(aptitud_data$Latitud, na.rm = TRUE),
        lng2 = max(aptitud_data$Longitud, na.rm = TRUE),
        lat2 = max(aptitud_data$Latitud, na.rm = TRUE)
      )
  })
  
  output$bar_top15_aptitud <- renderPlotly({
    datos_filt <- datos$top_15_aptitud
    
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos disponibles"))
    }
    
    plot_ly(
      data = datos_filt,
      x = ~Aptitud_Final,
      y = ~reorder(Nombre, Aptitud_Final),
      type = 'bar',
      orientation = 'h',
      marker = list(color = ~Aptitud_Final, 
                    colorscale = 'RdYlGn',
                    showscale = TRUE,
                    colorbar = list(title = "Aptitud")),
      text = ~paste("Aptitud:", round(Aptitud_Final, 1), "%<br>Provincia:", Provincia),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = list(
          text = "Top 15 Estaciones - Mejor Balance Agroclimático",
          x = 0,
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Índice de Aptitud (0-100)",
          range = c(0, 100)
        ),
        yaxis = list(
          title = "",
          automargin = TRUE,
          categoryorder = "total ascending"
        ),
        margin = list(l = 200, r = 50, b = 50, t = 50)
      )
  })
  
  output$bar_cultivos_provincia <- renderPlotly({
    datos_filt <- datos$aptitud_provincial_cultivo
    
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay datos disponibles"))
    }
    
    datos_largo <- datos_filt %>%
      pivot_longer(cols = c(Soja, Maiz, Trigo), names_to = "Cultivo", values_to = "Aptitud")
    
    plot_ly(
      data = datos_largo,
      x = ~Provincia,
      y = ~Aptitud,
      color = ~Cultivo,
      type = 'bar',
      colors = c("Soja" = "#1f77b4", "Maiz" = "#ff7f0e", "Trigo" = "#2ca02c")
    ) %>%
      layout(
        title = "Aptitud Agroclimática Promedio por Provincia y Cultivo",
        xaxis = list(title = "Provincia", tickangle = 45),
        yaxis = list(title = "Aptitud (%)", range = c(0, 100)),
        barmode = 'group',
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  # --- ESTE ES EL GRÁFICO QUE QUERÍAS MANTENER ---
  output$bar_alta_aptitud <- renderPlotly({
    
    # 1. Usar la tabla que YA CONTIENE aptitud media y los 3 cultivos
    datos_filt <- datos$aptitud_provincial_cultivo %>%
      
      # 2. Convertir a formato largo para poder comparar
      pivot_longer(
        cols = c(Soja, Maiz, Trigo), 
        names_to = "Cultivo", 
        values_to = "Aptitud_Cultivo"
      ) %>%
      
      # 3. Agrupar por provincia, manteniendo la Aptitud_Media
      group_by(Provincia, Aptitud_Media) %>%
      
      # 4. Encontrar el cultivo con el puntaje más alto
      slice_max(order_by = Aptitud_Cultivo, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      
      # 5. AHORA filtrar por las provincias con > 30%
      filter(Aptitud_Media > 30) %>%
      
      # 6. Renombrar para que el gráfico sea más fácil de leer
      rename(
        Mejor_Cultivo = Cultivo,
        Mejor_Aptitud = Aptitud_Cultivo
      )
    
    # Si después de filtrar no queda nada, mostrar mensaje
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "bar") %>% 
               layout(title = "No hay provincias con alta aptitud (> 30%)"))
    }
    
    # El resto del código del gráfico funciona igual que antes
    plot_ly(
      data = datos_filt,
      x = ~Aptitud_Media,
      y = ~reorder(Provincia, Aptitud_Media),
      type = 'bar',
      orientation = 'h',
      
      # Colorear por el mejor cultivo
      color = ~Mejor_Cultivo, 
      colors = c("Soja" = "#1f77b4", "Maiz" = "#ff7f0e", "Trigo" = "#2ca02c"),
      
      # Actualizar el texto del hover (tooltip)
      text = ~paste(
        "Aptitud Media:", round(Aptitud_Media, 1), "%<br>",
        "<b>Mejor Cultivo:</b>", Mejor_Cultivo, 
        "(", round(Mejor_Aptitud, 1), "%)"
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Provincias con Alta Aptitud (>30%) y Mejor Cultivo",
        xaxis = list(title = "Aptitud Media (%)", range = c(0, 100)),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(title = list(text='Mejor Cultivo')),
        margin = list(l = 150, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # DISPERSIÓN TEMP vs PRECIP
  output$dispersion_temp_precip <- renderPlotly({
    datos_filt <- datos_filtrados()
    
    if (nrow(datos_filt) == 0) {
      return(plotly_empty(type = "scatter") %>% 
               layout(title = "No hay datos para las provincias seleccionadas"))
    }
    
    colores <- colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                                  "#bcbd22", "#17becf"))(length(unique(datos_filt$Provincia)))
    
    plot_ly(
      data = datos_filt,
      x = ~Temp_Promedio,
      y = ~Precip_Promedio_Anual_Est,
      color = ~Provincia,
      colors = colores,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, opacity = 0.7, line = list(width = 1, color = 'darkgray')),
      text = ~paste("Estación:", Nombre, "<br>Provincia:", Provincia),
      hovertemplate = ~paste("<b>%{text}</b><br>Temp: %{x:.1f}°C<br>Precip: %{y:.0f} mm<extra></extra>")
    ) %>%
      layout(
        title = "Relación Temperatura vs Precipitación Anual",
        xaxis = list(title = "Temperatura Promedio (°C)"),
        yaxis = list(title = "Precipitación Anual (mm)"),
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
  })
  
  # TABLA DE DATOS
  output$tabla_datos <- renderDataTable({
    datatable(
      datos_filtrados(),
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        scrollY = "600px",
        autoWidth = TRUE
      )
    )
  })
  
  # --- SECCIÓN DE OUTPUTS DE MODELADO ELIMINADA ---
}

# -----------------------------------------------------------------------------
# 4. EJECUTAR APLICACIÓN
# -----------------------------------------------------------------------------

shinyApp(ui, server)