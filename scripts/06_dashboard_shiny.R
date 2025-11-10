# =============================================================================
# DASHBOARD SHINY - ANÁLISIS CLIMÁTICO ARGENTINA
# =============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(arrow)

# -----------------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN DE DATOS
# -----------------------------------------------------------------------------
cat("Cargando datos para dashboard...\n")

# Cargar datos procesados
datos_climaticos <- read_parquet("data/processed/datos_climaticos_unificados_imputados.parquet")

# Preparar datos resumidos para mejor performance
datos_dashboard <- datos_climaticos %>%
  mutate(
    Fecha = as.Date(Fecha),
    Año = year(Fecha),
    Mes = month(Fecha, label = TRUE),
    Semana = week(Fecha)
  ) %>%
  filter(!is.na(Provincia), !is.na(Temp), !is.na(Precipitacion_mm))

# Resumen por estación (para mapas)
estaciones_resumen <- datos_dashboard %>%
  group_by(Nro, Nombre, Provincia, Latitud, Longitud) %>%
  summarise(
    Temp_Promedio = mean(Temp, na.rm = TRUE),
    Precip_Total = sum(Precipitacion_mm, na.rm = TRUE),
    Precip_Promedio_Anual = mean(Precipitacion_mm * 365, na.rm = TRUE),
    Humedad_Promedio = mean(Hum, na.rm = TRUE),
    Registros = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(Latitud), !is.na(Longitud))

# Resumen temporal (para series)
series_temporales <- datos_dashboard %>%
  group_by(Fecha) %>%
  summarise(
    Temp_Media = mean(Temp, na.rm = TRUE),
    Precip_Total = sum(Precipitacion_mm, na.rm = TRUE),
    Estaciones = n_distinct(Nro),
    .groups = 'drop'
  ) %>%
  filter(Estaciones >= 5)  # Solo días con buena cobertura

cat("Datos cargados para dashboard.\n")

# -----------------------------------------------------------------------------
# 2. INTERFAZ DE USUARIO (UI)
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "🌤️ Análisis Climático Argentina",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Dashboard Principal", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("🗺️ Mapas Interactivos", tabName = "mapas", icon = icon("map")),
      menuItem("📈 Series Temporales", tabName = "series", icon = icon("chart-line")),
      menuItem("🌾 Análisis Agroclimático", tabName = "agro", icon = icon("tractor")),
      menuItem("📋 Datos Crudos", tabName = "datos", icon = icon("table"))
    ),
    
    # Filtros globales
    br(),
    dateRangeInput(
      "rango_fechas",
      "📅 Rango de Fechas:",
      start = min(datos_dashboard$Fecha),
      end = max(datos_dashboard$Fecha),
      min = min(datos_dashboard$Fecha),
      max = max(datos_dashboard$Fecha)
    ),
    
    selectizeInput(
      "provincias",
      "🏛️ Provincias:",
      choices = unique(datos_dashboard$Provincia),
      multiple = TRUE,
      selected = unique(datos_dashboard$Provincia)[1:5]
    ),
    
    selectizeInput(
      "estaciones",
      "📍 Estaciones:",
      choices = unique(datos_dashboard$Nombre),
      multiple = TRUE
    ),
    
    actionButton("actualizar", "🔄 Actualizar Datos", icon = icon("refresh"))
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      
      # TAB 1: Dashboard Principal
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # Value Boxes
          valueBoxOutput("total_estaciones", width = 3),
          valueBoxOutput("total_registros", width = 3),
          valueBoxOutput("temp_promedio", width = 3),
          valueBoxOutput("precip_total", width = 3)
        ),
        
        fluidRow(
          # Mapa principal
          box(
            title = "🗺️ Mapa de Estaciones Meteorológicas",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            leafletOutput("mapa_principal", height = 500)
          ),
          
          # Gráfico rápido de variables
          box(
            title = "📈 Distribución de Variables",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("grafico_distribucion", height = 500)
          )
        ),
        
        fluidRow(
          # Serie temporal rápida
          box(
            title = "🕒 Evolución Temporal",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("serie_temporal_rapida", height = 300)
          )
        )
      ),
      
      # TAB 2: Mapas Interactivos
      tabItem(
        tabName = "mapas",
        fluidRow(
          box(
            title = "Controles del Mapa",
            status = "primary",
            width = 3,
            selectInput(
              "variable_mapa",
              "Variable a Visualizar:",
              choices = c(
                "Temperatura Promedio" = "temp",
                "Precipitación Anual" = "precip",
                "Humedad Promedio" = "humedad"
              ),
              selected = "temp"
            ),
            sliderInput(
              "radio_mapa",
              "Radio de los puntos:",
              min = 1, max = 10, value = 5
            ),
            checkboxInput("mostrar_nombres", "Mostrar nombres", value = FALSE)
          ),
          
          box(
            title = "Mapa Interactivo",
            status = "primary",
            width = 9,
            leafletOutput("mapa_variable", height = 600)
          )
        )
      ),
      
      # TAB 3: Series Temporales
      tabItem(
        tabName = "series",
        fluidRow(
          box(
            title = "Configuración de Series",
            width = 3,
            status = "info",
            selectInput(
              "tipo_serie",
              "Tipo de Serie:",
              choices = c("Temperatura", "Precipitación", "Humedad"),
              selected = "Temperatura"
            ),
            selectInput(
              "agregacion",
              "Agregación Temporal:",
              choices = c("Diaria", "Semanal", "Mensual", "Anual"),
              selected = "Mensual"
            ),
            checkboxGroupInput(
              "componentes",
              "Componentes:",
              choices = c("Tendencia", "Estacionalidad", "Ruido"),
              selected = "Tendencia"
            )
          ),
          
          box(
            title = "Serie Temporal",
            width = 9,
            status = "info",
            plotlyOutput("grafico_serie", height = 500)
          )
        )
      ),
      
      # TAB 4: Análisis Agroclimático
      tabItem(
        tabName = "agro",
        fluidRow(
          box(
            title = "Indicadores Agroclimáticos",
            width = 12,
            status = "success",
            plotlyOutput("grafico_agro", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Zones de Riesgo",
            width = 6,
            status = "warning",
            plotlyOutput("riesgo_heladas", height = 300)
          ),
          
          box(
            title = "Zonas Óptimas",
            width = 6,
            status = "success",
            plotlyOutput("zonas_optimas", height = 300)
          )
        )
      ),
      
      # TAB 5: Datos Crudos
      tabItem(
        tabName = "datos",
        fluidRow(
          box(
            title = "Tabla de Datos",
            width = 12,
            status = "primary",
            DTOutput("tabla_datos")
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 3. SERVER LOGIC
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Datos reactivos
  datos_filtrados <- reactive({
    datos <- datos_dashboard
    
    # Aplicar filtros
    if (!is.null(input$rango_fechas)) {
      datos <- datos %>% 
        filter(Fecha >= input$rango_fechas[1] & Fecha <= input$rango_fechas[2])
    }
    
    if (!is.null(input$provincias)) {
      datos <- datos %>% 
        filter(Provincia %in% input$provincias)
    }
    
    if (!is.null(input$estaciones) && length(input$estaciones) > 0) {
      datos <- datos %>% 
        filter(Nombre %in% input$estaciones)
    }
    
    datos
  })
  
  # Value Boxes
  output$total_estaciones <- renderValueBox({
    datos <- datos_filtrados()
    n_estaciones <- n_distinct(datos$Nro)
    
    valueBox(
      n_estaciones,
      "Estaciones Activas",
      icon = icon("location-dot"),
      color = "blue"
    )
  })
  
  output$total_registros <- renderValueBox({
    datos <- datos_filtrados()
    n_registros <- nrow(datos)
    
    valueBox(
      format(n_registros, big.mark = ","),
      "Registros Totales",
      icon = icon("database"),
      color = "green"
    )
  })
  
  output$temp_promedio <- renderValueBox({
    datos <- datos_filtrados()
    temp <- mean(datos$Temp, na.rm = TRUE)
    
    valueBox(
      sprintf("%.1f°C", temp),
      "Temperatura Promedio",
      icon = icon("temperature-half"),
      color = "red"
    )
  })
  
  output$precip_total <- renderValueBox({
    datos <- datos_filtrados()
    precip <- sum(datos$Precipitacion_mm, na.rm = TRUE)
    
    valueBox(
      sprintf("%.0f mm", precip),
      "Precipitación Total",
      icon = icon("cloud-rain"),
      color = "aqua"
    )
  })
  
  # Mapa principal
  output$mapa_principal <- renderLeaflet({
    leaflet(estaciones_resumen) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        radius = 6,
        color = "red",
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>Estación:</b>", Nombre, "<br>",
          "<b>Provincia:</b>", Provincia, "<br>",
          "<b>Temp. Promedio:</b>", round(Temp_Promedio, 1), "°C<br>",
          "<b>Precip. Anual:</b>", round(Precip_Promedio_Anual, 0), "mm"
        )
      ) %>%
      fitBounds(
        lng1 = min(estaciones_resumen$Longitud, na.rm = TRUE),
        lat1 = min(estaciones_resumen$Latitud, na.rm = TRUE),
        lng2 = max(estaciones_resumen$Longitud, na.rm = TRUE),
        lat2 = max(estaciones_resumen$Latitud, na.rm = TRUE)
      )
  })
  
  # Gráfico de distribución
  output$grafico_distribucion <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) return(plotly_empty())
    
    fig <- plot_ly(datos, x = ~Temp, type = "histogram", 
                   name = "Temperatura", opacity = 0.7) %>%
      add_histogram(x = ~Precipitacion_mm, name = "Precipitación", opacity = 0.7) %>%
      layout(
        title = "Distribución de Variables",
        xaxis = list(title = "Valores"),
        yaxis = list(title = "Frecuencia"),
        barmode = "overlay"
      )
    
    fig
  })
  
  # Serie temporal rápida
  output$serie_temporal_rapida <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) return(plotly_empty())
    
    serie <- datos %>%
      group_by(Fecha) %>%
      summarise(
        Temp_Media = mean(Temp, na.rm = TRUE),
        Precip_Total = sum(Precipitacion_mm, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(serie) %>%
      add_trace(x = ~Fecha, y = ~Temp_Media, type = 'scatter', mode = 'lines',
                name = 'Temperatura', line = list(color = 'red')) %>%
      add_trace(x = ~Fecha, y = ~Precip_Total, type = 'scatter', mode = 'lines',
                name = 'Precipitación', yaxis = 'y2', line = list(color = 'blue')) %>%
      layout(
        title = "Evolución Temporal",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Temperatura (°C)", side = 'left'),
        yaxis2 = list(title = "Precipitación (mm)", side = 'right', overlaying = "y"),
        legend = list(x = 0.1, y = 0.9)
      )
  })
  
  # Mapa variable
  output$mapa_variable <- renderLeaflet({
    variable <- input$variable_mapa
    
    if (variable == "temp") {
      pal <- colorNumeric("RdBu", estaciones_resumen$Temp_Promedio, reverse = TRUE)
      valores <- estaciones_resumen$Temp_Promedio
      titulo <- "Temperatura (°C)"
    } else if (variable == "precip") {
      pal <- colorNumeric("Blues", estaciones_resumen$Precip_Promedio_Anual)
      valores <- estaciones_resumen$Precip_Promedio_Anual
      titulo <- "Precipitación Anual (mm)"
    } else {
      pal <- colorNumeric("Greens", estaciones_resumen$Humedad_Promedio)
      valores <- estaciones_resumen$Humedad_Promedio
      titulo <- "Humedad (%)"
    }
    
    leaflet(estaciones_resumen) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitud,
        lat = ~Latitud,
        radius = input$radio_mapa,
        color = ~pal(valores),
        fillOpacity = 0.8,
        popup = ~paste(
          "<b>", Nombre, "</b><br>",
          titulo, ": ", round(valores, 1)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = valores,
        title = titulo,
        position = "bottomright"
      )
  })
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    datos <- datos_filtrados() %>%
      select(Fecha, Nombre, Provincia, Temp, Precipitacion_mm, Hum, DD, FF) %>%
      head(1000)  # Limitar para performance
    
    datatable(
      datos,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
}

# -----------------------------------------------------------------------------
# 4. EJECUCIÓN DE LA APLICACIÓN
# -----------------------------------------------------------------------------
cat("Dashboard Shiny listo para ejecutar.\n")
cat("Para ejecutar: shiny::runApp('scripts/dashboard.R')\n")

# Ejecutar la aplicación
# shinyApp(ui, server)