# =============================================================================
# ANÁLISIS DE SERIES TEMPORALES Y PRONÓSTICOS
# =============================================================================
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("arrow")) install.packages("arrow")
library(arrow)
if (!require("prophet")) install.packages("prophet")
library(prophet)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)
if (!require("forecast")) install.packages("forecast")
library(forecast)

options(scipen = 999)

# -----------------------------------------------------------------------------
# 1. CARGA DE DATOS PREPARADOS
# -----------------------------------------------------------------------------
cat("Cargando datos limpios...\n")
datos_climaticos <- read_parquet("./data/processed/datos_climaticos_unificados_imputados.parquet")

# -----------------------------------------------------------------------------
# 2. PREPARACIÓN DE DATOS PARA SERIES TEMPORALES
# -----------------------------------------------------------------------------
cat("Preparando datos para análisis temporal...\n")

# Serie temporal nacional diaria (Temperatura)
serie_temp_nacional <- datos_climaticos %>%
  filter(!is.na(Temp), !is.na(Fecha)) %>%
  group_by(Fecha) %>%
  summarise(
    temp_media = mean(Temp, na.rm = TRUE),
    temp_min = min(Temp, na.rm = TRUE),
    temp_max = max(Temp, na.rm = TRUE),
    n_estaciones = n_distinct(Nro)
  ) %>%
  filter(n_estaciones >= 10) %>%  # Solo días con buena cobertura
  select(Fecha, y = temp_media) %>%
  rename(ds = Fecha)

# Serie temporal nacional mensual (Precipitación)
serie_precip_mensual <- datos_climaticos %>%
  filter(!is.na(Precipitacion_mm), !is.na(Fecha)) %>%
  mutate(mes = floor_date(Fecha, "month")) %>%
  group_by(mes) %>%
  summarise(
    precip_total = sum(Precipitacion_mm, na.rm = TRUE),
    n_estaciones = n_distinct(Nro)
  ) %>%
  filter(n_estaciones >= 5) %>%
  select(ds = mes, y = precip_total)

cat(sprintf("-> Temperatura: %d días desde %s hasta %s\n", 
            nrow(serie_temp_nacional), 
            min(serie_temp_nacional$ds), 
            max(serie_temp_nacional$ds)))

cat(sprintf("-> Precipitación: %d meses desde %s hasta %s\n", 
            nrow(serie_precip_mensual), 
            min(serie_precip_mensual$ds), 
            max(serie_precip_mensual$ds)))

# -----------------------------------------------------------------------------
# 3. DESCOMPOSICIÓN DE SERIES TEMPORALES
# -----------------------------------------------------------------------------
cat("Realizando descomposición de series...\n")

# Función para descomposición STL
descomponer_serie <- function(serie, frecuencia = 365) {
  # Convertir a ts object
  ts_data <- ts(serie$y, frequency = frecuencia, start = c(year(min(serie$ds)), yday(min(serie$ds))))
  
  # Descomposición STL
  stl_descomposicion <- stl(ts_data, s.window = "periodic", robust = TRUE)
  
  return(stl_descomposicion)
}

# Descomposición temperatura (diaria)
descomp_temp <- descomponer_serie(serie_temp_nacional, 365)

# Descomposición precipitación (mensual)
descomp_precip <- descomponer_serie(serie_precip_mensual, 12)

# -----------------------------------------------------------------------------
# 4. MODELOS PROPHET - TEMPERATURA
# -----------------------------------------------------------------------------
cat("Ajustando modelo Prophet para temperatura...\n")

# Dividir en train/test (últimos 6 meses para test)
fecha_corte <- max(serie_temp_nacional$ds) - months(6)
train_temp <- serie_temp_nacional %>% filter(ds <= fecha_corte)
test_temp <- serie_temp_nacional %>% filter(ds > fecha_corte)

# Modelo Prophet con estacionalidades
modelo_temp <- prophet(
  train_temp,
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  changepoint.prior.scale = 0.05,
  seasonality.prior.scale = 10
)

# Hacer predicciones en test
future_test <- make_future_dataframe(modelo_temp, periods = nrow(test_temp), include_history = FALSE)
forecast_test <- predict(modelo_temp, future_test)

# Evaluación del modelo
resultados_test <- test_temp %>%
  left_join(forecast_test %>% select(ds, yhat), by = "ds") %>%
  mutate(
    error = y - yhat,
    error_abs = abs(error),
    error_pct = error / y * 100
  )

mae_temp <- mean(resultados_test$error_abs, na.rm = TRUE)
rmse_temp <- sqrt(mean(resultados_test$error^2, na.rm = TRUE))

cat(sprintf("-> Temperatura - MAE: %.2f°C, RMSE: %.2f°C\n", mae_temp, rmse_temp))

# -----------------------------------------------------------------------------
# 5. PRONÓSTICO TEMPERATURA - 12 MESES
# -----------------------------------------------------------------------------
cat("Generando pronóstico de temperatura para 12 meses...\n")

future_temp <- make_future_dataframe(modelo_temp, periods = 365, include_history = TRUE)
forecast_temp <- predict(modelo_temp, future_temp)

# Gráfico interactivo del pronóstico
fig_pronostico_temp <- plot_ly() %>%
  add_trace(
    data = forecast_temp %>% filter(ds <= max(serie_temp_nacional$ds)),
    x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
    name = 'Ajuste Modelo', line = list(color = 'blue')
  ) %>%
  add_trace(
    data = serie_temp_nacional,
    x = ~ds, y = ~y, type = 'scatter', mode = 'lines',
    name = 'Real', line = list(color = 'black')
  ) %>%
  add_trace(
    data = forecast_temp %>% filter(ds > max(serie_temp_nacional$ds)),
    x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
    name = 'Pronóstico', line = list(color = 'red')
  ) %>%
  add_ribbons(
    data = forecast_temp %>% filter(ds > max(serie_temp_nacional$ds)),
    x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper,
    name = 'Intervalo Confianza', fillcolor = 'rgba(255,0,0,0.2)',
    line = list(color = 'rgba(255,0,0,0)')
  ) %>%
  layout(
    title = "Pronóstico de Temperatura Media - Próximos 12 Meses",
    xaxis = list(title = "Fecha"),
    yaxis = list(title = "Temperatura (°C)")
  )

saveWidget(fig_pronostico_temp, "./outputs/graficos/series_temporales/pronostico_temperatura_12meses.html")
cat("-> 'pronostico_temperatura_12meses.html' guardado.\n")

# -----------------------------------------------------------------------------
# 6. MODELOS PROPHET - PRECIPITACIÓN
# -----------------------------------------------------------------------------
cat("Ajustando modelo Prophet para precipitación...\n")

# Dividir en train/test (últimos 6 meses para test)
fecha_corte_precip <- max(serie_precip_mensual$ds) - months(6)
train_precip <- serie_precip_mensual %>% filter(ds <= fecha_corte_precip)
test_precip <- serie_precip_mensual %>% filter(ds > fecha_corte_precip)

modelo_precip <- prophet(
  train_precip,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  changepoint.prior.scale = 0.01,
  seasonality.prior.scale = 15
)

# Pronóstico precipitación - 12 meses
future_precip <- make_future_dataframe(modelo_precip, periods = 12, freq = 'month', include_history = TRUE)
forecast_precip <- predict(modelo_precip, future_precip)

# Gráfico interactivo del pronóstico
fig_pronostico_precip <- plot_ly() %>%
  add_trace(
    data = forecast_precip %>% filter(ds <= max(serie_precip_mensual$ds)),
    x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
    name = 'Ajuste Modelo', line = list(color = 'blue')
  ) %>%
  add_trace(
    data = serie_precip_mensual,
    x = ~ds, y = ~y, type = 'scatter', mode = 'lines',
    name = 'Real', line = list(color = 'black')
  ) %>%
  add_trace(
    data = forecast_precip %>% filter(ds > max(serie_precip_mensual$ds)),
    x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines',
    name = 'Pronóstico', line = list(color = 'red')
  ) %>%
  add_ribbons(
    data = forecast_precip %>% filter(ds > max(serie_precip_mensual$ds)),
    x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper,
    name = 'Intervalo Confianza', fillcolor = 'rgba(255,0,0,0.2)',
    line = list(color = 'rgba(255,0,0,0)')
  ) %>%
  layout(
    title = "Pronóstico de Precipitación Mensual - Próximos 12 Meses",
    xaxis = list(title = "Fecha"),
    yaxis = list(title = "Precipitación (mm)")
  )

saveWidget(fig_pronostico_precip, "./outputs/graficos/series_temporales/pronostico_precipitacion_12meses.html")
cat("-> 'pronostico_precipitacion_12meses.html' guardado.\n")

# -----------------------------------------------------------------------------
# 7. COMPONENTES ESTACIONALES
# -----------------------------------------------------------------------------
cat("Analizando componentes estacionales...\n")

# Componentes del modelo de temperatura
prophet_plot_components(modelo_temp, forecast_temp)

# Componentes del modelo de precipitación
prophet_plot_components(modelo_precip, forecast_precip)

# -----------------------------------------------------------------------------
# 8. MÉTRICAS DE EVALUACIÓN COMPLETAS
# -----------------------------------------------------------------------------
cat("Calculando métricas de evaluación...\n")

# Función para calcular métricas
calcular_metricas <- function(real, pronostico) {
  error <- real - pronostico
  mae <- mean(abs(error), na.rm = TRUE)
  rmse <- sqrt(mean(error^2, na.rm = TRUE))
  mape <- mean(abs(error/real), na.rm = TRUE) * 100
  r_cuadrado <- cor(real, pronostico, use = "complete.obs")^2
  
  return(list(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r_cuadrado))
}

# Métricas temperatura
metricas_temp <- calcular_metricas(
  resultados_test$y, 
  resultados_test$yhat
)

# Métricas precipitación
resultados_test_precip <- test_precip %>%
  left_join(forecast_precip %>% select(ds, yhat), by = "ds") %>%
  mutate(error = y - yhat)

metricas_precip <- calcular_metricas(
  resultados_test_precip$y, 
  resultados_test_precip$yhat
)

# Tabla resumen de métricas
metricas_resumen <- tibble(
  Variable = c("Temperatura", "Precipitación"),
  MAE = c(metricas_temp$MAE, metricas_precip$MAE),
  RMSE = c(metricas_temp$RMSE, metricas_precip$RMSE),
  MAPE = c(metricas_temp$MAPE, metricas_precip$MAPE),
  R2 = c(metricas_temp$R2, metricas_precip$R2)
)

print("Métricas de Evaluación:")
print(metricas_resumen)

# -----------------------------------------------------------------------------
# 9. EXPORTAR RESULTADOS
# -----------------------------------------------------------------------------
cat("Exportando resultados...\n")

# Guardar pronósticos
write_parquet(forecast_temp, "./outputs/pronosticos/pronostico_temperatura_completo.parquet")
write_parquet(forecast_precip, "./outputs/pronosticos/pronostico_precipitacion_completo.parquet")

# Guardar métricas
write_csv(metricas_resumen, "./outputs/tablas/metricas_modelos_series_temporales.csv")

# -----------------------------------------------------------------------------
# 10. ANÁLISIS DE TENDENCIAS
# -----------------------------------------------------------------------------
cat("Analizando tendencias a largo plazo...\n")

# Tendencia temperatura anual
tendencia_anual_temp <- datos_climaticos %>%
  filter(!is.na(Temp), !is.na(Fecha)) %>%
  mutate(año = year(Fecha)) %>%
  group_by(año) %>%
  summarise(
    temp_media_anual = mean(Temp, na.rm = TRUE),
    n_registros = n()
  ) %>%
  filter(n_registros > 1000)

# Gráfico de tendencia
fig_tendencia <- plot_ly(tendencia_anual_temp, x = ~año, y = ~temp_media_anual,
                         type = 'scatter', mode = 'lines+markers',
                         line = list(color = 'red', width = 3),
                         marker = list(size = 8)) %>%
  layout(title = "Tendencia de Temperatura Media Anual",
         xaxis = list(title = "Año"),
         yaxis = list(title = "Temperatura Media (°C)"))

saveWidget(fig_tendencia, "./outputs/graficos/series_temporales/tendencia_temperatura_anual.html")

cat("-> Análisis de series temporales COMPLETADO\n")
cat("-> Archivos generados:\n")
cat("   - pronostico_temperatura_12meses.html\n")
cat("   - pronostico_precipitacion_12meses.html\n")
cat("   - tendencia_temperatura_anual.html\n")
cat("   - metricas_modelos_series_temporales.csv\n")