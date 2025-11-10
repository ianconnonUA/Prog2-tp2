# =============================================================================
# SCRIPT MAESTRO - EJECUCIÓN COMPLETA DEL PROYECTO
# =============================================================================

cat("🚀 INICIANDO ANÁLISIS CLIMÁTICO COMPLETO\n")
cat("=========================================\n")

# 1. Configuración inicial
if (!require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# 2. Ejecutar scripts en orden
scripts <- c(
  "scripts/01_base_importacion.R",
  "scripts/02_limpieza.R", 
  "scripts/03_analisis_exploratorio.R",
  "scripts/04_visualizaciones.R",
  "scripts/05_series_temporales.R"  
)

for (script in scripts) {
  if (file.exists(script)) {
    cat("\n▶ Ejecutando:", script, "\n")
    source(script)
    cat("✅ Completado:", script, "\n")
  } else {
    cat("❌ No encontrado:", script, "\n")
  }
}

cat("\n🎉 ANÁLISIS COMPLETADO EXITOSAMENTE!\n")
cat("📊 Resultados en: outputs/\n")
cat("📈 Pronósticos en: outputs/pronosticos/\n")