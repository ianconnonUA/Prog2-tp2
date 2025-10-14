
Universidad Austral
Licenciatura en Ciencia de Datos
Programación II - 2025
Trabajo Práctico 2
Análisis exploratorio de datos de un dataset real
Objetivo: Aplicar técnicas de importación, limpieza, manipulación y visualización de datos.
Datos provistos:
- Archivos de datos meteorológicos:
- Conjunto de datos desordenado y "sucio" relacionado con datos climáticos de Argentina, provistos
por el Servicio Meteorológico Nacional (ver https://www.smn.gob.ar/descarga-de-datos). Se
entregan datos desde el 1-1-2018 hasta 7-10-2024.
- Los datos diarios están incluidos en archivos de texto, por ejemplo "datohorario20230102.txt",
donde se indican el año, mes y día.
- Los campos incluidos son FECHA, HORA [HOA], TEMP [ºC], HUM [%], PNM [hPa], DD [gr], FF [km/hr],
NOMBRE. Cada fila de datos tiene una longitud fija de 100 caracteres. Cada variable ocupa un número
fijo de caracteres dentro de la fila.
- Archivo de datos de precipitaciones:
- smn_precipitaciones.txt: con datos de precipitaciones desde 1-1-1991 hasta 20-9-2024.
- Archivo de datos de estaciones:
- smn_estaciones.csv: con datos de las estaciones meteorológicas de Argentina.
- Descripción del objetivo:
- Importar los datos y limpiarlos usando técnicas aprendidas en clase (dplyr, tidyr).
- Combinar los datos provistos en un único dataset que contenga todos las variables relevantes.
- Realizar un análisis exploratorio completo, incluyendo estadísticas descriptivas y visualizaciones
usando plotly. Realizar gráficos georeferenciados interactivos.
- Identificar y manejar valores faltantes y duplicados.
- Identificar y comunicar los hallazgos más importantes.
- Transformar variables según sea necesario (por ejemplo, convertir fechas a un formato adecuado,
categorizar variables, etc.).
- Graficar o crear un dashboard que permita visualizar distintas variables.
Entregable: archivo R estándar, R Markdown, Jupyter Notebook, Quarto Document o Shiny App que incluya
código, visualizaciones y conclusiones.
Bonus:
- Realizar un análisis de series temporales.
- Ajustar modelos de pronóstico (se recomienda usar Prophet) y evaluar su rendimiento.
- Realizar pronósticos para los próximos 12 meses.
