# LA GENTRIFICACIÓN COMO MEDIDA DE DESIGUALDAD EN LAS GRANDES CIUDADES. Impacto de este fenómeno en la ciudad de Londres

Este repositorio contiene el contenido elaborado dentro de la realización del Trabajo Fin de Máster en Ciencia de Datos de la Universitat Oberta de Catalunya.

El mismo fue desarrollado durante el semestre de septiembre de 2024 a enero de 2025.

## Contenido del repositorio

El repositorio consta de los siguientes directorios:

- Datos: Directorio que contiene los datos utilizados para el proyecto junto con los datos preparados para utilizar por el visor
  - 01_Fuentes: Orígenes de datos obtenidos del [London DataStore](https://data.london.gov.uk/dataset) y del [Urban Big Data Centre](https://www.ubdc.ac.uk/)
  - 02_Staging: Datos de los indicadores una vez se han limpiado los procedentes de los orígenes de datos
  - 03_Preparados: Datos con los omdocadpres agrupados para los tres períodos en estudio
  - 04_Analisis: Datos con los barrios agrupados según el modelo utilizado
- Memoria: Memoria del trabajo y varias carpetas con las imágenes incluidas en la misma
- Presentacion: Video con la presentación del trabajo
- Scripts:
  - 01_Limpieza: Script para la realización de la limpieza de datos
  - 02_Preparacion: Script para la preparación de los indicadores
  - 03_Analisis: Script para aplicar el modelo jerárquico y agrupar los barrios y un script para obtener el top 3 de barrios según cada indicador, de utilización para la elaboración de la memoria
- Tableau: Varios cuadros generados en Tableau para incluir en la memoria del proyecto
- Visor: Aplicación elaborada en [R](https://www.r-project.org/) con la librería [Shiny](https://shiny.posit.co/) para consultar los datos utilizados y analizar los resultados

## Ejecución de la aplicación

Los distintos scripts de limpieza, preparación y análisis disponibles en el directorio Scripts, pueden ser ejecutados desde el IDE [RStudio](https://posit.co/download/rstudio-desktop/). La salida será los distintos CSV que se utilizan en las siguientes etapas.

Se comenzaría con la ejecución del script 01_Limpieza/limpieza_datos.R. Este script a partir de los ficheros disponibles en el directorio 01_Fuentes genera los CSV para el segundo paso dentro del directorio 02_Staging.

El segundo paso se ejecuta mediante el script 02_Preparacion/preparacion_datos.R, generando los ficheros CSV para el tercer paso en la ruta 03_Preparados

Se finaliza con la ejecución del script 03_Analisis/analisis_datos_9VAR.R, que genera como salida su contenido en el directorio 04_Analisis

## Ejecución del visor

El visor ha sido publicado en [Shinyapp](https://www.shinyapps.io/) y es accesible a través del siguiente enlace: [Visor](https://0u6v7k-jorge-gonzlez0del0castillo.shinyapps.io/visor gentrificacion/)

Si se desea, es posible ejecutarlo desde RStudio. Previamente, ha de asegurarse que se dispone de los últimos datos disponibles en la ruta Visor/Visor_Gentrificacion/DATOS_VISOR
