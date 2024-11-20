library(shiny)
library(bslib)
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')

PATH_FICHEROS_BARRIOS <- 'DATOS/'

################################################################################
# Carga los barrios
################################################################################
carga_lista_barrios <- function() {
  ruta_fichero <- 'barrios_londres.csv'
  df = read_csv(paste(PATH_FICHEROS_BARRIOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_character()))
  return(df)
}

################################################################################
# Lógica de la aplicación
################################################################################
function(input, output, session) {

  # Filtro por barrios
  output$selDynBorough <- renderUI({
    df_barrios <- carga_lista_barrios()
    lista_barrios <- setNames(df_barrios$Code, df_barrios$Borough)
    selectizeInput(inputId = "selDynBorough", label = "Lista de barrios (max. 3)", choices = lista_barrios, multiple = TRUE, options = list(maxItems = 3))
  })
}
