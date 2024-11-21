library(shiny)
library(bslib)
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('DT')) install.packages('DT'); library('DT')

PATH_FICHEROS_DATOS <- 'DATOS/'

################################################################################
# Carga los indicadores
################################################################################
carga_indicadores <- function() {
  ruta_fichero <- 'DAT_Indicadores_Londres.csv'
  df = read_csv(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_integer(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_integer(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_integer(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double()
  ))
  return(df)
}

################################################################################
# Lee los orígenes de datos
################################################################################
df_indicadores <- carga_indicadores()

################################################################################
# Lógica de la aplicación
################################################################################
function(input, output, session) {

  carga_indicadores_barrios <- reactive({
    # Filtra por años
    df <- df_indicadores %>% filter(YEAR == input$sldYear)
    
    # Filtra por barrios
    if (length(input$selBorough) > 0)
      df <- df %>% filter(CODE %in% input$selBorough)

    df <- df %>% select(-starts_with("IND_"), -CODE) %>% arrange(BOROUGH)
    colnames(df) <- c("Barrio", "Año", "Edad", "Var01", "Blancos", "Var02", "Salario", "Var03", "Estudios", "Var04", "Tráfico", "Var05", "Esp.Vida", "Var06", "Delitos", "Var07", "Servicios", "Var08", "Pr.Vivienda", "Var09", "Alquiler", "Var10")
    
    return(df)
  })
  
  # Tabla del explorador de datos
  output$tblDatos <- DT::renderDT(expr = carga_indicadores_barrios(), rownames = FALSE, options = list(scrollX = TRUE, dom = 'tlip'))
}
