library(shiny)
library(shinydashboard)
library(bslib)
if (!require('DT')) install.packages('DT'); library('DT')

PATH_FICHEROS_DATOS <- 'DATOS/'

################################################################################
# Carga los barrios
################################################################################
carga_lista_barrios <- function() {
  ruta_fichero <- 'barrios_londres.csv'
  df = read_csv(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_character()))
  return(df)
}

df_barrios <- carga_lista_barrios()
lista_barrios <- setNames(df_barrios$Code, df_barrios$Borough)

################################################################################
# Interfaz
################################################################################
header <- dashboardHeader(
  title = "Dashboard"
  #titleWidth = 600
)

sidebar <- dashboardSidebar(
  tags$br(),
  # Filtro por periodos
  sliderInput(
    inputId = "sldYear",
    label = "Seleccione un año",
    min = 2011,
    max = 2031,
    value = 2021,
    step = 10,
    ticks = FALSE
  ),
  
  # Filtro por barrios
  selectizeInput(
    inputId = "selBorough",
    label = "Lista de barrios (max. 3)",
    choices = lista_barrios,
    multiple = TRUE,
    options = list(maxItems = 3)
  )
)

body <- dashboardBody(
  tags$br(),
  tags$h2("Análisis de la gentrificación en Londres"),
  page_fluid(
      accordion(
        # Mapa gentrificación
        accordion_panel(
          title = "Gentrificación de barrios",
        ),
        # Estado indicadores
        accordion_panel(
          title = "Indicadores",
        ),
        # Comparador de barrios
        accordion_panel(
          title = "Comparador por periodos"
        ),
        # Explorador de datos
        accordion_panel(
          title = "Explorador de datos",
          DTOutput("tblDatos")
        ),
     # )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)