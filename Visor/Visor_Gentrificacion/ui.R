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
  df <- df %>% arrange(Borough)
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
          fluidRow(
            column(6, tags$h5("Edad media")),
            column(6, tags$h5("Población raza blanca"))
          ),
          fluidRow(
            column(6, plotOutput("plt_01"), height="300px"),
            column(6, plotOutput("plt_02"), height="300px")
          ),
          fluidRow(
            column(6, tags$h5("Salario medio")),
            column(6, tags$h5("Población con estudios superiores"))
          ),
          fluidRow(
            column(6, plotOutput("plt_03"), height="300px"),
            column(6, plotOutput("plt_04"), height="300px")
          ),
          fluidRow(
            column(6, tags$h5("Volumen de tráfico")),
            column(6, tags$h5("Esperanza media de vida"))
          ),
          fluidRow(
            column(6, plotOutput("plt_05"), height="300px"),
            column(6, plotOutput("plt_06"), height="300px")
          ),
          fluidRow(
            column(6, tags$h5("Media de delitos por semana")),
            column(6, tags$h5("Volumen de servicios (restaurantes, tiendas)"))
          ),
          fluidRow(
            column(6, plotOutput("plt_07"), height="300px"),
            column(6, plotOutput("plt_08"), height="300px")
          ),
          fluidRow(
            column(6, tags$h5("Precio medio de la vivienda")),
            column(6, tags$h5("Precio medio del alquiler social"))
          ),
          fluidRow(
            column(6, plotOutput("plt_09"), height="300px"),
            column(6, plotOutput("plt_10"), height="300px")
          )
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