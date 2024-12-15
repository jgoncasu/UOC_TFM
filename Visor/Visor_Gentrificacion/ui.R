library(shiny)
library(shinydashboard)
library(bslib)
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('leaflet')) install.packages('leaflet'); library('leaflet')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('sf')) install.packages('sf'); library('sf')
# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
library('ggradar')
library(shinyjs)

PATH_FICHEROS_DATOS <- 'DATOS_VISOR/'

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
df_barrios_sin_londres <- df_barrios %>% filter(Borough != "London")
lista_barrios <- setNames(df_barrios$Code, df_barrios$Borough)
lista_solo_barrios <- setNames(df_barrios_sin_londres$Code, df_barrios_sin_londres$Borough)

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
  selectInput(
    inputId = "sldYear",
    label = "Seleccione un año",
    choices = list("2010" = 2010, "2020" = 2020, "2025" = 2025),
    selected = 2010
  )

  # Filtro por barrios
#  selectizeInput(
#    inputId = "selBorough",
#    label = "Lista de barrios (max. 3)",
#    choices = lista_barrios,
#    multiple = TRUE,
#    options = list(maxItems = 3)
#  )
)

body <- dashboardBody(
  tags$br(),
  tags$h2("Análisis de la gentrificación en Londres"),
  page_fluid(
    useShinyjs(),
    
    tags$style(HTML("
      .col-sm-4 {
        width: 100% !important;
        margin: 0 !important;
      }
    ")),
    
    accordion(
        # Mapa gentrificación
        accordion_panel(
          title = "Gentrificación de barrios",
          sliderInput(
            inputId = "selK",
            label = "Seleccione un valor de K",
            min = 2,
            max = 20,
            value = 2,
            step = 1
          ),
          leafletOutput(outputId = "mapLondon", width="650px", height="550px"),
          verbatimTextOutput("info"),
          # ValueBoxes para mostrar la información de los indicadores del barrio
          #fluidRow(
          #  column(12, textOutput("txt_barrio"))
          #),
          fluidRow(
            div(id = "indiceContainer",
                style = "display: flex; flex-wrap: wrap; gap: 5px; ",
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_cluster"))
                ),
            div(id = "valueBoxContainer",
                style = "display: flex; flex-wrap: wrap; gap: 5px; ",
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_01")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_02")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_03")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_04")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_05")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_06")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_07")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_08")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_09")),
                div(style = "flex: 1 1 18%; max-width: 18%; margin: 1px;", valueBoxOutput("ind_barrio_10"))
            )
          )
        ),
        
        # Análisis visual de indicadores
        accordion_panel(
          title = "Análisis visual de indicadores",
          # Filtro por barrios
          selectizeInput(
            inputId = "selBoroughRadar",
            label = "Filtro por barrios",
            choices = lista_solo_barrios,
            multiple = FALSE
          ),
          fluidRow(
            column(6, tags$h5("Año 2010"))
            #column(6, tags$h5("Año 2020"))
          ),
          fluidRow(
            column(6, plotOutput("radar_2010"), height="300px"),
            column(6, plotOutput("dot_2010"), height="300px")
          ),
          fluidRow(
            column(6, tags$h5("Año 2020"))
          ),
          fluidRow(
            column(6, plotOutput("radar_2020"), height="300px"),
            column(6, plotOutput("dot_2020"), height="300px")
          ),
          fluidRow(
            column(6, tags$h5("Año 2025"))
          ),
          fluidRow(
            column(6, plotOutput("radar_2025"), height="300px"),
            column(6, plotOutput("dot_2025"), height="300px")
          )
        ),
        
        # Explorador de indicadores
        accordion_panel(
          title = "Explorador de indicadores",
          tags$h3("Indicadores ciudad de Londres"),
          DTOutput("tblIndicadoresLondres"),
          tags$br(),
          tags$h3("Indicadores barrios de Londres"),
          DTOutput("tblIndicadores")
        ),
        
        # Evolución datos por períodos
        accordion_panel(
          title = "Evolución datos por períodos",

          # Filtro por barrios
          selectizeInput(
            inputId = "selBorough",
            label = "Filtro por barrios",
            #label = "Lista de barrios (max. 3)",
            choices = lista_barrios,
            multiple = TRUE#,
            #options = list(maxItems = 3)
          ),

          fluidRow(
            column(6, tags$h5("Edad media")),
            column(6, tags$h5("Población raza blanca"))
          ),
          fluidRow(
            column(6, plotOutput("plt_01"), height="100px"),
            column(6, plotOutput("plt_02"), height="100px")
          ),
          fluidRow(
            column(6, tags$h5("Salario medio semanal")),
            column(6, tags$h5("% Población con estudios superiores"))
          ),
          fluidRow(
            column(6, plotOutput("plt_03"), height="100px"),
            column(6, plotOutput("plt_04"), height="100px")
          ),
          fluidRow(
            column(6, tags$h5("Volumen de tráfico")),
            column(6, tags$h5("Esperanza media de vida"))
          ),
          fluidRow(
            column(6, plotOutput("plt_05"), height="100px"),
            column(6, plotOutput("plt_06"), height="100px")
          ),
          fluidRow(
            column(6, tags$h5("Media de delitos por semana")),
            column(6, tags$h5("Volumen de servicios (restaurantes, tiendas)"))
          ),
          fluidRow(
            column(6, plotOutput("plt_07"), height="100px"),
            column(6, plotOutput("plt_08"), height="100px")
          ),
          fluidRow(
            column(6, tags$h5("Precio medio de la vivienda")),
            column(6, tags$h5("Precio medio del alquiler social"))
          ),
          fluidRow(
            column(6, plotOutput("plt_09"), height="100px"),
            column(6, plotOutput("plt_10"), height="100px")
          )
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