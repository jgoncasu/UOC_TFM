library(shiny)
library(shinydashboard)
library(bslib)

header <- dashboardHeader(
  title = "Dashboard"
  #titleWidth = 600
)

sidebar <- dashboardSidebar(
  tags$br(),
  # Filtro por periodos
  selectInput(
    inputId = "selYear",
    label = "Seleccione un periodo",
    choices = list("2001 - 2011" = "2011", "2011 - 2021" = "2021", "2021 - 2031" = "2031"),
    selected = "2021"
  ),
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
  selectInput(
    inputId = "selBorough",
    label = "Lista de barrios",
    choices = list("City of London" = "AAA", "Camden" = "BBB", "Harley" = "CCC", "Kensington" = "DDD", "Chelsea" = "EEE"),
    multiple = FALSE,
    selectize = FALSE
  ),
  selectizeInput(
    inputId = "selBoroughV2",
    label = "Lista de barrios (max. 3)",
    choices = list("City of London" = "AAA", "Camden" = "BBB", "Harley" = "CCC", "Kensington" = "DDD", "Chelsea" = "EEE"),
    multiple = TRUE,
    options = list(maxItems = 3)
  ),
  uiOutput("selDynBorough")
)

body <- dashboardBody(
  tags$br(),
  tags$h2("Análisis de la gentrificación en Londres"),
  page_fluid(
    #title = "Análisis de la gentrificación en los barrios de Londres",
    #layout_sidebar(
    #  # Filtros
    #  sidebar = sidebar(
    #    title = "Filtros"
    #  ),
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
        ),
     # )
    )
  )
)

dashboardPage(
  #dashboardHeader(disable = TRUE),
  header,
  sidebar, #dashboardSidebar(disable = TRUE),
  body
)