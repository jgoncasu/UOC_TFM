#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(bslib)

header <- dashboardHeader(
  title = "Dashboard"
  #titleWidth = 600
)

sidebar <- dashboardSidebar(
  input_task_button("id_01", "Ejemplo filtro")
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