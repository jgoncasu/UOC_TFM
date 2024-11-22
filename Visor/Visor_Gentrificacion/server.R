library(shiny)
library(bslib)
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')

theme_set(theme_bw())

PATH_FICHEROS_DATOS <- 'DATOS/'

################################################################################
# Carga los datos en bruto
################################################################################
carga_datos_brutos <- function() {
  ruta_fichero <- 'DAT_Brutos_Londres.csv'
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
df_datos_brutos <- carga_datos_brutos()
#df_londres_brutos <- df_datos_brutos %>% filter(BOROUGH == "London")
#df_datos_brutos <- df_datos_brutos %>% filter(BOROUGH != "London")

################################################################################
# Lógica de la aplicación
################################################################################
function(input, output, session) {
  
  carga_datos_brutos_barrios <- reactive({
    # Filtra por años
    df <- df_datos_brutos %>% filter(YEAR <= input$sldYear & YEAR >= (input$sldYear - 10))
    
    # Filtra por barrios
    if (length(input$selBorough) > 0)
      df <- df %>% filter(CODE %in% input$selBorough)
    
    df <- df %>% arrange(BOROUGH)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler")
    
    return(df)
  })

  carga_indicadores_barrios <- reactive({
    # Filtra por años
    df <- df_indicadores %>% filter(YEAR == input$sldYear)
    
    # Filtra por barrios
    if (length(input$selBorough) > 0)
      df <- df %>% filter(CODE %in% input$selBorough)

    df <- df %>% select(-starts_with("IND_"), -CODE) %>% arrange(BOROUGH)
    colnames(df) <- c("Barrio", "Año", "Edad", "Var01", "Pob_Blanca", "Var02", "Salario", "Var03", "Estudios", "Var04", "Trafico", "Var05", "Esp_Vida", "Var06", "Delitos", "Var07", "Servicios", "Var08", "Pr_Vivienda", "Var09", "Alquiler", "Var10")
    
    return(df)
  })
  
  plot_datos_brutos_01 <- function() {
    #filtro <- ifelse(length(input$selBorough) == 0, c("E12000007"), input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    if (length(input$selBorough) == 0)
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    print(input$selBorough)
    print(filtro)
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Edad)) 
    return(g)
  }

  plot_datos_brutos_02 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pob_Blanca))
    return(g)
  }
  
  plot_datos_brutos_03 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Salario))
    return(g)
  }
  
  plot_datos_brutos_04 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Estudios))
    return(g)
  }
  
  plot_datos_brutos_05 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Trafico))
    return(g)
  }
  
  plot_datos_brutos_06 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Esp_Vida))
    return(g)
  }
  
  plot_datos_brutos_07 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Delitos))
    return(g)
  }
  
  plot_datos_brutos_08 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Servicios))
    return(g)
  }
  
  plot_datos_brutos_09 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pr_Vivienda))
    return(g)
  }
  
  plot_datos_brutos_10 <- function() {
    filtro <- ifelse(length(input$selBorough) == 0, "E12000007", input$selBorough)  # Si no hay filtros, se filtra por el total de la ciudad de Londres
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Alquiler))
    return(g)
  }
  
  # Gráficas de indicadores
  output$plt_01 <- renderPlot({
    plot_datos_brutos_01()
  })

    output$plt_02 <- renderPlot({
      plot_datos_brutos_02()
    })
    
    output$plt_03 <- renderPlot({
      plot_datos_brutos_03()
    })
    
    output$plt_04 <- renderPlot({
      plot_datos_brutos_04()
    })
    
    output$plt_05 <- renderPlot({
      plot_datos_brutos_05()
    })
    
    output$plt_06 <- renderPlot({
      plot_datos_brutos_06()
    })
    
    output$plt_07 <- renderPlot({
      plot_datos_brutos_07()
    })
    
    output$plt_08 <- renderPlot({
      plot_datos_brutos_08()
    })
    
    output$plt_09 <- renderPlot({
      plot_datos_brutos_09()
    })
    
    output$plt_10 <- renderPlot({
      plot_datos_brutos_10()
    })
    
    # Tabla del explorador de datos
    output$tblDatos <- DT::renderDT(expr = carga_datos_brutos_barrios(), rownames = FALSE, options = list(scrollX = TRUE, 
                                                                                                          dom = 'tlip', 
                                                                                                          columnDefs = list(list(visible=FALSE, targets=c(0)))))
    
}
