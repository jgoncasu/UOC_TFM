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
# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
library('ggradar')

theme_set(theme_bw())

PATH_FICHEROS_DATOS <- 'DATOS/'

################################################################################
# Carga los datos en bruto
################################################################################
carga_datos_brutos <- function() {
  ruta_fichero <- 'DAT_Brutos_Londres.csv'
  df = read_csv(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_integer(), col_double(),
                                                                                   col_double(), col_integer(), col_double(),
                                                                                   col_double()
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
# Valores mínimos y máximos para los gráficos de radar
df_tmp <- df_indicadores %>% select(starts_with("VAR_"))
min_value <- min(df_tmp[, sapply(df_tmp, is.numeric)], na.rm = TRUE)
max_value <- max(df_tmp[, sapply(df_tmp, is.numeric)], na.rm = TRUE)
df_datos_brutos <- carga_datos_brutos()

#df_tmp <- df_indicadores %>% select(starts_with("VAR_"))
df_min <- data.frame(t(apply(df_indicadores %>% select(starts_with("VAR_")), 2, min, na.rm = TRUE)))
colnames(df_min) <- c("Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler")
df_max <- data.frame(t(apply(df_indicadores %>% select(starts_with("VAR_")), 2, max, na.rm = TRUE)))
colnames(df_max) <- c("Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler")


################################################################################
# Lógica de la aplicación
################################################################################
function(input, output, session) {
  
  ################################################################################
  # Carga de datos
  ################################################################################
  carga_indicadores_londres_anyos <- reactive({
    # Filtra por años
    df <- df_indicadores %>% filter(YEAR == input$sldYear & BOROUGH == "London")
    df <- df %>% select(-starts_with("IND_"))
    colnames(df) <- c("Codigo", "Barrio", "Año", "Edad", "Var01", "Pob_Blanca", "Var02", "Salario", "Var03", "Estudios", "Var04", "Trafico", "Var05", "Esp_Vida", "Var06", "Delitos", "Var07", "Servicios", "Var08", "Pr_Vivienda", "Var09", "Alquiler", "Var10")
    return(df)
  })
  
  carga_indicadores_anyos <- reactive({
    # Filtra por años
    df <- df_indicadores %>% filter(YEAR == input$sldYear & BOROUGH != "London")
    df <- df %>% select(-starts_with("IND_")) %>% arrange(BOROUGH)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Edad", "Var01", "Pob_Blanca", "Var02", "Salario", "Var03", "Estudios", "Var04", "Trafico", "Var05", "Esp_Vida", "Var06", "Delitos", "Var07", "Servicios", "Var08", "Pr_Vivienda", "Var09", "Alquiler", "Var10")
    return(df)
  })
  
  carga_datos_brutos_anyos <- reactive({
    # Filtra por años
    df <- df_datos_brutos %>% filter(YEAR <= input$sldYear & YEAR >= (input$sldYear - 10))
    df <- df %>% arrange(BOROUGH)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler")
    return(df)
  })
  
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
  
  carga_indicadores_radar_barrios <- reactive({
    # Filtra por barrios
    filtro <- c("E12000007", input$selBoroughRadar)
    df <- df_indicadores %>% filter(CODE %in% c("E12000007", input$selBoroughRadar))
    df <- df %>% select(BOROUGH, YEAR, starts_with("VAR_"))
    colnames(df) <- c("Barrio", "Año", "Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler")
    return(df)    
  })    

  ################################################################################
  # Mapa gentrifcación barrios de Londres
  ################################################################################
  output$mapLondon <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #addProviderTiles("Esri.WorldImagery") %>%
      #addProviderTiles("Stamen.TerrainBackground") %>%
      addMarkers(lng = -0.1277,
                 lat = 51.5072,
                 popup = "You are here",
                 options = markerOptions(draggable = TRUE, riseOnHover = TRUE)) %>%
      addCircleMarkers(lng = -0.1377,
                       lat = 51.5472,
                       popup = "You are here",
                       fillColor = "lightblue", opacity = 0.8,
                       options = markerOptions(draggable = TRUE, title = "Whoops")) %>%
      setView(map, lng = -0.1276,
              lat = 51.5072,
              zoom = 9.5)
  })
  
  ################################################################################
  # Análisis visual de indicadores
  ################################################################################
  # Radar de indicadores para el barrio
  plot_indicadores_2011 <- function() {
    #df <- carga_indicadores_radar_barrios() %>% filter(Año == 2011) %>% select(-Año)
    #print(df)
    ggradar(carga_indicadores_radar_barrios() %>% filter(Año == 2011) %>% select(-Año), 
            grid.min = min_value, grid.max = max_value, legend.position = "bottom", 
            values.radar = c(min_value, (abs(min_value) + abs(max_value)) / 2, max_value),
            group.line.width = 1,
            group.point.size = 3,
            group.colours = c("#DF536B", "#2297E6"))
  }
  
  plot_indicadores_2021 <- function() {
    ggradar(carga_indicadores_radar_barrios() %>% filter(Año == 2021) %>% select(-Año), 
            grid.min = min_value, grid.max = max_value, legend.position = "bottom", 
            values.radar = c(min_value, (abs(min_value) + abs(max_value)) / 2, max_value),
            group.line.width = 1,
            group.point.size = 3,
            group.colours = c("#DF536B", "#2297E6"))
  }
  
  plot_indicadores_2031 <- function() {
    ggradar(carga_indicadores_radar_barrios() %>% filter(Año == 2031) %>% select(-Año), 
            grid.min = min_value, grid.max = max_value, legend.position = "bottom", 
            values.radar = c(min_value, (abs(min_value) + abs(max_value)) / 2, max_value),
            group.line.width = 1,
            group.point.size = 3,
            group.colours = c("#DF536B", "#2297E6"))
  }
  
  output$radar_2011 <- renderPlot({
    plot_indicadores_2011()
  })
  
  output$radar_2021 <- renderPlot({
    plot_indicadores_2021()
  })
  
  output$radar_2031 <- renderPlot({
    plot_indicadores_2031()
  })
  
  plot_dot_2011 <- function() {
    df_tmp <- carga_indicadores_radar_barrios() %>% filter(Año == 2011) %>%
      pivot_longer(
        cols = c(Edad, Pob_Blanca, Salario, Estudios, Trafico, Esp_Vida, Delitos, Servicios, Pr_Vivienda, Alquiler),
        names_to = "Variable",
        values_to = "Valor"
      )
    ggplot(df_tmp,
           aes(x=Valor, y=factor(Variable, levels = c("Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler"))
               , label=Valor)) +
      geom_point(stat='identity', aes(col=Barrio), size=5) +
      scale_color_manual(values=c("#DF536B", "#2297E6")) +
      labs(y = "Variables", x = "% Variación")
  }

  plot_dot_2021 <- function() {
    df_tmp <- carga_indicadores_radar_barrios() %>% filter(Año == 2021) %>%
      pivot_longer(
        cols = c(Edad, Pob_Blanca, Salario, Estudios, Trafico, Esp_Vida, Delitos, Servicios, Pr_Vivienda, Alquiler),
        names_to = "Variable",
        values_to = "Valor"
      )
    ggplot(df_tmp,
           aes(x=Valor, y=factor(Variable, levels = c("Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler"))
               , label=Valor)) +
      geom_point(stat='identity', aes(col=Barrio), size=5) +
      scale_color_manual(values=c("#DF536B", "#2297E6")) +
      labs(y = "Variables", x = "% Variación")
  }

  plot_dot_2031 <- function() {
    df_tmp <- carga_indicadores_radar_barrios() %>% filter(Año == 2031) %>%
      pivot_longer(
        cols = c(Edad, Pob_Blanca, Salario, Estudios, Trafico, Esp_Vida, Delitos, Servicios, Pr_Vivienda, Alquiler),
        names_to = "Variable",
        values_to = "Valor"
      )
    ggplot(df_tmp,
           aes(x=Valor, y=factor(Variable, levels = c("Edad", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda", "Alquiler"))
               , label=Valor)) +
      geom_point(stat='identity', aes(col=Barrio), size=5) +
      scale_color_manual(values=c("#DF536B", "#2297E6")) +
      labs(y = "Variables", x = "% Variación")
  }
  
  output$dot_2011 <- renderPlot({
    plot_dot_2011()
  })

  output$dot_2021 <- renderPlot({
    plot_dot_2021()
  })
  
  output$dot_2031 <- renderPlot({
    plot_dot_2031()
  })
  
  ################################################################################
  # Explorador de indicadores
  ################################################################################
  output$tblIndicadoresLondres <- DT::renderDT(
    expr = DT::datatable(
      carga_indicadores_londres_anyos() %>% mutate(across(starts_with("var"), ~ .x / 100)), rownames = FALSE, options = list(scrollX = TRUE,
                                                                                 dom = 't',
                                                                                 columnDefs = list(list(visible = FALSE, targets = c(0))))
    ) %>% formatStyle(c('Var01','Var02','Var03','Var04','Var05','Var06','Var07','Var08','Var09','Var10'), 
                      color = styleInterval(c(-0.0001, 0), c('red', 'black', 'limegreen'))) %>%
      formatPercentage(c('Var01','Var02','Var03','Var04','Var05','Var06','Var07','Var08','Var09','Var10'), 2)
  )
  
  output$tblIndicadores <- DT::renderDT(
    expr = DT::datatable(
      carga_indicadores_anyos() %>% mutate(across(starts_with("var"), ~ .x / 100)), rownames = FALSE, options = list(scrollX = TRUE, 
                                                                  columnDefs = list(list(visible=FALSE, targets=c(0))))
    ) %>% formatStyle(c('Var01','Var02','Var03','Var04','Var05','Var06','Var07','Var08','Var09','Var10'), 
                      color = styleInterval(c(-0.01, 0), c('red', 'black', 'limegreen'))) %>%
      formatPercentage(c('Var01','Var02','Var03','Var04','Var05','Var06','Var07','Var08','Var09','Var10'), 2)
  )
  
  ################################################################################
  # Evolución datos por décadas
  ################################################################################
  plot_datos_brutos_01 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Edad)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }

  plot_datos_brutos_02 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pob_Blanca)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_03 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Salario)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_04 <- function() {
    test <- carga_datos_brutos_barrios()
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Estudios)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_05 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Trafico)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_06 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Esp_Vida)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_07 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Delitos)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_08 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Servicios)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_09 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pr_Vivienda)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_10 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Alquiler)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(input$sldYear - 10,input$sldYear, by=2))
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

  ################################################################################
  # Explorador de datos
  ################################################################################
  # Tabla del explorador de datos
  output$tblDatos <- DT::renderDT(expr = carga_datos_brutos_anyos(), rownames = FALSE, options = list(scrollX = TRUE, 
                                                                                                        #dom = 'tlip', 
                                                                                                        columnDefs = list(list(visible=FALSE, targets=c(0)))))
}
