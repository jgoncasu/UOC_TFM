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
if (!require('sf')) install.packages('sf'); library('sf')

theme_set(theme_bw())

PATH_FICHEROS_DATOS <- 'DATOS_VISOR/'

################################################################################
# Carga los datos en bruto
################################################################################
carga_datos_brutos <- function() {
  ruta_fichero <- 'DAT_Brutos_Londres.csv'
  df = read_csv2(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_integer(), col_double(),
                                                                                   col_double(), col_integer(), col_double()
  ))
  return(df)
}

################################################################################
# Carga los indicadores
################################################################################
carga_indicadores <- function() {
  ruta_fichero <- 'DAT_Indicadores_Londres.csv'
  df = read_csv2(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double()
  ))
  return(df)
}

################################################################################
# Carga los datos en bruto
################################################################################
carga_clusters <- function() {
  ruta_fichero <- 'DAT_Clusters_Gentrificacion_Londres_9VAR.csv'
  df = read_csv2(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), 
                                                                                   col_double(), col_double(), col_double(), col_double(),
                                                                                   col_double(), col_double(), col_double(), col_double(),
                                                                                   col_double(), col_integer(), col_double(), col_integer()
  ))
  return(df)
}

################################################################################
# Carga el shapefile con los barrios de Londres
################################################################################
carga_shapefile <- function() {
  ruta_fichero <- '/shapefile/London_Borough_Excluding_MHW.shp'
  shp = st_read(paste(PATH_FICHEROS_DATOS, ruta_fichero, sep=""))
  return(shp)
}

################################################################################
# Según el año, calcula la longitud del periodo
################################################################################
longitud_periodo <- function(anyo) {
  if (anyo != 2025)
    return(9)
  else
    return(4)
}

################################################################################
# Devuelve un nombre de color en base a un valor:
#   < 0: rojo
#   = 0: gris
#   > 0: verde
################################################################################
valor2color <- function(valor) {
  if (valor < 0)
    return("red")
  if (valor > 0)
    return("green")
  else
    return("blue")
}

################################################################################
# Devuelve un icono en base a un valor
################################################################################
valor2icono <- function(valor) {
  if (valor < 0)
    return("arrow-trend-down")
  if (valor > 0)
    return("arrow-trend-up")
  else
    return("equals")
}


################################################################################
# Lee los orígenes de datos
################################################################################
df_indicadores <- carga_indicadores()
# Valores mínimos y máximos para los gráficos de radar
df_tmp <- df_indicadores %>% select(starts_with("IND_"))
min_value <- min(df_tmp[, sapply(df_tmp, is.numeric)], na.rm = TRUE)
max_value <- max(df_tmp[, sapply(df_tmp, is.numeric)], na.rm = TRUE)

df_tmp_2010 <- df_indicadores %>% filter(YEAR == 2010) %>% select(starts_with("IND_"))
min_value_2010 <- min(df_tmp_2010[, sapply(df_tmp_2010, is.numeric)], na.rm = TRUE)
max_value_2010 <- max(df_tmp_2010[, sapply(df_tmp_2010, is.numeric)], na.rm = TRUE)
df_tmp_2020 <- df_indicadores %>% filter(YEAR == 2020) %>% select(starts_with("IND_"))
min_value_2020 <- min(df_tmp_2020[, sapply(df_tmp_2020, is.numeric)], na.rm = TRUE)
max_value_2020 <- max(df_tmp_2020[, sapply(df_tmp_2020, is.numeric)], na.rm = TRUE)
df_tmp_2025 <- df_indicadores %>% filter(YEAR == 2025) %>% select(starts_with("IND_"))
min_value_2025 <- min(df_tmp_2025[, sapply(df_tmp_2025, is.numeric)], na.rm = TRUE)
max_value_2025 <- max(df_tmp_2025[, sapply(df_tmp_2025, is.numeric)], na.rm = TRUE)

df_datos_brutos <- carga_datos_brutos()

#df_tmp <- df_indicadores %>% select(starts_with("VAR_"))
df_min <- data.frame(t(apply(df_indicadores %>% select(starts_with("IND_")), 2, min, na.rm = TRUE)))
colnames(df_min) <- c("Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda")
df_max <- data.frame(t(apply(df_indicadores %>% select(starts_with("IND_")), 2, max, na.rm = TRUE)))
colnames(df_max) <- c("Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda")

shp <- carga_shapefile()

df_clusters <- carga_clusters()
print(df_clusters)
df_clusters <- df_clusters %>% inner_join(shp, by = c("CODE" = "GSS_CODE"))

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
    df <- df %>% select(-starts_with("VAR_"))
    colnames(df) <- c("Codigo", "Barrio", "Año", "Pob_25_40", "Ind01", "Pob_Blanca", "Ind02", "Salario", "Ind03", "Estudios", "Ind04", "Trafico", "Ind05", "Esp_Vida", "Ind06", "Delitos", "Ind07", "Servicios", "Ind08", "Pr_Vivienda", "Ind09")
    return(df)
  })
  
  carga_indicadores_anyos <- reactive({
    # Filtra por años
    df <- df_indicadores %>% filter(YEAR == input$sldYear & BOROUGH != "London")
    df <- df %>% select(-starts_with("VAR_")) %>% arrange(BOROUGH)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Pob_25_40", "Ind01", "Pob_Blanca", "Ind02", "Salario", "Ind03", "Estudios", "Ind04", "Trafico", "Ind05", "Esp_Vida", "Ind06", "Delitos", "Ind07", "Servicios", "Ind08", "Pr_Vivienda", "Ind09")
    return(df)
  })
  
  carga_datos_brutos_anyos <- reactive({
    # Filtra por años
    limite <- longitud_periodo(as.integer(input$sldYear))
    df <- df_datos_brutos %>% filter(YEAR <= input$sldYear & YEAR >= (as.integer(input$sldYear) - limite))
    df <- df %>% arrange(BOROUGH)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda")
    return(df)
  })
  
  carga_datos_brutos_barrios <- reactive({
    # Filtra por años
    limite <- longitud_periodo(as.integer(input$sldYear))
    df <- df_datos_brutos %>% filter(YEAR <= input$sldYear & YEAR >= (as.integer(input$sldYear) - limite))
    # Filtra por barrios
    if (length(input$selBorough) > 0)
      df <- df %>% filter(CODE %in% input$selBorough)
    df <- df %>% arrange(BOROUGH)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda")
    return(df)
  })
  
  carga_indicadores_barrios <- reactive({
    # Filtra por años
    df <- df_indicadores %>% filter(YEAR == input$sldYear)
    
    # Filtra por barrios
    if (length(input$selBorough) > 0)
      df <- df %>% filter(CODE %in% input$selBorough)
    
    df <- df %>% select(-starts_with("VAR_"), -CODE) %>% arrange(BOROUGH)
    colnames(df) <- c("Barrio", "Año", "Pob_25_40", "Ind01", "Pob_Blanca", "Ind02", "Salario", "Ind03", "Estudios", "Ind04", "Trafico", "Ind05", "Esp_Vida", "Ind06", "Delitos", "Ind07", "Servicios", "Ind08", "Pr_Vivienda", "Ind09")
    
    return(df)
  })
  
  carga_indicadores_radar_barrios <- reactive({
    # Filtra por barrios
    #filtro <- c("E12000007", input$selBoroughRadar)
    filtro <- c(input$selBoroughRadar)
    #df <- df_indicadores %>% filter(CODE %in% c("E12000007", input$selBoroughRadar))
    df <- df_indicadores %>% filter(CODE == input$selBoroughRadar)
    df <- df %>% select(BOROUGH, YEAR, starts_with("IND_"))
    colnames(df) <- c("Barrio", "Año", "Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda")
    return(df)    
  })
  
  carga_clusters_anyo <- reactive({
    df <- df_clusters %>% filter(YEAR == input$sldYear & K == input$selK)
    colnames(df) <- c("Codigo", "Barrio", "Año", "Cluster", "Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda")
    
    return(df)
  })

  
  ################################################################################
  # Mapa gentrificación barrios de Londres
  ################################################################################
  output$mapLondon <- renderLeaflet({
    #clusters <- df_clusters %>% filter(YEAR == input$sldYear)
    clusters <- st_as_sf(df_clusters %>% filter(YEAR == input$sldYear & K == input$selK))
    shp_tra <- st_transform(clusters, crs = 4326)
    
    clusters <- st_make_valid(clusters)
    pal <- colorFactor(palette = "RdYlBu", domain = clusters$CLUSTER)
    unique_clusters <- unique(shp_tra$CLUSTER)
    unique_gentr_index <- unique(shp_tra$GENTRIFICATION_INDEX)
    
    cluster_labels <- shp_tra %>%
      distinct(CLUSTER, GENTRIFICATION_INDEX) %>%
      arrange(CLUSTER)
    
    leaflet(data = shp_tra) %>%
      addTiles() %>%
      #addProviderTiles("Esri.WorldImagery") %>%
      #addProviderTiles("Stamen.TerrainBackground") %>%
      addPolygons(fillColor = ~pal(CLUSTER), 
                  weight = 1, 
                  color = "black", 
                  fillOpacity = 0.7, 
                  #popup = ~paste0("Barrio: <b>", BOROUGH, "</b><br />Cluster: <b>", CLUSTER, "</b>"),
                  label = ~paste0(toupper(BOROUGH), " - Cluster ", CLUSTER),
                  layerId = ~CODE) %>%
      setView(map, lng = -0.0876,
              lat = 51.4872,
              zoom = 9.5) %>%
      addLegend(
        position = "bottomright",
        title = "Índice de Gentrificación",
        #pal = pal,
        colors = pal(cluster_labels$CLUSTER),
        #values = cluster_labels$GENTRIFICATION_INDEX,
        labels = paste0("Clúster ", cluster_labels$CLUSTER, " (", cluster_labels$GENTRIFICATION_INDEX, ")"),
        opacity = 0.7
        #position = "bottomright",
        #title = "Clúster",
        #pal = pal,
        ##values = unique_gentr_index,
        ##labels = paste("Cluster ", unique_gentr_index)
        #values = unique_clusters,
        #labels = paste("Cluster ", unique_clusters)
      )
  })
  
  observeEvent(input$sldYear, {
    shinyjs::hide(id = "indiceContainer")
    shinyjs::hide(id = "valueBoxContainer01")
    shinyjs::hide(id = "valueBoxContainer02")
    shinyjs::hide(id = "valueBoxContainer03")
  })
  
  observeEvent(input$selK, {
    shinyjs::hide(id = "indiceContainer")
    shinyjs::hide(id = "valueBoxContainer01")
    shinyjs::hide(id = "valueBoxContainer02")
    shinyjs::hide(id = "valueBoxContainer03")
  })
  
  observeEvent(input$mapLondon_shape_click, {
    click <- input$mapLondon_shape_click
    if (!is.null(click)) {
      clicked_code <- click$id
      barrio <- df_clusters %>% filter(CODE == clicked_code & YEAR == input$sldYear & K == input$selK) #%>% distinct(BOROUGH)
      cluster_barrio <- df_clusters %>%
        filter(YEAR == input$sldYear & K == input$selK & CLUSTER == barrio$CLUSTER) %>%
        summarise(
          GENTRIFICATION_INDEX = round(mean(GENTRIFICATION_INDEX, na.rm = TRUE), 2),
          mean_IND_01_AGE = round(mean(IND_01_AGE, na.rm = TRUE), 2),
          mean_IND_02_RACE_WHITE = round(mean(IND_02_RACE_WHITE, na.rm = TRUE), 2),
          mean_IND_03_WEEK_EARNINGS = round(mean(IND_03_WEEK_EARNINGS, na.rm = TRUE), 2),
          mean_IND_04_PERCENT_NVQ4 = round(mean(IND_04_PERCENT_NVQ4, na.rm = TRUE), 2),
          mean_IND_05_CAR_TRAFFIC = round(mean(IND_05_CAR_TRAFFIC, na.rm = TRUE), 2),
          mean_IND_06_EXP_LIFE = round(mean(IND_06_EXP_LIFE, na.rm = TRUE), 2),
          mean_IND_07_CRIMES = round(mean(IND_07_CRIMES, na.rm = TRUE), 2),
          mean_IND_08_SERVICES = round(mean(IND_08_SERVICES, na.rm = TRUE), 2),
          mean_IND_09_HOUSE_PRICE = round(mean(IND_09_HOUSE_PRICE, na.rm = TRUE), 2)
        )
      print(cluster_barrio)
      #output$txt_barrio <- renderText(paste0("BARRIO: ", barrio$BOROUGH))
      
      vb_tmp_ind <- valueBox(
        value = cluster_barrio$GENTRIFICATION_INDEX,
        subtitle = "Índice Gentrificación",
        color = "orange",
        width = NULL,
        icon = icon("circle-info")
      )
      output$ind_cluster <- renderValueBox(vb_tmp_ind)
      
      vb_tmp_01 <- valueBox(
        value = cluster_barrio$mean_IND_01_AGE,
        subtitle = "Indicador: Variación población 24-50 años",
        color = valor2color(cluster_barrio$mean_IND_01_AGE),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_01_AGE), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_01 <- renderValueBox(vb_tmp_01)

      vb_tmp_02 <- valueBox(
        value = cluster_barrio$mean_IND_02_RACE_WHITE,
        subtitle = "Indicador: Variación habitantes raza blanca",
        color = valor2color(cluster_barrio$mean_IND_02_RACE_WHITE),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_02_RACE_WHITE), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_02 <- renderValueBox(vb_tmp_02)

      vb_tmp_03 <- valueBox(
        value = cluster_barrio$mean_IND_03_WEEK_EARNINGS,
        subtitle = "Indicador: Variación salario semanal",
        color = valor2color(cluster_barrio$mean_IND_03_WEEK_EARNINGS),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_03_WEEK_EARNINGS), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_03 <- renderValueBox(vb_tmp_03)
      
      vb_tmp_04 <- valueBox(
        value = cluster_barrio$mean_IND_04_PERCENT_NVQ4,
        subtitle = "Indicador: Variación titulados superiores",
        color = valor2color(cluster_barrio$mean_IND_04_PERCENT_NVQ4),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_04_PERCENT_NVQ4), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_04 <- renderValueBox(vb_tmp_04)
      
      vb_tmp_05 <- valueBox(
        value = cluster_barrio$mean_IND_05_CAR_TRAFFIC,
        subtitle = "Indicador: Variación tráfico",
        color = valor2color(cluster_barrio$mean_IND_05_CAR_TRAFFIC),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_05_CAR_TRAFFIC), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_05 <- renderValueBox(vb_tmp_05)
      
      vb_tmp_06 <- valueBox(
        value = cluster_barrio$mean_IND_06_EXP_LIFE,
        subtitle = "Indicador: Variación esperanza de vida",
        color = valor2color(cluster_barrio$mean_IND_06_EXP_LIFE),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_06_EXP_LIFE), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_06 <- renderValueBox(vb_tmp_06)
      
      vb_tmp_07 <- valueBox(
        value = cluster_barrio$mean_IND_07_CRIMES,
        subtitle = "Indicador: Variación delitos semanales",
        color = valor2color(cluster_barrio$mean_IND_07_CRIMES),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_07_CRIMES), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_07 <- renderValueBox(vb_tmp_07)
      
      vb_tmp_08 <- valueBox(
        value = cluster_barrio$mean_IND_08_SERVICES,
        subtitle = "Indicador: Variación servicios",
        color = valor2color(cluster_barrio$mean_IND_08_SERVICES),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_08_SERVICES), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_08 <- renderValueBox(vb_tmp_08)
      
      vb_tmp_09 <- valueBox(
        value = cluster_barrio$mean_IND_09_HOUSE_PRICE,
        subtitle = "Indicador: Variación precio de la vivienda",
        color = valor2color(cluster_barrio$mean_IND_09_HOUSE_PRICE),
        width = NULL,
        icon = icon(valor2icono(cluster_barrio$mean_IND_09_HOUSE_PRICE), style = "color: white; opacity: 0.6;")
      )
      output$ind_barrio_09 <- renderValueBox(vb_tmp_09)
      
      shinyjs::show(id = "indiceContainer")
      shinyjs::show(id = "valueBoxContainer01")
      shinyjs::show(id = "valueBoxContainer02")
      shinyjs::show(id = "valueBoxContainer03")
    }
  })
  
  
  ################################################################################
  # Análisis visual de indicadores
  ################################################################################
  # Radar de indicadores para el barrio
  plot_indicadores_2010 <- function() {
    ggradar(carga_indicadores_radar_barrios() %>% filter(Año == 2010) %>% select(-Año), 
            #grid.min = min_value, grid.max = max_value, 
            grid.min = min_value_2010, grid.max = max_value_2010, 
            legend.position = "bottom", 
            values.radar = c(min_value_2010, (abs(min_value_2010) + abs(max_value_2010)) / 2, max_value_2010),
            group.line.width = 1,
            group.point.size = 3#,
            #group.colours = c("#DF536B", "#2297E6")
            )
  }
  
  plot_indicadores_2020 <- function() {
    ggradar(carga_indicadores_radar_barrios() %>% filter(Año == 2020) %>% select(-Año), 
            #grid.min = min_value, grid.max = max_value, 
            grid.min = min_value_2020, grid.max = max_value_2020, 
            legend.position = "bottom", 
            values.radar = c(min_value_2020, (abs(min_value_2020) + abs(max_value_2020)) / 2, max_value_2020),
            group.line.width = 1,
            group.point.size = 3#,
            #group.colours = c("#DF536B", "#2297E6")
            )
  }
  
  plot_indicadores_2025 <- function() {
    ggradar(carga_indicadores_radar_barrios() %>% filter(Año == 2025) %>% select(-Año), 
            #grid.min = min_value, grid.max = max_value, 
            grid.min = min_value_2025, grid.max = max_value_2025, 
            legend.position = "bottom", 
            values.radar = c(min_value_2025, (abs(min_value_2025) + abs(max_value_2025)) / 2, max_value_2025),
            group.line.width = 1,
            group.point.size = 3#,
            #group.colours = c("#DF536B", "#2297E6")
            )
  }
  
  output$radar_2010 <- renderPlot({
    plot_indicadores_2010()
  })
  
  output$radar_2020 <- renderPlot({
    plot_indicadores_2020()
  })
  
  output$radar_2025 <- renderPlot({
    plot_indicadores_2025()
  })
  
  plot_dot_2010 <- function() {
    df_tmp <- carga_indicadores_radar_barrios() %>% filter(Año == 2010) %>%
      pivot_longer(
        cols = c(Pob_25_40, Pob_Blanca, Salario, Estudios, Trafico, Esp_Vida, Delitos, Servicios, Pr_Vivienda),
        names_to = "Variable",
        values_to = "Valor"
      )
    ggplot(df_tmp,
           aes(x=Valor, y=factor(Variable, levels = c("Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda"))
               , label=Valor)) +
      geom_point(stat='identity', aes(col=Barrio), size=5) +
      scale_color_manual(values=c("#DF536B", "#2297E6")) +
      labs(y = "Variables", x = "% Variación")
  }
  
  plot_dot_2020 <- function() {
    df_tmp <- carga_indicadores_radar_barrios() %>% filter(Año == 2020) %>%
      pivot_longer(
        cols = c(Pob_25_40, Pob_Blanca, Salario, Estudios, Trafico, Esp_Vida, Delitos, Servicios, Pr_Vivienda),
        names_to = "Variable",
        values_to = "Valor"
      )
    ggplot(df_tmp,
           aes(x=Valor, y=factor(Variable, levels = c("Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda"))
               , label=Valor)) +
      geom_point(stat='identity', aes(col=Barrio), size=5) +
      scale_color_manual(values=c("#DF536B", "#2297E6")) +
      labs(y = "Variables", x = "% Variación")
  }
  
  plot_dot_2025 <- function() {
    df_tmp <- carga_indicadores_radar_barrios() %>% filter(Año == 2025) %>%
      pivot_longer(
        cols = c(Pob_25_40, Pob_Blanca, Salario, Estudios, Trafico, Esp_Vida, Delitos, Servicios, Pr_Vivienda),
        names_to = "Variable",
        values_to = "Valor"
      )
    ggplot(df_tmp,
           aes(x=Valor, y=factor(Variable, levels = c("Pob_25_40", "Pob_Blanca", "Salario", "Estudios", "Trafico", "Esp_Vida", "Delitos", "Servicios", "Pr_Vivienda"))
               , label=Valor)) +
      geom_point(stat='identity', aes(col=Barrio), size=5) +
      scale_color_manual(values=c("#DF536B", "#2297E6")) +
      labs(y = "Variables", x = "% Variación")
  }

  output$dot_2010 <- renderPlot({
    plot_dot_2010()
  })
  
  output$dot_2020 <- renderPlot({
    plot_dot_2020()
  })
  
  output$dot_2025 <- renderPlot({
    plot_dot_2025()
  })
  
  
  ################################################################################
  # Explorador de indicadores
  ################################################################################
  output$tblIndicadoresLondres <- DT::renderDT(
    expr = DT::datatable(
      carga_indicadores_londres_anyos() %>% mutate(across(starts_with("ind"), ~ .x / 100)), rownames = FALSE, options = list(scrollX = TRUE,
                                                                                                                             dom = 't',
                                                                                                                             columnDefs = list(list(visible = FALSE, targets = c(0))))
    ) %>% formatStyle(c('Ind01','Ind02','Ind03','Ind04','Ind05','Ind06','Ind07','Ind08','Ind09'), 
                      color = styleInterval(c(-0.0001, 0), c('red', 'black', 'limegreen'))) 
  )
  
  output$tblIndicadores <- DT::renderDT(
    expr = DT::datatable(
      carga_indicadores_anyos() %>% mutate(across(starts_with("var"), ~ .x / 100)), rownames = FALSE, options = list(scrollX = TRUE, 
                                                                                                                     columnDefs = list(list(visible=FALSE, targets=c(0))))
    ) %>% formatStyle(c('Ind01','Ind02','Ind03','Ind04','Ind05','Ind06','Ind07','Ind08','Ind09'), 
                      color = styleInterval(c(-0.01, 0), c('red', 'black', 'limegreen')))
  )

    
  ################################################################################
  # Evolución datos por períodos
  ################################################################################
  plot_datos_brutos_01 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pob_25_40)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_02 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pob_Blanca)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_03 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Salario)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_04 <- function() {
    test <- carga_datos_brutos_barrios()
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Estudios)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_05 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Trafico)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_06 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Esp_Vida)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite,input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_07 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Delitos)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_08 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Servicios)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
    return(g)
  }
  
  plot_datos_brutos_09 <- function() {
    if (length(input$selBorough) == 0) # Si no hay filtros, se filtra por el total de la ciudad de Londres
      filtro <- c("E12000007")
    else
      filtro <- input$selBorough
    limite <- longitud_periodo(as.integer(input$sldYear))
    g <- ggplot(carga_datos_brutos_barrios() %>% filter(Codigo %in% filtro), aes(x=Año, group=Barrio, color=Barrio)) +
      geom_line(aes(y=Pr_Vivienda)) +
      theme(legend.position = "top") +
      scale_x_discrete(name="Año", limits=seq(as.integer(input$sldYear) - limite, input$sldYear, by=2))
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
  
  ################################################################################
  # Explorador de datos
  ################################################################################
  # Tabla del explorador de datos
  output$tblDatos <- DT::renderDT(
    expr = carga_datos_brutos_anyos(), 
    rownames = FALSE, 
    options = list(scrollX = TRUE,
      #stateSave = TRUE,
      #dom = 'tlip',
      columnDefs = list(list(visible=FALSE, targets=c(0)))
      )
    )
}