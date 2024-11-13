# Librerías
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('forecast')) install.packages('forecast'); library('forecast')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Constantes
PATH_FICHEROS_ENTRADA <- 'DATOS/01_Fuentes/'
PATH_FICHEROS_SALIDA <- 'DATOS/02_Staging/'

################################################################################
# 00) Carga y limpieza de datos de los datos de los barrios
################################################################################

carga_lista_barrios <- function() {
  #ruta_fichero <- '00_Barrios/barrios_londres.csv'
  ruta_fichero <- '00_Barrios/barrios_londres_Con_Londres_y_UK.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df)
}


################################################################################
# 01) Carga y limpieza de datos de los indicadores demográficos (edad)
################################################################################

carga_indicador_01_edad <- function() {
  ruta_fichero <- '01_Demograficos/ons-mye-custom-age-tool-2020.xlsx'
  nulos <- c("", "-")
  edades <- 0:90
  col_hombres <- paste0("M_", edades)
  col_mujeres <- paste0("F_", edades)
  columnas <- c("Code", "Year", "Borough", col_hombres, "No_Males", col_mujeres)
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Single year of age", skip = 3, na = nulos, col_names = columnas)
  return(df)
}

limpieza_indicador_01_edad <- function(df) {
  # TODO: Ver si nos quedamos con edad media por año o la cantidad de personas jóvenes (ver rango de edad)
  
  # Elimina la columna "No_Males"
  df$No_Males <- NULL
  
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))

  # Imputa valores NA de los años 1999 y 2000 para los grupos de edad mayores a 85 años
  df <- df %>% mutate(
    M_86 = ifelse(Year %in% c(1999, 2000), round(M_85 / 6, 0), M_86),
    M_87 = ifelse(Year %in% c(1999, 2000), round(M_85 / 6, 0), M_87),
    M_88 = ifelse(Year %in% c(1999, 2000), round(M_85 / 6, 0), M_88),
    M_89 = ifelse(Year %in% c(1999, 2000), round(M_85 / 6, 0), M_89),
    M_90 = ifelse(Year %in% c(1999, 2000), trunc(M_85 / 6, 0), M_90),
    M_85 = ifelse(Year %in% c(1999, 2000), round(M_85 / 6, 0), M_85),
    F_86 = ifelse(Year %in% c(1999, 2000), round(F_85 / 6, 0), F_86),
    F_87 = ifelse(Year %in% c(1999, 2000), round(F_85 / 6, 0), F_87),
    F_88 = ifelse(Year %in% c(1999, 2000), round(F_85 / 6, 0), F_88),
    F_89 = ifelse(Year %in% c(1999, 2000), round(F_85 / 6, 0), F_89),
    F_90 = ifelse(Year %in% c(1999, 2000), trunc(F_85 / 6, 0), F_90),
    F_85 = ifelse(Year %in% c(1999, 2000), round(F_85 / 6, 0), F_85),  
  )

  # Calcula la media de edad por año entre hombre y mujeres
  edades <- 0:90
  total_hombres <- rowSums(df %>% select(starts_with("M_")), na.rm = TRUE)
  total_mujeres <- rowSums(df %>% select(starts_with("F_")), na.rm = TRUE)
  total_poblacion <- total_hombres + total_mujeres
  df["Mean_Age_Men"] <- round(rowSums(df %>% select(starts_with("M_")) * edades, na.rm = TRUE) / total_hombres, 2)
  df["Mean_Age_Women"] <- round(rowSums(df %>% select(starts_with("F_")) * edades, na.rm = TRUE) / total_mujeres, 2)
  df["Avg_Age"] <- round(rowSums((df %>% select(starts_with("M_")) + df %>% select(starts_with("F_"))) * edades, na.rm = TRUE) / total_poblacion, 2)

  # Elimina las columnas de datos agrupados por edad
  df <- df %>% select(-starts_with("M_"))
  df <- df %>% select(-starts_with("F_"))
  
  # Imputar valores hasta 2031
  anyos_pred <- 2021:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Men <- ts(df %>% filter(Borough == barrio) %>% pull(Mean_Age_Men), start = 1999, end = 2020, frequency = 1)
    serie_Women <- ts(df %>% filter(Borough == barrio) %>% pull(Mean_Age_Women), start = 1999, end = 2020, frequency = 1)
    serie_Avg <- ts(df %>% filter(Borough == barrio) %>% pull(Avg_Age), start = 1999, end = 2020, frequency = 1)
    modelo_Men <- ets(serie_Men)
    modelo_Women <- ets(serie_Women)
    modelo_Avg <- ets(serie_Avg)
    pred_Men <- forecast(modelo_Men, h = length(anyos_pred))
    pred_Women <- forecast(modelo_Women, h = length(anyos_pred))
    pred_Avg <- forecast(modelo_Avg, h = length(anyos_pred))
    predicciones[["Mean_Age_Men"]] <- round(pred_Men$mean, 2)
    predicciones[["Mean_Age_Women"]] <- round(pred_Women$mean, 2)
    predicciones[["Avg_Age"]] <- round(pred_Avg$mean, 2)
    
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    
    df <- bind_rows(df, predicciones_df)
  }

  # Ordena los datos por barrio
  df <- df %>% arrange(Code, Year) 
  
  # Se predice el valor de la población para los años 2021 y 2031  
#  anyos_pred <- 2021:2031
#  barrios <- unique(df$Borough)
#  for (barrio in barrios) {
#    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
#    predicciones <- list()
#    for (edad in edades) {
#      serie_hombres <- ts(df %>% filter(Borough == barrio) %>% pull(paste0("M_", edad)), start = 1999, end = 2020, frequency = 1)
#      serie_mujeres <- ts(df %>% filter(Borough == barrio) %>% pull(paste0("F_", edad)), start = 1999, end = 2020, frequency = 1)
#      modelo_hombres <- ets(serie_hombres)
#      modelo_mujeres <- ets(serie_mujeres)
#      pred_hombres <- forecast(modelo_hombres, h = length(anyos_pred))
#      pred_mujeres <- forecast(modelo_mujeres, h = length(anyos_pred))
#      predicciones[[paste0("M_", edad)]] <- round(pred_hombres$mean, 0)
#      predicciones[[paste0("F_", edad)]] <- round(pred_mujeres$mean, 0)
#    }
#    predicciones_df <- as.data.frame(predicciones)
#    predicciones_df$Year <- anyos_pred
#    predicciones_df$Borough <- barrio
#    predicciones_df$Code <- codigo_barrio
#    
#    df <- bind_rows(df, predicciones_df)
#  }

  # Transforma los años en columnas y los datos de edad a valores de las filas
#  df <- df %>% 
#    mutate(Year = paste0("Year_", Year)) %>%
#    pivot_wider(
#      names_from = Year,
#      values_from = Avg_Age
#    )
  
  # Elimina las columnas de los años 1999 y 2000
#  df$Year_1999 <- NULL
#  df$Year_2000 <- NULL
  

  
  return(df)
}

validar_indicador_01_edad <- function(df) {
  #if (any(is.na(df)))
  #  return(FALSE)
  return(TRUE)
}

guardar_indicador_01_edad <- function(df) {
  ruta_fichero <- 'STG_01_indicador_edad.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 02) Carga y limpieza de datos de los indicadores demográficos (raciales)
################################################################################

#carga_indicador_02_raza <- function() {
#  ruta_fichero <- '01_Demograficos/ethnic-groups-by-borough.xls'
#  lista_pestanas <- 2012:2020
#  nulos <- c("", "-")
#  columnas <- c("Code", "Borough", "White", "Asian", "Black", "Other", "Total", "CI_Empty", "CI_White", "CI_Asian", "CI_Black", "CI_Other", "CI_Total")
#  df <- data.frame(Code = NA, Borough = NA, White = NA, Asian = NA, Black = NA, Other = NA, Total = NA, CI_Empty = NA, CI_White = NA, CI_Asian = NA, CI_Black = NA, CI_Other = NA, CI_Total = NA)[0,]
#  # Lee la lista de pestañas (desde 2012 hasta 2020)
#  for (pestana in lista_pestanas) {
#    df_tmp = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = (2022 - pestana), skip=2, col_names = columnas, na = nulos)
#    # Añade el dato del año
#    df_tmp["Year"] <- pestana
#    # Fusiona los datos
#    df <- rbind(df, df_tmp)
#  }
#  df <- df %>% select("Code", "Borough", "Year", "White", "Asian", "Black", "Other", "Total")
#  return(df)
#}

carga_indicador_02_raza <- function() {
  ruta_fichero <- '01_Demograficos/Ethnic group projections (2016-based central trend).xlsx'
  lista_anyos <- paste0("Year_", 2011:2050)
  nulos <- c("", "-")
  columnas <- c("Code", "Borough", "Sex", "Age", "Ethnic_group", lista_anyos)
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Population - Persons", skip = 1, na = nulos, col_names = columnas)
  return(df)
}

limpieza_indicador_02_raza <- function(df) {
  # Filtrar solo los registros "All ages"
  df <- df %>% filter(Age == "All ages")

    # Eliminar datos más allá de 2031 y las columnas Sex y Age
  col_anyos_eliminar <- paste0("Year_", 2032:2050)
  df <- df %>% select(-c(col_anyos_eliminar, "Sex", "Age"))
  
  # Crear registro White = White British + White Irish + Other White
  df_white <- df %>%
    filter(Ethnic_group %in% c("White British", "White Irish", "Other White")) %>%
    group_by(Code, Borough) %>%
    summarise(across(starts_with("Year_"), sum, na.rm = TRUE)) %>%
    mutate(Ethnic_group = "White")
  df <- bind_rows(df, df_white)
  
  # Eliminar registros que no son White ni BAME
  df <- df %>% filter(Ethnic_group %in% c("White", "BAME"))

  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
    
  # Ordena los datos
  df <- df %>% arrange(Code, Ethnic_group)
  
  return(df)
}
#limpieza_indicador_02_raza <- function(df) {
#  # Filtra los registros que no corresponden con barrios de Londres
#  barrios_londres <- unlist(lista_barrios)
#  barrios <- unique(df$Borough)
#  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
#  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
#  
#  # El barrio de "Richmond upon Thames" no presenta en todos los años los habitantes de raza negra, se calcula su valor en base al resto de datos
#  df <- df %>%
#    mutate(Black = if_else(is.na(Black) & Borough == "Richmond upon Thames", Total - (White + Asian + Other), Black)
#  )
#  
#  # TODO: Eliminar valores para City of London
#  # Imputa valores para el barrio "City of London" en base a los datos de la ciudad de Londres
#  anyos <- 2012:2020
#  for (anyo in anyos) {
#    sum_valores <- df %>% 
#      filter(Year == anyo) %>% 
#      summarise(
#        white_london = sum(White[Borough == "London"], na.rm = TRUE),
#        white_others = sum(White[!(Borough %in% c("United Kingdom", "London"))], na.rm = TRUE),
#        black_london = sum(Black[Borough == "London"], na.rm = TRUE),
#        black_others = sum(Black[!(Borough %in% c("United Kingdom", "London"))], na.rm = TRUE),
#        asian_london = sum(Asian[Borough == "London"], na.rm = TRUE),
#        asian_others = sum(Asian[!(Borough %in% c("United Kingdom", "London"))], na.rm = TRUE),
#        other_london = sum(Other[Borough == "London"], na.rm = TRUE),
#        other_others = sum(Other[!(Borough %in% c("United Kingdom", "London"))], na.rm = TRUE),
#      ) %>%
#      mutate(
#        white_city_of_london = if_else(white_london - white_others >= 0, white_london - white_others, 0),
#        black_city_of_london = if_else(black_london - black_others >= 0, black_london - black_others, 0),
#        asian_city_of_london = if_else(asian_london - asian_others >= 0, asian_london - asian_others, 0),
#        other_city_of_london = if_else(other_london - other_others >= 0, other_london - other_others, 0),
#      )
#      
#    sum_white <- pull(sum_valores, white_city_of_london)
#    sum_black <- pull(sum_valores, black_city_of_london)
#    sum_asian <- pull(sum_valores, asian_city_of_london)
#    sum_other <- pull(sum_valores, other_city_of_london)
#    
#    df <- df %>%
#      mutate(
#        White = if_else(Year == anyo & Borough == "City of London" & is.na(White), sum_white, White),
#        Black = if_else(Year == anyo & Borough == "City of London" & is.na(Black), sum_black, Black),
#        Asian = if_else(Year == anyo & Borough == "City of London" & is.na(Asian), sum_asian, Asian),
#        Other = if_else(Year == anyo & Borough == "City of London" & is.na(Other), sum_other, Other),
#        Total = if_else(Year == anyo & Borough == "City of London" & is.na(Total), White + Black + Asian + Other, Total)
#      )
#  }
#  
#  # Se realiza predicción para los años posteriores a 2020
#  anyos_pred_prev <- 2001:2011
#  anyos_pred_post <- 2021:2031
#  razas <- c("White", "Asian", "Black", "Other")
#  barrios <- unique(df$Borough)
#  for (barrio in barrios) {
#    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
#    predicciones <- list()
#    serie_white <- ts(df %>% filter(Borough == barrio) %>% pull("White"), start = 2012, end = 2020, frequency = 1)
#    serie_black <- ts(df %>% filter(Borough == barrio) %>% pull("Black"), start = 2012, end = 2020, frequency = 1)
#    serie_asian <- ts(df %>% filter(Borough == barrio) %>% pull("Asian"), start = 2012, end = 2020, frequency = 1)
#    serie_other <- ts(df %>% filter(Borough == barrio) %>% pull("Other"), start = 2012, end = 2020, frequency = 1)
#    
#    modelo_white <- ets(serie_white)
#    modelo_black <- ets(serie_black)
#    modelo_asian <- ets(serie_asian)
#    modelo_other <- ets(serie_other)
#    
#    pred_white_prev <- forecast(modelo_white, h = length(anyos))
#    
#    for (raza in razas) {
#      serie_hombres <- ts(df %>% filter(Borough == barrio) %>% pull(paste0("M_", edad)), start = 1999, end = 2020, frequency = 1)
#      serie_mujeres <- ts(df %>% filter(Borough == barrio) %>% pull(paste0("F_", edad)), start = 1999, end = 2020, frequency = 1)
#      modelo_hombres <- ets(serie_hombres)
#      modelo_mujeres <- ets(serie_mujeres)
#      pred_hombres <- forecast(modelo_hombres, h = length(anyos_pred))
#      pred_mujeres <- forecast(modelo_mujeres, h = length(anyos_pred))
#      predicciones[[paste0("M_", edad)]] <- round(pred_hombres$mean, 0)
#      predicciones[[paste0("F_", edad)]] <- round(pred_mujeres$mean, 0)
#    }
#    predicciones_df <- as.data.frame(predicciones)
#    predicciones_df$Year <- anyos_pred
#    predicciones_df$Borough <- barrio
#    predicciones_df$Code <- codigo_barrio
#    
#    df <- bind_rows(df, predicciones_df)
#  }
#  
#  # Ver si los datos de City of London se pueden imputar en base al total de Londres
#  # Pivotar resultados
#  return(df)
#}

validar_indicador_02_raza <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_02_raza <- function(df) {
  ruta_fichero <- 'STG_02_indicador_raza.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 03) Carga y limpieza de datos de los indicadores de empleo
################################################################################

carga_indicador_03_empleo <- function() {
  ruta_fichero <- '02_Empleo/earnings-residence-borough.xlsx'
  nulos <- c("", "-", "!", "#")
  columnas <- c("Code", "Borough", "Year_2002", "Conf_2002", "Year_2003", "Conf_2003", "Year_2004", "Conf_2004", "Year_2005", "Conf_2005"
                , "Year_2006", "Conf_2006", "Year_2007", "Conf_2007", "Year_2008", "Conf_2008", "Year_2009", "Conf_2009", "Year_2010", "Conf_2010"
                , "Year_2011", "Conf_2011", "Year_2012", "Conf_2012", "Year_2013", "Conf_2013", "Year_2014", "Conf_2014", "Year_2015", "Conf_2015"
                , "Year_2016", "Conf_2016", "Year_2017", "Conf_2017", "Year_2018", "Conf_2018", "Year_2019", "Conf_2019", "Year_2020", "Conf_2020"
                , "Year_2021", "Conf_2021", "Year_2022", "Conf_2022")
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Total, weekly", skip = 3, na = nulos, col_names = columnas)
  return(df)
}

limpieza_indicador_03_empleo <- function(df) {
  # Eliminar columnas Conf_Y
  col_intconf <- paste0("Conf_", 2002:2022)
  df <- df %>% select(-col_intconf)
  
  # Eliminar registros que no sean barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
  
  # Actualizar los códigos de barrio
  df <- df %>%
    left_join(select(df_barrios, Borough, Code), by = "Borough", suffix = c("", "_barrio")) %>%
    mutate(Code = ifelse(!is.na(Code_barrio), Code_barrio, Code)) %>%
    select(-Code_barrio)

  # Imputamos valores para "City of London", mismos que ya tiene los años posteriores
  df <- df %>%
    mutate(across(starts_with("Year_"),
                  ~ ifelse(Borough == "City of London" & is.na(.),
                         ifelse(as.numeric(gsub("Year_", "", cur_column())) < 2009,
                                Year_2009, Year_2018),.)))
  
  # Previsión valores hasta 2031
  df_tmp <- df %>%
    pivot_longer(cols = starts_with("Year_"), names_to = "Year", names_prefix = "Year_", values_to = "Week_Earnings") %>%
    mutate(Year = as.numeric(Year))
  forecast_results <- data.frame()
  for (barrio in unique(df_tmp$Borough)) {
    df_barrio <- df_tmp %>% filter(Borough == barrio)
    ts_data <- ts(df_barrio$Week_Earnings, start = min(df_barrio$Year), frequency = 1)
    model <- auto.arima(ts_data)
    forecast_values <- round(forecast(model, h = 9)$mean, 2)
    code <- df %>% filter(Borough == barrio) %>% distinct(Code)
    forecast_df <- data.frame(Code = code,
                              Borough = replicate(9, barrio),
                              Year = 2023:2031,
                              Week_Earnings = as.numeric(forecast_values))
    forecast_results <- bind_rows(forecast_results, forecast_df)
  }
  
  print(forecast_results)
  
  df <- df %>%
    pivot_longer(cols = starts_with("Year_"),
                 names_to = "Year",
                 names_prefix = "Year_",
                 values_to = "Week_Earnings") %>%
    mutate(Year = as.numeric(Year)) %>%
    bind_rows(forecast_results) %>%
    pivot_wider(names_from = Year, values_from = Week_Earnings, names_prefix = "Year_")
  
  return(df)
}

validar_indicador_03_empleo <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_03_empleo <- function(df) {
  ruta_fichero <- 'STG_03_indicador_empleo.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
  
}


################################################################################
# 04) Carga y limpieza de datos de los indicadores educativos
################################################################################

carga_indicador_04_estudios <- function() {
  ruta_fichero <- '03_Estudios/Qualifications-of-working-age-NVQ.csv'
  nulos <- c("", "-", "!", "#")
  columnas <- c("Code", "Borough", "Year", "Qualifications", "Number", "Habitants", "Percent", "Confidence")
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_names = columnas, na = nulos, skip = 1)
  return(df) 
}

limpieza_indicador_04_estudios <- function(df) {
  # Filtrar por NVQ4+ y eliminar campo intervalo de confianza
  df <- df %>% filter(Qualifications == "NVQ4+") %>% select(-c(Confidence, Qualifications))

  # Eliminar registros que no sean barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
  
  # Reemplazar , por . decimal, eliminar entrecomillado en campo Number
  df <- df %>%
    mutate(Number = as.numeric(gsub(",", "", gsub('"', "", Number))))
  
  # Imputar valores nulos para el porcentaje
  df <- df %>%
    mutate(Percent = ifelse(Borough == "City of London" & is.na(Percent), 80, Percent))
  
  # Actualiza el valor del campo Number en base a los campos Habitants y Percent
  df <- df %>%
    mutate(Number = ifelse(Borough == "City of London" & is.na(Number), round((Habitants * Percent) / 100), Number))
  
  # Imputar valores hasta 2031
  anyos_pred <- 2022:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Number <- ts(df %>% filter(Borough == barrio) %>% pull(Number), start = 2004, end = 2021, frequency = 1)
    serie_Habitants <- ts(df %>% filter(Borough == barrio) %>% pull(Habitants), start = 2004, end = 2021, frequency = 1)
    serie_Percent <- ts(df %>% filter(Borough == barrio) %>% pull(Percent), start = 2004, end = 2021, frequency = 1)
    modelo_Number <- ets(serie_Number)
    modelo_Habitants <- ets(serie_Habitants)
    modelo_Percent <- ets(serie_Percent)
    pred_Number <- forecast(modelo_Number, h = length(anyos_pred))
    pred_Habitants <- forecast(modelo_Habitants, h = length(anyos_pred))
    pred_Percent <- forecast(modelo_Percent, h = length(anyos_pred))
    predicciones[["Number"]] <- round(pred_Number$mean, 0)
    predicciones[["Habitants"]] <- round(pred_Habitants$mean, 0)
    predicciones[["Percent"]] <- round(pred_Percent$mean, 0)

    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio

    df <- bind_rows(df, predicciones_df)
  }

  # Ordenar por barrio
  df <- df %>% arrange(Code, Year)
  
  # Pivotar años a columnas
  return(df)
}

validar_indicador_04_estudios <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_04_estudios <- function(df) {
  ruta_fichero <- 'STG_04_indicador_estudios.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 05) Carga y limpieza de datos de los indicadores medio ambientales
################################################################################

carga_indicador_05_trafico <- function() {
  nulos <- c("", "-", "!", "#")
  columnas <- c("Code", "Borough", paste0("Year_", 1993:2023))
  ruta_fichero <- '04_Medio_Ambiente/traffic-flow-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_names = columnas, na = nulos, skip = 1, sheet = "Traffic Flows - Cars")
  return(df)
}

limpieza_indicador_05_trafico <- function(df) {
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))

  # Se pasan las columnas de años a una sola columna
  df <- df %>%
    pivot_longer(cols = starts_with("Year_"),
                 names_to = "Year",
                 names_prefix = "Year_",
                 values_to = "Car_Traffic") %>%
    mutate(Year = as.numeric(Year))
    
  # Imputar valores hasta 2031
  anyos_pred <- 2024:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Traffic <- ts(df %>% filter(Borough == barrio) %>% pull(Car_Traffic), start = 1993, end = 2023, frequency = 1)
    modelo_Traffic <- ets(serie_Traffic)
    pred_Traffic <- forecast(modelo_Traffic, h = length(anyos_pred))
    predicciones[["Car_Traffic"]] <- round(pred_Traffic$mean, 0)
    
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    
    df <- bind_rows(df, predicciones_df)
  }
  
  # Ordenar por barrio
  df <- df %>% arrange(Code, Year)

  return(df)
}

validar_indicador_05_trafico <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_05_trafico <- function(df) {
  ruta_fichero <- 'STG_05_indicador_trafico.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 06) Carga y limpieza de datos de los indicadores sanitarios
################################################################################

carga_indicador_06_esperanza_vida <- function() {
  nulos <- c("", "-")
  anyos <- 2003:2022
  rep_anyos <- rep(anyos, each = 3)
  etiquetas <- c("life_exp_", "lower_ci_", "upper_ci_")
  columnas <- paste0(etiquetas, rep_anyos)
  columnas <- c("Code", "Borough", "Sex", "Age", columnas)
  ruta_fichero <- '05_Sanidad/lifeexpectancylocalareas.xlsx'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = 5, skip = 12, na = nulos, col_names = columnas)
  return(df)
}

limpieza_indicador_06_esperanza_vida <- function(df) {
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
  
  # Filtrar Age = < 1
  df <- df %>% filter(Age == "<1")
  
  # Eliminar columnas lower_ci, upper_ci y edad
  df <- df %>% select(-paste0("lower_ci_", 2003:2022))
  df <- df %>% select(-paste0("upper_ci_", 2003:2022))
  df <- df %>% select(-Age)
  
  # Pivot años a filas
  df <- df %>%
    pivot_longer(cols = starts_with("life_exp_"),
                 names_to = "Year",
                 names_prefix = "life_exp_",
                 values_to = "Life_Expectancy") %>%
    mutate(Year = as.numeric(Year))

  # Previsión hasta 2031
  anyos_pred <- 2023:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones_Female <- list()
    predicciones_Male <- list()
    serie_Female <- ts(df %>% filter(Borough == barrio & Sex == "Female") %>% pull(Life_Expectancy), start = 2003, end = 2022, frequency = 1)
    serie_Male <- ts(df %>% filter(Borough == barrio & Sex == "Male") %>% pull(Life_Expectancy), start = 2003, end = 2022, frequency = 1)
    modelo_Female <- ets(serie_Female)
    modelo_Male <- ets(serie_Male)
    pred_Female <- forecast(modelo_Female, h = length(anyos_pred))
    pred_Male <- forecast(modelo_Male, h = length(anyos_pred))
    predicciones_Female[["Life_Expectancy"]] <- round(pred_Female$mean, 2)
    predicciones_Female[["Sex"]] <- "Female"
    predicciones_Male[["Life_Expectancy"]] <- round(pred_Male$mean, 2)
    predicciones_Male[["Sex"]] <- "Male"
    
    predicciones_df <- as.data.frame(predicciones_Female)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    df <- bind_rows(df, predicciones_df)
    
    predicciones_df <- as.data.frame(predicciones_Male)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    df <- bind_rows(df, predicciones_df)
  }
  
  # Ordenar datos por barrio, sexo y año
  df <- df %>% arrange(Code, Sex, Year)
  return(df)
}

validar_indicador_06_esperanza_vida <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_06_esperanza_vida <- function(df) {
  ruta_fichero <- 'STG_06_indicador_esperanza_vidas.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 07) Carga y limpieza de datos de los indicadores sobre seguridad
################################################################################

carga_indicador_07_delitos <- function() {
  nulos <- c("", "-")
  cols_2010 <- seq.Date(as.Date("2010-04-01"), as.Date("2010-12-01"), by = "month")
  cols_2011_2021 <- seq.Date(as.Date("2011-01-01"), as.Date("2021-12-01"), by = "month")
  cols_2022 <- seq.Date(as.Date("2022-01-01"), as.Date("2022-09-01"), by = "month")
  columnas <- c("MajorText", "MinorText", "Borough", paste0("M_", format(c(cols_2010, cols_2011_2021, cols_2022), "%Y%m")))
  ruta_fichero <- '06_Seguridad/MPS Borough Level Crime (Historical).csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), skip = 1, na = nulos, col_names = columnas)
  return(df)
}

limpieza_indicador_07_delitos <- function(df) {
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
  
  # Eliminar columnas tipo de delito
  df <- df %>% select(-c("MajorText", "MinorText"))
  
  columnas <- colnames(df)
  # Agrupar por barrios
  df <- df %>%
    group_by(Borough) %>%
    summarise(across(starts_with("M_"), sum, na.rm = TRUE))

  # Crear columna media por año
  df <- df %>%
    rowwise() %>%  # Permite realizar operaciones fila por fila
    mutate(across(starts_with("M_"), as.numeric)) %>%  # Asegura que los datos sean numéricos
    summarise(
      Borough = first(Borough),
      # Calcula la media anual para cada año
      Mean_2010 = round(mean(c_across(starts_with("M_2010")), na.rm = TRUE), 2),
      Mean_2011 = round(mean(c_across(starts_with("M_2011")), na.rm = TRUE), 2),
      Mean_2012 = round(mean(c_across(starts_with("M_2012")), na.rm = TRUE), 2),
      Mean_2013 = round(mean(c_across(starts_with("M_2013")), na.rm = TRUE), 2),
      Mean_2014 = round(mean(c_across(starts_with("M_2014")), na.rm = TRUE), 2),
      Mean_2015 = round(mean(c_across(starts_with("M_2015")), na.rm = TRUE), 2),
      Mean_2016 = round(mean(c_across(starts_with("M_2016")), na.rm = TRUE), 2),
      Mean_2017 = round(mean(c_across(starts_with("M_2017")), na.rm = TRUE), 2),
      Mean_2018 = round(mean(c_across(starts_with("M_2018")), na.rm = TRUE), 2),
      Mean_2019 = round(mean(c_across(starts_with("M_2019")), na.rm = TRUE), 2),
      Mean_2020 = round(mean(c_across(starts_with("M_2020")), na.rm = TRUE), 2),
      Mean_2021 = round(mean(c_across(starts_with("M_2021")), na.rm = TRUE), 2),
      Mean_2022 = round(mean(c_across(starts_with("M_2022")), na.rm = TRUE), 2),
  )

  df <- df %>%
    left_join(select(df_barrios, Borough, Code), by = "Borough", suffix = c("", "_barrio"))

  # Pivotar los años
  df <- df %>%
    pivot_longer(cols = starts_with("Mean_"),
                 names_to = "Year",
                 names_prefix = "Mean_",
                 values_to = "Crimes") %>%
    mutate(Year = as.numeric(Year))
  
  # Estimación datos hasta 2031
  anyos_pred <- 2023:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Crimes <- ts(df %>% filter(Borough == barrio) %>% pull(Crimes), start = 2010, end = 2022, frequency = 1)
    modelo_Crimes <- ets(serie_Crimes)
    pred_Crimes <- forecast(modelo_Crimes, h = length(anyos_pred))
    predicciones[["Crimes"]] <- round(pred_Crimes$mean, 0)
    
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    
    df <- bind_rows(df, predicciones_df)
  }
  
  # Ordenar los datos y mostrar las columnas en orden correcto
  df <- df %>% select(Code, Borough, Year, Crimes) %>% arrange(Code, Year)
  
  return(df)
}

validar_indicador_07_delitos <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_07_delitos <- function(df) {
  ruta_fichero <- 'STG_07_indicador_delitos.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 08) Carga y limpieza de datos de los indicadores sobre servicios
################################################################################

carga_indicador_08_servicios <- function() {
  ruta_fichero <- '07_Servicios/local-units-by-broad-industry-group.xls'
  # Lectura de las pestañas desde 2009 hasta 2024
  lista_pestanas <- 2009:2024
  nulos <- c("", "-")
  columnas <- c("Code", "Borough", "Agriculture", "Production", "Construction", "Motor", "Wholesale", "Retail", "Transport", "Food", "Information", "Finance", "Property", "Professional", "Business", "Public", 
                "Education", "Health", "Entertainment", "Total")
  df <- data.frame(Code = NA, Borough = NA, Agriculture = NA, Production = NA, Construction = NA, Motor = NA, Wholesale = NA, Retail = NA, Transport = NA, Food = NA, Information = NA, Finance = NA, 
                   Property = NA, Professional = NA, Business = NA, Public = NA, Education = NA, Health = NA, Entertainment = NA, Total = NA)[0,]
  # Lee la lista de pestañas (desde 2009 hasta 2024)
  for (pestana in lista_pestanas) {
    df_tmp = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = (8 + (pestana - 2009)), skip = 3, col_names = columnas, na = nulos)
    # Añade el dato del año
    df_tmp["Year"] <- pestana
    # Fusiona los datos
    df <- rbind(df, df_tmp)
  }  
  df <- df %>% select(Code, Borough, Year, Retail, Food, Entertainment, Total)
  return(df) 
}

limpieza_indicador_08_servicios <- function(df) {
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))

    # Actualizar códigos de barrio
  df <- df %>%
    left_join(select(df_barrios, Borough, Code), by = "Borough", suffix = c("", "_barrio")) %>%
    mutate(Code = ifelse(!is.na(Code_barrio), Code_barrio, Code)) %>%
    select(-Code_barrio)
  
  # Previsión hasta 2031
  anyos_pred <- 2025:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Retail <- ts(df %>% filter(Borough == barrio) %>% pull(Retail), start = 2009, end = 2024, frequency = 1)
    serie_Food <- ts(df %>% filter(Borough == barrio) %>% pull(Food), start = 2009, end = 2024, frequency = 1)
    serie_Entertainment <- ts(df %>% filter(Borough == barrio) %>% pull(Entertainment), start = 2009, end = 2024, frequency = 1)
    serie_Total <- ts(df %>% filter(Borough == barrio) %>% pull(Total), start = 2009, end = 2024, frequency = 1)
    modelo_Retail <- ets(serie_Retail)
    modelo_Food <- ets(serie_Food)
    modelo_Entertainment <- ets(serie_Entertainment)
    modelo_Total <- ets(serie_Total)
    pred_Retail <- forecast(modelo_Retail, h = length(anyos_pred))
    pred_Food <- forecast(modelo_Food, h = length(anyos_pred))
    pred_Entertainment <- forecast(modelo_Entertainment, h = length(anyos_pred))
    pred_Total <- forecast(modelo_Total, h = length(anyos_pred))
    predicciones[["Retail"]] <- round(pred_Retail$mean, 2)
    predicciones[["Food"]] <- round(pred_Food$mean, 2)
    predicciones[["Entertainment"]] <- round(pred_Entertainment$mean, 2)
    predicciones[["Total"]] <- round(pred_Total$mean, 2)
    print(predicciones)
    
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    df <- bind_rows(df, predicciones_df)
  }

  # Ordenar salida
  df <- df %>% arrange(Code, Year)
  return(df)
}

validar_indicador_08_servicios <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_08_servicios <- function(df) {
  ruta_fichero <- 'STG_08_indicador_servicios.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 09) Carga y limpieza de datos de los indicadores sobre vivienda (precio)
################################################################################

carga_indicador_09_vivienda_precio <- function() {
  nulos <- c("", "-")
  anyos <- rep(1996:2017, each=4)
  print(anyos)
  meses <- c("Mar_", "Jun_", "Sep_", "Dec_")
  print(meses)
  columnas <- c("Code", "Borough", "Dec_1995", paste0(meses, anyos))
  ruta_fichero <- '08_Vivienda/land-registry-house-prices-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Median", skip = 2, na = nulos, col_names = columnas)
  return(df)  
}

limpieza_indicador_09_vivienda_precio <- function(df) {
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))
  
  # Eliminar columnas que empiecen por Mar_, Jun_ y Sep_
  df <- df %>% select(-starts_with("Mar_"), -starts_with("Jun_"), -starts_with("Sep_"))
  
  # Pivotar los años
  df <- df %>%
    pivot_longer(cols = starts_with("Dec_"),
                 names_to = "Year",
                 names_prefix = "Dec_",
                 values_to = "Price") %>%
    mutate(Year = as.numeric(Year))
  
  # Estimación datos hasta 2031
  anyos_pred <- 2018:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Price <- ts(df %>% filter(Borough == barrio) %>% pull(Price), start = 1995, end = 2017, frequency = 1)
    modelo_Price <- ets(serie_Price)
    pred_Price <- forecast(modelo_Price, h = length(anyos_pred))
    predicciones[["Price"]] <- round(pred_Price$mean, 2)
    
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    
    df <- bind_rows(df, predicciones_df)
  }
  
  # Ordenar los datos y mostrar las columnas en orden correcto
  df <- df %>% arrange(Code, Year)
  
  return(df)
}

validar_indicador_09_vivienda_precio <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_09_vivienda_precio <- function(df) {
  ruta_fichero <- 'STG_09_indicador_vivienda_precio.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 10) Carga y limpieza de datos de los indicadores sobre vivienda (alquiler)
################################################################################

carga_indicador_10_vivienda_alquiler <- function() {
  nulos <- c("", "-", "..")
  columnas <- c("ShortCode", "Code", "Borough", paste0("Year_", 1997:2023))
  ruta_fichero <- '08_Vivienda/social-landlord-rents-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "RSL Rents", skip = 2, na = nulos, col_names = columnas)
  return(df)  
}

limpieza_indicador_10_vivienda_alquiler <- function(df) {
  # Eliminar columna ShortCode
  df <- df %>% select(-ShortCode)
  
  # Elimina el registro de London que tiene muchos NA
  df <- df %>% filter(Borough != "London")
  
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Borough %in% barrios_a_eliminar))

  # Reemplazar dobles comillas por nada y redondear valores a dos dígitos en columnas Year_
  df <- df %>%
    mutate(across(starts_with("Year_"), ~ round(as.numeric(gsub('"','',.)), 2)))
  
  # Pivotar los años
  df <- df %>%
    pivot_longer(cols = starts_with("Year_"),
                 names_to = "Year",
                 names_prefix = "Year_",
                 values_to = "Rent") %>%
    mutate(Year = as.numeric(Year))
  
  # Estimación datos hasta 2031
  anyos_pred <- 2024:2031
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    serie_Rent <- ts(df %>% filter(Borough == barrio) %>% pull(Rent), start = 1997, end = 2023, frequency = 1)
    modelo_Rent <- ets(serie_Rent)
    pred_Rent <- forecast(modelo_Rent, h = length(anyos_pred))
    predicciones[["Rent"]] <- round(pred_Rent$mean, 2)
    
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    
    df <- bind_rows(df, predicciones_df)
  }
  
  # Ordenar los datos y mostrar las columnas en orden correcto
  df <- df %>% arrange(Code, Year)
  return(df)
}

validar_indicador_10_vivienda_alquiler <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_10_vivienda_alquiler <- function(df) {
  ruta_fichero <- 'STG_10_indicador_vivienda_alquiler.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# Cuerpo principal del programa
################################################################################

# TODO:
#   - Crear versión de los CSV sin pivotar tablas de años, es posible que sea necesario unificado en un campo para series temporales, disponer de ambas versiones
#   - Ver qué modelo usar para las predicciones de series temporales
#   - Ver el parámetro frequency en las estimaciones a qué se refiere con 1 (1 mes, 1 año). Por ejemplo en el indicador 5 - Tráfico no cambia valores desde el primer año estimado en adelante
#   - Buscar datos más actuales sobre todo indicadores que no llegan a 2021
#   - En el indicador 8 falta incluir un total para los tipos de negocios para gentrificación

# Barrios de Londres
df_barrios <- carga_lista_barrios()
lista_barrios <- as.list(df_barrios %>% select(Borough))

# Indicador 01 - Edad
df_edad = carga_indicador_01_edad()
df_edad = limpieza_indicador_01_edad(df_edad)
if (validar_indicador_01_edad(df_edad)) guardar_indicador_01_edad(df_edad) else print("[ERROR]: Revisar los datos del indicador 01 - Edad")

# Indicador 02 - Raza
#df_raza = carga_indicador_02_raza()
#df_raza = limpieza_indicador_02_raza(df_raza)
#if (validar_indicador_02_raza(df_raza)) guardar_indicador_02_raza(df_raza) else print("ERROR")

# Indicador 03 - Empleo
#df_empleo = carga_indicador_03_empleo()
#df_empleo = limpieza_indicador_03_empleo(df_empleo)
#if (validar_indicador_03_empleo(df_empleo))
#  guardar_indicador_03_empleo(df_empleo)

# Indicador 04 - Estudios
#df_estudios = carga_indicador_04_estudios()
#df_estudios = limpieza_indicador_04_estudios(df_estudios)
#if (validar_indicador_04_estudios(df_estudios))
#  guardar_indicador_04_estudios(df_estudios)

# Indicador 05 - Tráfico
#df_trafico = carga_indicador_05_trafico()
#df_trafico = limpieza_indicador_05_trafico(df_trafico)
#if (validar_indicador_05_trafico(df_trafico))
#  guardar_indicador_05_trafico(df_trafico)

# Indicador 06 - Esperanza de vida
#df_esperanza_vida = carga_indicador_06_esperanza_vida()
#df_esperanza_vida = limpieza_indicador_06_esperanza_vida(df_esperanza_vida)
#if (validar_indicador_06_esperanza_vida(df_esperanza_vida))
#  guardar_indicador_06_esperanza_vida(df_esperanza_vida)

# Indicador 07 - Delitos
#df_delitos = carga_indicador_07_delitos()
#df_delitos = limpieza_indicador_07_delitos(df_delitos)
#if (validar_indicador_07_delitos(df_delitos))
#  guardar_indicador_07_delitos(df_delitos)

# Indicador 08 - Servicios
#df_servicios = carga_indicador_08_servicios()
#df_servicios = limpieza_indicador_08_servicios(df_servicios)
#if (validar_indicador_08_servicios(df_servicios))
#  guardar_indicador_08_servicios(df_servicios)

# Indicador 09 - Precio vivienda
#df_vivienda_precio = carga_indicador_09_vivienda_precio()
#df_vivienda_precio = limpieza_indicador_09_vivienda_precio(df_vivienda_precio)
#if (validar_indicador_09_vivienda_precio(df_vivienda_precio))
#  guardar_indicador_09_vivienda_precio(df_vivienda_precio)

# Indicador 10 - Precio alquiler
#df_vivienda_alquiler = carga_indicador_10_vivienda_alquiler()
#df_vivienda_alquiler = limpieza_indicador_10_vivienda_alquiler(df_vivienda_alquiler)
#if (validar_indicador_10_vivienda_alquiler(df_vivienda_alquiler))
#  guardar_indicador_10_vivienda_alquiler(df_vivienda_alquiler)
  
# Carga ficheros
# Renombra columnas
# Elimina columnas
# Filtra filas (solo barrios y datos de la ciudad de Londres)
# Revisión separadores decimales
# Revisión tipo de los datos
# Sustituir valores no válidos
# Imputar valores nulos
# Revisar todos los barrios tienen datos de 2001, 2011, 2021 y 2031 (datos por censo)
