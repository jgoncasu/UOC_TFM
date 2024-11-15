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

  # Estimar valores hasta 2031
  anyos_pred <- 2021:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    # Men
    ts_data_men <- ts(df_barrio$Mean_Age_Men, start = 1999, end = 2020, frequency = 1)
    model_men <- auto.arima(ts_data_men)
    forecast_values_men <- round(forecast(model_men, h = length(anyos_pred))$mean, 2)
    # Women
    ts_data_women <- ts(df_barrio$Mean_Age_Women, start = 1999, end = 2020, frequency = 1)
    model_women <- auto.arima(ts_data_women)
    forecast_values_women <- round(forecast(model_women, h = length(anyos_pred))$mean, 2)
    # Avg_Age
    ts_data_avg <- ts(df_barrio$Avg_Age, start = 1999, end = 2020, frequency = 1)
    model_avg <- auto.arima(ts_data_avg)
    forecast_values_avg <- round(forecast(model_avg, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Mean_Age_Men = forecast_values_men,
                              Mean_Age_Women = forecast_values_women,
                              Avg_Age = forecast_values_avg)
    
    df <- rbind(df, forecast_df)
  }
  
  # Ordena los datos por barrio
  df <- df %>% arrange(Code, Year) 

  return(df)
}

guardar_indicador_01_edad <- function(df) {
  ruta_fichero <- 'STG_01_indicador_edad.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 02) Carga y limpieza de datos de los indicadores demográficos (raciales)
################################################################################

carga_indicador_02_raza <- function() {
  # Lee el fichero con datos entre 2001 y 2009
  ruta_fichero <- '01_Demograficos/ethnic-groups-by-gender.xls'
  lista_pestanas <- 2001:2009
  nulos <- c("", "-")
  columnas <- c("Code", "Borough", "All", paste0("White_", 1:3), paste0("BAME_", 1:13), paste0("Male_", 1:17), paste0("Female_", 1:17))
  df_aux <- data.frame(Code = NA, Borough = NA, All = NA, White_1 = NA, White_2 = NA, White_3 = NA, BAME_1 = NA, BAME_2 = NA, BAME_3 = NA, BAME_4 = NA, BAME_5 = NA,
                       BAME_6 = NA, BAME_7 = NA, BAME_8 = NA, BAME_9 = NA, BAME_10 = NA, BAME_11 = NA, BAME_12 = NA, BAME_13 = NA, Male_1 = NA, Male_2 = NA, 
                       Male_3 = NA, Male_4 = NA, Male_5 = NA, Male_6 = NA, Male_7 = NA, Male_8 = NA, Male_9 = NA, Male_10 = NA, Male_11 = NA, Male_12 = NA,
                       Male_13 = NA, Male_14 = NA, Male_15 = NA, Male_16 = NA, Male_17 = NA, Female_1 = NA, Female_2 = NA, Female_3 = NA, Female_4 = NA, 
                       Female_5 = NA, Female_6 = NA, Female_7 = NA, Female_8 = NA, Female_9 = NA, Female_10 = NA, Female_11 = NA, Female_12 = NA,
                       Female_13 = NA, Female_14 = NA, Female_15 = NA, Female_16 = NA, Female_17 = NA, Year = NA)
  # Lee la lista de pestañas (desde 2001 hasta 2009)
  for (pestana in lista_pestanas) {
    if (pestana == 2009)
      columnas <- c("NewCode", columnas)
    df_tmp = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = pestana - 1999, skip = 3, na = nulos, col_names = columnas)
    df_tmp["Year"] <- pestana
    if (pestana == 2009)
      df_tmp <- df_tmp %>% select(-NewCode)
    df_aux <- rbind(df_aux, df_tmp)
  }
  df_aux <- df_aux %>% select(Code, Borough, Year, starts_with("White_"), starts_with("BAME_"))

  # Lee el fichero con datos entre 2011 y 2050
  ruta_fichero <- '01_Demograficos/Ethnic group projections (2016-based central trend).xlsx'
  lista_anyos <- paste0("Year_", 2011:2050)
  columnas <- c("Code", "Borough", "Sex", "Age", "Ethnic_group", lista_anyos)
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Population - Persons", skip = 1, na = nulos, col_names = columnas)
  return(list(df_aux = df_aux, df = df))
}

limpieza_indicador_02_raza <- function(df_aux, df) {
  # LIMPIEZA DE LOS DATOS HASTA 2009
  # Elimina las columnas de datos de hombres y mujeres
  df_aux <- df_aux %>% select(-starts_with("Male_"), -starts_with("Female_"))

  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df_aux$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df_aux <- df_aux %>% filter(!(Borough %in% barrios_a_eliminar))

  df_aux <- df_aux %>%
    mutate(across(starts_with("White_"), as.numeric),
           across(starts_with("BAME_"), as.numeric)
           ) %>%
    group_by(Code, Borough, Year) %>%
    summarise(
      White = 1000 * rowSums(across(starts_with("White_")), na.rm = TRUE),
      BAME = 1000 * rowSums(across(starts_with("BAME_")), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Estimar valores para 2010
  anyos_pred <- 2010
  for (barrio in unique(df_aux$Borough)) {
    df_barrio <- df_aux %>% filter(Borough == barrio)
    # White
    ts_data_white <- ts(df_barrio$White, start = 2001, end = 2009, frequency = 1)
    model_white <- auto.arima(ts_data_white)
    forecast_values_white <- round(forecast(model_white, h = length(anyos_pred))$mean, 0)
    # BAME
    ts_data_bame <- ts(df_barrio$BAME, start = 2001, end = 2009, frequency = 1)
    model_bame <- auto.arima(ts_data_bame)
    forecast_values_bame <- round(forecast(model_bame, h = length(anyos_pred))$mean, 0)
    # Percent
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              BAME = forecast_values_bame,
                              White = forecast_values_white)
    
    df_aux <- rbind(df_aux, forecast_df)
  }
  
  # LIMPIEZA DE LOS DATOS DESDE 2011
  
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
  
  # Pivotar los años
  df <- df %>%
    pivot_longer(cols = starts_with("Year_"),
                 names_to = "Year",
                 names_prefix = "Year_",
                 values_to = "Persons") %>%
    mutate(Year = as.numeric(Year)) %>%
    pivot_wider(names_from = Ethnic_group, values_from = Persons, names_prefix = "")
  
  # Fusiona los dos dataframes
  df <- bind_rows(df, df_aux)

  # Actualizar códigos de barrio
  df <- df %>%
    left_join(select(df_barrios, Borough, Code), by = "Borough", suffix = c("", "_barrio")) %>%
    mutate(Code = ifelse(!is.na(Code_barrio), Code_barrio, Code)) %>%
    select(-Code_barrio)
  
  # Ordena los datos
  df <- df %>% arrange(Code, Borough, Year)
  
  return(df)
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
  df <- df %>% select(-starts_with("Conf_"))

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

  # Se pasan las columnas de años a una sola columna
  df <- df %>%
    pivot_longer(cols = starts_with("Year_"),
                 names_to = "Year",
                 names_prefix = "Year_",
                 values_to = "Week_Earnings") %>%
    mutate(Year = as.numeric(Year))

  # Estimar valores hasta 2031
  anyos_pred <- 2023:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    ts_data <- ts(df_barrio$Week_Earnings, start = 2002, end = 2022, frequency = 1)
    model <- auto.arima(ts_data)
    forecast_values <- round(forecast(model, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Week_Earnings = forecast_values)
    
    df <- rbind(df, forecast_df)
  }

  # Ordenar por barrio
  df <- df %>% arrange(Code, Year)
  
  return(df)
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

  # Estimar valores hasta 2031
  anyos_pred <- 2022:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    # Number
    ts_data_number <- ts(df_barrio$Number, start = 2004, end = 2021, frequency = 1)
    model_number <- auto.arima(ts_data_number)
    forecast_values_number <- round(forecast(model_number, h = length(anyos_pred))$mean, 0)
    # Habitants
    ts_data_habitants <- ts(df_barrio$Habitants, start = 2004, end = 2021, frequency = 1)
    model_habitants <- auto.arima(ts_data_habitants)
    forecast_values_habitants <- round(forecast(model_habitants, h = length(anyos_pred))$mean, 0)
    # Percent
    ts_data_percent <- ts(df_barrio$Percent, start = 2004, end = 2021, frequency = 1)
    model_percent <- auto.arima(ts_data_percent)
    forecast_values_percent <- round(forecast(model_percent, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Number = forecast_values_number,
                              Habitants = forecast_values_habitants,
                              Percent = forecast_values_percent)
    
    df <- rbind(df, forecast_df)
  }
    
  # Ordenar por barrio
  df <- df %>% arrange(Code, Year)

  return(df)
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

  # Estimar valores hasta 2031
  anyos_pred <- 2024:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    ts_data <- ts(df_barrio$Car_Traffic, start = 1993, end = 2023, frequency = 1)
    model <- auto.arima(ts_data)
    forecast_values <- round(forecast(model, h = length(anyos_pred))$mean, 0)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Car_Traffic = forecast_values)
    
    df <- rbind(df, forecast_df)
  }
  
  # Ordenar por barrio
  df <- df %>% arrange(Code, Year)

  return(df)
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
  
  # Pivot sexo a columnas
  df <- df %>%
    pivot_wider(names_from = Sex, values_from = Life_Expectancy)
  # Asignamos la edad media (suponiendo mismo número de mujeres que de hombres)
  print("A1")
  df <- df %>% mutate(Avg_Sex = rowMeans(select(., Female, Male), na.rm = TRUE))
  print("A2")

  # Estimar valores hasta 2031
  anyos_pred <- 2023:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    # Female
    ts_data_female <- ts(df_barrio$Female, start = 2003, end = 2022, frequency = 1)
    model_female <- auto.arima(ts_data_female)
    forecast_values_female <- round(forecast(model_female, h = length(anyos_pred))$mean, 2)
    # Male
    ts_data_male <- ts(df_barrio$Male, start = 2003, end = 2022, frequency = 1)
    model_male <- auto.arima(ts_data_male)
    forecast_values_male <- round(forecast(model_male, h = length(anyos_pred))$mean, 2)
    # Avg_Sex
    ts_data_avg <- ts(df_barrio$Avg_Sex, start = 2003, end = 2022, frequency = 1)
    model_avg <- auto.arima(ts_data_avg)
    forecast_values_avg <- round(forecast(model_avg, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Female = forecast_values_female,
                              Male = forecast_values_male,
                              Avg_Sex = forecast_values_avg)
    
    df <- rbind(df, forecast_df)
  }
  
  # Ordenar datos por barrio, sexo y año
  df <- df %>% arrange(Code, Year)

  return(df)
}

guardar_indicador_06_esperanza_vida <- function(df) {
  ruta_fichero <- 'STG_06_indicador_esperanza_vidas.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 07) Carga y limpieza de datos de los indicadores sobre seguridad
################################################################################

carga_indicador_07_delitos <- function() {
  # Carga datos antiguos hasta 2009
  nulos <- c("", "-")
  columnas <- c("Code", "Borough", paste0("Crimes_", 1999:2013), paste0("DEL_VIO_", 1999:2013), paste0("DEL_SEX_", 1999:2013), paste0("DEL_ROB_", 1999:2013), 
                paste0("DEL_BUR_", 1999:2013), paste0("DEL_THE_", 1999:2013), paste0("DEL_FRA_", 1999:2013), paste0("DEL_CRI_", 1999:2013),
                paste0("DEL_DRU_", 1999:2013),  paste0("DEL_OTH_", 1999:2013))
  ruta_fichero <- '06_Seguridad/met-police-recorded-offences-rates-borough.xls'
  df_aux = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Recorded Offences", skip = 3, na = nulos, col_names = columnas)
  
  # Carga datos desde 2010
  cols_2010 <- seq.Date(as.Date("2010-04-01"), as.Date("2010-12-01"), by = "month")
  cols_2011_2021 <- seq.Date(as.Date("2011-01-01"), as.Date("2021-12-01"), by = "month")
  cols_2022 <- seq.Date(as.Date("2022-01-01"), as.Date("2022-09-01"), by = "month")
  columnas <- c("MajorText", "MinorText", "Borough", paste0("M_", format(c(cols_2010, cols_2011_2021, cols_2022), "%Y%m")))
  ruta_fichero <- '06_Seguridad/MPS Borough Level Crime (Historical).csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), skip = 1, na = nulos, col_names = columnas)
  return(list(df_aux = df_aux, df = df))
}

limpieza_indicador_07_delitos <- function(df_aux, df) {
  # LIMPIEZA REGISTROS HASTA 2009
  
  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_barrios)
  barrios <- unique(df_aux$Borough)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df_aux <- df_aux %>% filter(!(Borough %in% barrios_a_eliminar))

  # Eliminar columnas que no son totales
  df_aux <- df_aux %>% select("Code", "Borough", starts_with("Crimes_"))
  
  # Pivotar columnas de años en filas
  df_aux <- df_aux %>%
    pivot_longer(cols = starts_with("Crimes_"),
                 names_to = "Year",
                 names_prefix = "Crimes_",
                 values_to = "Crimes") %>%
    mutate(Year = as.numeric(Year))  

  # Actualizar códigos de barrio
  df_aux <- df_aux %>%
    left_join(select(df_barrios, Borough, Code), by = "Borough", suffix = c("", "_barrio")) %>%
    mutate(Code = ifelse(!is.na(Code_barrio), Code_barrio, Code)) %>%
    select(-Code_barrio)
    
  # Calcular media mensual
  df_aux <- df_aux %>%
    mutate(Crimes = round(Crimes / 12, 2))
  
  # Filtra los datos hasta 2009
  df_aux <- df_aux %>%
    filter(Year <= 2009)
  

  # LIMPIEZA REGISTROS A PARTIR DE 2010
  
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
  
  # Estimar valores hasta 2031
  anyos_pred <- 2023:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    ts_data <- ts(df_barrio$Crimes, start = 2010, end = 2022, frequency = 1)
    model <- auto.arima(ts_data)
    forecast_values <- round(forecast(model, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Crimes = forecast_values)
    
    df <- rbind(df, forecast_df)
  }
  
  # Fusiona los dos dataframes
  df <- bind_rows(df, df_aux)
  
  # Ordenar los datos y mostrar las columnas en orden correcto
  df <- df %>% select(Code, Borough, Year, Crimes) %>% arrange(Code, Year)
  
  return(df)
}

guardar_indicador_07_delitos <- function(df) {
  ruta_fichero <- 'STG_07_indicador_delitos.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 08) Carga y limpieza de datos de los indicadores sobre servicios
################################################################################

carga_indicador_08_servicios <- function() {
  nulos <- c("", "-")
  ruta_fichero <- '07_Servicios/local-units-by-broad-industry-group.xls'
  # Lectura de datos desde 2003 hasta 2008
  lista_pestanas <- 2003:2008
  columnas <- c("Code", "Borough", "Agriculture", "Production", "Construction", "Motor", "Wholesale", "Retail", "Food_Hotels", "Transport", "Post", "Finance", "Property",
                "Education", "Health", "Public", "Total")
  df_aux <- data.frame(Code = NA, Borough = NA, Agriculture = NA, Production = NA, Construction = NA, Motor = NA, Wholesale = NA, Retail = NA, Food_Hotels = NA,
                       Transport = NA, Post = NA, Finance = NA, Property = NA, Education = NA, Health = NA, Public = NA, Total = NA)[0,]
  # Lee la lista de pestañas (desde 2003 hasta 2008)
  for (pestana in lista_pestanas) {
    df_tmp = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = (2 + (pestana - 2003)), skip = 3, col_names = columnas, na = nulos)
    # Añade el dato del año
    df_tmp["Year"] <- pestana
    # Fusiona los datos
    df_aux <- rbind(df_aux, df_tmp)
  }
  df_aux <- df_aux %>% select(Code, Borough, Year, Retail, Food_Hotels, Total)

  # Lectura de las pestañas desde 2009 hasta 2024
  lista_pestanas <- 2009:2024
  columnas <- c("Code", "Borough", "Agriculture", "Production", "Construction", "Motor", "Wholesale", "Retail", "Transport", "Food_Hotels", "Information", "Finance", "Property", "Professional", "Business", "Public", 
                "Education", "Health", "Entertainment", "Total")
  df <- data.frame(Code = NA, Borough = NA, Agriculture = NA, Production = NA, Construction = NA, Motor = NA, Wholesale = NA, Retail = NA, Transport = NA, Food_Hotels = NA, Information = NA, Finance = NA, 
                   Property = NA, Professional = NA, Business = NA, Public = NA, Education = NA, Health = NA, Entertainment = NA, Total = NA)[0,]
  # Lee la lista de pestañas (desde 2009 hasta 2024)
  for (pestana in lista_pestanas) {
    df_tmp = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = (8 + (pestana - 2009)), skip = 3, col_names = columnas, na = nulos)
    # Añade el dato del año
    df_tmp["Year"] <- pestana
    # Fusiona los datos
    df <- rbind(df, df_tmp)
  }
  df <- df %>% select(Code, Borough, Year, Retail, Food_Hotels, Total)
  
  df <- rbind(df, df_aux)
  
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

  # Estimar valores hasta 2031
  anyos_pred <- 2025:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    # Retail
    ts_data_retail <- ts(df_barrio$Retail, start = 2003, end = 2024, frequency = 1)
    model_retail <- auto.arima(ts_data_retail)
    forecast_values_retail <- round(forecast(model_retail, h = length(anyos_pred))$mean, 0)
    # Food
    ts_data_food <- ts(df_barrio$Food_Hotels, start = 2003, end = 2024, frequency = 1)
    model_food <- auto.arima(ts_data_food)
    forecast_values_food <- round(forecast(model_food, h = length(anyos_pred))$mean, 0)
    # Total
    ts_data_total <- ts(df_barrio$Total, start = 2003, end = 2024, frequency = 1)
    model_total <- auto.arima(ts_data_total)
    forecast_values_total <- round(forecast(model_total, h = length(anyos_pred))$mean, 0)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Retail = forecast_values_retail,
                              Food_Hotels = forecast_values_food,
                              Total = forecast_values_total)
    
    df <- rbind(df, forecast_df)
  }
  
  # Ordenar salida
  df <- df %>% arrange(Code, Year)
  return(df)
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
  ruta_fichero <- '08_Vivienda/UK House price index.xlsx'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), sheet = "Average price", na = nulos, col_names = FALSE)  
  return(df)
}

limpieza_indicador_09_vivienda_precio <- function(df) {
  # Crea un dataframe con los códigos y nombres de los barrios
  boroughs <- data.frame(Code = as.character(df[2, -1]), Borough = as.character(df[1, -1]))
  # Obtiene los precios
  prices <- df[-c(1, 2), -1]
  # Obtiene las fechas de la primera columna
  dates <- df %>% slice(-1, -2)
  dates <- dates %>% 
    mutate(Fechas = format(as.Date(dates[[1]]), "M_%Y_%m")) %>% select(Fechas)
  dates <- dates %>%
    pivot_wider(names_from = Fechas, values_from = everything())

  df <- data.frame(t(prices))
  colnames(df) <- dates
  df <- cbind(boroughs, df)

  # Filtra los registros que no corresponden con barrios de Londres
  barrios_londres <- unlist(lista_codes)
  barrios <- unique(df$Code)
  barrios_a_eliminar <- setdiff(barrios, barrios_londres)
  df <- df %>% filter(!(Code %in% barrios_a_eliminar))
  
  # Convierte los datos de precios en numéricos
  df <- df %>%
    mutate(across(starts_with("M_"), ~ parse_number(.)))
  
  # Calcula la media por año
  df <- df %>%
    pivot_longer(cols = starts_with("M_"), names_to = "month_year", values_to = "Price") %>%
    mutate(Year = as.integer(sub("M_(\\d{4})_\\d{2}", "\\1", month_year))) %>%
    group_by(Code, Borough, Year) %>%
    summarize(Price = mean(Price, na.rm = TRUE), .groups = "drop")
  
  # Redondea a dos dígitos
  df <- df %>%
    mutate(Price = round(Price, 2))

  # Estimar valores hasta 2031
  anyos_pred <- 2025:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    ts_data <- ts(df_barrio$Price, start = 1995, end = 2024, frequency = 1)
    model <- auto.arima(ts_data)
    forecast_values <- round(forecast(model, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Price = forecast_values)
    
    df <- rbind(df, forecast_df)
  }
  
  # Ordenar los datos y mostrar las columnas en orden correcto
  df <- df %>% arrange(Code, Year)
  
  return(df)
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

  # Estimar valores hasta 2031
  anyos_pred <- 2024:2031
  for (barrio in unique(df$Borough)) {
    df_barrio <- df %>% filter(Borough == barrio)
    ts_data <- ts(df_barrio$Rent, start = 1997, end = 2023, frequency = 1)
    model <- auto.arima(ts_data)
    forecast_values <- round(forecast(model, h = length(anyos_pred))$mean, 2)
    forecast_df <- data.frame(Code = rep(c(df_barrio %>% slice_head(n = 1) %>% pull(Code)), length(anyos_pred)),
                              Borough = rep(c(barrio), length(anyos_pred)),
                              Year = anyos_pred,
                              Rent = forecast_values)
    
    df <- rbind(df, forecast_df)
  }
  
  # Ordenar los datos y mostrar las columnas en orden correcto
  df <- df %>% arrange(Code, Year)
  
  return(df)
}

guardar_indicador_10_vivienda_alquiler <- function(df) {
  ruta_fichero <- 'STG_10_indicador_vivienda_alquiler.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# Cuerpo principal del programa
################################################################################

# TODO:
#   - Ver qué modelo usar para las predicciones de series temporales
#   - En el indicador 8 falta incluir un total para los tipos de negocios para gentrificación
#   - Revisar los que no tienen datos de un barrio para algún año, ponerlos al último valor conocido
#   - Revisar barrio/año que aparezcan todos
#   - Donde no haya datos de Londres, añadir totales
#   - Eliminar UK de los barrios
#   - De la estimación solo guardar los datos de 2031 y si acaso en edad los de 2021
#   - Eliminar datos previos a 2001
#   - Homogeneizar los comentarios

# Barrios de Londres
df_barrios <- carga_lista_barrios()
lista_barrios <- as.list(df_barrios %>% select(Borough))
lista_codes <- as.list(df_barrios %>% select(Code))

# Indicador 01 - Edad
df_edad = carga_indicador_01_edad()
df_edad = limpieza_indicador_01_edad(df_edad)
guardar_indicador_01_edad(df_edad)

# Indicador 02 - Raza
#df_result = carga_indicador_02_raza()
#df_raza_aux <- df_result$df_aux
#df_raza <- df_result$df
#df_raza = limpieza_indicador_02_raza(df_raza_aux, df_raza)
#guardar_indicador_02_raza(df_raza)

# Indicador 03 - Empleo
#df_empleo = carga_indicador_03_empleo()
#df_empleo = limpieza_indicador_03_empleo(df_empleo)
#guardar_indicador_03_empleo(df_empleo)

# Indicador 04 - Estudios
#df_estudios = carga_indicador_04_estudios()
#df_estudios = limpieza_indicador_04_estudios(df_estudios)
#guardar_indicador_04_estudios(df_estudios)

# Indicador 05 - Tráfico
#df_trafico = carga_indicador_05_trafico()
#df_trafico = limpieza_indicador_05_trafico(df_trafico)
#guardar_indicador_05_trafico(df_trafico)

# Indicador 06 - Esperanza de vida
#df_esperanza_vida = carga_indicador_06_esperanza_vida()
#df_esperanza_vida = limpieza_indicador_06_esperanza_vida(df_esperanza_vida)
#guardar_indicador_06_esperanza_vida(df_esperanza_vida)

# Indicador 07 - Delitos
#df_result = carga_indicador_07_delitos()
#df_delitos_aux <- df_result$df_aux
#df_delitos <- df_result$df
#df_delitos = limpieza_indicador_07_delitos(df_delitos_aux, df_delitos)
#guardar_indicador_07_delitos(df_delitos)

# Indicador 08 - Servicios
#df_servicios = carga_indicador_08_servicios()
#df_servicios = limpieza_indicador_08_servicios(df_servicios)
#guardar_indicador_08_servicios(df_servicios)

# Indicador 09 - Precio vivienda
#df_vivienda_precio = carga_indicador_09_vivienda_precio()
#df_vivienda_precio = limpieza_indicador_09_vivienda_precio(df_vivienda_precio)
#guardar_indicador_09_vivienda_precio(df_vivienda_precio)

# Indicador 10 - Precio alquiler
#df_vivienda_alquiler = carga_indicador_10_vivienda_alquiler()
#df_vivienda_alquiler = limpieza_indicador_10_vivienda_alquiler(df_vivienda_alquiler)
#guardar_indicador_10_vivienda_alquiler(df_vivienda_alquiler)
  
# Carga ficheros
# Renombra columnas
# Elimina columnas
# Filtra filas (solo barrios y datos de la ciudad de Londres)
# Revisión separadores decimales
# Revisión tipo de los datos
# Sustituir valores no válidos
# Imputar valores nulos
# Revisar todos los barrios tienen datos de 2001, 2011, 2021 y 2031 (datos por censo)
