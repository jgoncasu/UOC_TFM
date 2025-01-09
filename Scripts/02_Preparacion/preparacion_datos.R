# Librerías
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Constantes
PATH_FICHEROS_BARRIOS <- 'DATOS/01_Fuentes/'
PATH_FICHEROS_ENTRADA <- 'DATOS/02_Staging/'
PATH_FICHEROS_SALIDA <- 'DATOS/03_Preparados/'

################################################################################
# 00) Carga los barrios
################################################################################
carga_lista_barrios <- function() {
  ruta_fichero <- '00_Barrios/barrios_londres.csv'
  df = read_csv(paste(PATH_FICHEROS_BARRIOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_character()))
  return(df)
}

################################################################################
# 00) Calcula la variación de un indicador en un barrio
################################################################################
calcula_variacion_barrio <- function(valor_inicial, valor_final, anyos) {
  valor <- round((((valor_final / valor_inicial)^(1 / anyos)) - 1) * 100, 4)
  return(valor)
}

################################################################################
# 00) Calcula la variación de un indicador respecto a Londres
################################################################################
calcula_variacion_londres <- function(valor_barrio, valor_londres) {
  #valor <- round(((valor_barrio - valor_londres) / abs(valor_londres)) * 100, 4)
  valor <- round((valor_barrio - valor_londres) / abs(valor_londres), 4)
  return(valor)
}

################################################################################
# 01) Carga los datos demográficos (edad)
################################################################################
carga_datos_01_edad <- function() {
  ruta_fichero <- 'STG_01_indicador_edad.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double(), col_double(), col_double()))
  return(df)
}

################################################################################
# 01) Carga los indicadores demográficos (edad)
################################################################################
prep_indicador_01_edad <- function(df_STG_datos) {
  
  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2001) %>%
    select(Code, Borough, Year, Pct_25_40)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2001 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Pct_25_40 = mean(Pct_25_40, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Pct_25_40)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Pct_25_40 = mean(Pct_25_40, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Pct_25_40)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Pct_25_40 = mean(Pct_25_40, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Pct_25_40)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)

  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_01_AGE")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_01_AGE = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_01_AGE), VAL_01_AGE, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_01_AGE), VAL_01_AGE, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_01_AGE) %>%
        rename(VAR_LONDON = VAR_01_AGE),
      by = "YEAR"
    ) %>%
    mutate (
      IND_01_AGE = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_01_AGE, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_01_AGE = round(VAL_01_AGE, 2),
      VAR_01_AGE = round(VAR_01_AGE, 3),
      IND_01_AGE = round(IND_01_AGE, 3)
    )
  
  return(df)
}

################################################################################
# 01) Prepara los datos en bruto demográficos (edad)
################################################################################
datos_brutos_01_edad <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Pct_25_40)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_01_AGE")
  
  return(df)
}

################################################################################
# 02) Carga los datos demográficos (raciales)
################################################################################
carga_datos_02_raza <- function() {
  ruta_fichero <- 'STG_02_indicador_raza.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_double()))
  return(df)
}

################################################################################
# 02) Carga los indicadores demográficos (raciales)
################################################################################
prep_indicador_02_raza <- function(df_STG_datos) {
  
  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2001) %>%
    select(Code, Borough, Year, Percent_White)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2001 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Percent_White = mean(Percent_White, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Percent_White)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Percent_White = mean(Percent_White, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Percent_White)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Percent_White = mean(Percent_White, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Percent_White)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_02_RACE_WHITE")
  
  # Calcula la variación del indicador en los tres períodos
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_02_RACE_WHITE = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_02_RACE_WHITE), VAL_02_RACE_WHITE, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_02_RACE_WHITE), VAL_02_RACE_WHITE, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_02_RACE_WHITE) %>%
        rename(VAR_LONDON = VAR_02_RACE_WHITE),
      by = "YEAR"
    ) %>%
    mutate (
      IND_02_RACE_WHITE = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_02_RACE_WHITE, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_02_RACE_WHITE = round(VAL_02_RACE_WHITE, 2),
      VAR_02_RACE_WHITE = round(VAR_02_RACE_WHITE, 3),
      IND_02_RACE_WHITE = round(IND_02_RACE_WHITE, 3)
    )
  
  return(df)
}
  

################################################################################
# 02) Prepara los datos en bruto demográficos (raciales)
################################################################################
datos_brutos_02_raza <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Percent_White)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_02_RACE_WHITE")
  
  return(df)
}

################################################################################
# 03) Carga los datos de empleo
################################################################################
carga_datos_03_empleo <- function() {
  ruta_fichero <- 'STG_03_indicador_empleo.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))
  return(df)
}

################################################################################
# 03) Carga los indicadores de empleo
################################################################################
prep_indicador_03_empleo <- function(df_STG_datos) {
  
  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2002) %>%
    mutate(Year = 2001) %>%
    select(Code, Borough, Year, Week_Earnings)
  
  # Calcula la media para el periodo 2001-2011
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2002 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Week_Earnings = mean(Week_Earnings, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Week_Earnings)
  
  # Calcula la media para el periodo 2011-2021
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Week_Earnings = mean(Week_Earnings, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Week_Earnings)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Week_Earnings = mean(Week_Earnings, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Week_Earnings)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_03_WEEK_EARNINGS")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_03_WEEK_EARNINGS = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2010 ~ calcula_variacion_barrio(lag(VAL_03_WEEK_EARNINGS), VAL_03_WEEK_EARNINGS, 9),
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_03_WEEK_EARNINGS), VAL_03_WEEK_EARNINGS, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_03_WEEK_EARNINGS), VAL_03_WEEK_EARNINGS, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_03_WEEK_EARNINGS) %>%
        rename(VAR_LONDON = VAR_03_WEEK_EARNINGS),
      by = "YEAR"
    ) %>%
    mutate (
      IND_03_WEEK_EARNINGS = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_03_WEEK_EARNINGS, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_03_WEEK_EARNINGS = round(VAL_03_WEEK_EARNINGS, 2),
      VAR_03_WEEK_EARNINGS = round(VAR_03_WEEK_EARNINGS, 3),
      IND_03_WEEK_EARNINGS = round(IND_03_WEEK_EARNINGS, 3)
    )
  
  return(df)
}

################################################################################
# 03) Prepara los datos en bruto de empleo
################################################################################
datos_brutos_03_empleo <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Week_Earnings)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_03_WEEK_EARNINGS")
  
  return(df)
}

################################################################################
# 04) Carga los datos educativos
################################################################################
carga_datos_04_estudios <- function() {
  ruta_fichero <- 'STG_04_indicador_estudios.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))
  return(df)
}

################################################################################
# 04) Carga los indicadores educativos
################################################################################
prep_indicador_04_estudios <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2004) %>%
    mutate(Year = 2001) %>%
    select(Code, Borough, Year, Percent)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2004 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Percent = mean(Percent, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Percent)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Percent = mean(Percent, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Percent)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Percent = mean(Percent, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Percent)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_04_PERCENT_NVQ4")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_04_PERCENT_NVQ4 = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2011 ~ calcula_variacion_barrio(lag(VAL_04_PERCENT_NVQ4), VAL_04_PERCENT_NVQ4, 7),
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_04_PERCENT_NVQ4), VAL_04_PERCENT_NVQ4, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_04_PERCENT_NVQ4), VAL_04_PERCENT_NVQ4, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_04_PERCENT_NVQ4) %>%
        rename(VAR_LONDON = VAR_04_PERCENT_NVQ4),
      by = "YEAR"
    ) %>%
    mutate (
      IND_04_PERCENT_NVQ4 = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_04_PERCENT_NVQ4, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_04_PERCENT_NVQ4 = round(VAL_04_PERCENT_NVQ4, 2),
      VAR_04_PERCENT_NVQ4 = round(VAR_04_PERCENT_NVQ4, 3),
      IND_04_PERCENT_NVQ4 = round(IND_04_PERCENT_NVQ4, 3)
    )
  
  return(df)
}

################################################################################
# 04) Prepara los datos en bruto educativos
################################################################################
datos_brutos_04_estudios <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Percent)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_04_PERCENT_NVQ4")
  
  return(df)
}

################################################################################
# 05) Carga los datos medio ambientales
################################################################################
carga_datos_05_trafico <- function() {
  ruta_fichero <- 'STG_05_indicador_trafico.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_integer()))
  return(df)
}

################################################################################
# 05) Carga los indicadores medio ambientales
################################################################################
prep_indicador_05_trafico <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2001) %>%
    select(Code, Borough, Year, Car_Traffic)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2001 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Car_Traffic = mean(Car_Traffic, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Car_Traffic)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Car_Traffic = mean(Car_Traffic, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Car_Traffic)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Car_Traffic = mean(Car_Traffic, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Car_Traffic)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_05_CAR_TRAFFIC")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_05_CAR_TRAFFIC = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_05_CAR_TRAFFIC), VAL_05_CAR_TRAFFIC, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_05_CAR_TRAFFIC), VAL_05_CAR_TRAFFIC, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_05_CAR_TRAFFIC) %>%
        rename(VAR_LONDON = VAR_05_CAR_TRAFFIC),
      by = "YEAR"
    ) %>%
    mutate (
      IND_05_CAR_TRAFFIC = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_05_CAR_TRAFFIC, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_05_CAR_TRAFFIC = round(VAL_05_CAR_TRAFFIC, 2),
      VAR_05_CAR_TRAFFIC = round(VAR_05_CAR_TRAFFIC, 3),
      IND_05_CAR_TRAFFIC = round(IND_05_CAR_TRAFFIC, 3)
    )
  
  return(df)
}

################################################################################
# 05) Prepara los datos en bruto medio ambientales
################################################################################
datos_brutos_05_trafico <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Car_Traffic)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_05_CAR_TRAFFIC")
  
  return(df)
}

################################################################################
# 06) Carga los datos sanitarios
################################################################################
carga_datos_06_esperanza_vida <- function() {
  ruta_fichero <- 'STG_06_indicador_esperanza_vida.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))
  return(df)
}

################################################################################
# 06) Carga los indicadores sanitarios
################################################################################
prep_indicador_06_esperanza_vida <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2003) %>%
    mutate(Year = 2001) %>%
    select(Code, Borough, Year, Avg_Sex)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2003 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Avg_Sex = mean(Avg_Sex, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Avg_Sex)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Avg_Sex = mean(Avg_Sex, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Avg_Sex)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Avg_Sex = mean(Avg_Sex, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Avg_Sex)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_06_EXP_LIFE")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_06_EXP_LIFE = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2011 ~ calcula_variacion_barrio(lag(VAL_06_EXP_LIFE), VAL_06_EXP_LIFE, 8),
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_06_EXP_LIFE), VAL_06_EXP_LIFE, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_06_EXP_LIFE), VAL_06_EXP_LIFE, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_06_EXP_LIFE) %>%
        rename(VAR_LONDON = VAR_06_EXP_LIFE),
      by = "YEAR"
    ) %>%
    mutate (
      IND_06_EXP_LIFE = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_06_EXP_LIFE, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_06_EXP_LIFE = round(VAL_06_EXP_LIFE, 2),
      VAR_06_EXP_LIFE = round(VAR_06_EXP_LIFE, 3),
      IND_06_EXP_LIFE = round(IND_06_EXP_LIFE, 3)
    )
  
  return(df)
}

################################################################################
# 06) Prepara los datos en bruto sanitarios
################################################################################
datos_brutos_06_esperanza_vida <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Avg_Sex)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_06_EXP_LIFE")
  
  return(df)
}

################################################################################
# 07) Carga los datos sobre seguridad
################################################################################
carga_datos_07_delitos <- function() {
  ruta_fichero <- 'STG_07_indicador_delitos.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))
  return(df)
}

################################################################################
# 07) Carga los indicadores sobre seguridad
################################################################################
prep_indicador_07_delitos <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2001) %>%
    select(Code, Borough, Year, Crimes)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2001 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Crimes = mean(Crimes, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Crimes)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Crimes = mean(Crimes, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Crimes)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Crimes = mean(Crimes, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Crimes)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_07_CRIMES")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_07_CRIMES = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_07_CRIMES), VAL_07_CRIMES, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_07_CRIMES), VAL_07_CRIMES, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_07_CRIMES) %>%
        rename(VAR_LONDON = VAR_07_CRIMES),
      by = "YEAR"
    ) %>%
    mutate (
      IND_07_CRIMES = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_07_CRIMES, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_07_CRIMES = round(VAL_07_CRIMES, 2),
      VAR_07_CRIMES = round(VAR_07_CRIMES, 3),
      IND_07_CRIMES = round(IND_07_CRIMES, 3)
    )
  
  return(df)
}

################################################################################
# 07) Prepara los datos en bruto sobre seguridad
################################################################################
datos_brutos_07_delitos <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Crimes)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_07_CRIMES")
  
  return(df)
}

################################################################################
# 08) Carga los datos sobre servicios
################################################################################
carga_datos_08_servicios <- function() {
  ruta_fichero <- 'STG_08_indicador_servicios.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_integer()))
  return(df)
}

################################################################################
# 08) Carga los indicadores sobre servicios
################################################################################
prep_indicador_08_servicios <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2003) %>%
    mutate(Year = 2001) %>%
    select(Code, Borough, Year, Total)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2003 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Total = mean(Total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Total)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Total = mean(Total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Total)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Total = mean(Total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Total)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_08_SERVICES")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_08_SERVICES = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2011 ~ calcula_variacion_barrio(lag(VAL_08_SERVICES), VAL_08_SERVICES, 8),
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_08_SERVICES), VAL_08_SERVICES, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_08_SERVICES), VAL_08_SERVICES, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_08_SERVICES) %>%
        rename(VAR_LONDON = VAR_08_SERVICES),
      by = "YEAR"
    ) %>%
    mutate (
      IND_08_SERVICES = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_08_SERVICES, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_08_SERVICES = round(VAL_08_SERVICES, 2),
      VAR_08_SERVICES = round(VAR_08_SERVICES, 3),
      IND_08_SERVICES = round(IND_08_SERVICES, 3)
    )
  
  return(df)
}

################################################################################
# 08) Prepara los datos en bruto sobre servicios
################################################################################
datos_brutos_08_servicios <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Total)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_08_SERVICES")
  
  return(df)
}

################################################################################
# 09) Carga los datos sobre vivienda (precio)
################################################################################
carga_datos_09_vivienda_precio <- function() {
  ruta_fichero <- 'STG_09_indicador_vivienda_precio.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))
  return(df)
}

################################################################################
# 09) Carga los indicadores sobre vivienda (precio)
################################################################################
prep_indicador_09_vivienda_precio <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2001) %>%
    select(Code, Borough, Year, Price)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2001 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Price = mean(Price, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Price)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Price = mean(Price, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Price)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Price = mean(Price, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Price)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_09_HOUSE_PRICE")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_09_HOUSE_PRICE = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_09_HOUSE_PRICE), VAL_09_HOUSE_PRICE, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_09_HOUSE_PRICE), VAL_09_HOUSE_PRICE, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_09_HOUSE_PRICE) %>%
        rename(VAR_LONDON = VAR_09_HOUSE_PRICE),
      by = "YEAR"
    ) %>%
    mutate (
      IND_09_HOUSE_PRICE = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_09_HOUSE_PRICE, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_09_HOUSE_PRICE = round(VAL_09_HOUSE_PRICE, 2),
      VAR_09_HOUSE_PRICE = round(VAR_09_HOUSE_PRICE, 3),
      IND_09_HOUSE_PRICE = round(IND_09_HOUSE_PRICE, 3)
    )
  
  return(df)
}

################################################################################
# 09) Prepara los datos en bruto sobre vivienda (precio)
################################################################################
datos_brutos_09_vivienda_precio <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Price)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAR_09_HOUSE_PRICE")
  
  return(df)
}

################################################################################
# 10) Carga los datos sobre vivienda (alquiler)
################################################################################
carga_datos_10_vivienda_alquiler <- function() {
  ruta_fichero <- 'STG_10_indicador_vivienda_alquiler.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))
  return(df)
}

################################################################################
# 10) Carga los indicadores sobre vivienda (alquiler)
################################################################################
prep_indicador_10_vivienda_alquiler <- function(df_STG_datos) {

  # Guarda el valor inicial para 2001
  df_2001 <- df_STG_datos %>% 
    filter(Year == 2001) %>%
    select(Code, Borough, Year, Rent)
  
  # Calcula la media para el periodo 2001-2010
  df_2010 <- df_STG_datos %>%
    filter(Year >= 2001 & Year <= 2010) %>%
    group_by(Code, Borough) %>%
    summarise(
      Rent = mean(Rent, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2010) %>%
    select(Code, Borough, Year, Rent)
  
  # Calcula la media para el periodo 2011-2020
  df_2020 <- df_STG_datos %>%
    filter(Year >= 2011 & Year <= 2020) %>%
    group_by(Code, Borough) %>%
    summarise(
      Rent = mean(Rent, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2020) %>%
    select(Code, Borough, Year, Rent)
  
  # Calcula la media para el periodo 2021-2025
  df_2025 <- df_STG_datos %>%
    filter(Year >= 2021 & Year <= 2025) %>%
    group_by(Code, Borough) %>%
    summarise(
      Rent = mean(Rent, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Year = 2025) %>%
    select(Code, Borough, Year, Rent)
  df <- bind_rows(df_2001, df_2010, df_2020, df_2025)
  
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_10_HOUSE_RENT")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_10_HOUSE_RENT = case_when(
        YEAR == 2001 ~ 0,
        YEAR == 2025 ~ calcula_variacion_barrio(lag(VAL_10_HOUSE_RENT), VAL_10_HOUSE_RENT, 5),
        TRUE ~ calcula_variacion_barrio(lag(VAL_10_HOUSE_RENT), VAL_10_HOUSE_RENT, 10)
      )
    ) %>%
    ungroup()
  
  # Calcula la variación respecto a la ciudad de Londres
  df <- df %>%
    left_join(
      df %>%
        filter(BOROUGH == "London") %>%
        select(YEAR, VAR_10_HOUSE_RENT) %>%
        rename(VAR_LONDON = VAR_10_HOUSE_RENT),
      by = "YEAR"
    ) %>%
    mutate (
      IND_10_HOUSE_RENT = if_else(
        !is.na(VAR_LONDON) & VAR_LONDON != 0,
        calcula_variacion_londres(VAR_10_HOUSE_RENT, VAR_LONDON),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  # Redondea los valores
  df <- df %>%
    mutate(
      VAL_10_HOUSE_RENT = round(VAL_10_HOUSE_RENT, 2),
      VAR_10_HOUSE_RENT = round(VAR_10_HOUSE_RENT, 3),
      IND_10_HOUSE_RENT = round(IND_10_HOUSE_RENT, 3)
    )
  
  return(df)
}

################################################################################
# 10) Prepara los datos en bruto sobre vivienda (alquiler)
################################################################################
datos_brutos_10_vivienda_alquiler <- function(df_STG_datos) {
  # Selecciona el campo con el valor para el indicador
  df <- df_STG_datos %>% filter(Year >= 2001 & Year <= 2025) %>% select(Code, Borough, Year, Rent)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_10_HOUSE_RENT")
  
  return(df)
}

################################################################################
# 11) Fusiona los datos de los indicadores
################################################################################
fusiona_indicadores <- function(df_01_edad, df_02_raza, df_03_empleo, df_04_estudios, df_05_trafico,
                                df_06_esperanza_vida, df_07_delitos, df_08_servicios, df_09_vivienda_precio, df_10_vivienda_alquiler) {
  df <- merge(df_01_edad, df_02_raza, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  
  df <- merge(df, df_03_empleo, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_04_estudios, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_05_trafico, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_06_esperanza_vida, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_07_delitos, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_08_servicios, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_09_vivienda_precio, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  #df <- merge(df, df_10_vivienda_alquiler, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  
  return(df)
}

################################################################################
# 12) Guarda los datos
################################################################################
guardar_indicadores <- function(df) {
  ruta_fichero <- 'DAT_Indicadores_Londres.csv'
  write.csv2(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}

################################################################################
# 13) Fusiona los datos en bruto para las visualizaciones
################################################################################
fusiona_datos_en_bruto <- function(df_brutos_01_edad, df_brutos_02_raza, df_brutos_03_empleo, df_brutos_04_estudios, df_brutos_05_trafico,
                                df_brutos_06_esperanza_vida, df_brutos_07_delitos, df_brutos_08_servicios, df_brutos_09_vivienda_precio, df_brutos_10_vivienda_alquiler) {
  df <- merge(df_brutos_01_edad, df_brutos_02_raza, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  
  df <- merge(df, df_brutos_03_empleo, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_brutos_04_estudios, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_brutos_05_trafico, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_brutos_06_esperanza_vida, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_brutos_07_delitos, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_brutos_08_servicios, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_brutos_09_vivienda_precio, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  #df <- merge(df, df_brutos_10_vivienda_alquiler, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  
  return(df)
}

################################################################################
# 14) Guarda los datos en bruto
################################################################################
guardar_datos_brutos <- function(df) {
  ruta_fichero <- 'DAT_Brutos_Londres.csv'
  write.csv2(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}

################################################################################
# 15) Guarda los indicadores para visualización
################################################################################
guardar_indicadores_visualizacion <- function(df) {
  
  df_global <- df %>%
    pivot_longer(
      cols = c("VAL_01_AGE","VAR_01_AGE","IND_01_AGE",
               "VAL_02_RACE_WHITE","VAR_02_RACE_WHITE","IND_02_RACE_WHITE",
               "VAL_03_WEEK_EARNINGS","VAR_03_WEEK_EARNINGS","IND_03_WEEK_EARNINGS",
               "VAL_04_PERCENT_NVQ4","VAR_04_PERCENT_NVQ4","IND_04_PERCENT_NVQ4",
               "VAL_05_CAR_TRAFFIC","VAR_05_CAR_TRAFFIC","IND_05_CAR_TRAFFIC",
               "VAL_06_EXP_LIFE","VAR_06_EXP_LIFE","IND_06_EXP_LIFE",
               "VAL_07_CRIMES","VAR_07_CRIMES","IND_07_CRIMES",
               "VAL_08_SERVICES","VAR_08_SERVICES","IND_08_SERVICES",
               "VAL_09_HOUSE_PRICE","VAR_09_HOUSE_PRICE","IND_09_HOUSE_PRICE"
               ),
      names_to = "VARIABLE"
    )
  
  # Renombrar columnas
  df_global <- df_global %>%
    rename(VALUE = value)
  
  ruta_fichero <- 'DAT_Indicadores_Visualizacion_Londres.csv'
  write.csv2(df_global, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# Cuerpo principal del programa
################################################################################

print("***** PASO 0: CARGANDO BARRIOS DE LONDRES *****")
df_barrios <- carga_lista_barrios()

print("***** PASO 1: CARGA DATOS INDICADOR 01 - EDAD *****")
df_STG_01_edad <- carga_datos_01_edad()
#print(df_STG_01_edad)
print("***** PASO 2: CARGA DATOS INDICADOR 02 - RAZA *****")
df_STG_02_raza <- carga_datos_02_raza()
#print(df_STG_02_raza)
print("***** PASO 3: CARGA DATOS INDICADOR 03 - EMPLEO *****")
df_STG_03_empleo <- carga_datos_03_empleo()
#print(df_STG_03_empleo)
print("***** PASO 4: CARGA DATOS INDICADOR 04 - ESTUDIOS *****")
df_STG_04_estudios <- carga_datos_04_estudios()
#print(df_STG_04_estudios)
print("***** PASO 5: CARGA DATOS INDICADOR 05 - TRÁFICO *****")
df_STG_05_trafico <- carga_datos_05_trafico()
#print(df_STG_05_trafico)
print("***** PASO 6: CARGA DATOS INDICADOR 06 - ESPERANZA DE VIDA *****")
df_STG_06_esperanza_vida <- carga_datos_06_esperanza_vida()
#print(df_STG_06_esperanza_vida)
print("***** PASO 7: CARGA DATOS INDICADOR 07 - DELITOS *****")
df_STG_07_delitos <- carga_datos_07_delitos()
#print(df_STG_07_delitos)
print("***** PASO 8: CARGA DATOS INDICADOR 08 - SERVICIOS *****")
df_STG_08_servicios <- carga_datos_08_servicios()
#print(df_STG_08_servicios)
print("***** PASO 9: CARGA DATOS INDICADOR 09 - PRECIO DE LA VIVIENDA *****")
df_STG_09_vivienda_precio <- carga_datos_09_vivienda_precio()
#print(df_STG_09_vivienda_precio)
print("***** PASO 10: CARGA DATOS INDICADOR 10 - ALQUILER *****")
df_STG_10_vivienda_alquiler <- carga_datos_10_vivienda_alquiler()
#print(df_STG_10_vivienda_alquiler)


print("***** PASO 11: PREPARACIÓN INDICADOR 01 - EDAD *****")
df_01_edad <- prep_indicador_01_edad(df_STG_01_edad)
#print(df_01_edad)
print("***** PASO 12: PREPARACIÓN INDICADOR 02 - RAZA *****")
df_02_raza <- prep_indicador_02_raza(df_STG_02_raza)
#print(df_02_raza)
print("***** PASO 13: PREPARACIÓN INDICADOR 03 - EMPLEO *****")
df_03_empleo <- prep_indicador_03_empleo(df_STG_03_empleo)
#print(df_03_empleo)
print("***** PASO 14: PREPARACIÓN INDICADOR 04 - ESTUDIOS *****")
df_04_estudios <- prep_indicador_04_estudios(df_STG_04_estudios)
#print(df_04_estudios)
print("***** PASO 15: PREPARACIÓN INDICADOR 05 - TRÁFICO *****")
df_05_trafico <- prep_indicador_05_trafico(df_STG_05_trafico)
#print(df_05_trafico)
print("***** PASO 16: PREPARACIÓN INDICADOR 06 - ESPERANZA DE VIDA *****")
df_06_esperanza_vida <- prep_indicador_06_esperanza_vida(df_STG_06_esperanza_vida)
#print(df_06_esperanza_vida)
print("***** PASO 17: PREPARACIÓN INDICADOR 07 - DELITOS *****")
df_07_delitos <- prep_indicador_07_delitos(df_STG_07_delitos)
#print(df_07_delitos)
print("***** PASO 18: PREPARACIÓN INDICADOR 08 - SERVICIOS *****")
df_08_servicios <- prep_indicador_08_servicios(df_STG_08_servicios)
#print(df_08_servicios)
print("***** PASO 19: PREPARACIÓN INDICADOR 09 - PRECIO DE LA VIVIENDA *****")
df_09_vivienda_precio <- prep_indicador_09_vivienda_precio(df_STG_09_vivienda_precio)
#print(df_09_vivienda_precio)
print("***** PASO 20: PREPARACIÓN INDICADOR 10 - ALQUILER *****")
df_10_vivienda_alquiler <- prep_indicador_10_vivienda_alquiler(df_STG_10_vivienda_alquiler)
#print(df_10_vivienda_alquiler)

print("***** PASO 21: DATOS EN BRUTO 01 - EDAD *****")
df_brutos_01_edad <- datos_brutos_01_edad(df_STG_01_edad)
#print(df_01_edad)
print("***** PASO 22: DATOS EN BRUTO 02 - RAZA *****")
df_brutos_02_raza <- datos_brutos_02_raza(df_STG_02_raza)
#print(df_02_raza)
print("***** PASO 23: DATOS EN BRUTO 03 - EMPLEO *****")
df_brutos_03_empleo <- datos_brutos_03_empleo(df_STG_03_empleo)
#print(df_03_empleo)
print("***** PASO 24: DATOS EN BRUTO 04 - ESTUDIOS *****")
df_brutos_04_estudios <- datos_brutos_04_estudios(df_STG_04_estudios)
#print(df_04_estudios)
print("***** PASO 25: DATOS EN BRUTO 05 - TRÁFICO *****")
df_brutos_05_trafico <- datos_brutos_05_trafico(df_STG_05_trafico)
#print(df_05_trafico)
print("***** PASO 26: DATOS EN BRUTO 06 - ESPERANZA DE VIDA *****")
df_brutos_06_esperanza_vida <- datos_brutos_06_esperanza_vida(df_STG_06_esperanza_vida)
#print(df_06_esperanza_vida)
print("***** PASO 27: DATOS EN BRUTO 07 - DELITOS *****")
df_brutos_07_delitos <- datos_brutos_07_delitos(df_STG_07_delitos)
#print(df_07_delitos)
print("***** PASO 28: DATOS EN BRUTO 08 - SERVICIOS *****")
df_brutos_08_servicios <- datos_brutos_08_servicios(df_STG_08_servicios)
#print(df_08_servicios)
print("***** PASO 29: DATOS EN BRUTO 09 - PRECIO DE LA VIVIENDA *****")
df_brutos_09_vivienda_precio <- datos_brutos_09_vivienda_precio(df_STG_09_vivienda_precio)
#print(df_09_vivienda_precio)
print("***** PASO 30: DATOS EN BRUTOR 10 - ALQUILER *****")
df_brutos_10_vivienda_alquiler <- datos_brutos_10_vivienda_alquiler(df_STG_10_vivienda_alquiler)
#print(df_10_vivienda_alquiler)

print("***** PASO 11: PREPARACIÓN DE DATOS PARA ANÁLISIS *****")
df <- fusiona_indicadores(df_01_edad, df_02_raza, df_03_empleo, df_04_estudios, df_05_trafico,
                    df_06_esperanza_vida, df_07_delitos, df_08_servicios, df_09_vivienda_precio, df_10_vivienda_alquiler)

print("***** PASO 12: GUARDAR DATOS PARA ANÁLISIS *****")
guardar_indicadores(df)

print("***** PASO 13: EXTRACCIÓN DATOS EN BRUTO PARA VISUALIZACIÓN *****")
df_brutos <- fusiona_datos_en_bruto(df_brutos_01_edad, df_brutos_02_raza, df_brutos_03_empleo, df_brutos_04_estudios, df_brutos_05_trafico,
                             df_brutos_06_esperanza_vida, df_brutos_07_delitos, df_brutos_08_servicios, df_brutos_09_vivienda_precio, df_brutos_10_vivienda_alquiler)

print("***** PASO 14: GUARDAR DATOS EN BRUTO PARA VISUALIZACIÓN *****")
guardar_datos_brutos(df_brutos)

print("***** PASO 15: GUARDAR INDICADORES PARA VISUALIZACIÓN *****")
guardar_indicadores_visualizacion(df)

