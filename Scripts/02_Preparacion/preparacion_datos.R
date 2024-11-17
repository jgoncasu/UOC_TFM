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
# 01) Carga los indicadores demográficos (edad)
################################################################################
prep_indicador_01_edad <- function() {
  ruta_fichero <- 'STG_01_indicador_edad.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double(), col_double(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2001, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Avg_Age)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_01_AGE")
  
  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_01_AGE = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_01_AGE - lag(VAL_01_AGE)) / lag(VAL_01_AGE) * 100, 2)
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
        round(((VAR_01_AGE - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)

  return(df)
}


################################################################################
# 02) Carga los indicadores demográficos (raciales)
################################################################################
prep_indicador_02_raza <- function() {
  ruta_fichero <- 'STG_02_indicador_raza.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_integer(), col_integer()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2001, 2011, 2021, 2031)) %>% select(Code, Borough, Year, White)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_02_RACE_WHITE")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_02_RACE_WHITE = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_02_RACE_WHITE - lag(VAL_02_RACE_WHITE)) / lag(VAL_02_RACE_WHITE) * 100, 2)
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
        round(((VAR_02_RACE_WHITE - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 03) Carga los indicadores de empleo
################################################################################
prep_indicador_03_empleo <- function() {
  ruta_fichero <- 'STG_03_indicador_empleo.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2002, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Week_Earnings)
  # Se asigna el valor del año 2001 a los datos más antiguos correspondientes a 2002
  df <- df %>% mutate(Year = ifelse(Year == 2002, 2001, Year))
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_03_WEEK_EARNINGS")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_03_WEEK_EARNINGS = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_03_WEEK_EARNINGS - lag(VAL_03_WEEK_EARNINGS)) / lag(VAL_03_WEEK_EARNINGS) * 100, 2)
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
        round(((VAR_03_WEEK_EARNINGS - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)

    return(df) 
}


################################################################################
# 04) Carga los indicadores educativos
################################################################################
prep_indicador_04_estudios <- function() {
  ruta_fichero <- 'STG_04_indicador_estudios.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double(), col_integer(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2004, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Percent)
  # Se asigna el valor del año 2001 a los datos más antiguos correspondientes a 2004
  df <- df %>% mutate(Year = ifelse(Year == 2004, 2001, Year))
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_04_PERCENT_NVQ4")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_04_PERCENT_NVQ4 = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_04_PERCENT_NVQ4 - lag(VAL_04_PERCENT_NVQ4)) / lag(VAL_04_PERCENT_NVQ4) * 100, 2)
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
        round(((VAR_04_PERCENT_NVQ4 - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 05) Carga los indicadores medio ambientales
################################################################################
prep_indicador_05_trafico <- function() {
  ruta_fichero <- 'STG_05_indicador_trafico.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_integer()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2001, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Car_Traffic)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_05_CAR_TRAFFIC")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_05_CAR_TRAFFIC = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_05_CAR_TRAFFIC - lag(VAL_05_CAR_TRAFFIC)) / lag(VAL_05_CAR_TRAFFIC) * 100, 2)
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
        round(((VAR_05_CAR_TRAFFIC - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 06) Carga los indicadores sanitarios
################################################################################
prep_indicador_06_esperanza_vida <- function() {
  ruta_fichero <- 'STG_06_indicador_esperanza_vida.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double(), col_double(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2003, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Avg_Sex)
  # Se asigna el valor del año 2001 a los datos más antiguos correspondientes a 2003
  df <- df %>% mutate(Year = ifelse(Year == 2003, 2001, Year))
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_06_EXP_LIFE")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_06_EXP_LIFE = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_06_EXP_LIFE - lag(VAL_06_EXP_LIFE)) / lag(VAL_06_EXP_LIFE) * 100, 2)
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
        round(((VAR_06_EXP_LIFE - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 07) Carga los indicadores sobre seguridad
################################################################################
prep_indicador_07_delitos <- function() {
  ruta_fichero <- 'STG_07_indicador_delitos.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2001, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Crimes)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_07_CRIMES")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_07_CRIMES = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_07_CRIMES - lag(VAL_07_CRIMES)) / lag(VAL_07_CRIMES) * 100, 2)
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
        round(((VAR_07_CRIMES - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 08) Carga los indicadores sobre servicios
################################################################################
prep_indicador_08_servicios <- function() {
  ruta_fichero <- 'STG_08_indicador_servicios.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_integer(), col_integer(), col_integer()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2003, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Total)
  # Se asigna el valor del año 2001 a los datos más antiguos correspondientes a 2003
  df <- df %>% mutate(Year = ifelse(Year == 2003, 2001, Year))
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_08_SERVICES")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_08_SERVICES = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_08_SERVICES - lag(VAL_08_SERVICES)) / lag(VAL_08_SERVICES) * 100, 2)
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
        round(((VAR_08_SERVICES - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 09) Carga los indicadores sobre vivienda (precio)
################################################################################
prep_indicador_09_vivienda_precio <- function() {
  ruta_fichero <- 'STG_09_indicador_vivienda_precio.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2001, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Price)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_09_HOUSE_PRICE")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_09_HOUSE_PRICE = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_09_HOUSE_PRICE - lag(VAL_09_HOUSE_PRICE)) / lag(VAL_09_HOUSE_PRICE) * 100, 2)
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
        round(((VAR_09_HOUSE_PRICE - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 10) Carga los indicadores sobre vivienda (alquiler)
################################################################################
prep_indicador_10_vivienda_alquiler <- function() {
  ruta_fichero <- 'STG_10_indicador_vivienda_alquiler.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_integer(), col_double()))

  # Selecciona el campo con el valor para el indicador
  df <- df %>% filter(Year %in% c(2001, 2011, 2021, 2031)) %>% select(Code, Borough, Year, Rent)
  colnames(df) <- c("CODE", "BOROUGH", "YEAR", "VAL_10_HOUSE_RENT")

  # Calcula la variación del indicador en las tres décadas
  df <- df %>%
    arrange(CODE, BOROUGH, YEAR) %>%
    group_by(CODE, BOROUGH) %>%
    mutate(
      VAR_10_HOUSE_RENT = case_when(
        YEAR == 2001 ~ 0,
        TRUE ~ round((VAL_10_HOUSE_RENT - lag(VAL_10_HOUSE_RENT)) / lag(VAL_10_HOUSE_RENT) * 100, 2)
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
        round(((VAR_10_HOUSE_RENT - VAR_LONDON) / abs(VAR_LONDON)) * 100, 2),
        0
      )
    ) %>%
    select(-VAR_LONDON)
  
  return(df) 
}


################################################################################
# 11) Fusiona todos los datos
################################################################################
fusiona_indicadores <- function(df_01_edad, df_02_raza, df_03_empleo, df_04_estudios, df_05_trafico,
                                df_06_esperanza_vida, df_07_delitos, df_08_servicios, df_09_vivienda_precio, df_10_vivienda_alquiler) {
  df <- merge(df_01_edad, df_02_raza, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  
  #print(df)

  df <- merge(df, df_03_empleo, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_04_estudios, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_05_trafico, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_06_esperanza_vida, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_07_delitos, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_08_servicios, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_09_vivienda_precio, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  df <- merge(df, df_10_vivienda_alquiler, by = c("CODE", "BOROUGH", "YEAR"), all = TRUE)
  
  return(df)
}


################################################################################
# 12) Guarda los datos
################################################################################
guardar_indicadores <- function(df) {
  ruta_fichero <- 'DAT_Indicadores_Londres.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}

################################################################################
# Cuerpo principal del programa
################################################################################

print("***** PASO 0: CARGANDO BARRIOS DE LONDRES *****")
df_barrios <- carga_lista_barrios()

print("***** PASO 1: PREPARACIÓN INDICADOR 01 - EDAD *****")
df_01_edad <- prep_indicador_01_edad()
#print(df_01_edad)
print("***** PASO 2: PREPARACIÓN INDICADOR 02 - RAZA *****")
df_02_raza <- prep_indicador_02_raza()
#print(df_02_raza)
print("***** PASO 3: PREPARACIÓN INDICADOR 03 - EMPLEO *****")
df_03_empleo <- prep_indicador_03_empleo()
#print(df_03_empleo)
print("***** PASO 4: PREPARACIÓN INDICADOR 04 - ESTUDIOS *****")
df_04_estudios <- prep_indicador_04_estudios()
#print(df_04_estudios)
print("***** PASO 5: PREPARACIÓN INDICADOR 05 - TRÁFICO *****")
df_05_trafico <- prep_indicador_05_trafico()
#print(df_05_trafico)
print("***** PASO 6: PREPARACIÓN INDICADOR 06 - ESPERANZA DE VIDA *****")
df_06_esperanza_vida <- prep_indicador_06_esperanza_vida()
#print(df_06_esperanza_vida)
print("***** PASO 7: PREPARACIÓN INDICADOR 07 - DELITOS *****")
df_07_delitos <- prep_indicador_07_delitos()
#print(df_07_delitos)
print("***** PASO 8: PREPARACIÓN INDICADOR 08 - SERVICIOS *****")
df_08_servicios <- prep_indicador_08_servicios()
#print(df_08_servicios)
print("***** PASO 9: PREPARACIÓN INDICADOR 09 - PRECIO DE LA VIVIENDA *****")
df_09_vivienda_precio <- prep_indicador_09_vivienda_precio()
#print(df_09_vivienda_precio)
print("***** PASO 10: PREPARACIÓN INDICADOR 10 - ALQUILER *****")
df_10_vivienda_alquiler <- prep_indicador_10_vivienda_alquiler()
#print(df_10_vivienda_alquiler)

print("***** PASO 11: PREPARACIÓN DE DATOS PARA ANÁLISIS *****")
df <- fusiona_indicadores(df_01_edad, df_02_raza, df_03_empleo, df_04_estudios, df_05_trafico,
                    df_06_esperanza_vida, df_07_delitos, df_08_servicios, df_09_vivienda_precio, df_10_vivienda_alquiler)

print("***** PASO 12: GUARDAR DATOS PARA ANÁLISIS *****")
guardar_indicadores(df)

# TODO: Crear columna variación respecto a Londres
