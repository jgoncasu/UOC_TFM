# Librerías
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')

# Constantes
PATH_FICHEROS_BARRIOS <- 'DATOS/01_Fuentes/'
PATH_FICHEROS_ENTRADA <- 'DATOS/03_Preparados/'
#PATH_FICHEROS_SALIDA <- 'DATOS/04_Analisis/'

################################################################################
# 00) Carga los barrios
################################################################################
carga_lista_barrios <- function() {
  ruta_fichero <- '00_Barrios/barrios_londres.csv'
  df = read_csv(paste(PATH_FICHEROS_BARRIOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_character()))
  return(df)
}

################################################################################
# 00) Carga los datos de los indicadores
################################################################################
carga_indicadores <- function() {
  ruta_fichero <- 'DAT_Indicadores_Londres.csv'
  df = read_csv2(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), 
                col_types = list(col_character(), col_character(), col_integer(), 
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_integer(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_integer(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double()),
                locale = locale(decimal_mark = ".", grouping_mark = ""))
  return(df)
}

################################################################################
# 01) Muestra el top3 del indicador 01
################################################################################
muestra_top_3_ind_01 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_01_AGE_PREV = lag(VAL_01_AGE)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2031), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, IND_01_AGE) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_01_AGE_PREV, VAL_01_AGE, VAR_01_AGE, IND_01_AGE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2031)) %>%
    select(CODE, BOROUGH, YEAR, VAL_01_AGE_PREV, VAL_01_AGE, VAR_01_AGE, IND_01_AGE)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, IND_01_AGE) %>%
    mutate(across(c(VAL_01_AGE_PREV, VAL_01_AGE), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_01_AGE, IND_01_AGE), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_01_AGE_PREV, VAL_01_AGE, VAR_01_AGE, IND_01_AGE), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 02) Muestra el top3 del indicador 02
################################################################################
muestra_top_3_ind_02 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_02_RACE_WHITE_PREV = lag(VAL_02_RACE_WHITE)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2031), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_02_RACE_WHITE)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_02_RACE_WHITE_PREV, VAL_02_RACE_WHITE, VAR_02_RACE_WHITE, IND_02_RACE_WHITE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2031)) %>%
    select(CODE, BOROUGH, YEAR, VAL_02_RACE_WHITE_PREV, VAL_02_RACE_WHITE, VAR_02_RACE_WHITE, IND_02_RACE_WHITE)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_02_RACE_WHITE)) %>%
    mutate(across(c(VAL_02_RACE_WHITE_PREV, VAL_02_RACE_WHITE), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_02_RACE_WHITE, IND_02_RACE_WHITE), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_02_RACE_WHITE_PREV, VAL_02_RACE_WHITE, VAR_02_RACE_WHITE, IND_02_RACE_WHITE), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 03) Muestra el top3 del indicador 03
################################################################################
muestra_top_3_ind_03 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_03_WEEK_EARNINGS_PREV = lag(VAL_03_WEEK_EARNINGS)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2031), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_03_WEEK_EARNINGS)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_03_WEEK_EARNINGS_PREV, VAL_03_WEEK_EARNINGS, VAR_03_WEEK_EARNINGS, IND_03_WEEK_EARNINGS) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2031)) %>%
    select(CODE, BOROUGH, YEAR, VAL_03_WEEK_EARNINGS_PREV, VAL_03_WEEK_EARNINGS, VAR_03_WEEK_EARNINGS, IND_03_WEEK_EARNINGS)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_03_WEEK_EARNINGS)) %>%
    mutate(across(c(VAL_03_WEEK_EARNINGS_PREV, VAL_03_WEEK_EARNINGS), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_03_WEEK_EARNINGS, IND_03_WEEK_EARNINGS), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_03_WEEK_EARNINGS_PREV, VAL_03_WEEK_EARNINGS, VAR_03_WEEK_EARNINGS, IND_03_WEEK_EARNINGS), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 04) Muestra el top3 del indicador 04
################################################################################
muestra_top_3_ind_04 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_04_PERCENT_NVQ4_PREV = lag(VAL_04_PERCENT_NVQ4)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2031), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_04_PERCENT_NVQ4)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_04_PERCENT_NVQ4_PREV, VAL_04_PERCENT_NVQ4, VAR_04_PERCENT_NVQ4, IND_04_PERCENT_NVQ4) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2031)) %>%
    select(CODE, BOROUGH, YEAR, VAL_04_PERCENT_NVQ4_PREV, VAL_04_PERCENT_NVQ4, VAR_04_PERCENT_NVQ4, IND_04_PERCENT_NVQ4)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_04_PERCENT_NVQ4)) %>%
    mutate(across(c(VAL_04_PERCENT_NVQ4_PREV, VAL_04_PERCENT_NVQ4), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_04_PERCENT_NVQ4, IND_04_PERCENT_NVQ4), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_04_PERCENT_NVQ4_PREV, VAL_04_PERCENT_NVQ4, VAR_04_PERCENT_NVQ4, IND_04_PERCENT_NVQ4), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 05) Muestra el top3 del indicador 05
################################################################################
muestra_top_3_ind_05 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_05_CAR_TRAFFIC_PREV = lag(VAL_05_CAR_TRAFFIC)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2031), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, IND_05_CAR_TRAFFIC) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_05_CAR_TRAFFIC_PREV, VAL_05_CAR_TRAFFIC, VAR_05_CAR_TRAFFIC, IND_05_CAR_TRAFFIC) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2031)) %>%
    select(CODE, BOROUGH, YEAR, VAL_05_CAR_TRAFFIC_PREV, VAL_05_CAR_TRAFFIC, VAR_05_CAR_TRAFFIC, IND_05_CAR_TRAFFIC)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, IND_05_CAR_TRAFFIC) %>%
    mutate(across(c(VAL_05_CAR_TRAFFIC_PREV, VAL_05_CAR_TRAFFIC), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_05_CAR_TRAFFIC, IND_05_CAR_TRAFFIC), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_05_CAR_TRAFFIC_PREV, VAL_05_CAR_TRAFFIC, VAR_05_CAR_TRAFFIC, IND_05_CAR_TRAFFIC), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 06) Muestra el top3 del indicador 06
################################################################################
muestra_top_3_ind_06 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_06_EXP_LIFE_PREV = lag(VAL_06_EXP_LIFE)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2031), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_06_EXP_LIFE)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_06_EXP_LIFE_PREV, VAL_06_EXP_LIFE, VAR_06_EXP_LIFE, IND_06_EXP_LIFE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2031)) %>%
    select(CODE, BOROUGH, YEAR, VAL_06_EXP_LIFE_PREV, VAL_06_EXP_LIFE, VAR_06_EXP_LIFE, IND_06_EXP_LIFE)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_06_EXP_LIFE)) %>%
    mutate(across(c(VAL_06_EXP_LIFE_PREV, VAL_06_EXP_LIFE), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_06_EXP_LIFE, IND_06_EXP_LIFE), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_06_EXP_LIFE_PREV, VAL_06_EXP_LIFE, VAR_06_EXP_LIFE, IND_06_EXP_LIFE), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################

df_barrios <- carga_lista_barrios()

# Carga datos preparados
df_datos <- carga_indicadores()

#df_tmp <- muestra_top_3_ind_01(df_datos)
#df_tmp <- muestra_top_3_ind_02(df_datos)
#df_tmp <- muestra_top_3_ind_03(df_datos)
#df_tmp <- muestra_top_3_ind_04(df_datos)
#df_tmp <- muestra_top_3_ind_05(df_datos)
df_tmp <- muestra_top_3_ind_06(df_datos)