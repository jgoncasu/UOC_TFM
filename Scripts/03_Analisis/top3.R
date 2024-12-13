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
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
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
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, IND_01_AGE) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_01_AGE_PREV, VAL_01_AGE, VAR_01_AGE, IND_01_AGE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
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
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_02_RACE_WHITE)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_02_RACE_WHITE_PREV, VAL_02_RACE_WHITE, VAR_02_RACE_WHITE, IND_02_RACE_WHITE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
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
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_03_WEEK_EARNINGS)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_03_WEEK_EARNINGS_PREV, VAL_03_WEEK_EARNINGS, VAR_03_WEEK_EARNINGS, IND_03_WEEK_EARNINGS) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
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
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_04_PERCENT_NVQ4)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_04_PERCENT_NVQ4_PREV, VAL_04_PERCENT_NVQ4, VAR_04_PERCENT_NVQ4, IND_04_PERCENT_NVQ4) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
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
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, IND_05_CAR_TRAFFIC) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_05_CAR_TRAFFIC_PREV, VAL_05_CAR_TRAFFIC, VAR_05_CAR_TRAFFIC, IND_05_CAR_TRAFFIC) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
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
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_06_EXP_LIFE)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_06_EXP_LIFE_PREV, VAL_06_EXP_LIFE, VAR_06_EXP_LIFE, IND_06_EXP_LIFE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
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
# 07) Muestra el top3 del indicador 07
################################################################################
muestra_top_3_ind_07 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_07_CRIMES_PREV = lag(VAL_07_CRIMES)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, IND_07_CRIMES) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_07_CRIMES_PREV, VAL_07_CRIMES, VAR_07_CRIMES, IND_07_CRIMES) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
    select(CODE, BOROUGH, YEAR, VAL_07_CRIMES_PREV, VAL_07_CRIMES, VAR_07_CRIMES, IND_07_CRIMES)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, IND_07_CRIMES) %>%
    mutate(across(c(VAL_07_CRIMES_PREV, VAL_07_CRIMES), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_07_CRIMES, IND_07_CRIMES), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_07_CRIMES_PREV, VAL_07_CRIMES, VAR_07_CRIMES, IND_07_CRIMES), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 08) Muestra el top3 del indicador 08
################################################################################
muestra_top_3_ind_08 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_08_SERVICES_PREV = lag(VAL_08_SERVICES)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_08_SERVICES)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_08_SERVICES_PREV, VAL_08_SERVICES, VAR_08_SERVICES, IND_08_SERVICES) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
    select(CODE, BOROUGH, YEAR, VAL_08_SERVICES_PREV, VAL_08_SERVICES, VAR_08_SERVICES, IND_08_SERVICES)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_08_SERVICES)) %>%
    mutate(across(c(VAL_08_SERVICES_PREV, VAL_08_SERVICES), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_08_SERVICES, IND_08_SERVICES), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_08_SERVICES_PREV, VAL_08_SERVICES, VAR_08_SERVICES, IND_08_SERVICES), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 09) Muestra el top3 del indicador 09
################################################################################
muestra_top_3_ind_09 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_09_HOUSE_PRICE_PREV = lag(VAL_09_HOUSE_PRICE)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_09_HOUSE_PRICE)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_09_HOUSE_PRICE_PREV, VAL_09_HOUSE_PRICE, VAR_09_HOUSE_PRICE, IND_09_HOUSE_PRICE) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
    select(CODE, BOROUGH, YEAR, VAL_09_HOUSE_PRICE_PREV, VAL_09_HOUSE_PRICE, VAR_09_HOUSE_PRICE, IND_09_HOUSE_PRICE)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_09_HOUSE_PRICE)) %>%
    mutate(across(c(VAL_09_HOUSE_PRICE_PREV, VAL_09_HOUSE_PRICE), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_09_HOUSE_PRICE, IND_09_HOUSE_PRICE), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_09_HOUSE_PRICE_PREV, VAL_09_HOUSE_PRICE, VAR_09_HOUSE_PRICE, IND_09_HOUSE_PRICE), 1, function(x) paste(x, collapse = " & "))
  cat(paste(rows_with_ampersand, collapse = "\n"))
  return(final_df)
}

################################################################################
# 10) Muestra el top3 del indicador 10
################################################################################
muestra_top_3_ind_10 <- function(df) {
  df <- df %>%
    group_by(CODE, BOROUGH) %>%
    arrange(YEAR) %>%
    mutate(VAL_10_HOUSE_RENT_PREV = lag(VAL_10_HOUSE_RENT)) %>% 
    ungroup()
  
  # Filtrar los top 3 de cada año
  top3_df <- df %>%
    filter(YEAR %in% c(2011, 2021, 2025), BOROUGH != "London") %>%
    group_by(YEAR) %>%
    arrange(YEAR, desc(IND_10_HOUSE_RENT)) %>%
    slice_head(n = 3) %>%
    select(CODE, BOROUGH, YEAR, VAL_10_HOUSE_RENT_PREV, VAL_10_HOUSE_RENT, VAR_10_HOUSE_RENT, IND_10_HOUSE_RENT) %>%
    ungroup()
  london_df <- df %>%
    filter(BOROUGH == "London", YEAR %in% c(2011, 2021, 2025)) %>%
    select(CODE, BOROUGH, YEAR, VAL_10_HOUSE_RENT_PREV, VAL_10_HOUSE_RENT, VAR_10_HOUSE_RENT, IND_10_HOUSE_RENT)
  final_df <- bind_rows(top3_df, london_df) %>%
    arrange(YEAR, desc(IND_10_HOUSE_RENT)) %>%
    mutate(across(c(VAL_10_HOUSE_RENT_PREV, VAL_10_HOUSE_RENT), 
                  ~ sprintf("%.2f", .x))) %>%
    mutate(across(c(VAR_10_HOUSE_RENT, IND_10_HOUSE_RENT), 
                  ~ sprintf("%.3f", .x)))
  rows_with_ampersand <- apply(final_df %>% select(YEAR, BOROUGH, VAL_10_HOUSE_RENT_PREV, VAL_10_HOUSE_RENT, VAR_10_HOUSE_RENT, IND_10_HOUSE_RENT), 1, function(x) paste(x, collapse = " & "))
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
#df_tmp <- muestra_top_3_ind_06(df_datos)
#df_tmp <- muestra_top_3_ind_07(df_datos)
#df_tmp <- muestra_top_3_ind_08(df_datos)
#df_tmp <- muestra_top_3_ind_09(df_datos)
df_tmp <- muestra_top_3_ind_10(df_datos)