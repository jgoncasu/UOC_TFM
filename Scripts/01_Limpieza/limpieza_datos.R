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
  ruta_fichero <- '00_Barrios/barrios_londres.csv'
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
  
  # Se predice el valor de la población para los años 2021 y 2031  
  anyos_pred <- 2021:2031
  edades <- 0:90
  barrios <- unique(df$Borough)
  for (barrio in barrios) {
    codigo_barrio <- df %>% filter(Borough == barrio) %>% slice_head(n = 1) %>% pull(Code)
    predicciones <- list()
    for (edad in edades) {
      serie_hombres <- ts(df %>% filter(Borough == barrio) %>% pull(paste0("M_", edad)), start = 1999, end = 2020, frequency = 1)
      serie_mujeres <- ts(df %>% filter(Borough == barrio) %>% pull(paste0("F_", edad)), start = 1999, end = 2020, frequency = 1)
      modelo_hombres <- ets(serie_hombres)
      modelo_mujeres <- ets(serie_mujeres)
      pred_hombres <- forecast(modelo_hombres, h = length(anyos_pred))
      pred_mujeres <- forecast(modelo_mujeres, h = length(anyos_pred))
      predicciones[[paste0("M_", edad)]] <- round(pred_hombres$mean, 0)
      predicciones[[paste0("F_", edad)]] <- round(pred_mujeres$mean, 0)
    }
    predicciones_df <- as.data.frame(predicciones)
    predicciones_df$Year <- anyos_pred
    predicciones_df$Borough <- barrio
    predicciones_df$Code <- codigo_barrio
    
    df <- bind_rows(df, predicciones_df)
  }

  # Calcula la media de edad por año entre hombre y mujeres
  total_hombres <- rowSums(df %>% select(starts_with("M_")), na.rm = TRUE)
  total_mujeres <- rowSums(df %>% select(starts_with("F_")), na.rm = TRUE)
  total_poblacion <- total_hombres + total_mujeres
  #df["Mean_Age_Men"] <- round(rowSums(df %>% select(starts_with("M_")) * edades, na.rm = TRUE) / total_hombres, 2)
  #df["Mean_Age_Women"] <- round(rowSums(df %>% select(starts_with("F_")) * edades, na.rm = TRUE) / total_mujeres, 2)
  df["Avg_Age"] <- round(rowSums((df %>% select(starts_with("M_")) + df %>% select(starts_with("F_"))) * edades, na.rm = TRUE) / total_poblacion, 2)
  
  # Elimina las columnas de datos agrupados por edad
  df <- df %>% select(-starts_with("M_"))
  df <- df %>% select(-starts_with("F_"))

  # Transforma los años en columnas y los datos de edad a valores de las filas
  df <- df %>% 
    mutate(Year = paste0("Year_", Year)) %>%
    pivot_wider(
      names_from = Year,
      values_from = Avg_Age
    )
  
  # Ordena los datos por barrio
  df <- df %>% arrange(Code)
  
  return(df)
}

validar_indicador_01_edad <- function(df) {
  if (any(is.na(df)))
    return(FALSE)
  return(TRUE)
}

guardar_indicador_01_edad <- function(df) {
  ruta_fichero <- 'STG_01_indicador_edad.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# 02) Carga y limpieza de datos de los indicadores demográficos (raciales)
################################################################################

carga_indicador_02_raza <- function() {
  ruta_fichero <- '01_Demograficos/ethnic-groups-by-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df)
}

limpieza_indicador_02_raza <- function(df) {
  # Sustituye valores "-" por NA
  #df[df== "-"]<-NA
  return(df)
}

imputar_valores_indicador_02_raza <- function(df) {
  return(df)
}

validar_indicador_02_raza <- function(df) {
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
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df) 
}

limpieza_indicador_03_empleo <- function(df) {
  return(df)
}

imputar_valores_indicador_03_empleo <- function(df) {
  return(df)
}

validar_indicador_03_empleo <- function(df) {
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
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df) 
}

limpieza_indicador_04_estudios <- function(df) {
  return(df)
}

imputar_valores_indicador_04_estudios <- function(df) {
  return(df)
}

validar_indicador_04_estudios <- function(df) {
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
  ruta_fichero <- '04_Medio_Ambiente/traffic-flow-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df)
}

limpieza_indicador_05_trafico <- function(df) {
  return(df)
}

imputar_valores_indicador_05_trafico <- function(df) {
  return(df)
}

validar_indicador_05_trafico <- function(df) {
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
  ruta_fichero <- '05_Sanidad/life-expectancy-birth-over65-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df) 
}

limpieza_indicador_06_esperanza_vida <- function(df) {
  return(df)
}

imputar_valores_indicador_06_esperanza_vida <- function(df) {
  return(df)
}

validar_indicador_06_esperanza_vida <- function(df) {
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
  ruta_fichero <- '06_Seguridad/MPS Borough Level Crime (Historical).csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df) 
}

limpieza_indicador_07_delitos <- function(df) {
  return(df)
}

imputar_valores_indicador_07_delitos <- function(df) {
  return(df)
}

validar_indicador_07_delitos <- function(df) {
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
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df) 
}

limpieza_indicador_08_servicios <- function(df) {
  return(df)
}

imputar_valores_indicador_08_servicios <- function(df) {
  return(df)
}

validar_indicador_08_servicios <- function(df) {
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
  ruta_fichero <- '08_Vivienda/land-registry-house-prices-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df)  
}

limpieza_indicador_09_vivienda_precio <- function(df) {
  return(df)
}

imputar_valores_indicador_09_vivienda_precio <- function(df) {
  return(df)
}

validar_indicador_09_vivienda_precio <- function(df) {
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
  ruta_fichero <- '08_Vivienda/social-landlord-rents-borough.xls'
  df = read_excel(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""))
  return(df)  
}

limpieza_indicador_10_vivienda_alquiler <- function(df) {
  return(df)
}

imputar_valores_indicador_10_vivienda_alquiler <- function(df) {
  return(df)
}

validar_indicador_10_vivienda_alquiler <- function(df) {
  return(TRUE)
}

guardar_indicador_10_vivienda_alquiler <- function(df) {
  ruta_fichero <- 'STG_10_indicador_vivienda_alquiler.csv'
  write.csv(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# Cuerpo principal del programa
################################################################################

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
#df_raza = imputar_valores_indicador_02_raza(df_raza)
#if (validar_indicador_02_raza(df_raza)) guardar_indicador_02_raza(df_raza) else print("ERROR")

# Indicador 03 - Empleo
#df_empleo = carga_indicador_03_empleo()
#df_empleo = limpieza_indicador_03_empleo(df_empleo)
#df_empleo = imputar_valores_indicador_03_empleo(df_empleo)
#if (validar_indicador_03_empleo(df_empleo))
#  guardar_indicador_03_empleo(df_empleo)

# Indicador 04 - Estudios
#df_estudios = carga_indicador_04_estudios()
#df_estudios = limpieza_indicador_04_estudios(df_estudios)
#df_estudios = imputar_valores_indicador_04_estudios(df_estudios)
#if (validar_indicador_04_estudios(df_estudios))
#  guardar_indicador_04_estudios(df_estudios)

# Indicador 05 - Tráfico
#df_trafico = carga_indicador_05_trafico()
#df_trafico = limpieza_indicador_05_trafico(df_trafico)
#df_trafico = imputar_valores_indicador_05_trafico(df_trafico)
#if (validar_indicador_05_trafico(df_trafico))
#  guardar_indicador_05_trafico(df_trafico)

# Indicador 06 - Esperanza de vida
#df_esperanza_vida = carga_indicador_06_esperanza_vida()
#df_esperanza_vida = limpieza_indicador_06_esperanza_vida(df_esperanza_vida)
#df_esperanza_vida = imputar_valores_indicador_06_esperanza_vida(df_esperanza_vida)
#if (validar_indicador_06_esperanza_vida(df_esperanza_vida))
#  guardar_indicador_06_esperanza_vida(df_esperanza_vida)

# Indicador 07 - Delitos
#df_delitos = carga_indicador_07_delitos()
#df_delitos = limpieza_indicador_07_delitos(df_delitos)
#df_delitos = imputar_valores_indicador_07_delitos(df_delitos)
#if (validar_indicador_07_delitos(df_delitos))
#  guardar_indicador_07_delitos(df_delitos)

# Indicador 08 - Servicios
#df_servicios = carga_indicador_08_servicios()
#df_servicios = limpieza_indicador_08_servicios(df_servicios)
#df_servicios = imputar_valores_indicador_08_servicios(df_servicios)
#if (validar_indicador_08_servicios(df_servicios))
#  guardar_indicador_08_servicios(df_servicios)

# Indicador 09 - Precio vivienda
#df_vivienda_precio = carga_indicador_09_vivienda_precio()
#df_vivienda_precio = limpieza_indicador_09_vivienda_precio(df_vivienda_precio)
#df_vivienda_precio = imputar_valores_indicador_09_vivienda_precio(df_vivienda_precio)
#if (validar_indicador_09_vivienda_precio(df_vivienda_precio))
#  guardar_indicador_09_vivienda_precio(df_vivienda_precio)

# Indicador 10 - Precio alquiler
#df_vivienda_alquiler = carga_indicador_10_vivienda_alquiler()
#df_vivienda_alquiler = limpieza_indicador_10_vivienda_alquiler(df_vivienda_alquiler)
#df_vivienda_alquiler = imputar_valores_indicador_10_vivienda_alquiler(df_vivienda_alquiler)
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
