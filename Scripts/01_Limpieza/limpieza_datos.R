# Librerías
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')

################################################################################
# 00) Carga y limpieza de datos de los datos de los barrios
################################################################################

carga_lista_barrios <- function() {
  
}


################################################################################
# 01) Carga y limpieza de datos de los indicadores demográficos (edad)
################################################################################

carga_indicador_01_edad <- function() {
  
}

limpieza_indicador_01_edad <- function() {
  
}

imputar_valores_indicador_01_edad <- function() {
  
}

validar_indicador_01_edad <- function() {
  return(TRUE)
}

guardar_indicador_01_edad <- function() {
  
}


################################################################################
# 02) Carga y limpieza de datos de los indicadores demográficos (raciales)
################################################################################

carga_indicador_02_raza <- function() {
  
}

limpieza_indicador_02_raza <- function() {
  
}

imputar_valores_indicador_02_raza <- function() {
  
}

validar_indicador_02_raza <- function() {
  return(TRUE)
}

guardar_indicador_02_raza <- function() {
  
}


################################################################################
# 03) Carga y limpieza de datos de los indicadores de empleo
################################################################################

carga_indicador_03_empleo <- function() {
  
}

limpieza_indicador_03_empleo <- function() {
  
}

imputar_valores_indicador_03_empleo <- function() {
  
}

validar_indicador_03_empleo <- function() {
  return(TRUE)
}

guardar_indicador_03_empleo <- function() {
  
}



################################################################################
# 04) Carga y limpieza de datos de los indicadores educativos
################################################################################

carga_indicador_04_estudios <- function() {
  
}

limpieza_indicador_04_estudios <- function() {
  
}

imputar_valores_indicador_04_estudios <- function() {
  
}

validar_indicador_04_estudios <- function() {
  return(TRUE)
}

guardar_indicador_04_estudios <- function() {
  
}


################################################################################
# 05) Carga y limpieza de datos de los indicadores medio ambientales
################################################################################

carga_indicador_05_trafico <- function() {
  
}

limpieza_indicador_05_trafico <- function() {
  
}

imputar_valores_indicador_05_trafico <- function() {
  
}

validar_indicador_05_trafico <- function() {
  return(TRUE)
}

guardar_indicador_05_trafico <- function() {
  
}


################################################################################
# 06) Carga y limpieza de datos de los indicadores sanitarios
################################################################################

carga_indicador_06_esperanza_vida <- function() {
  
}

limpieza_indicador_06_esperanza_vida <- function() {
  
}

imputar_valores_indicador_06_esperanza_vida <- function() {
  
}

validar_indicador_06_esperanza_vida <- function() {
  return(TRUE)
}

guardar_indicador_06_esperanza_vida <- function() {
  
}


################################################################################
# 07) Carga y limpieza de datos de los indicadores sobre seguridad
################################################################################

carga_indicador_07_delitos <- function() {
  
}

limpieza_indicador_07_delitos <- function() {
  
}

imputar_valores_indicador_07_delitos <- function() {
  
}

validar_indicador_07_delitos <- function() {
  return(TRUE)
}

guardar_indicador_07_delitos <- function() {
  
}


################################################################################
# 08) Carga y limpieza de datos de los indicadores sobre servicios
################################################################################

carga_indicador_08_servicios <- function() {
  
}

limpieza_indicador_08_servicios <- function() {
  
}

imputar_valores_indicador_08_servicios <- function() {
  
}

validar_indicador_08_servicios <- function() {
  return(TRUE)
}

guardar_indicador_08_servicios <- function() {
  
}


################################################################################
# 09) Carga y limpieza de datos de los indicadores sobre vivienda (precio)
################################################################################

carga_indicador_09_vivienda_precio <- function() {
  
}

limpieza_indicador_09_vivienda_precio <- function() {
  
}

imputar_valores_indicador_09_vivienda_precio <- function() {
  
}

validar_indicador_09_vivienda_precio <- function() {
  return(TRUE)
}

guardar_indicador_09_vivienda_precio <- function() {
  
}


################################################################################
# 10) Carga y limpieza de datos de los indicadores sobre vivienda (alquiler)
################################################################################

carga_indicador_10_vivienda_alquiler <- function() {
  
}

limpieza_indicador_10_vivienda_alquiler <- function() {
  
}

imputar_valores_indicador_10_vivienda_alquiler <- function() {
  
}

validar_indicador_10_vivienda_alquiler <- function() {
  return(TRUE)
}

guardar_indicador_10_vivienda_alquiler <- function() {
  
}


################################################################################
# Cuerpo principal del programa
################################################################################
main <- function() {
  
  # Barrios de Londres
  df_barrios <- carga_lista_barrios()

  # Indicador 01 - Edad
  df_edad = carga_indicador_01_edad()
  df_edad = limpieza_indicador_01_edad()
  df_edad = imputar_valores_indicador_01_edad()
  if (validar_indicador_01_edad())
    guardar_indicador_01_edad()

  # Indicador 02 - Raza
  df_raza = carga_indicador_02_raza()
  df_raza = limpieza_indicador_02_raza()
  df_raza = imputar_valores_indicador_02_raza()
  if (validar_indicador_02_raza())
    guardar_indicador_02_raza()
  
  # Indicador 03 - Empleo
  df_empleo = carga_indicador_03_empleo()
  df_empleo = limpieza_indicador_03_empleo()
  df_empleo = imputar_valores_indicador_03_empleo()
  if (validar_indicador_03_empleo())
    guardar_indicador_03_empleo()

  # Indicador 04 - Estudios
  df_estudios = carga_indicador_04_estudios()
  df_estudios = limpieza_indicador_04_estudios()
  df_estudios = imputar_valores_indicador_04_estudios()
  if (validar_indicador_04_estudios())
    guardar_indicador_04_estudios()
  
  # Indicador 05 - Tráfico
  df_trafico = carga_indicador_05_trafico()
  df_trafico = limpieza_indicador_05_trafico()
  df_trafico = imputar_valores_indicador_05_trafico()
  if (validar_indicador_05_trafico())
    guardar_indicador_05_trafico()
  
  # Indicador 06 - Esperanza de vida
  df_esperanza_vida = carga_indicador_06_esperanza_vida()
  df_esperanza_vida = limpieza_indicador_06_esperanza_vida()
  df_esperanza_vida = imputar_valores_indicador_06_esperanza_vida()
  if (validar_indicador_06_esperanza_vida())
    guardar_indicador_06_esperanza_vida()  

  # Indicador 07 - Delitos
  df_delitos = carga_indicador_07_delitos()
  df_delitos = limpieza_indicador_07_delitos()
  df_delitos = imputar_valores_indicador_07_delitos()
  if (validar_indicador_07_delitos())
    guardar_indicador_07_delitos()  
 
  # Indicador 08 - Servicios
  df_servicios = carga_indicador_08_servicios()
  df_servicios = limpieza_indicador_08_servicios()
  df_servicios = imputar_valores_indicador_08_servicios()
  if (validar_indicador_08_servicios())
    guardar_indicador_08_servicios()
  
  # Indicador 09 - Precio vivienda
  df_vivienda_precio = carga_indicador_09_vivienda_precio()
  df_vivienda_precio = limpieza_indicador_09_vivienda_precio()
  df_vivienda_precio = imputar_valores_indicador_09_vivienda_precio()
  if (validar_indicador_09_vivienda_precio())
    guardar_indicador_09_vivienda_precio()    
  
  # Indicador 10 - Precio alquiler
  df_vivienda_alquiler = carga_indicador_10_vivienda_alquiler()
  df_vivienda_alquiler = limpieza_indicador_10_vivienda_alquiler()
  df_vivienda_alquiler = imputar_valores_indicador_10_vivienda_alquiler()
  if (validar_indicador_10_vivienda_alquiler())
    guardar_indicador_10_vivienda_alquiler()  
}

main()

# Carga ficheros
# Elimina columnas
# Filtra filas (solo barrios y datos de la ciudad de Londres)
# Revisión separadores decimales
# Revisión tipo de los datos
# Sustituir valores no válidos
# Imputar valores nulos
# Revisar todos los barrios tienen datos de 2001, 2011, 2021 y 2031 (datos por censo)
