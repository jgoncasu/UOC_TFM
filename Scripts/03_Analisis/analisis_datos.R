# Librerías
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')

# Constantes
PATH_FICHEROS_BARRIOS <- 'DATOS/01_Fuentes/'
PATH_FICHEROS_ENTRADA <- 'DATOS/03_Preparados/'
PATH_FICHEROS_SALIDA <- 'DATOS/04_Analisis/'

################################################################################
# 00) Carga los barrios
################################################################################
carga_lista_barrios <- function() {
  ruta_fichero <- '00_Barrios/barrios_londres.csv'
  df = read_csv(paste(PATH_FICHEROS_BARRIOS, ruta_fichero, sep=""), col_types = list(col_character(), col_character(), col_character()))
  return(df)
}

################################################################################
# 01) Carga los datos de los indicadores
################################################################################
carga_indicadores <- function() {
  ruta_fichero <- 'DAT_Indicadores_Londres.csv'
  df = read_csv(paste(PATH_FICHEROS_ENTRADA, ruta_fichero, sep=""), 
                col_types = list(col_character(), col_character(), col_integer(), 
                                 col_double(), col_double(), col_double(),
                                 col_integer(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_integer(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_integer(), col_double(), col_double(),
                                 col_double(), col_double(), col_double(),
                                 col_double(), col_double(), col_double()))
  return(df)
}

################################################################################
# 02) Calcula clústers
################################################################################
calcula_clusters_VAR <- function(df) {
  df <- scale(df[4:13])
  result <- fviz_nbclust(df, kmeans, method = "wss")
  print(result)
  gap_stat <- clusGap(df,
                      FUN = kmeans,
                      nstart = 35,
                      K.max = 10,
                      B = 50)
  result2 <- fviz_gap_stat(gap_stat)
  print(result2)
  
  km <- kmeans(df, centers = 2, nstart = 35)
  print(km)
}

calcula_clusters_IND <- function(df) {
#  df <- scale(df[4:13])
  df <- scale(df[4:11])
  result <- fviz_nbclust(df, kmeans, method = "wss")
  print(result)
  gap_stat <- clusGap(df,
                      FUN = kmeans,
                      nstart = 35,
                      K.max = 10,
                      B = 50)
  result2 <- fviz_gap_stat(gap_stat)
  print(result2)

  km <- kmeans(df, centers = 2, nstart = 35)
  print(km)
}

################################################################################
# 03) Guardar información clústers
################################################################################
guarda_info_clusters <- function() {
  
}


################################################################################
# Cuerpo principal del programa
################################################################################

print("***** PASO 0: CARGANDO BARRIOS DE LONDRES *****")
df_barrios <- carga_lista_barrios()

# Carga datos preparados
df_datos <- carga_indicadores()

# Selecciona columnas VAR
df_datos_VAR <- df_datos %>% select(-starts_with("VAL_"), -starts_with("IND_"))

# Selecciona columnas IND
df_datos_IND <- df_datos %>% select(-starts_with("VAL_"), -starts_with("VAR_"))

set.seed(7)

# Calcula los clusters en base a las columnas VAR
for (i in c(2011, 2021, 2031)) {
  df_tmp <- df_datos_VAR %>% filter(YEAR == i, !BOROUGH %in% c("London"))
  calcula_clusters_VAR(df_tmp)
}

# Calcula los clusters en base a las columnas IND
for (i in c(2011, 2021, 2031)) {
  #df_tmp <- df_datos_IND %>% filter(YEAR == i, !BOROUGH %in% c("London", "City of London")) %>% select(CODE, BOROUGH, YEAR, IND_01_AGE, IND_02_RACE_WHITE, IND_03_WEEK_EARNINGS, IND_05_CAR_TRAFFIC, IND_06_EXP_LIFE, IND_07_CRIMES, IND_08_SERVICES, IND_09_HOUSE_PRICE)
  #calcula_clusters_IND(df_tmp)
}

# Para cada año, calcular clústers
  # Calcular K
  # Graficar
  # Mostrar clústers
  # Mostrar valores medios

# Selecciona columnas IND

# Para cada año, calcular clústers
  # Calcular K
  # Graficar
  # Mostrar clústers
  # Mostrar valores medios

