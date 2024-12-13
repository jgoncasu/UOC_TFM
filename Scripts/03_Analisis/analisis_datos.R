# Librerías
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('cluster')) install.packages('cluster'); library('cluster')
#if (!require('ggcorrplot')) install.packages('ggcorrplot'); library('ggcorrplot')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
library(RColorBrewer)

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
                                 col_double(), col_double(), col_double()))
  return(df)
}

################################################################################
# 02) Calcula clústers
################################################################################
calcula_clusters_kmeans <- function(df) {
  #df <- scale(df[4:13])
  df <- scale(df)
  result <- fviz_nbclust(df, kmeans, method = "wss")
  print(result)
  gap_stat <- clusGap(df,
                      FUN = kmeans,
                      nstart = 33,
                      K.max = 10,
                      B = 50)
  result2 <- fviz_gap_stat(gap_stat)
  print(result2)

  km <- kmeans(df, centers = 4, nstart = 33)
  print(km)
  
  print(fviz_cluster(km, data = df))
}

calcula_clusters_kmedoids <- function(df) {
  df <- scale(df)
  result <- fviz_nbclust(df, pam, method = "wss")
  print(result)
  gap_stat <- clusGap(df,
                      FUN = pam,
                      nstart = 33,
                      K.max = 10,
                      B = 50)
  result2 <- fviz_gap_stat(gap_stat)
  print(result2)
  
  km <- pam(df, k = 4)
  print(km)
  
  print(fviz_cluster(km, data = df))
}

calcula_clusters_hcluster <- function(df) {
  df <- scale(df)
  
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average", "single", "complete", "ward")
  
  print(sapply(m, ac, df = df))
  
  clust <- agnes(df, method = "ward")
  print(clust)
  
  dendogram <- pltree(clust, cex = 0.6, hang = -1, main = "Dendogram")
  print(dendogram)
  
  gap_stat <- clusGap(df, FUN = hcut, nstart = 33, K.max = 10, B = 50)
  result <- fviz_gap_stat(gap_stat)
  print(result)
}

calcula_pca <- function(df) {
  #df <- scale(df)
  
  results <- prcomp(df, scale = TRUE)
  results$rotation <- -1*results$rotation
  
  print(results$rotation)
  
  results$x <- -1*results$x
  #print(results$x)
  
  print(biplot(results, scale = 0))
  
  var_explained = results$sdev^2 / sum(results$sdev^2)
  print(var_explained)
  
  p <- qplot(c(1:10), var_explained) + 
    geom_line() + 
    xlab("Principal Component") + 
    ylab("Variance Explained") +
    ggtitle("Scree Plot") +
    ylim(0, 1)
  print(p)
}

calcula_correlacion <- function(df, titulo) {
  corr_matrix <- round(cor(df), 2)
  etiquetas <- c("01 - Edad", "02 - Raza", "03 - Salario", "04 - Estudios", "05 - Tráfico", "06 - Esp.Vida", "07 - Delitos", "08 - Servicios", "09 - Pr.Vivienda", "10 - Pr.Alquiler")
  colnames(corr_matrix) <- etiquetas
  rownames(corr_matrix) <- etiquetas
  p <- corrplot(corr_matrix,
                method = 'circle',
                tl.col = 'black',
                type = 'lower',
                addCoef.col ='black',
                col = brewer.pal(n = 10, name = 'RdBu'),
                title = titulo,
                mar=c(0,0,1,0)
  )
  print(p)
}

ac <- function(x, df) {
  round(agnes(df, method = x)$ac, 2)
}

calcula_clusters_jerarquico <- function(df, titulo_dendograma, fichero_salida) {
  # Se escalan los valores para que tengan una media = 0 y una desviación estándar = 1
  df_scaled <- scale(df[2:11])
  #df <- scale(df)

  # Calcula el método de cálculo de la distancia entre clusters  
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average", "single", "complete", "ward")
  print("Algoritmo para el cálculo de la distancia entre clusters")
  print(sapply(m, ac, df = df_scaled))

  # Se aplica el clustering jerárquico  
  clust <- agnes(df_scaled, method = "ward")
  print("RESULTADO ALGORITMO")
  print(clust)
  
  dendograma <- pltree(clust, cex = 1, hang = -1, main = titulo_dendograma, labels = as.list(df$BOROUGH), ylab=NA, xlab=NA)
  #dendograma <- pltree(clust, cex = 0.6, hang = -1, main = titulo_dendograma)
  print(dendograma)
  
  gap_stat <- clusGap(df_scaled, FUN = hcut, nstart = 1, K.max = 10, B = 100)
  print(fviz_gap_stat(gap_stat))
  
  print("RESULTADO CLUSTERING")
  # Matriz de distancia
  d <- dist(df_scaled, method = "euclidean")
  # Clustering jerárquico
  final_clust <- hclust(d, method = "ward.D2")
  # Corta el dendograma en 4 grupos
  groups <- cutree(final_clust, k = 14)
  print(groups)
  # Número de observaciones en cada grupo
  table(groups)
  #print(table(groups))
#  # Crea los grupos
  final_data <- cbind(df_scaled, cluster = groups)
  #print(final_data)
#  head(final_data)
#  
#  print("A1")
  final_data <- as.data.frame(final_data)
  print(typeof(final_data))
  df_summary <- final_data %>%
    group_by(cluster) %>%
    summarise(across(starts_with("IND_"), mean, na.rm = TRUE))
  view(df_summary)
  #print(aggregate(final_data, by=list(cluster=final_data$cluster), mean))
#  print("A2")
  
  
#  return(result)
}


################################################################################
# 03) Guardar información clústers
################################################################################
guarda_info_clusters <- function() {
  
}


################################################################################
# Cuerpo principal del programa
################################################################################

set.seed(2759)

print("***** PASO 0: CARGANDO BARRIOS DE LONDRES *****")
df_barrios <- carga_lista_barrios()

# Carga datos preparados
df_datos <- carga_indicadores()

# Selecciona columnas IND
df_datos <- df_datos %>% select(-starts_with("VAL_"), -starts_with("VAR_")) #%>% filter(!BOROUGH %in% c("London"))

df_correlacion <- df_datos %>% select(-c(CODE, BOROUGH, YEAR))

# Calcula correlación
calcula_correlacion(df_correlacion, 'Matriz de correlación')

# Cálculo de clústers usando Hierarchical clustering

# Período 2001-2011
#df_analisis_2011 <- df_datos %>% filter(YEAR == 2011 & BOROUGH != 'London') %>% select(-c(CODE, YEAR))
#result_clusters_2011 <- calcula_clusters_jerarquico(df_analisis_2011, 'Dendograma 2001-2011', 'RES_Clusters_2011.csv')
#print("RESULTADO 2001-2011")
#print(result_clusters_2011)

# Período 2011-2021
#df_analisis_2021 <- df_datos %>% filter(YEAR == 2021 & BOROUGH != 'London') %>% select(-c(CODE, YEAR))
#result_clusters_2021 <- calcula_clusters_jerarquico(df_analisis_2021, 'Dendograma 2011-2021', 'RES_Clusters_2021.csv')
#print("RESULTADO 2011-2021")

# Período 2021-2025
df_analisis_2025 <- df_datos %>% filter(YEAR == 2025 & BOROUGH != 'London') %>% select(-c(CODE, YEAR))
result_clusters_2025 <- calcula_clusters_jerarquico(df_analisis_2025, 'Dendograma 2021-2025', 'RES_Clusters_2025.csv')
print("RESULTADO 2021-2025")


