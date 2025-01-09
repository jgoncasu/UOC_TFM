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
library(data.table)

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

calcula_correlacion <- function(df, titulo) {
  corr_matrix <- round(cor(df), 2)
  etiquetas <- c("01 - Edad", "02 - Raza", "03 - Salario", "04 - Estudios", "05 - Tráfico", "06 - Esp.Vida", "07 - Delitos", "08 - Servicios", "09 - Pr.Vivienda")
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

analisis_clusters <- function (df, periodo) {
  # Se escalan los valores para que tengan una media = 0 y una desviación estándar = 1
  df_scaled <- scale(df[4:12])

  # Calcula el método de cálculo de la distancia entre clusters  
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average", "single", "complete", "ward")
  print("Algoritmo para el cálculo de la distancia entre clusters")
  print(sapply(m, ac, df = df_scaled))
  
  # Se aplica el clustering jerárquico  
  clust <- agnes(df_scaled, method = "ward")

  dendrograma <- pltree(clust, cex = 1, hang = -1, main = paste0('Dendrograma ', periodo), labels = as.list(df$BOROUGH), ylab=NA, xlab=NA)
  print(dendrograma)
  
  gap_stat <- clusGap(df_scaled, FUN = hcut, nstart = 1, K.max = 10, B = 100)
  # Visualización del número óptimo de clusters
  dat <- data.table(gap_stat$Tab)
  dat[, k := .I]
  p <- ggplot(dat, aes(k, gap)) + geom_line() + geom_point(size = 3) +
    geom_errorbar(aes(ymax = gap + SE.sim, ymin = gap - SE.sim), width = 0.25) +
    ggtitle(paste0("Número óptimo de clusters ", periodo)) +
    labs(x = "Número de clusters", y = "Gap Statistic") +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 12, face = "bold")) +
    scale_x_continuous(breaks = seq(1, 10, by = 1)) +
    theme_minimal()
  print(p)
}

calcula_clusters_jerarquico <- function(df, periodo, k) {
  print(paste0("RESULTADO CLUSTERING K = ", k))
  # Se calculan los grupos para el K parametrizado
  df_scaled <- scale(df[4:12])
  d <- dist(df_scaled, method = "euclidean")
  final_clust <- hclust(d, method = "ward.D2")
  groups <- cutree(final_clust, k = k)
  print(table(groups))
  df_clusters <- cbind(df_scaled, CLUSTER = groups)
  
  df_clusters <- as.data.frame(df_clusters)
  # Calcula los índices de gentrificación
  df_tmp <- df_clusters %>% mutate(
    IND_05_CAR_TRAFFIC = -1 * IND_05_CAR_TRAFFIC,
    IND_07_CRIMES = -1 * IND_07_CRIMES
  )
  
  df_tmp <- df_tmp %>% group_by(CLUSTER) %>%
    summarise(
      mean_IND_01_AGE = mean(IND_01_AGE, na.rm = TRUE),
      mean_IND_02_RACE_WHITE = mean(IND_02_RACE_WHITE, na.rm = TRUE),
      mean_IND_03_WEEK_EARNINGS = mean(IND_03_WEEK_EARNINGS, na.rm = TRUE),
      mean_IND_04_PERCENT_NVQ4 = mean(IND_04_PERCENT_NVQ4, na.rm = TRUE),
      mean_IND_05_CAR_TRAFFIC = mean(IND_05_CAR_TRAFFIC, na.rm = TRUE),
      mean_IND_06_EXP_LIFE = mean(IND_06_EXP_LIFE, na.rm = TRUE),
      mean_IND_07_CRIMES = mean(IND_07_CRIMES, na.rm = TRUE),
      mean_IND_08_SERVICES = mean(IND_08_SERVICES, na.rm = TRUE),
      mean_IND_09_HOUSE_PRICE = mean(IND_09_HOUSE_PRICE, na.rm = TRUE)
    )

  df_indice <- df_tmp %>%
    rowwise() %>%
    mutate(
      GENTRIFICATION_INDEX = round(sum(
      c_across(starts_with("mean_")),
      na.rm = TRUE
      ), 2)
    ) %>%
    ungroup() %>%
    select(CLUSTER, GENTRIFICATION_INDEX)
  df_indice <- df_indice %>%
    arrange(desc(GENTRIFICATION_INDEX)) %>%
    mutate(CLUSTER_NEW = row_number())
  
  df_tmp <- df_tmp %>%
    inner_join(df_indice %>% select(CLUSTER, CLUSTER_NEW, GENTRIFICATION_INDEX), by="CLUSTER") %>%
    mutate(CLUSTER = CLUSTER_NEW) %>%
    select(-CLUSTER_NEW)
  colnames(df_tmp) <- c("CLUSTER","AGE","RACE","SALARY","NVQ4","TRAFFIC","EXPLIFE","CRIMES","SERVICES","HOUSING","INDEX")
  print(df_tmp)
  
  df_clusters <- df_clusters %>%
    inner_join(df_indice %>% select(CLUSTER, CLUSTER_NEW, GENTRIFICATION_INDEX), by="CLUSTER") %>%
    mutate(CLUSTER = CLUSTER_NEW) %>%
    select(-CLUSTER_NEW)

  #final_data <- as.data.frame(final_data)
  df_summary <- df_clusters %>%
    group_by(CLUSTER) %>%
    summarise(across(starts_with("IND_"), mean, na.rm = TRUE))
  #print(aggregate(final_data, by=list(cluster=final_data$cluster), mean))

  return(df_clusters)
}


################################################################################
# 03) Guarda los datos de los clusters
################################################################################
guardar_clusters <- function(df) {
  ruta_fichero <- 'DAT_Clusters_Gentrificacion_Londres_9VAR.csv'
  write.csv2(df, paste(PATH_FICHEROS_SALIDA, ruta_fichero, sep=""), row.names = FALSE)
}


################################################################################
# Cuerpo principal del programa
################################################################################

set.seed(1)

print("***** PASO 0: CARGANDO BARRIOS DE LONDRES *****")
df_barrios <- carga_lista_barrios()

# Carga datos preparados
df_datos <- carga_indicadores()

# Selecciona columnas IND
df_datos <- df_datos %>% select(-starts_with("VAL_"), -starts_with("VAR_")) #%>% filter(!BOROUGH %in% c("London"))

df_correlacion <- df_datos %>% filter(BOROUGH != 'London') %>% select(-c(CODE, BOROUGH, YEAR))

# Calcula correlación
calcula_correlacion(df_correlacion, 'Matriz de correlación')

# Cálculo de clústers usando Hierarchical clustering

# Período 2001-2010
df_analisis_2010 <- df_datos %>% filter(YEAR == 2010 & BOROUGH != 'London')
analisis_clusters(df_analisis_2010, '2001-2010')
df_aux <- data.frame(CODE = NA, BOROUGH = NA, YEAR = NA, IND_01_AGE = NA, IND_02_RACE_WHITE = NA, IND_03_WEEK_EARNINGS = NA, IND_04_PERCENT_NVQ4 = NA,
                     IND_05_CAR_TRAFFIC = NA, IND_06_EXP_LIFE = NA, IND_07_CRIMES = NA, IND_08_SERVICES = NA, IND_09_HOUSE_PRICE = NA,
                     CLUSTER = NA, GENTRIFICATION_INDEX = NA, K = NA)
for (k in c(2:10)) {
  result_clusters_2011 <- calcula_clusters_jerarquico(df_analisis_2010, '2001-2010', k)
  df_2010 <- cbind(df_analisis_2010, result_clusters_2011 %>% select(CLUSTER, GENTRIFICATION_INDEX))
  df_2010 <- df_2010 %>% mutate(K = k)
  df_aux <- rbind(df_aux, df_2010)
}
df_aux <- df_aux %>% filter(!is.na(CODE))
df_resultado <- df_aux

# Período 2011-2020
df_analisis_2020 <- df_datos %>% filter(YEAR == 2020 & BOROUGH != 'London')
analisis_clusters(df_analisis_2020, '2011-2020')
df_aux <- data.frame(CODE = NA, BOROUGH = NA, YEAR = NA, IND_01_AGE = NA, IND_02_RACE_WHITE = NA, IND_03_WEEK_EARNINGS = NA, IND_04_PERCENT_NVQ4 = NA,
                     IND_05_CAR_TRAFFIC = NA, IND_06_EXP_LIFE = NA, IND_07_CRIMES = NA, IND_08_SERVICES = NA, IND_09_HOUSE_PRICE = NA,
                     CLUSTER = NA, GENTRIFICATION_INDEX = NA, K = NA)
for (k in c(2:10)) {
  result_clusters_2020 <- calcula_clusters_jerarquico(df_analisis_2020, '2011-2020', k)
  df_2020 <- cbind(df_analisis_2020, result_clusters_2020 %>% select(CLUSTER, GENTRIFICATION_INDEX))
  df_2020 <- df_2020 %>% mutate(K = k)
  df_aux <- rbind(df_aux, df_2020)
}
df_aux <- df_aux %>% filter(!is.na(CODE))
df_resultado <- rbind(df_resultado, df_aux)

# Período 2021-2025
df_analisis_2025 <- df_datos %>% filter(YEAR == 2025 & BOROUGH != 'London')
analisis_clusters(df_analisis_2025, '2021-2025')
df_aux <- data.frame(CODE = NA, BOROUGH = NA, YEAR = NA, IND_01_AGE = NA, IND_02_RACE_WHITE = NA, IND_03_WEEK_EARNINGS = NA, IND_04_PERCENT_NVQ4 = NA,
                     IND_05_CAR_TRAFFIC = NA, IND_06_EXP_LIFE = NA, IND_07_CRIMES = NA, IND_08_SERVICES = NA, IND_09_HOUSE_PRICE = NA,
                     CLUSTER = NA, GENTRIFICATION_INDEX = NA, K = NA)
for (k in c(2:10)) {
  result_clusters_2025 <- calcula_clusters_jerarquico(df_analisis_2025, '2021-2025', k)
  df_2025 <- cbind(df_analisis_2025, result_clusters_2025 %>% select(CLUSTER, GENTRIFICATION_INDEX))
  df_2025 <- df_2025 %>% mutate(K = k)
  df_aux <- rbind(df_aux, df_2025)
}
df_aux <- df_aux %>% filter(!is.na(CODE))
df_resultado <- rbind(df_resultado, df_aux)

df_resultado <- df_resultado %>% mutate(GENTRIFICATION_INDEX = round(GENTRIFICATION_INDEX, 3))
#view(df_resultado)

guardar_clusters(df_resultado)
