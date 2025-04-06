# Cargar librerías
library(haven)
library(dplyr)
library(ggplot2)
library(skimr) # Para resumen detallado
library(corrplot) # Para matriz de correlación

# Leer el archivo
spss_2014 <- read_sav("C:/Users/ricar/Documents/UVG-CUARTO AÑO/MINERÍA DE DATOS/div_2014.sav")

#ESTRUCTURA GENERAL
# Ver primeras filas
head(spss_2014)

# Ver estructura
str(spss_2014)

# Dimensiones
dim(spss_2014)

# Nombres de columnas
names(spss_2014)



#ESTADÍSTICAS DESCRIPTIVAS
# Resumen básico
summary(spss_2014)

# Resumen más completo con skimr
skim(spss_2014)

#VALORES FALTANTES

# Total de valores NA por columna
colSums(is.na(spss_2014))

# Visualización rápida de presencia de NAs
na_df <- data.frame(
  variable = names(spss_2014),
  na_count = colSums(is.na(spss_2014))
)
ggplot(na_df, aes(x = reorder(variable, -na_count), y = na_count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Cantidad de valores faltantes por variable", x = "", y = "NAs")

#VISUALIZACIONES BÁSICAS


#HISTOGRAMA para una variable numérica
# Reemplaza 'edad' por una variable numérica que tengas
ggplot(spss_2014, aes(x = EDADHOM)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Edad en Hombres")

# Boxplot para detectar outliers
ggplot(spss_2014, aes(y = EDADHOM)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot de Edad")

# Frecuencia de variable categórica
# Reemplaza 'ocupación hombre' por una variable categórica real del dataset
ggplot(spss_2014, aes(x = CIUOHOM)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Frecuencia por Género")

#MATRIZ DE CORRELACIÓN
# Seleccionar solo variables numéricas
num_vars <- spss_2014 %>% select(where(is.numeric))

# Matriz de correlación
cor_matrix <- cor(num_vars, use = "complete.obs")

# Visualización
corrplot(cor_matrix, method = "color", tl.cex = 0.7, number.cex = 0.7)


#PARTE DEL CLUSTERING
#Preambulo
# Seleccionar solo columnas numéricas
datos_numericos <- spss_2014 %>% select(where(is.numeric))

# Eliminar columnas con NA si las hubiera
datos_numericos <- na.omit(datos_numericos)

# Escalar los datos (muy importante en clustering)
datos_escalados <- scale(datos_numericos)

#a. Determinar el número óptimo de clusters
# Calcular total within-cluster sum of squares para k = 1 a 10
wss <- sapply(1:10, function(k) {
  kmeans(datos_escalados, centers = k, nstart = 10)$tot.withinss
})

# Graficar el codo
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de clusters",
     ylab = "Suma de cuadrados intra-cluster",
     main = "Método del Codo")
#Con índice de silueta
library(cluster)

# Calcular ancho promedio de silueta para k = 2 a 10
sil <- sapply(2:10, function(k) {
  pam(datos_escalados, k)$silinfo$avg.width
})

plot(2:10, sil, type = "b", pch = 19,
     xlab = "Número de clusters",
     ylab = "Ancho promedio de silueta",
     main = "Análisis de Silueta")

#b. Ahora aplicar K-means cluster
# Elegir k según el gráfico anterior (ejemplo: 3)
k <- 3

set.seed(123)
kmeans_resultado <- kmeans(datos_escalados, centers = k, nstart = 25)

# Añadir el cluster al dataframe original
spss_2014$cluster <- factor(kmeans_resultado$cluster)

#c. Visualización de los clusters
# PCA para reducir dimensiones
pca <- prcomp(datos_escalados)
pca_df <- data.frame(pca$x[, 1:2], cluster = spss_2014$cluster)

# Visualización
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters visualizados con PCA") +
  theme_minimal()

# Ver que caracteriza cada cluster
aggregate(datos_numericos, by = list(Cluster = spss_2014$cluster), FUN = mean)
