# Instalar paquetes si no los tienes
install.packages("haven")
install.packages("tidyverse")

# Cargar paquetes
library(haven)
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(skimr) # Para resumen detallado
library(corrplot) # Para matriz de correlación
library(summarytools)


# Definir la ruta a los archivos SPSS
ruta <- "C:\\Users\\javie\\Downloads\\Divorcios\\"

# Leer archivos SPSS (2015-2022)
años_spss <- 2014:2024
datos_spss <- list()
for (año in años_spss) {
  archivo <- paste0(ruta, "div_", año, ".sav")
  datos_spss[[as.character(año)]] <- read_sav(archivo)
}

# Seleccionar columnas comunes y convertir CIUOHOM/CIUOMUJ a character
columnas_comunes <- c("AÑOREG", "EDADHOM", "EDADMUJ", "CIUOHOM", "CIUOMUJ")
for (año in names(datos_spss)) {
  datos_spss[[año]] <- datos_spss[[año]] %>%
    select(all_of(columnas_comunes)) %>%
    mutate(
      CIUOHOM = as.character(CIUOHOM),
      CIUOMUJ = as.character(CIUOMUJ)
    )
}

# Unificar datasets SPSS
datos_unificados <- bind_rows(datos_spss, .id = "año")

# Limpiar datos: eliminar NA en columnas clave y filtrar edades inválidas
datos_limpios <- datos_unificados %>%
  drop_na(AÑOREG, EDADHOM, EDADMUJ) %>%
  filter(EDADHOM != 999 & EDADMUJ != 999)

# Guardar el dataset limpio
write.csv(datos_limpios, "divorcios_limpios_2015_2022.csv", row.names = FALSE)

# Verificar el dataset limpio
summary(datos_limpios)
dim(datos_limpios)
names(datos_limpios)

# Mostrar avisos si los hay
warnings()

# 1. Estadísticas descriptivas para edad_hombre y edad_mujer
print("Estadísticas descriptivas para edad_hombre:")
summary(datos_limpios$EDADHOM)
print("Estadísticas descriptivas para edad_mujer:")
summary(datos_limpios$EDADMUJ)

# 2. Análisis temporal: Número de divorcios por año
divorcios_por_año <- datos_limpios %>%
  group_by(año) %>%
  summarise(cantidad = n())
print("Número de divorcios por año:")
print(divorcios_por_año)

# Gráfico de tendencia temporal
ggplot(divorcios_por_año, aes(x = año, y = cantidad)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Tendencia de divorcios por año (2014-2023)",
       x = "Año", y = "Número de divorcios") +
  theme_minimal()

# 3. Comparación por grupos: Diferencia de edad promedio entre hombres y mujeres
datos_limpios <- datos_limpios %>%
  mutate(dif_edad = EDADHOM - EDADMUJ)

print("Estadísticas de la diferencia de edad (hombre - mujer):")
summary(datos_limpios$dif_edad)

# Histograma de la diferencia de edad
ggplot(datos_limpios, aes(x = dif_edad)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribución de la diferencia de edad (hombre - mujer)",
       x = "Diferencia de edad", y = "Frecuencia") +
  theme_minimal()

# 4. Distribución por ocupación: Divorcios por ocupación_hombre y ocupación_mujer
divorcios_ocupacion_hombre <- datos_limpios %>%
  group_by(CIUOHOM) %>%
  summarise(cantidad = n()) %>%
  arrange(desc(cantidad))

divorcios_ocupacion_mujer <- datos_limpios %>%
  group_by(CIUOMUJ) %>%
  summarise(cantidad = n()) %>%
  arrange(desc(cantidad))

print("Divorcios por ocupación (hombre):")
print(divorcios_ocupacion_hombre)
print("Divorcios por ocupación (mujer):")
print(divorcios_ocupacion_mujer)

# Gráfico de barras para ocupación_hombre (top 10 ocupaciones)
ggplot(head(divorcios_ocupacion_hombre, 10), aes(x = reorder(CIUOHOM, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Top 10 ocupaciones con más divorcios (hombre)",
       x = "Ocupación", y = "Número de divorcios") +
  theme_minimal() +
  coord_flip()


# 5. Edad promedio por ocupación (hombre y mujer)
edad_promedio_ocupacion <- datos_limpios %>%
  group_by(CIUOHOM) %>%
  summarise(edad_prom_hombre = mean(EDADHOM, na.rm = TRUE)) %>%
  arrange(desc(edad_prom_hombre))

print("Edad promedio de hombres por ocupación:")
print(head(edad_promedio_ocupacion, 10))

# Histogramas de edades (hombre y mujer) - Corregido
ggplot(datos_limpios, aes(x = EDADHOM)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribución de edad de hombres",
       x = "Edad", y = "Frecuencia") +
  theme_minimal()

ggplot(datos_limpios, aes(x = EDADMUJ)) +
  geom_histogram(binwidth = 5, fill = "lightpink", color = "black", alpha = 0.7) +
  labs(title = "Distribución de edad de mujeres",
       x = "Edad", y = "Frecuencia") +
  theme_minimal()

#PARTE DEL CLUSTERING
#Preambulo
# Seleccionar solo columnas numéricas
datos_numericos <- datos_limpios %>% select(where(is.numeric))

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
k <- 2

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

