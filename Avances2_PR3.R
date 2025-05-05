# Cargar librerías
library(haven)
library(dplyr)
library(ggplot2)
library(skimr) # Para resumen detallado
library(corrplot) # Para matriz de correlación
library(caret) # Para modelado y evaluación
library(randomForest) # Para Random Forest
library(cluster) # Para clustering

# Leer el archivo
spss_2014 <- read_sav("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\Bases_P3_Mineria\\div_2014.sav")

# --- ESTRUCTURA GENERAL ---
# Ver primeras filas
head(spss_2014)

# Ver estructura
str(spss_2014)

# Dimensiones
dim(spss_2014)

# Nombres de columnas
names(spss_2014)

# --- ESTADÍSTICAS DESCRIPTIVAS ---
# Resumen básico
summary(spss_2014)

# Resumen más completo con skimr
skim(spss_2014)

# --- VALORES FALTANTES ---
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

# --- VISUALIZACIONES BÁSICAS ---
# Histograma para una variable numérica
ggplot(spss_2014, aes(x = EDADHOM)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Edad en Hombres")

# Boxplot para detectar outliers
ggplot(spss_2014, aes(y = EDADHOM)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot de Edad")

# Frecuencia de variable categórica
ggplot(spss_2014, aes(x = CIUOHOM)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Frecuencia por Ocupación de Hombres")

# --- MATRIZ DE CORRELACIÓN ---
# Seleccionar solo variables numéricas
num_vars <- spss_2014 %>% select(where(is.numeric))

# Matriz de correlación
cor_matrix <- cor(num_vars, use = "complete.obs")

# Visualización
corrplot(cor_matrix, method = "color", tl.cex = 0.7, number.cex = 0.7)

# --- PARTE DEL CLUSTERING ---
# Seleccionar solo columnas numéricas
datos_numericos <- spss_2014 %>% select(where(is.numeric))

# Eliminar columnas con NA
datos_numericos <- na.omit(datos_numericos)

# Escalar los datos
datos_escalados <- scale(datos_numericos)

# Determinar el número óptimo de clusters
# Método del codo
wss <- sapply(1:10, function(k) {
  kmeans(datos_escalados, centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de clusters",
     ylab = "Suma de cuadrados intra-cluster",
     main = "Método del Codo")

# Análisis de silueta
sil <- sapply(2:10, function(k) {
  pam(datos_escalados, k)$silinfo$avg.width
})
plot(2:10, sil, type = "b", pch = 19,
     xlab = "Número de clusters",
     ylab = "Ancho promedio de silueta",
     main = "Análisis de Silueta")

# Aplicar K-means
k <- 3
set.seed(123)
kmeans_resultado <- kmeans(datos_escalados, centers = k, nstart = 25)
spss_2014$cluster <- factor(kmeans_resultado$cluster)

# Visualización de clusters con PCA
pca <- prcomp(datos_escalados)
pca_df <- data.frame(pca$x[, 1:2], cluster = spss_2014$cluster)
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters visualizados con PCA") +
  theme_minimal()

# Características de cada cluster
aggregate(datos_numericos, by = list(Cluster = spss_2014$cluster), FUN = mean)

# --- NUEVA SECCIÓN: MODELADO SUPERVISADO ---
# 1. Especificar la variable respuesta
response_var <- "CIUOHOM"

# 2. Preparación de conjuntos de entrenamiento y prueba
# Preprocesamiento: Seleccionar variables relevantes y manejar NAs
na_percentage <- colSums(is.na(spss_2014)) / nrow(spss_2014)
selected_cols <- names(spss_2014)[na_percentage < 0.5]
model_data <- spss_2014 %>% select(all_of(selected_cols))

# Imputar valores faltantes y eliminar filas con NA restantes
preprocess <- preProcess(model_data, method = c("medianImpute"))
model_data <- predict(preprocess, model_data)
model_data <- na.omit(model_data) # Eliminar filas con NA después de imputar

# Convertir la variable respuesta a factor y ajustar niveles
model_data[[response_var]] <- as.factor(model_data[[response_var]])
original_levels <- levels(model_data[[response_var]])
cat("Niveles originales de CIUOHOM:\n")
print(original_levels)
new_levels <- paste0("Occup_", seq_along(original_levels))
levels(model_data[[response_var]]) <- new_levels
cat("Nuevos niveles de CIUOHOM:\n")
print(levels(model_data[[response_var]]))

# Dividir en entrenamiento (70%) y prueba (30%)
set.seed(123)
trainIndex <- createDataPartition(model_data[[response_var]], p = 0.7, list = FALSE)
train_data <- model_data[trainIndex, ]
test_data <- model_data[-trainIndex, ]

# Verificar balance de clases
train_class_dist <- table(train_data[[response_var]])
test_class_dist <- table(test_data[[response_var]])
cat("Distribución de clases en conjunto de entrenamiento:\n")
print(train_class_dist)
cat("\nDistribución de clases en conjunto de prueba:\n")
print(test_class_dist)

# Visualización de balance
ggplot(data = data.frame(class = names(train_class_dist), count = as.numeric(train_class_dist)), 
       aes(x = class, y = count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Distribución de Clases en Conjunto de Entrenamiento", x = "Clase", y = "Frecuencia") +
  theme_minimal()

# 3. Selección del algoritmo y preprocesamiento
cat_cols <- names(model_data)[sapply(model_data, is.factor) | sapply(model_data, is.character)]
if (length(cat_cols) > 0) {
  model_data[cat_cols] <- lapply(model_data[cat_cols], as.factor)
  for (col in cat_cols) {
    if (col != response_var) {
      levels(model_data[[col]]) <- make.names(levels(model_data[[col]]), unique = TRUE)
    }
  }
}

# 4. Construcción de múltiples modelos Random Forest
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE,
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Modelo 1: mtry=2, ntree=100
set.seed(123)
rf_model1 <- train(
  as.formula(paste(response_var, "~ .")),
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = data.frame(mtry = 2),
  ntree = 100
)

# Modelo 2: mtry=4, ntree=200
set.seed(123)
rf_model2 <- train(
  as.formula(paste(response_var, "~ .")),
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = data.frame(mtry = 4),
  ntree = 200
)

# Modelo 3: mtry=6, ntree=300
set.seed(123)
rf_model3 <- train(
  as.formula(paste(response_var, "~ .")),
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = data.frame(mtry = 6),
  ntree = 300
)

# Comparar modelos
results <- resamples(list(Model1 = rf_model1, Model2 = rf_model2, Model3 = rf_model3))
summary(results)

# Seleccionar el mejor modelo (basado en Accuracy)
best_model <- list(rf_model1, rf_model2, rf_model3)[[which.max(c(
  max(rf_model1$results$Accuracy),
  max(rf_model2$results$Accuracy),
  max(rf_model3$results$Accuracy)
))]]
cat("Mejor modelo seleccionado:\n")
print(best_model$results)

# Modelo final con los mejores parámetros
final_rf_model <- train(
  as.formula(paste(response_var, "~ .")),
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = data.frame(mtry = best_model$bestTune$mtry),
  ntree = best_model$finalModel$ntree
)

# 5. Evaluación del modelo final
test_pred <- predict(final_rf_model, test_data)
conf_matrix <- confusionMatrix(test_pred, test_data[[response_var]])
print(conf_matrix)

conf_matrix_table <- as.data.frame(conf_matrix$table)
ggplot(conf_matrix_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Matriz de Confusión del Modelo Final", x = "Predicción", y = "Real") +
  theme_minimal()

cat("Métricas del modelo final:\n")
print(conf_matrix$overall)