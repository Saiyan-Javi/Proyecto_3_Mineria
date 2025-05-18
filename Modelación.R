# Instalar paquetes si no los tienes
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("glmnet")
install.packages("ggplot2")

# Cargar paquetes
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(glmnet)
library(ggplot2)

# Cargar dataset agregado
datos_agregados <- read.csv("divorcios_agregados_2014_2024.csv")

# Diagnóstico inicial: Verificar NA en datos_agregados
cat("Resumen de datos_agregados:\n")
summary(datos_agregados)
cat("\nValores NA en datos_agregados:\n")
print(colSums(is.na(datos_agregados)))
cat("\nDatos agregados completos:\n")
print(datos_agregados)

# Imputar NA en edadhom_promedio y edadmuj_promedio (si los hay)
datos_agregados <- datos_agregados %>%
  mutate(
    edadhom_promedio = ifelse(is.na(edadhom_promedio), 
                              mean(edadhom_promedio, na.rm = TRUE), 
                              edadhom_promedio),
    edadmuj_promedio = ifelse(is.na(edadmuj_promedio), 
                              mean(edadmuj_promedio, na.rm = TRUE), 
                              edadmuj_promedio)
  )

# Verificar después de imputación
cat("\nValores NA en datos_agregados después de imputación:\n")
print(colSums(is.na(datos_agregados)))

# Dividir datos (entrenamiento: 2014-2019, prueba: 2020-2024)
train_data <- datos_agregados %>% filter(AÑOREG <= 2019)
test_data <- datos_agregados %>% filter(AÑOREG >= 2020)

# Escalar predictores (excluir pandemia por varianza cero)
predictors <- c("AÑOREG", "edadhom_promedio", "edadmuj_promedio")
train_scaled <- train_data
test_scaled <- test_data
train_scaled[predictors] <- scale(train_data[predictors])
test_scaled[predictors] <- scale(test_data[predictors], 
                                 center = attr(scale(train_data[predictors]), "scaled:center"),
                                 scale = attr(scale(train_data[predictors]), "scaled:scale"))

# Diagnóstico: Verificar NA y varianza
cat("Resumen de train_data:\n")
summary(train_data)
cat("\nValores NA en train_data:\n")
print(colSums(is.na(train_data)))
cat("\nResumen de train_scaled:\n")
summary(train_scaled)
cat("\nValores NA en train_scaled:\n")
print(colSums(is.na(train_scaled)))
cat("\nVarianza de predictores en train_data:\n")
print(sapply(train_data[predictors], var, na.rm = TRUE))

# Configurar validación cruzada para series temporales
ctrl <- trainControl(method = "timeslice",
                     initialWindow = 4,
                     horizon = 1,
                     fixedWindow = TRUE,
                     skip = 0,
                     summaryFunction = defaultSummary)

# Modelo 1: Lasso
lasso_model <- train(
  divorcios ~ ., 
  data = train_data,
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = expand.grid(alpha = 1, lambda = c(0.1, 0.5, 1, 2)),
  metric = "RMSE"
)

# Modelo 2: Random Forest
rf_model <- train(
  divorcios ~ ., 
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = expand.grid(mtry = c(1, 2, 3)),
  metric = "RMSE"
)

# Modelo 3: SVR
svr_model <- train(
  divorcios ~ ., 
  data = train_scaled,
  method = "svmRadial",
  trControl = ctrl,
  tuneGrid = expand.grid(sigma = c(0.1, 1), C = c(0.1, 1, 10)),
  metric = "RMSE"
)

# Evaluar modelos en el conjunto de prueba
lasso_pred <- predict(lasso_model, test_data)
rf_pred <- predict(rf_model, test_data)
svr_pred <- predict(svr_model, test_scaled)

lasso_metrics <- postResample(lasso_pred, test_data$divorcios)
rf_metrics <- postResample(rf_pred, test_data$divorcios)
svr_metrics <- postResample(svr_pred, test_data$divorcios)

# Mostrar resultados
cat("Lasso Metrics:\n"); print(lasso_metrics)
cat("Random Forest Metrics:\n"); print(rf_metrics)
cat("SVR Metrics:\n"); print(svr_metrics)

# Mejores parámetros
cat("\nMejores parámetros:\n")
cat("Lasso:", toString(lasso_model$bestTune), "\n")
cat("Random Forest:", toString(rf_model$bestTune), "\n")
cat("SVR:", toString(svr_model$bestTune), "\n")

# Guardar resultados en un CSV
resultados <- data.frame(
  Modelo = c("Lasso", "Random Forest", "SVR"),
  RMSE = c(lasso_metrics["RMSE"], rf_metrics["RMSE"], svr_metrics["RMSE"]),
  MAE = c(lasso_metrics["MAE"], rf_metrics["MAE"], svr_metrics["MAE"])
)
write.csv(resultados, "resultados_modelos_2014_2024.csv", row.names = FALSE)

# Graficar predicciones vs. reales (todos los modelos)
predicciones <- data.frame(
  Año = test_data$AÑOREG,
  Real = test_data$divorcios,
  Lasso = lasso_pred,
  Random_Forest = rf_pred,
  SVR = svr_pred
)
predicciones_long <- predicciones %>%
  pivot_longer(cols = c(Lasso, Random_Forest, SVR), 
               names_to = "Modelo", 
               values_to = "Predicho")

ggplot(predicciones_long, aes(x = Año, y = Predicho, color = Modelo)) +
  geom_line() +
  geom_point(aes(y = Real, color = "Real"), size = 3) +
  theme_minimal() +
  labs(title = "Predicciones vs. Reales (2020-2024)",
       x = "Año", y = "Divorcios") +
  scale_color_manual(values = c("Lasso" = "blue", "Random_Forest" = "green", 
                                "SVR" = "purple", "Real" = "black"))
ggsave("predicciones_vs_reales_2014_2024.png")

# Graficar residuos
residuos <- data.frame(
  Año = test_data$AÑOREG,
  Lasso = test_data$divorcios - lasso_pred,
  Random_Forest = test_data$divorcios - rf_pred,
  SVR = test_data$divorcios - svr_pred
)
residuos_long <- residuos %>%
  pivot_longer(cols = c(Lasso, Random_Forest, SVR), 
               names_to = "Modelo", 
               values_to = "Residuo")

ggplot(residuos_long, aes(x = Año, y = Residuo, color = Modelo)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuos por Modelo (2020-2024)",
       x = "Año", y = "Residuo (Real - Predicho)") +
  scale_color_manual(values = c("Lasso" = "blue", "Random_Forest" = "green", "SVR" = "purple"))
ggsave("residuos_modelos_2014_2024.png")

# Guardar modelos
saveRDS(lasso_model, "lasso_model_2014_2024.rds")
saveRDS(rf_model, "rf_model_2014_2024.rds")
saveRDS(svr_model, "svr_model_2014_2024.rds")
