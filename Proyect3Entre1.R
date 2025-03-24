# Cargar librerías necesarias
library(readxl)
library(tidyverse)
library(summarytools)
library(corrplot)
library(GGally)
library(dplyr)
library(reshape2)

# 1. Cargar los datos
# Asumiendo que el archivo está en el directorio de trabajo
datos_edad <- read_excel("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx", sheet = "Grupos edad hombre y mujer")
datos_pueblo <- read_excel("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx", sheet = "Pueblo del hombre y mujer")
datos_registro <- read_excel("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx", sheet = "Mes de registro y departamento")
datos_ocupacion <- read_excel("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx", sheet = "Ocupaciones de mujer y hombre")
datos_ocurrencia <- read_excel("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx", sheet = "Mes ocurrencia y día")

# 2. Descripción general del conjunto de datos
cat("Descripción General del Conjunto de Datos:\n")
cat("----------------------------------------\n")
cat("El archivo contiene datos sobre divorcios organizados en 5 hojas:\n")
cat("1. Grupos edad hombre y mujer:", nrow(datos_edad), "filas,", ncol(datos_edad), "columnas\n")
cat("2. Pueblo del hombre y mujer:", nrow(datos_pueblo), "filas,", ncol(datos_pueblo), "columnas\n")
cat("3. Mes de registro y departamento:", nrow(datos_registro), "filas,", ncol(datos_registro), "columnas\n")
cat("4. Ocupaciones de mujer y hombre:", nrow(datos_ocupacion), "filas,", ncol(datos_ocupacion), "columnas\n")
cat("5. Mes ocurrencia y día:", nrow(datos_ocurrencia), "filas,", ncol(datos_ocurrencia), "columnas\n\n")

# 3. Limpieza y preparación de datos

# Hoja 1: Grupos edad hombre y mujer
datos_edad_limpio <- datos_edad %>% 
  rename(Edad_Esposa = 1) %>% 
  filter(!is.na(Edad_Esposa)) %>% 
  select(-Total) %>% 
  pivot_longer(cols = -Edad_Esposa, names_to = "Edad_Esposo", values_to = "Cantidad")

# Hoja 2: Pueblo del hombre y mujer
datos_pueblo_limpio <- datos_pueblo %>% 
  rename(Pueblo_Esposa = 1) %>% 
  filter(!is.na(Pueblo_Esposa)) %>% 
  select(-Total) %>% 
  pivot_longer(cols = -Pueblo_Esposa, names_to = "Pueblo_Esposo", values_to = "Cantidad")

# 4. Análisis de variables numéricas
cat("\nAnálisis de Variables Numéricas:\n")
cat("--------------------------------\n")

# Asegurar que las edades sean categóricas
datos_edad_limpio$Edad_Esposa <- as.factor(datos_edad_limpio$Edad_Esposa)
datos_edad_limpio$Edad_Esposo <- as.factor(datos_edad_limpio$Edad_Esposo)

# Gráfico de barras para la edad de las esposas
p1 <- ggplot(datos_edad_limpio, aes(x = Edad_Esposa, y = Cantidad, fill = Edad_Esposa)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Divorcios según la Edad de la Esposa",
       x = "Edad de la Esposa", y = "Cantidad de Divorcios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras para la edad de los esposos
p2 <- ggplot(datos_edad_limpio, aes(x = Edad_Esposo, y = Cantidad, fill = Edad_Esposo)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Divorcios según la Edad del Esposo",
       x = "Edad del Esposo", y = "Cantidad de Divorcios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar ambos gráficos juntos
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# Limpieza y preparación de datos
datos_registro_limpio <- datos_registro %>%
  # Eliminar filas vacías o descriptivas
  filter(!is.na(`Departamento de registro`),
         `Departamento de registro` != "Todos los departamentos") %>%
  # Seleccionar columnas relevantes
  select(`Departamento de registro`, Total, Enero:Diciembre) %>%
  # Renombrar columnas para facilitar el trabajo
  rename(Departamento = `Departamento de registro`) %>%
  # Convertir a formato largo para el análisis por mes
  pivot_longer(cols = Enero:Diciembre, 
               names_to = "Mes", 
               values_to = "Divorcios") %>%
  # Convertir Mes a factor ordenado
  mutate(Mes = factor(Mes, levels = month.name[1:12], ordered = TRUE))

# Gráfico de divorcios por departamento (ordenado descendente)
ggplot(datos_registro_limpio %>% 
         distinct(Departamento, Total),
       aes(x = reorder(Departamento, Total), y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Total), hjust = -0.2, size = 3) +
  labs(title = "Total de Divorcios por Departamento (2015)",
       x = "Departamento",
       y = "Número de Divorcios") +
  coord_flip() +  # Barras horizontales para mejor lectura
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8))




library(tidyverse)
library(lubridate)
library(viridis)

## 1. Limpieza de datos robusta
datos_dias_meses <- datos_ocurrencia %>%
  # Renombrar primera columna correctamente (Dia en lugar de 0ia)
  rename(Dia = 1) %>%  
  # Filtrar filas no deseadas (CORRECCIÓN: usar filter() correctamente)
  filter(
    !is.na(Dia),
    !str_detect(Dia, "Total|Todos"),  # Excluir totales
    Dia != ""  # Excluir filas vacías
  ) %>%
  # Convertir valores a numéricos (manejando "-" como 0)
  mutate(across(-Dia, ~as.numeric(gsub("-", "0", .x)))) %>%  
  # Pivotear a formato largo
  pivot_longer(
    cols = -Dia,
    names_to = "Mes",
    values_to = "Divorcios"
  ) %>%
  # Convertir tipos de datos
  mutate(
    Dia = as.numeric(Dia),
    Mes = factor(Mes, levels = month.name[1:12], ordered = TRUE)
  )

## 2. Heatmap optimizado
ggplot(datos_dias_meses, aes(x = Mes, y = Dia, fill = Divorcios)) +
  geom_tile(color = "white", linewidth = 0.3) +
  # Mostrar solo valores significativos
  geom_text(aes(label = ifelse(Divorcios > quantile(Divorcios, 0.75), Divorcios, "")), 
            size = 3, color = "white", fontface = "bold") +
  scale_y_reverse(breaks = seq(1, 31, by = 5)) +
  scale_fill_viridis(option = "inferno", direction = -1) +
  labs(
    title = "Distribución Diaria de Divorcios",
    subtitle = "Intensidad del color representa la cantidad de divorcios",
    x = NULL,
    y = "Día del Mes",
    fill = "N° Divorcios"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "right",
    panel.grid = element_blank()
  )
