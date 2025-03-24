library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Ruta del archivo
file_path <- "C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx"

# Cargar todas las hojas
sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(sheet) {
  df <- read_excel(file_path, sheet = sheet, skip = 1) # Saltar la primera fila si tiene encabezados incorrectos
  df <- janitor::clean_names(df) # Limpiar nombres de columnas
  return(df)
})
names(data_list) <- sheets

# Función para inspeccionar cada hoja
describe_data <- function(df) {
  cat("Cantidad de observaciones:", nrow(df), "\n")
  cat("Cantidad de variables:", ncol(df), "\n")
  cat("Tipos de variables:\n")
  print(sapply(df, class))
}

# Aplicar la descripción a cada hoja
for (sheet in sheets) {
  cat("---", sheet, "---\n")
  describe_data(data_list[[sheet]])
}

# Análisis de variables numéricas y categóricas
analyze_data <- function(df) {
  num_vars <- df %>% select(where(is.numeric))
  cat_vars <- df %>% select(where(is.character))
  
  if (ncol(num_vars) > 0) {
    cat("\nMedidas de tendencia central y dispersión:\n")
    print(summary(num_vars))
  }
  
  if (ncol(cat_vars) > 0) {
    cat("\nTablas de frecuencia:\n")
    print(sapply(cat_vars, table))
  }
}

# Aplicar análisis a cada hoja
for (sheet in sheets) {
  cat("--- Análisis de", sheet, "---\n")
  analyze_data(data_list[[sheet]])
}

# Visualización de relaciones entre variables numéricas
plot_relationships <- function(df) {
  num_vars <- df %>% select(where(is.numeric))
  if (ncol(num_vars) > 1) {
    pairs(num_vars)
  }
}

# Aplicar visualización a cada hoja
for (sheet in sheets) {
  cat("--- Gráficos de", sheet, "---\n")
  plot_relationships(data_list[[sheet]])
}
