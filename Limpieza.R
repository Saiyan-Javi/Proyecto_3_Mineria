# Instalar paquetes si no los tienes
install.packages("haven")
install.packages("tidyverse")
install.packages("ggplot2")

# Cargar paquetes
library(haven)
library(tidyverse)
library(ggplot2)

# Definir la ruta a los archivos SPSS
ruta <- "C:/Users/ricar/Documents/UVG-CUARTO AÑO/Divorcios/"

# Leer archivos SPSS (2014-2024)
años_spss <- 2014:2024
datos_spss <- list()
for (año in años_spss) {
  archivo <- paste0(ruta, "divorcios_", año, ".sav")
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

# Agregar datos por año para crear la variable respuesta
datos_agregados <- datos_limpios %>%
  group_by(AÑOREG) %>%
  summarise(
    divorcios = n(),
    edadhom_promedio = mean(EDADHOM, na.rm = TRUE),
    edadmuj_promedio = mean(EDADMUJ, na.rm = TRUE)
  ) %>%
  mutate(
    pandemia = ifelse(AÑOREG %in% 2020:2022, 1, 0)
  )

# Guardar el dataset limpio y agregado
write.csv(datos_limpios, "divorcios_limpios_2014_2024.csv", row.names = FALSE)
write.csv(datos_agregados, "divorcios_agregados_2014_2024.csv", row.names = FALSE)

# Verificar los datasets
cat("Dataset limpio (individual):\n")
summary(datos_limpios)
dim(datos_limpios)
names(datos_limpios)

cat("\nDataset agregado (anual):\n")
print(datos_agregados)
dim(datos_agregados)
names(datos_agregados)

# Gráfico exploratorio: Divorcios por año
ggplot(datos_agregados, aes(x = AÑOREG, y = divorcios)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Cantidad de Divorcios por Año (2014-2024)",
       x = "Año", y = "Divorcios") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red")

# Guardar el gráfico
ggsave("divorcios_por_año_2014_2024.png")

# Mostrar avisos si los hay
warnings()
