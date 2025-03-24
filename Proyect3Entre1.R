
## Carga de Datos

```{r}
library(dplyr)
library(ggplot2)
divorcios <- read.csv("C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\Merged file.csv", stringsAsFactors = FALSE)
head(divorcios)

## 1. Exploración de los Datos

summary(divorcios)
str(divorcios)

#2. Tipos de variables
variables_tipo <- sapply(divorcios, class)
variables_tipo
