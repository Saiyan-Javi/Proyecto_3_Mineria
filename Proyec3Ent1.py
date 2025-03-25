import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path

# Configuración inicial
plt.style.use('ggplot')
sns.set_palette("viridis")

# 1. Cargar los datos
file_path = "C:\\Users\\javie\\Documents\\UVG\\Cuarto año\\Primer Semestre\\Mineria\\P32015.xlsx"

datos_edad = pd.read_excel(file_path, sheet_name="Grupos edad hombre y mujer")
datos_pueblo = pd.read_excel(file_path, sheet_name="Pueblo del hombre y mujer")
datos_registro = pd.read_excel(file_path, sheet_name="Mes de registro y departamento")
datos_ocupacion = pd.read_excel(file_path, sheet_name="Ocupaciones de mujer y hombre")
datos_ocurrencia = pd.read_excel(file_path, sheet_name="Mes ocurrencia y día")

# 2. Descripción general del conjunto de datos
print("Descripción General del Conjunto de Datos:")
print("----------------------------------------")
print("El archivo contiene datos sobre divorcios organizados en 5 hojas:")
print(f"1. Grupos edad hombre y mujer: {datos_edad.shape[0]} filas, {datos_edad.shape[1]} columnas")
print(f"2. Pueblo del hombre y mujer: {datos_pueblo.shape[0]} filas, {datos_pueblo.shape[1]} columnas")
print(f"3. Mes de registro y departamento: {datos_registro.shape[0]} filas, {datos_registro.shape[1]} columnas")
print(f"4. Ocupaciones de mujer y hombre: {datos_ocupacion.shape[0]} filas, {datos_ocupacion.shape[1]} columnas")
print(f"5. Mes ocurrencia y día: {datos_ocurrencia.shape[0]} filas, {datos_ocurrencia.shape[1]} columnas\n")

# 3. Limpieza y preparación de datos

# Hoja 1: Grupos edad hombre y mujer
datos_edad_limpio = datos_edad.rename(columns={datos_edad.columns[0]: "Edad_Esposa"})
datos_edad_limpio = datos_edad_limpio.dropna(subset=["Edad_Esposa"])
datos_edad_limpio = datos_edad_limpio.drop(columns=["Total"], errors="ignore")
datos_edad_limpio = datos_edad_limpio.melt(id_vars=["Edad_Esposa"], 
                                          var_name="Edad_Esposo", 
                                          value_name="Cantidad")

# Hoja 2: Pueblo del hombre y mujer
datos_pueblo_limpio = datos_pueblo.rename(columns={datos_pueblo.columns[0]: "Pueblo_Esposa"})
datos_pueblo_limpio = datos_pueblo_limpio.dropna(subset=["Pueblo_Esposa"])
datos_pueblo_limpio = datos_pueblo_limpio.drop(columns=["Total"], errors="ignore")
datos_pueblo_limpio = datos_pueblo_limpio.melt(id_vars=["Pueblo_Esposa"], 
                                             var_name="Pueblo_Esposo", 
                                             value_name="Cantidad")

# 4. Análisis de variables numéricas
print("\nAnálisis de Variables Numéricas:")
print("--------------------------------")

# Convertir edades a categóricas
datos_edad_limpio["Edad_Esposa"] = pd.Categorical(datos_edad_limpio["Edad_Esposa"])
datos_edad_limpio["Edad_Esposo"] = pd.Categorical(datos_edad_limpio["Edad_Esposo"])

# Gráfico de barras para la edad de las esposas
plt.figure(figsize=(12, 6))
plt.subplot(1, 2, 1)
sns.barplot(data=datos_edad_limpio, x="Edad_Esposa", y="Cantidad", 
            estimator=sum, ci=None, 
            order=sorted(datos_edad_limpio["Edad_Esposa"].unique()))
plt.title("Divorcios según la Edad de la Esposa")
plt.xlabel("Edad de la Esposa")
plt.ylabel("Cantidad de Divorcios")
plt.xticks(rotation=45)

# Gráfico de barras para la edad de los esposos
plt.subplot(1, 2, 2)
sns.barplot(data=datos_edad_limpio, x="Edad_Esposo", y="Cantidad", 
            estimator=sum, ci=None,
            order=sorted(datos_edad_limpio["Edad_Esposo"].unique()))
plt.title("Divorcios según la Edad del Esposo")
plt.xlabel("Edad del Esposo")
plt.ylabel("Cantidad de Divorcios")
plt.xticks(rotation=45)


plt.tight_layout()
plt.show()


print(datos_edad_limpio)


# Limpieza y preparación de datos de registro
datos_registro_limpio = datos_registro.dropna(subset=[datos_registro.columns[0]])
datos_registro_limpio = datos_registro_limpio[
    datos_registro_limpio[datos_registro_limpio.columns[0]] != "Todos los departamentos"
]
datos_registro_limpio = datos_registro_limpio.rename(
    columns={datos_registro_limpio.columns[0]: "Departamento"}
)
meses = ['Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 
         'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre']
datos_registro_limpio = datos_registro_limpio.melt(
    id_vars=["Departamento", "Total"], 
    value_vars=meses,
    var_name="Mes", 
    value_name="Divorcios"
)
datos_registro_limpio["Mes"] = pd.Categorical(
    datos_registro_limpio["Mes"], 
    categories=meses, 
    ordered=True
)

# Gráfico de divorcios por departamento (ordenado descendente)
plt.figure(figsize=(10, 8))
deptos = datos_registro_limpio.groupby("Departamento")["Total"].first().sort_values()
ax = deptos.plot(kind="barh", color="steelblue")
plt.title("Total de Divorcios por Departamento (2015)", pad=20)
plt.xlabel("Número de Divorcios")
plt.ylabel("Departamento")

for i, v in enumerate(deptos):
    ax.text(v + 3, i, str(v), color='black', ha='left', va='center')

plt.tight_layout()
plt.show()

# Gráfico de divorcios por mes
datos_meses = datos_registro_limpio.groupby("Mes")["Divorcios"].sum().reset_index()

plt.figure(figsize=(12, 6))
ax = sns.barplot(data=datos_meses, x="Mes", y="Divorcios", color="#2c7bb6")
plt.title("Total de Divorcios por Mes (2015)", pad=20)
plt.xlabel("Mes")
plt.ylabel("Número de Divorcios")
plt.xticks(rotation=45)

for p in ax.patches:
    ax.annotate(f"{int(p.get_height())}", 
                (p.get_x() + p.get_width() / 2., p.get_height()),
                ha='center', va='center', xytext=(0, 10), 
                textcoords='offset points')

plt.tight_layout()
plt.show()

######################

# Limpieza de datos de días y meses
datos_dias_meses = datos_ocurrencia.rename(columns={datos_ocurrencia.columns[0]: "Dia"})
datos_dias_meses = datos_dias_meses[
    (~datos_dias_meses["Dia"].isna()) & 
    (~datos_dias_meses["Dia"].astype(str).str.contains("Total|Todos")) &
    (datos_dias_meses["Dia"] != "")
]

# Convertir valores a numéricos (manejando "-" como 0)
for col in datos_dias_meses.columns[1:]:
    datos_dias_meses[col] = pd.to_numeric(datos_dias_meses[col].astype(str).str.replace("-", "0"), errors="coerce")

datos_dias_meses = datos_dias_meses.melt(
    id_vars=["Dia"], 
    value_vars=meses,
    var_name="Mes", 
    value_name="Divorcios"
)
datos_dias_meses["Dia"] = pd.to_numeric(datos_dias_meses["Dia"])
datos_dias_meses["Mes"] = pd.Categorical(
    datos_dias_meses["Mes"], 
    categories=meses, 
    ordered=True
)

# Heatmap optimizado
plt.figure(figsize=(12, 8))
pivot_data = datos_dias_meses.pivot("Dia", "Mes", "Divorcios")
sns.heatmap(pivot_data, cmap="inferno_r", linewidths=0.3, linecolor="white", 
            annot=pivot_data > np.quantile(pivot_data.values.flatten()[~np.isnan(pivot_data.values.flatten())], 0.75), 
            fmt=".0f", annot_kws={"size": 8, "color": "white", "weight": "bold"}, 
            cbar_kws={'label': 'N° Divorcios'})

plt.title("Distribución Diaria de Divorcios\nIntensidad del color representa la cantidad de divorcios", pad=20)
plt.xlabel("Mes")
plt.ylabel("Día del Mes")
plt.gca().invert_yaxis()

plt.tight_layout()
plt.show()
