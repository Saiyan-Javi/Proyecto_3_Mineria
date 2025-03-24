import pandas as pd

# Especificar la ruta del archivo de Excel
df = pd.read_csv("Merged_file.csv")

# Especificar el motor manualmente (openpyxl para .xlsx, xlrd para .xls)
engine = 'openpyxl'  # Cambia a 'xlrd' si es un archivo .xls

# Cargar el archivo de Excel
try:
    sheets_dict = pd.read_excel(df, sheet_name=None, engine=engine)
    print("Archivo cargado correctamente.")
except Exception as e:
    print(f"Error al cargar el archivo: {e}")


    # Descripción general del conjunto de datos
def describe_dataset(sheets_dict):
    for sheet_name, df in sheets_dict.items():
        print(f"\nDescripción de la hoja: {sheet_name}")
        print(f"Número de filas: {df.shape[0]}")
        print(f"Número de columnas: {df.shape[1]}")
        print("\nPrimeras filas del DataFrame:")
        print(df.head())
        print("\nInformación del DataFrame:")
        print(df.info())
        print("\nResumen estadístico de las variables numéricas:")
        print(df.describe())
        print("\nResumen de las variables categóricas:")
        print(df.describe(include=['object']))

describe_dataset(sheets_dict)

# Significado y tipo de cada variable
def variable_types(sheets_dict):
    for sheet_name, df in sheets_dict.items():
        print(f"\nTipos de variables en la hoja: {sheet_name}")
        print(df.dtypes)

variable_types(sheets_dict)

# Cantidad de variables y observaciones
def count_variables_observations(sheets_dict):
    for sheet_name, df in sheets_dict.items():
        print(f"\nHoja: {sheet_name}")
        print(f"Cantidad de variables: {df.shape[1]}")
        print(f"Cantidad de observaciones: {df.shape[0]}")

count_variables_observations(sheets_dict)

# Exploración de variables numéricas
def explore_numeric_variables(sheets_dict):
    for sheet_name, df in sheets_dict.items():
        print(f"\nExploración de variables numéricas en la hoja: {sheet_name}")
        numeric_columns = df.select_dtypes(include=['int64', 'float64']).columns
        for col in numeric_columns:
            print(f"\nVariable: {col}")
            print(f"Media: {df[col].mean()}")
            print(f"Mediana: {df[col].median()}")
            print(f"Desviación estándar: {df[col].std()}")
            print(f"Mínimo: {df[col].min()}")
            print(f"Máximo: {df[col].max()}")
            print(f"Distribución:")
            print(df[col].value_counts())

explore_numeric_variables(sheets_dict)

# Exploración de variables categóricas
def explore_categorical_variables(sheets_dict):
    for sheet_name, df in sheets_dict.items():
        print(f"\nExploración de variables categóricas en la hoja: {sheet_name}")
        categorical_columns = df.select_dtypes(include=['object']).columns
        for col in categorical_columns:
            print(f"\nVariable: {col}")
            print(f"Frecuencia de categorías:")
            print(df[col].value_counts())

explore_categorical_variables(sheets_dict)

# Relaciones entre las variables
def explore_relationships(sheets_dict):
    for sheet_name, df in sheets_dict.items():
        print(f"\nRelaciones entre las variables en la hoja: {sheet_name}")
        numeric_columns = df.select_dtypes(include=['int64', 'float64']).columns
        if len(numeric_columns) > 1:
            print("Matriz de correlación:")
            print(df[numeric_columns].corr())
        else:
            print("No hay suficientes variables numéricas para calcular la correlación.")

explore_relationships(sheets_dict)

# Ejecutar todas las funciones
describe_dataset(sheets_dict)
variable_types(sheets_dict)
count_variables_observations(sheets_dict)
explore_numeric_variables(sheets_dict)
explore_categorical_variables(sheets_dict)
explore_relationships(sheets_dict)