# Ayudantía 4: REPASO
# Autora: Francesca Alej. Roco
# Segundo semestre 2025
# Metodología Cuantitativa con RStudio

# Repaso Prueba 
# - R básico
# - Tidyverse 
# - Data Wrangling


# R BÁSICO ---------------------------------------------------------------------

# 1.Crear datos y guardar objetos ####
edades <- sample(x = 22:61, size = 10, replace = TRUE)
ingresos <- sample(x = 120000:650000, size = 10, replace = TRUE)
escolaridad <- sample(x = 0:8, size = 10, replace = TRUE)

# 2.Crear data frame ####
desigualdad_poblacion <- data.frame(edades, ingresos, escolaridad)

# 3.Explorar data ####
summary(desigualdad_poblacion)
nrow(desigualdad_poblacion)
ncol(desigualdad_poblacion)
names(desigualdad_poblacion)
class(desigualdad_poblacion)
class(edades)
class(ingresos)
class(escolaridad)

# 4.Realizamos cálculos básicos ####
max(ingresos) - min(ingresos)
max(escolaridad) - min(escolaridad)

# 5.Calculamos estadísticas básicas ####
total_edades <- sum(edades)
promedio_ingresos <- mean(ingresos)
mediana_escolaridad <- median(escolaridad)

# 6.Observamos situaciones específicas con indexación ####
edad_menor_ingreso <- edades[which.min(ingresos)]
escolaridad_mayor_ingreso <- escolaridad[which.max(ingresos)]

# Conclusiones R Básico
# Los resultados indican que una baja escolaridad puede estar vinculada a grandes desigualdades en los ingresos,
# reflejando una distribución desigual de recursos y oportunidades.
# También se observa que los ingresos no dependen exclusivamente del nivel educativo.


# TIDYVERSE Y DATA WRANGLING ---------------------------------------------------

# Instalar y cargar paquetes 
pacman::p_load(tidyverse, openxlsx, readxl)

# Importar base de datos
base_antropologia_limpia <- read_excel("/Users/fran/Desktop/Ayudantías Cuanti 2 R/Ayudantías markdown/base_antropologia_limpia.xlsx")

# Verificar base cargada
ls()
View(base_antropologia_limpia)

# Explorar base
glimpse(base_antropologia_limpia)
summary(base_antropologia_limpia)
names(base_antropologia_limpia)

# Seleccionar variables ####
names(base_antropologia_limpia)

base_antropologia_limpia <- base_antropologia_limpia %>% 
  select(-c(marca, direc, n_encuestador))

seleccionadas <- base_antropologia_limpia %>% 
  select(ea_01_horas_estudio_semana, ea_02_horas_estudio_fin_semana, ea_04_notas_ultimo_semestre)

print(seleccionadas)

# Filtrar casos: comunas sector norte ####
unique(base_antropologia_limpia$comuna)

base_antropologia_limpia %>% 
  filter(comuna %in% c("huechuraba", "quilicura", "lampa", "pudahuel", "conchali", "renca"))

# Mutar categorías con mutate(case_when) ####
unique(base_antropologia_limpia$ea_06_nivel_estres_ultimo_semestre)

base_antropologia_limpia <- base_antropologia_limpia %>%
  mutate(nivel_estres_r = case_when(
    ea_06_nivel_estres_ultimo_semestre == 1 ~ "Muy poco estrés",
    ea_06_nivel_estres_ultimo_semestre == 2 ~ "Poco estrés",
    ea_06_nivel_estres_ultimo_semestre == 3 ~ "Algo de estrés",
    ea_06_nivel_estres_ultimo_semestre == 4 ~ "Suficiente estrés",
    ea_06_nivel_estres_ultimo_semestre == 5 ~ "Mucho estrés"
  ))

unique(base_antropologia_limpia$nivel_estres_r)

# Conclusiones Tidyverse 
# En este ejercicio aplicamos herramientas fundamentales del Tidyverse para importar, explorar,
# seleccionar, filtrar y recodificar datos. Estas herramientas nos ayudan a organizar la información
# y prepararla para análisis más profundos. También reforzamos el criterio al decidir qué variables y casos
# nos interesan, lo que es esencial en cualquier investigación cuantitativa.
