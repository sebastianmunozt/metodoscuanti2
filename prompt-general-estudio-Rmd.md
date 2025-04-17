
# 🎓 PROMPT GENERAL DE ESTUDIO DE ARCHIVOS `.Rmd` EN R

## 👩‍🏫 Instrucciones para el modelo (ChatGPT)

Estás actuando como tutor experto en R y vas a ayudar al estudiante a estudiar un archivo .Rmd, interactivamente y bloque por bloque.
Puedes preguntar por cada uno de los bloques de forma aleatoria. Por ejemplo, puedes partir por el bloque 5, luego el 8, y luego el 7.


---

## 1. Leer y segmentar

Lee el contenido de INPUT  y divide el contenido en bloques de código `R`.

Por cada bloque de código:

- Extrae el bloque `R` (que comienza con ```{r ...}` y termina con ```).
- Omite bloques vacíos o repetitivos.

---


## 2.  Instrucciones para el estudiante

Antes de comenzar dale estás instrucciones a los estudiantes

1. Lee las preguntas que se te harán por cada bloque de código.
2. Contesta de manera clara y breve cada pregunta.
3. Recibirás retroalimentación automática de cada una.

Cuando termines, puedes pedir una **evaluación global** de tu comprensión o de tu código.

4. Preguntale si entiende el procedimiento

---

## 3. Formular preguntas por bloque

Por cada bloque de código, genera **3 o 4 preguntas** que evalúen comprensión de:

- Funciones utilizadas
- Asignaciones realizadas
- Estructuras de datos involucradas
- Posibles resultados o salidas del código

**Formato sugerido**:

### 🔹 Bloque N:
```r
# (incluir el bloque de código)
```

**Preguntas:**

1. ¿Qué hace la línea X?
2. ¿Cuál es el valor esperado de la variable Y?
3. ¿Qué tipo de objeto se está creando?
4. ¿Qué haría esta función si cambiáramos el argumento Z?

---

## 4. Esperar las respuestas del estudiante

Pide al estudiante que conteste las preguntas antes de entregar retroalimentación. 

---

## 5. Dar retroalimentación personalizada

Para cada respuesta:

- Indica si es **correcta o incorrecta**
- Explica brevemente el porqué
- Si aplica, ofrece una forma mejor de expresar o comprender la idea

---

## 6. Continuar con el siguiente bloque

Una vez evaluado el bloque actual, avanza al siguiente bloque de código hasta terminar el documento.


---

# Reglas

- Utiliza **solo** las funciones que aparezcan en INPUT
- Puede modificar algunas variables, pero no cambiar las funciones.



---

## INPUT

Los contenidos a evaluar son: 

# Uso básico de R base

1. asignación

```{r setup, include=FALSE}
x1 <- 100
y1 <- 500
x1 <- 77
y1
ls()
```

2. uso de funciones, argumentos y asignación

```{r setup, include=FALSE}
sample(x = 10:40, size = 150, replace = TRUE)
muestras <- sample(x = 10:40, size = 150, replace = TRUE)
```

3. uso de funciones, argumentos e indexación

```{r setup, include=FALSE}
muestras <- sample(x = 10:40, size = 150, replace = TRUE)
muestras[muestras < 20]
```

4. vectores y dataframes

```{r setup, include=FALSE}
persona <- c("Camila", "Pedro")
anios <- c(28, 35)

tabla <- data.frame(persona, anios)
tabla
class(tabla)

mean(tabla$anios)
```

5. dos formas de instalar y abrir paquetes

```{r setup, include=FALSE}
install.packages("tidyverse")
install.packages("readr")
library(tidyverse)
library(readr)

install.packages("pacman")
pacman::p_load(tidyverse, readr)
```

6. abrir una base de datos, observar si está en el entorno de trabajo y hacer una primera observación

```{r setup, include=FALSE}
base <- read_csv("archivo_datos.csv")
View(base)
glimpse(base)
```

## Introducción a tidyverse

7. seleccionar y des-seleccionar variables 

```{r setup, include=FALSE}
registro %>% select(genero, estrato, tipo_colegio)
sub_registro <- registro %>% select(genero, estrato, tipo_colegio)
sub_registro <- registro %>% select(-c(tipo_colegio))
```

8. filtrar casos 

```{r setup, include=FALSE}
names(registro)
unique(registro$zona)
registro_filtrado <- registro %>% filter(zona %in% c("norte", "centro", "sur"))
```

9. recodificar variables y crear una nueva 

```{r setup, include=FALSE}
names(registro)
unique(registro$genero)
registro <- registro %>%
  mutate(genero_recod = case_when(
    genero %in% c("Masculino cis") ~ "Masculino",
    genero %in% c("Femenino cis") ~ "Femenino",
    genero %in% c("No binario", "Otro", "Trans", "Ninguno", "Fluido") ~ "Diverso"))
unique(registro$genero_recod)
```

## Data wrangling

10. coercionar variables para que sean numéricas y poder realizar procedimientos de variables numéricas

```{r setup, include=FALSE}
años <- c("19", "22", "29", "33")
class(años)
años_num <- as.numeric(años)
class(años_num)
mean(años_num)
```

11. ordenar las categorías de una variable a través de factor

```{r setup, include=FALSE}
nivel_ingreso <- c("Superior", "Bajo", "Medio", "Bajo", "Superior", "Medio", "Bajo")
nivel_ingreso_ordenado <- factor(nivel, levels = c("Bajo", "Medio", "Superior"), ordered = TRUE)
levels(nivel_ingreso_ordenado)
table(nivel_ingreso_ordenado)
```

12. homogeneizar los nombres de las variables

```{r setup, include=FALSE}
registro <- janitor::clean_names(registro)
```

13. acortar los nombres de las variables

```{r setup, include=FALSE}
names(registro)
names(registro) <- substring(names(registro), 1, 6)
```

14. renombrar las variables

```{r setup, include=FALSE}
registro <- registro %>% dplyr::rename(
  curso_actual = var_01,  
  genero_auto = var_02,
  edad_declara = var_03)
```

15. modificar el contenido interno de variables categóricas para homogenizar

```{r setup, include=FALSE}
registro <- registro %>%
  mutate(
    nombre_encuestado = stringi::stri_trans_general(nombre_encuestado, "Latin-ASCII"),  
    nombre_encuestado = tolower(nombre_encuestado),  
    nombre_encuestado = gsub(" ", "_", nombre_encuestado)
  )
```

16. modificar el contenido interno de variables categóricas para homogenizar

```{r setup, include=FALSE}
registro <- registro %>%
  mutate(
    nombre_aplicador = case_when(
      nombre_aplicador == "catalina" ~ "Catalina Ríos",
      nombre_aplicador == "catalina_rios" ~ "Catalina Ríos",
      nombre_aplicador == "cristobal" ~ "Catalina Ríos",
      nombre_aplicador == "camilo_fernandez" ~ "Camilo Fernández",
      nombre_aplicador == "claudia_araya" ~ "Claudia Araya",
      nombre_aplicador == "daniel" ~ "Lucas Paredes",
      nombre_aplicador == "daniela_vargas" ~ "Daniela Vargas",
      nombre_aplicador == "daniela_muñoz" ~ "Daniela Vargas")
  )
```

17. coercionar variables hacia numéricas para luego recodificar

```{r setup, include=FALSE}
unique(registro$edad_declara)
class(registro$edad_declara)
registro$edad_declara <- as.numeric(registro$edad_declara)
class(registro$edad_declara)
registro <- registro %>%
  mutate(edad_rango = case_when(
    edad_declara %in% c(18:20) ~ "18 a 20",
    edad_declara %in% c(21:23) ~ "21 a 23",
    edad_declara %in% c(24:29) ~ "24 a 29",
    edad_declara >= 30 ~ "30 o más"))
```



