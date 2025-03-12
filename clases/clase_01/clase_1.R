
# Repaso y Razonamiento programación

#I. Elementos básicos -----------------------------------------------------------

#1. r y rstudio
#r: lenguaje de programación !- a software
#r: interfaz gráfica (IDE: Interfaz de desarrollo integrado)

#2. Pestañas de R.
#a. donde se escriben líneas de código
#b. consola de resultados
#c. environment: donde se sitúan los objetos (lenguaje orientado a objetos)
#d.1 Files (documento en proyecto), Plots (Gráficos), Packages (librerias)
#d.2. Help (ayuda), Viewer y presentation (presentaciones)

#3. File, new file, r script
#a. Script de trabajo
#b. disquete: guardar como... ¿Qué es un proyecto? 
#c: búscar en script (lupa)

#4. ctrl+mayúscula (shift) + r: insertar sección 
#Observar las secciones realizadas
# los 4 #


# II. R como calculadora ------------------------------------------------------

2+2
3*2+5*3

#¿Cómo ejecutar comandos?: 
#Click Run 
#shortcut: situarse en línea CTRL+Enter

#ej. Realice otras operaciones + - * / **


#Limpiezas: escobas: en consola y en environment

#1. Ejecución de funciones
2+2+3+4
2+2+sum(3,4)
  
?sum

#función como el resumen de un procedimiento que genera una acción
#usar tab irán mostrando las posibles argumentos que se inician con lo que se nombra
#se luego de nombrarla, parentesis ()
#al interior los argumentos: elementos que tiene la función, por defecto o elegidos, argumentos se separan por comma


# III. Concatenación de Objetos -------------------------------------------
# entrelazar, enlazar, juntar lo que está dentro de parentesis: c()

c(5,2,1)
c(5,2,1) + c(3,6,2)
6* c(2,4,6)


# IV. Operadores Lógicos --------------------------------------------------
#entrega verdaderos (TRUE) o falsos (FALSE) 

30 > 10 # mayor (posterior, para ir explicando lo que sucede)
20 < 30 # menor
20 == 10 #igualdad 
10 != 5 #no es igual 

#esto permitirá,por ejemplo, filtrar bases de datos


# V. Lenguaje orientado a objetos -----------------------------------------

#1. Asignación al Global Enviroment

#shortcut: alt + -
x <- 15
x


y <- 5
y

#permite utilizar esos objetos ya asignados

x + y
x == y
x != y


mi_objeto <- 200
sqrt(mi_objeto)* log(5)

operacion <- sqrt(mi_objeto)* log(5)

#sobreescribir o "pisar"

operacion <- mi_objeto*2
operacion


#objetos serán diversos, ¿cuáles?
vector <- c(3,4,5)
vector*2

a <- 5
b <- a
a <- 4
b

#¿Por qué sigue siendo 5 b?


# VI. Secuencias y Repeticiones ----------------------------------------------

seq (from =-3, to =6, by=1)

#seq: r haz una secuencia
#(argumentos separados por una coma)
#from: desde
#to: hasta
#by: con un intervalo de; salto

#ej: haga su secuencia
seq (from= -100, to=100, by=10)

#otras formas:
1:10 # cuando es lineal, de 1 

rep(c(-1,0,1), times=5)
rep (c(1,2,3), times=100)

#repetir
#algo
#una cierta cantidad de veces

pares <-  seq(from =2, to=10, by=2)
rep (pares, times=10)

anios <- seq(from=2010, to=2022, by=1)
anios


# ex: haga una secuencia de su año de nacimiento hasta ahora
# pongale: anios_sunombre

rep (anios, each=12) # repite cada elemento
rep (anios, times=12) # repite toda la secuencia

#¿Cómo hacer lo siguiente?
# 2 2 3 4 4 4 5
rep (2:5, times = c(2, 1, 3,1))

#cómo hacer 2,3 y 6 tres veces
rep (c(2,4,6), each =3)



# VI. Otras funciones Básicas ---------------------------------------------------

sample(x= 20:50, size =56, replace = TRUE)
edades <- sample(x= 20:50, size =56, replace = TRUE)

#x=20:50: Este argumento define el conjunto de datos o vector del cual se extraerán las muestras. 
#size=56: Este argumento especifica el tamaño de la muestra a generar
#replace=TRUE, significa que un elemento puede ser seleccionado más de una vez en la muestra


#esto es un vector: 
#int, num: cuanti
#tiene 56 elementos

#1. algunas funciones
edades
min(edades)
max(edades)
which.min(edades) #donde está el menor elemento, posición
which.max(edades) #donde está el mayor elemento, posición

edades
#valores []: son posiciones

#señalar posiciones
edades [5]

mean(edades)
var(edades) # varianza: dispersión de los datos respecto a su media.
sqrt(var(edades)) #desvio estandar: dispersión de los datos respecto a su media, comparable directamete con datos. 
sd (edades) # es lo mismo que arriba
class(edades) # tipo de objeto
length(edades) # largo de objeto
unique(edades) # categoría de objetos




# VII. Indexación ---------------------------------------------------------

edades
edades [5] # rescatar posición 5

# de esto, ¿qué rescatará [3]?

#2. Rescatar más objetos

edades [c(2, 10, 30)]


#cantidad de evaluaciones docentes en una universidad
sede_1 <-  c(80, 90, 50, 40, 35)
sede_2 <-  c(75, 68, 50, 90, 98)

dias_encuestas <-  c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes")
# por qué con ""
# qué tipo de variable son?

dias_encuestas [3]

#asignar etiquetas a un vector indefinido
names(sede_1) <- dias_encuestas
sede_1

#ej. ¿Cómo hacerlo con sede_2


#¿Cuántas encuestas se realizaron en sede_1
total_sede_1 <- sum (sede_1)
total_sede_2 <-  sum (sede_2)

# ¿Dónde se contestaron más encuestas?
total_sede_1 > total_sede_2

#ojo: procesos de ir escalando, ir relacionando elementos, bola de nieve

#¿Cuál es el día con mayor cantidad de encuestas?
which.max(sede_1)

#¿Cuántas se hicieron?
sede_1[2]



# VIII.Data frames ---------------------------------------------------------
# Definir las variables
edad <- c(22, 18, 25, 20, 21)
tipo_colegio <- c("privado", "publico", "publico", "privado", "publico")
tendencia_politica <- c("izquierda", "derecha", "centro", "izquierda", "centro")
genero <- c("mujer", "hombre", "mujer", "mujer", "hombre")

# Generar el data.frame
data.frame(edad, tipo_colegio, tendencia_politica, genero)

# Para guardar la base de datos
estudiantes_antropologia <- data.frame(edad, tipo_colegio, tendencia_politica, genero)

# Explorar la base de datos
str(estudiantes_antropologia) # Estructura
nrow(estudiantes_antropologia) # Número de filas
ncol(estudiantes_antropologia) # Número de columnas
View(estudiantes_antropologia) # Abrir la base de datos

# Llamar a una variable de la base
estudiantes_antropologia$edad
estudiantes_antropologia$tipo_colegio[1]

# Buscar en la base de datos [fila, columna]
estudiantes_antropologia[1,] # Rescato todo lo de la fila 1
estudiantes_antropologia[,2] # Rescato todo lo de la columna 2
estudiantes_antropologia[, c(2,3)] # Rescato columnas 2 y 3
estudiantes_antropologia[, "edad"] # Rescato columna edad
estudiantes_antropologia$genero == "mujer" # Donde el género es mujer

# Uso de %in% para filtrar
estudiantes_antropologia$tendencia_politica %in% c("izquierda", "centro")



# Bases de datos ----------------------------------------------------------

#.1 Construir una base de datos que contenga datos de 10 personas, considerando:
#género, edad, ingresos, nombre, puntaje en la PAES
#para las variables cuantitativas obtenga la media
#seleccione el caso que sacó mejor puntaje mediante indexación

