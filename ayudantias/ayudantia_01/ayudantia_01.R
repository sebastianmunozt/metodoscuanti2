#AYUDANTIA 01
#REPASO Y EJERCICIOS


#I. Elementos basicos -----------------------------------------------------------
#Diferencia R y Rstudio
#Pestañas de R.
#Estructura de una funcion

#Ejercicios
# Preguntas basicas iniciales: 
# a) Describe en tus propias palabras la diferencia entre R como lenguaje de programacion y RStudio.
# b) Enumera las pestañas principales de RStudio y describe brevemente su funcion.
# c) Explica que elementos tiene una función en R y cómo se estructura.

#Recuerden que esto está en el ppt


# II. Uso de R como calculadora -------------------------------------------------------
20+1 #sumar
22-1 #restar
10/2 #dividir
40*2 #multiplicar
5**2 #elevado a 2
log(2) #logaritmo
sqrt(21) #raiz cuadrada

#Ejercicios
# Realiza las siguientes operaciones matemáticas:
# 1. Suma 150 y 350.
# 2. Resta 1000 de 500.
# 3. Multiplica 25 por 43.
# 4. Divide 1000 por 250.

#1
150+350
#2
1000-500
#3
25*43
#4
1000/250

# III. Concatenación de Objetos ------------------------------------------------
#Agrupacion
# entrelazar, enlazar, juntar lo que esta dentro de parentesis: c()
#Todo lo que se una va a formar un vector
#Un vector es un conjunto de datos en una dimension (lo que es distinto a un valor)
#ejemplo: vector = conjunto de edades de los estudiantes del curso, valores = numero de las edades 
#una base de datos contiene vectores en distintas dimensiones, por ejemplo: edades y género

c(21, 12)
c(21, 12) + c(2, 1)
6 * c(3, 6, 9)

#Ejercicios
# Realiza y guarda los resultados en vectores:
# 1. Concatena los números 10, 20, y 30 en un vector llamado 'vector1'.
# 2. Concatena los números 5, 15, y 25 en un vector llamado 'vector2'.
# 3. Suma 5 a cada elemento de 'vector1' y guarda el resultado en 'resultado1'.

#1
vector1 <- c(10, 20, 30)

#2
vector2 <- c(5, 15, 25)

#3
resultado1 <- 5 + c(10, 20, 30)
#o también
resultado1 <- 5 + vector1


# IV. Operadores Lógicos ------------------------------------------------------
#Entrega verdaderos (TRUE) o falsos (FALSE) 
#Nos indican si se cumplen ciertas condiciones: TRUE or FALSE

5 == 2 #5 es igual a 2? Falso
5 != 2 #5 es distinto de 2? Verdadero
2 > 4 #Mayor
3 <= 2 # Menor o igual
3 & 7  # 3 y 7
10 | 2 # 10 o 2

4 | 2+3 == 2+2 | 5
#Sigue un orden logico de izq a derecha

c(38 <= 15, 3 < 5 & 6, 3 == 5)
#Las comas separan las operaciones

#Ejercicios
# Evalúa las siguientes expresiones lógicas y predice el resultado:
# 1. ¿Es 50 igual a 50?
# 2. ¿Es 100 diferente de 200?
# 3. ¿Es 30 mayor que 25 y menor que 50 al mismo tiempo?

#1
50==50

#2
100!=200

#3
#principales errores:
30 > 25 & 50 #30 es mayor a 25 y 50 al mismo tiempo?
(30 > 25) < 50  #30 es mayor a 25? esto seria menor a 50?

#respuesta correcta:
30 > 25 & 30 < 50


# V. Lenguaje orientado a objetos -----------------------------------------------------------------
#podemos crear objetos que se guarden como tales en distintos lenguajes, 
# es decir, podemos designar distintos datos a un objeto que podremos nombrar
#básicamente como nosotros determinemos, dependiendo de lo que busquemos

#Asignación al Global Enviroment
#asigna un valor al objeto y se guarda en el enviroment

p <- 20 # p va a tomar el valor de 20, almacenándose en el Enviroment

#Podemos Sobreescribir el objeto
a <- 5
b <- a
a <- 4
a
b

#¿Que valores puede tomar b? el ultimo valor que se le asigne 
#En este caso b queda con el valor de 5, porque es el primer valor que le dimos al objeto a
#nosotros cambiamos el valor de a despues de asignarle el valor a b
#entonces, b no va a tomar ese ultimo valor 
#para que b tomara el nuevo valor que le asignamos al objeto a, necesitamos repetir la operacion 
#que indica que b toma el valor de a, tomando asi el nuevo valor que le asignamos

#depende del orden, en este caso el orden si altera el producto 


#se pueden hacer operaciones directamente con los objetos:
presupuesto_2020 <- 20000
presupuesto_2021 <- 30000
presupuesto_total <- presupuesto_2020 + presupuesto_2021
presupuesto_total

#Ejercicios
# Asigna valores y realiza operaciones, guardando los resultados:
# 1. Asigna el número 100 a un objeto llamado 'numero1' y 200 a 'numero2'.
# 2. Suma 'numero1' y 'numero2', y guarda el resultado en 'suma_total'.
# 3. Multiplica 'numero1' por 2 y guarda el resultado en 'doble_numero1'.

#1
numero1 <- 100
numero2 <- 200

#2
suma_total <- numero1+numero2

#3
doble_numero1 <- 2 * numero1


# VI. Funciones básicas [seq, rep] ---------------------------------
# Generación de secuencias y repeticiones 

#Secuencias
seq(from = -3, to = 6, by = 1) #secuencia del -3 al 6 con un salto de 1 en 1
seq(from = 2, to = 10, by = 2) #secuencia del 2 al 10, de 2 en 2
seq(from = 1, to = 10, by = 1)
1:10 #otra forma, solo con salto de 1 en 1 
#(los dos puntos significan que la secuencia va de un numero hasta el otro)

seq(from = 1, to = 10, by = 1) == 1:10


seq(from = 0, to = 1, length.out = 11) #longitud 
seq(from = 0, to = 1, length.out = 30)

#Repeticiones
rep(21, times = 21)
rep(c(3,2,1), times = 3) #repetición del vector 3,2,1, en 3 veces
rep(1:6, times = 2) #repetición del 1 al 6, 2 veces
rep(1:4, times = 2) #repetición del 1 al 4, 2 veces


#22334456
rep(2:6, times = c(2, 2, 2, 1, 1)) #repetición del 2 al 6, que va a tener los
#tiempos estimados en la concatenación anterior
#esto es: (dos veces, dos veces, dos veces, una vez, una vez) 
#cada número de la secuencia se va a repetir esa cantidad de veces (en orden)

#rep 2 4 y 6 cada uno tres veces
rep(c(2,4,6), each = 3) #que se repita cada uno 3 veces
rep(1:4, each = 2)
rep(1:3, each = 2)

#Se utilizan distintos terminos para distintos objetivos
#Por ejemplo, para destimar la cantidad de veces que se repite
#EACH: Cada uno se repite
#TIMES: Se repiten todos

#Mezclando distintas funciones
Repetición_pares <- seq(from = 2, to = 10, by = 2) 

seq(from = 1, to = 10, by = 1) > 6 #vector con una secuencia con números del 1 al 10
#cuales valores de la secuencia son mayores a 6?

#Agregando mas cosas
which(seq(from = 1, to = 17, by = 1) > 12) #nos entrega la posición, de qué número en adelante
#donde estan los numeros de la secuencia mayores a 12 (en que posicion)

#Ejercicios
# 1. Crea una secuencia del 1 al 20 con intervalo de 2 y guárdala en 'secuencia1'.
# 2. Repite el número 5, diez veces y guarda el resultado en 'repeticion1'.
# 3. Crea una secuencia del 10 al 50 con un intervalo de 5 y guárdala en 'secuencia2'.

#1
secuencia1 <- seq(from = 1, to = 20, by = 2)

#2
repeticion1 <- rep(5, times = 10)

#3
secuencia2 <- seq(from = 10, to = 50, by = 5)


# VII. Indexación y data.frame ----------------------------------------------------------------
#Data frame: base de datos que incluye distintas variables
#Ejemplo: data frame de caracteristicas de los estudiantes del curso
#contiene vectores en distintas dimensiones: edad, tipo de colegio, tendencia politica, genero

##DATA FRAME
#seleccion de las variables
edad <- c(22, 18, 25, 20, 21, 20)
tipo_colegio <- c("privado", "publico", "publico", "privado", "publico", "publico")
tendencia_politica <- c("izquierda", "derecha", "centro", "izquierda", "centro", "centro")
genero <- c("mujer", "hombre", "mujer", "otro", "mujer", "hombre")

#creacion de una base de datos con las caracteristicas de los estudiantes del curso
data.frame(edad, tipo_colegio, tendencia_politica, genero)

#para guardar la base de datos 
estudiantes_antropologia <- data.frame(edad, tipo_colegio, tendencia_politica, genero)

#ver la data
View(estudiantes_antropologia)

##INDEXAR
#Rescatar objetos de una data frame o base de datos
#se representa por los corchetes [x]

#Puedes indexar un vector desde una data frame
#Puedes rescatar ciertos elementos de una base de datos 
#depende de lo que quieras hacer, en qué quieras trabajar

edad[edad > 20] #quiero rescatar las edades mayores a 20
edad[edad < 20] #quiero rescatar las edades menores a 20
edad[edad < 18] #quiero rescatar las edades menores a 20

# Explorar la base de datos
str(estudiantes_antropologia) # Estructura de la base
#4 variables con 6 observaciones

nrow(estudiantes_antropologia) # Número de filas CASOS
ncol(estudiantes_antropologia) # Número de columnas VARIABLES

View(estudiantes_antropologia) # Abrir la base de datos

# Llamar a una variable de la base
estudiantes_antropologia$edad #llamar a la variable edad que contiene la base
estudiantes_antropologia$tipo_colegio[1] #llamar la posicion 1 de la variable tipo de colegio
estudiantes_antropologia$genero[4] #llamar la posicion 4 de la variable genero

# Buscar en la base de datos [fila, columna]
#lado izq fila, lado derecho columna
estudiantes_antropologia[1,] # Rescato todo lo de la fila 1
estudiantes_antropologia[,1] # Rescato todo lo de la columna 1
estudiantes_antropologia[,2] # Rescato todo lo de la columna 2
estudiantes_antropologia[,3] # Rescato todo lo de la columna 3
estudiantes_antropologia[,4] # Rescato todo lo de la columna 4

estudiantes_antropologia[, c(2,3)] # Rescato columnas 2 y 3
estudiantes_antropologia[, "edad"] # Rescato columna edad
estudiantes_antropologia$genero == "mujer" # Donde el género es mujer

#Ejercicios
# Con los vectores dados, crea un data.frame llamado 'estudiantes_antropologia' y realiza las tareas solicitadas:
# Edad, Sexo, Tendencia Política, Ingreso Familiar, Comuna de Residencia
# Utiliza los siguientes vectores para crear un data.frame llamado 'estudiantes_antropologia':
edad <- c(22, 18, 25, 20, 21, 19, 23, 24, 26, 22, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36)
sexo <- c("masculino", "femenino", "no-binario", "femenino", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario", "femenino", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario")
tendencia_politica <- c("izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha")
ingreso_familiar <- c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000, 10500)
comuna_residencia <- c("Providencia", "Las Condes", "Ñuñoa", "Santiago", "Vitacura", "La Reina", "Peñalolén", "Macul", "La Florida", "Puente Alto", "Maipú", "Pudahuel", "Cerrillos", "Quilicura", "Recoleta", "Independencia", "Conchalí", "Renca", "Cerro Navia", "Lo Prado")

#Ya estan las variables, así que ahora creamos la data que incluya esas variables
estudiantes_antropologia <- data.frame(edad, sexo, tendencia_politica, ingreso_familiar, comuna_residencia)


# Funciones básicas -------------------------------------------------------

min() #para obtener el mínimo de un vector
max() #para obtener el máximo de un vector
length() #para obtener la longitud de un vector, conteo de casos
range() #para obtener el rango de valores de un vector
sum() #entrega la suma de todos los elementos de un vector
prod() #multiplica todos los elementos de un vector
which.min() #entrega la posición del valor min de un vector
which.max() #entrega la posición del valor máximo del vector
rev() #invierte un vector
str() #estructura de la base
nrow() #número de filas
ncol() #número de columnas

#MTC
mean() #media
median() #mediana
sd() #desviacion estandar

#Ejercicios
# 1. Calcula la edad promedio de los estudiantes.
# 2. Calcula la desviación estándar de la edad de los estudiantes.
# 3. Muestra la información del décimo estudiante.
# 4. Muestra todas las comunas de residencia de los estudiantes.

#1
mean(estudiantes_antropologia$edad) # Edad promedio

#2
sd(estudiantes_antropologia$edad) # Desviación estándar de la edad

#3
estudiantes_antropologia[10, ] # Información del décimo estudiante

#4
estudiantes_antropologia$comuna_residencia # Columna de comunas de residencia

