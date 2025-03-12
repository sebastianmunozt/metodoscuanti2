# Script de R - Ejercicios Resueltos

# I. Preguntas básicas iniciales ----------------------------------------------
# a) R es un lenguaje de programación principalmente usado para análisis estadístico y gráficos. RStudio es un IDE que proporciona una interfaz de usuario amigable para R, facilitando la escritura de código, visualización de datos y manejo de archivos.
# b) Las pestañas de RStudio incluyen:
#    Consola: lugar donde se ejecutan comandos de R.
#    Source: editor de scripts para escribir y ejecutar código.
#    Environment/History: muestra los objetos en el entorno actual y el historial de comandos.
#    Files/Plots/Packages/Help/Viewer: manejo de archivos, visualización de gráficos, gestión de paquetes, acceso a ayuda y visualización de contenido web respectivamente.
# c) Una función en R tiene un nombre, argumentos (entradas) y un cuerpo. El cuerpo contiene el conjunto de instrucciones que ejecuta la función, y puede devolver un valor como resultado.

# II. Uso de R como calculadora -------------------------------------------------
150 + 350
1000 - 500
25 * 43
1000 / 250

# III. Concatenación de Objetos ------------------------------------------------
vector1 <- c(10, 20, 30)
vector2 <- c(5, 15, 25)
resultado1 <- vector1 + 5

# IV. Operadores Lógicos -----------------------------------------------------
50 == 50
100 != 200
30 > 25 & 30 < 50

# V. Lenguaje orientado a objetos --------------------------------------------
numero1 <- 100
numero2 <- 200
suma_total <- numero1 + numero2
doble_numero1 <- numero1 * 2

# VI. Funciones básicas [seq, rep] ---------------------------------------------
secuencia1 <- seq(1, 20, by = 2)
repeticion1 <- rep(5, times = 10)
secuencia2 <- seq(10, 50, by = 5)

# VII. Indexación y data.frame -------------------------------------------------
# Crear data.frame llamado 'estudiantes_antropologia':
edad <- c(22, 18, 25, 20, 21, 19, 23, 24, 26, 22, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36)
sexo <- c("masculino", "femenino", "no-binario", "femenino", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario", "femenino", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario", "masculino", "femenino", "no-binario")
tendencia_politica <- c("izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha", "centro", "izquierda", "derecha")
ingreso_familiar <- c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000, 10500)
comuna_residencia <- c("Providencia", "Las Condes", "Ñuñoa", "Santiago", "Vitacura", "La Reina", "Peñalolén", "Macul", "La Florida", "Puente Alto", "Maipú", "Pudahuel", "Cerrillos", "Quilicura", "Recoleta", "Independencia", "Conchalí", "Renca", "Cerro Navia", "Lo Prado")


estudiantes_antropologia <- data.frame(edad, sexo, tendencia_politica, ingreso_familiar, comuna_residencia)

# Operaciones con data.frame:
mean(estudiantes_antropologia$edad) # Edad promedio
sd(estudiantes_antropologia$edad) # Desviación estándar de la edad
estudiantes_antropologia[10, ] # Información del décimo estudiante
estudiantes_antropologia$comuna_residencia # Columna de comunas de residencia
