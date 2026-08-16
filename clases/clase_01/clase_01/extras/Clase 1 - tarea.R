#clase 1: tarea personal

# carga de datos
data()
data("AirPassengers")
AirPassengers

#media y mediana
mean (AirPassengers)
median (AirPassengers)
summary(AirPassengers)


#sumas
112+118

#asignación
Ene1949 <- 112
Ene1949

#remover
rm(Ene1949)

#listado de objetos
ls()

#ayudas
help(ls)



#práctica
AirPassengers

en1950 <- 115
en1960 <- 417

en1950 + en1960
en1960 - en1950 


#creación de vectores
Air49<- c(112,118,132,129,121,135,148,148,136,119,104,118)


#indexación
Air49 [2]

#secuencias
y <- 1:10
y


#creación de vectores mediante una selección
Air49 <- AirPassengers[1:12]

#sum
sum(Air49)

#length
length(Air49)

#práctica
AirPassengers
Air50 <- AirPassengers[13:24]
Air50[2]
length(Air50)
sum(Air50)

#creación de distintos vectores
Air49 <- AirPassengers[1:12]
Air50 <- AirPassengers[13:24]
Air51 <- AirPassengers[25:36]
Air52 <- AirPassengers[37:48]
Air53 <- AirPassengers[49:60]
Air54 <- AirPassengers[61:72]
Air55 <- AirPassengers[73:84]
Air56 <- AirPassengers[85:96]
Air57 <- AirPassengers[97:108]
Air58 <- AirPassengers[109:120]
Air59 <- AirPassengers[121:132]
Air60 <- AirPassengers[133:144]

sum(Air49)
sum(Air50)
sum(Air51)
sum(Air52)
sum(Air53)
sum(Air54)
sum(Air55)
sum(Air56)
sum(Air57)
sum(Air58)
sum(Air59)
sum(Air60)



# Trabajar con bases de datos más grandes ---------------------------------

data("mtcars")
mtcars

#seleccionar filas (x), columnas (y) mediante indexación
#dataset[x,y]

#selección primera fila
mtcars [1,]

#selección segunda columna (cyl)
mtcars [,2]

#selección datos individuales
mtcars [1,2]

#selección de una columna y aplicación de función
#millas por galón en diferentes autos
summary (mtcars [,1])


# Matrices ----------------------------------------------------------------

Robos <-  c(2,30,38, 13)
RobosViolentos <- c(7,20,36,3)

#unir columnas (col)
Crimen <- cbind(Robos, RobosViolentos)
Crimen

#unir filas (row)
Crimen2 <- rbind(Robos, RobosViolentos)
Crimen2

#mediante matrix
matrix(c(2,30,3,4,7,20,36,3),nrow=2)


#appy()
#argumentos: apply (matriz_utilizar, terminos, función)
#1 = rows (filas)
#2 = columns (columnas)


apply(Crimen, 1, mean)
apply(Crimen, 2, mean)
help(apply)



#práctica: 
#1.
matrix(c(2,3,3,44,51,17),ncol=2)


#2. 
QuebrantamientoPaz <- c(2,3,3) 
Asesinatos <- c(44,51,17)


PazAsesinatos <- cbind(QuebrantamientoPaz, Asesinatos)
PazAsesinatos

#3. 
apply(PazAsesinatos, 2, mean)

