### PRUEBA 1 ###
# Parte 1: R base ---------------------------------------------------------
#0.1 Crear un VECTOR llamado AGE que contenga las edades de los/as miembros/as de su familia 

#0.2 Modificar el segundo elemento de AGE, asignándole el valor 30

#0.3 Crear 3 vectores (llamarlos NAME, PARENTESCO, COMUNA_RES) 
# con la misma cantidad de elementos que EDAD, pero con variables de tipo character (texto).
# En la variable PARENTESCO: asigne los valores: Padre, Madre, Hijo, Hija, Tía, etcétera

#0.4 Crear un dataframe (llamarla: BBDD) que contenga todos los vectores creados previamente

#0.5 Crear un objeto llamado PRIMERA_FILA que guarde el valor contenido de la primera fila del dataframe BBDD

#0.6 Crear un objeto llamado SEGUNDA_COLUMNA que guarde el valor contenido de la segunda columna del dataframe BBDD

#0.7 Crear un objeto llamado OTRA_COSA que guarde el valor contenido de la segunda fila y tercera columna del dataframe BBDD



# Parte 2: Abrir, guardar e indexar bases de datos ---------------------------------------
#01. Cree una carpeta, realice un proyecto e importen la base de datos base_covid_sample.RDS
#detalle la ruta desde el punto de partida del proyecto y asignándola a bbdd_covid
#considere el tipo de archivo y dónde se sitúa

  
#02. Supongan que sólo les interesa trabajar con las variables edad, clasificacion_resumen y  fallecido. 
# Utilice el método de acceso de los CORCHETES [] para crear un nuevo objeto que contenga: 
# todas las filas de la base, pero solo esas columnas (edad, clasificacion_resumen y  fallecido)

#02.a. realicelo con el método de NOMBRE de las variables, guarde lo realizado como bbdd_covid_r1

#02.c realicelo con la POSICIÓN de las variables, guardelo como bbdd_covid_r2.

#02.d observe si son iguales mediante dim()


#03. Tome esa misma base (bbdd_covid_r1) y con el método de acceso de corchetes [] conserve  los primeros 200 casos/filas
#guardela en bbdd_covid_r3


#04. borre los objetos del "Enivornment"

# Parte 3: tidyverse: select, filter, mutate ---------------------------------------------------

#01. Importar la base de datos de casos COVID (base_covid_sample.RDS) y cargar tidyverse
#asignela con el nombre bbdd_covid


#02. Crear un objeto nuevo (bbdd_covid2) y seleccionar entre 3 y 6 columnas de interés COMBINANDO la función select() con:
# El nombre de las variables
# La posición de las variables
# Un patrón común en el nombre de las variables (starts_with() - ends_with() - contains())

# NO UTILICE LAS DEL EJERICIO EN CLASE!



#03. Crear un objeto nuevo (bbdd_covid3) que contenga:
# únicamente a la población residente en CABA y Mendoza 
# y sólo los mayores de 16


#04. mediante la función table () observe si su filtro está bien realizado para residencia_provincia_nombre

#0.5 mediante la función table () observe si su filtro está bien realizado para edad


#06. Crear una nueva base (bbdd_covid4) recodificando la variable edad en 4 rangos etáreos.
# guardela como edadr en una nueva base llamada: base_covid4

#07. compruebe lo realizado con una table() a edadr


#08. Para finalizar: 
#a) Guarde el script como prueba1_nombresuyo_apellido (ejemplo: prueba1_sebastian_muñoz) en la misma carpeta donde se sitúa su proyecto
#b) Envíe los archivos en la tarea
#c) Envíe el script por mail a: semunoz@uahurtado.cl



