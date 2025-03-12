### PRUEBA 1 ###

# Parte 1: R base ---------------------------------------------------------
#0.1 Crear un VECTOR llamado EDAD que contenga las edades de los/as miembros/as de su familia 

#0.2 Modificar el tercer elemento de EDAD, asignandole el valor 30

#0.3 Crear 3 vectores (llamarlos NOMBRE, PARENTESCO, COMUNA_RESIDENCIA) 
# con la misma cantidad de elementos que EDAD, pero con variables de tipo character (texto).
# PARENTESCO: asigne los valores: Padre, Madre, Hijo, Hija

#0.4 Crear un dataframe (llamarla: BASE_DATOS) que contenga los vectores creados previamente

#0.5 Crear un objeto llamado OTRA_COSA que guarde el valor contenido de la segunda fila del dataframe BASE_DATOS

#0.6 Crear un objeto llamado OTRA_COSA1 que guarde el valor contenido de la tercera columna del dataframe BASE_DATOS

#0.7 Crear un objeto llamado OTRA_COSA2 que guarde el valor contenido de la segunda fila y tercera columna del dataframe BASE_DATOS



# Parte 2: Abrir, guardar e indexar bases de datos ---------------------------------------
#01. Cree una carpeta en el escritorio (eg. Prueba 1), cree un proyecto e importen la base de datos sobre los casos de covid 
#detallando la ruta desde el punto de partida del proyecto y asignandola a base_covid
#recuerden que dicha base es una muestra del 2% de los casos diarios

base_covid <- readRDS(file = "fuente/base_covid_sample.RDS")
  
#02. Supongan que sólo les interesa trabajar con las variables edad, clasificacion_resumen y  fallecido. 
# para saber los nombres utilice función names ()
# Utilice el método de acceso de los corchetes [] para crear un nuevo objeto que contenga: 
# a) todas las filas de la base, 
# b) pero solo esas columnas
#02.1 realicelo con el nombre de las variables (base_covid_r1)
#02.2 realicelo con la posición de las variables (base_covid_r2)
#02.3 observe si son iguales 
names(base_covid)
base_covid_r1 <- base_covid [, c("edad", "clasificacion_resumen", "fallecido")]
base_covid_r2 <- base_covid [, c(3, 21, 15)]


#03. Tome esa misma base (base_covid_r1) y con el método de acceso de corchetes [] conserve  los primeros 200 casos/filas
#guardela en base_covid_r3
base_covid_r3 <- base_covid [1:200,]

#06. Borre todos los objetos del Environment
rm(list=ls())


# Parte 2: tidyverse: select, filter, mutate ---------------------------------------------------

#01. Importar la base de datos de casos COVID (base_covid_sample.RDS) y cargar tidyverse
#asignela con el nombre base_covid 
library(tidyverse)
base_covid <- readRDS(file = "fuente/base_covid_sample.RDS")


#02. Crear un objeto nuevo (base_covid2) y seleccionar entre 3 y 6 columnas de interés combinando la función select() con:
# El nombre de las variables
# La posición de las variables
# Un patrón común en el nombre de las variables (starts_with() - ends_with() - contains())
# para observar el nombre de las variables puede hacerlo con names ()
# NO UTILICE LAS DEL EJERICIO EN CLASE!

names(base_covid)
base_covid2 <- base_covid %>% 
  dplyr::select(sexo, 3, starts_with(match = "fecha"), ends_with(match = "id"), contains(match = "nombre"))


#03. Crear un objeto nuevo (base_covid3) que contenga:
# únicamente a la población residente en CABA, Mendoza y Córdoba
# y sólo los mayores de 18

names(base_covid)
unique(base_covid$residencia_provincia_nombre)

base_covid3 <- base_covid %>% 
  filter(residencia_provincia_nombre %in% c("CABA", "Mendoza", "Córdoba"), 
         edad >= 18)

#04. mediante la función table () observe si su filtro está bien realizado para residencia_provincia_nombre
table(base_covid3$residencia_provincia_nombre)

#0.5 mediante la función table () observe si su filtro está bien realizado para edad
table(base_covid3$edad)

#06. Crear una variable recodificando (base_covid4) la variable edad en 4 rangos etáreos.
# guardela como edadr en una nueva base llamada: base_covid4
# recuerde que para recodificar se usa: mutate(variable_nueva = case_when(variable_vieja %in% c(desde:hasta) ~ "desde a hasta"
# para poner el "~": presione AltGR y asterisco.

base_covid4 <- base_covid %>% 
  mutate (edadr = case_when(edad <18 ~ "1 a 17", 
                            edad %in% c(18:34) ~ "18 a 34", 
                            edad %in% c(35:64)~ "35 a 64", 
                            edad >64 ~ "65 o más"))


#07. compruebe lo realizado con una table() a edadr
table(base_covid4$edadr)



