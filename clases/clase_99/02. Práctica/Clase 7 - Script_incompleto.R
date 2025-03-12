#Práctica

# 01. Procesamiento de base de datos --------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, readxl,readr,
               janitor, forcats, writexl, DataExplorer) #dos formatos de excel xlsx y xl


#Importar el archivo y la asigno en el environment


#Observe los datos mediante names y fijese cómo vienen los datos



#limpieza inicial con janitor
#estructura: datos <- janitor::clean_names(base a limpiar)



#observe como quedaron con names ()


#observación de base
# cantidad de casos: nrow(base)
# cantidad de variables: ncol(base)
# casos y variables: dim(datos)
# sapply: realiza un a función a varias variables: sapply(base, FUN = class)

#observa algunas variables que parecen estar mal clasificadas según el tipo de variable?

# Renombrar variables -----------------------------------------------------
# a pesar que cambié el nombre de las variables continuan siendo muy largas
# quiero acortarlas



#extraigo el nombre de todas las variables
names (datos)

#procedimiento: 
#1. generar un vector con nombres de variables
#2. acortar ese vector quedandose sólo con 3 primeras letras
#3. reemplazar los nombres viejos por los nuevos

#copie los nombres de todas las variables en el script, vía names
#borrele el indicador de posición



# genero un vector con todas las columnas que quiero renombrar
# utilice lo siguiente: cols_a_renombrar <- c()  
#copie los nombres de las columnas a renombrar desde la sección anterior
# entre cada nombre: ¿Qué debería poner?



#observelas
cols_a_renombrar


#genero un vector sólo con las 3 primeras letras: p01
#forma: nuevos_nombres <- str_sub(string = cols_a_renombrar, start = desde donde, end = hasta donde )

#primer argumento - string = de donde saco los nombres: el vector creado
#segundo argumento - start = desde que posición extraigo (p)
#tercer argumento - end= hasta donde 

#observelas
nuevos_nombres


#renombro considerando todas las columnas elegidas asignando nuevos nombres
# las asigno como datos_r
# utilizo: rename_at(vars(cols_a_renombrar), ~ nuevos_nombres) para hacer variadas recodificaciones que siguen un mismo orden
# o versión actual: rename_at(vars(all_of(cols_a_renombrar)), ~ nuevos_nombres)


#para renombrar algunas variables en específico

#veo categorías de todas las variables
sapply(datos_r, FUN = unique)

#posibilidad de renombrar uno por uno las variables de interés. 
#renombre las 18 primeras variables: desde p01 a 018
# bbdd_r <- bbdd  %>% dplyr::rename(nombre_nuevo1 = nombre_viejo1, nombre_nuevo2 = nombre_viejo2, etc)
# renombre: nombre, edad, genero, annio, comuna_actual, comuna_pre, tipo_colegio
# renombre: tipo_colegio, puntaje, estudio_trabajo, educacion_madre, trabaja_madre, empleo_madre 
# renombre: trabaja_padre, educación padre, empleo_padre, psdhogar, clase_social_subjetiva



# selección y transformación de variables ---------------------------------
# Observación general de datos: DataExplorer::create_report(BBDD) 

DataExplorer::create_report(datos_r)

#¿Cómo lo solucioné?
#Intento por chatgpt: 



class(datos_r)

update.packages("DataExplorer")
update.packages("data.table")

#Copie el error en google
# Quitting from lines 205-218 (report.rmd) 
# Error in `[.data.table`(data.table(pca$rotation), , seq.int(nrow(pc_var2)),  : 
#                           Item 1 of j is 1 which is outside the column number range [1,ncol=0]

DataExplorer::create_report(datos_r, config = configure_report(add_plot_prcomp = FALSE))


# sino funciona el DataExplorer
# install.packages("htmltools", version = "0.5.4")
# library(htmltools)
# library(DataExplorer)


#función para realizar un reporte general introductorio
#me permite ver posibles transformaciones o limpiezas

#Hacer una primera observación de datos con el report!
#qué valores están mal clasificados en el tipo de variable?
#qué valores están fuera de rango o tienen resultados erroneos?


# Transformaciones/limpieza en variables categóricas
# Quiero cambiar datos de empleo_madre y empleo_padre

names(datos)
datos_r <- datos_r %>%
  mutate(empleo_madre = stringi::stri_trans_general(empleo_madre, "Latin-ASCII"),
         empleo_padre = stringi::stri_trans_general(empleo_padre, "Latin-ASCII"), 
         empleo_madre = tolower (empleo_madre),
         empleo_padre = tolower (empleo_padre), 
         empleo_madre = gsub(" ", "_", empleo_madre),
         empleo_padre= gsub(" ", "_", empleo_padre))

table(datos_r$empleo_madre)
table(datos_r$empleo_padre)

#ej:
#Realice lo anterior para comuna_actual y comuna_pre 


#compruebe con table() para comuna_actual y comuna_pre



#Observe que hay un caso que pone:region_del_maule,_linares.
#y hay otro de linares
#¿Cómo lo homogeneizo?
#uso de ifelse
#estructura: ifelse(test, yes=, no=)

#test= lo que se quiere probar [argumento 1]
#yes= que valor poner si se cumple [argumento 2]
#no= que valor poner si no se cumple [argumento 3]
?ifelse

table(datos_r$comuna_pre)
datos_r <- datos_r %>%
  mutate(comuna_pre = ifelse(comuna_pre == "region_del_maule,_linares.", yes= "linares", no= comuna_pre))

table(datos_r$comuna_pre)      

#¿Cómo lo podría hacer para puntaje?
table(datos_r$puntaje)
class(datos_r$puntaje)
unique (datos_r$puntaje)


datos_r <- datos_r %>%
  mutate(puntaje = ifelse(puntaje == "700+", yes= "700", no= puntaje))


datos_r <- datos_r %>%
  mutate(puntaje = ifelse(puntaje=="No se", yes= NA, 
                          no=ifelse(puntaje=="No se aplica (ACT 30)", yes=NA, 
                          no=ifelse(puntaje=="no me acuerdo, pero creo que eran como 590/600", yes="590",
                          no=puntaje))))


table(datos_r$puntaje)
class(datos_r$puntaje) # observo que está en character, ¿Qué pasa si está en character?

datos_r$puntaje <- as.numeric(datos_r$puntaje) #lo transformo a numeric
class(datos_r$puntaje)


#ej
# realice el mismo proceso con la edad y cambielo a numeric
table(datos_r$edad)




#realizar recodificaciones
#edad
#recodifico en 18 a 20, 21 a 23, mayores que 24

table(datos_r$edad) #observo las frecuencias de edad



# observe la recodificación



# ¿Quiero saber si estudiante es o no primera generación universitaria?
#realizo una tipología 

unique (datos_r$educacion_madre)
unique (datos_r$educacion_padre)


datos_r <-  datos_r %>% 
  mutate(generacion = ifelse(educacion_madre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ) | 
                               educacion_padre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ), 
                             "No es primera generación", 
                             "Si es primera generación")) 


table (datos_r$generacion) #observo mi recodificación


#recordar operadores lógicos
# == (un valor), %in% c(más de un valor), <,>, >=, <=, != 
# & (and), | (or)



# Análisis exploratorio de Datos ------------------------------------------


#observo la base
str(datos)
dplyr::glimpse(datos)
glimpse(datos)
dim (datos) 
 

# Exportar ----------------------------------------------------------------
write.xlsx(x = base_en_environment,file = "ruta_donde_la_guardo") # en general se usa output

if(!dir.exists("output")) dir.create("output")
write.xlsx(x = datos_r,file = "output/Encuesta_Antropología_Limpia")





