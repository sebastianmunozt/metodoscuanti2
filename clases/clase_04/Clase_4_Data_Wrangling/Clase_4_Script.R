

# 01. Repaso: Actividad de subsetting (Lenguaje R Base)------------------------------------------
#Considerando la siguiente base

Candidato <- c("Kast", "Boric")
Votos <- c("64000", "78000")
Mesas <- c("Mesa1", "Mesa1")

Conteo <- data.frame(Candidato, Votos, Mesas)

#Actividad: realice las siguientes selecciones con r base y tidyverse

#1. Seleccionar solo la columna votos: por posición y por nombre de variable
#r_base

Conteo[,2]
Conteo[, "Votos"]

#tidyverse
library(tidyverse)

Conteo %>% 
  select(2)

Conteo %>% 
  select(Votos) #ojo en select de tidyverse no se usan las comillas

#2. Seleccionar la fila 1 y columna 1 y 2.
Conteo[1, c(1,2)]

Conteo %>% 
  slice(1) %>%  #casos
  select(1,2) #variables

#3. Seleccionar columna 2 mediante operador $.
Conteo$Votos

#4. Selccionar la fila 1 y columna 1 y 3.
Conteo[1, c("Candidato", "Votos", "Mesas")]

#5. Seleccionar fila 2, además de la columna 3.
Conteo[1, c(1,3)]
Conteo[2, 3]


#02. Introducción-------------------------------------------------
#flujo de trabajo
#orden del script


#Tipos de datos ------------------------------------------------------

#1. Tipo numeric: números
#especificando: 
#(integer): enteros: 1,2,3,4, etc.
#(double): con decimales

a <- c(1:10)
b <- c(1, 5, 200, 500)
c <- c(30, 35, 26, 47) 

names(b) <- c("antropologia", "sociologia", "geografía", "historia")

#2. Tipo character
nombres <- c ("pedro", "juan", "diego")

#3. Factor: se suele usa para variables ordinales
factor1 <-  factor(c("orange", "red", "yellow"))
factor2 <- factor(c("ABC1","C2C3", "DE" ))


#otros ejemplo
genero <- c(1,2,2,2,1,2,1,99,99)
genero_f <- factor(genero, labels = c("Hombre", "Mujer", NA)) 
table(genero_f)


#4. Lógico
lógico = c(FALSE, FALSE, TRUE, TRUE)


#Coerción explícita####
x <- as.numeric(c(2,4,6,8,10))
q <- as.character(c("Valparaíso", "Santiago", "Viña del Mar"))

q <- as.factor(c("Santiago", "Valparaíso", "Viña del Mar"))
table(q)

y <- as.factor(c("ABC1", "C2", "C3", "D", "E"))
z <- as.logical(c(TRUE, TRUE, FALSE))
t <- as.Date("22-10-2020")


#class(): identificar el tipo de objeto
class(t)

#qué pasa si yo tranformo un numérico a character
x <- as.character(x)

#bases de datos####

#data frame
Candidato <- c("Kast", "Boric")
Votos <- c("64000", "78000")
Mesas <- c("Mesa1", "Mesa1")

opcion_2 <- data.frame(Candidato, Votos, Mesas)
opcion_2


class(opcion_2)

# listas#### 
# objeto que puede almacenar muchos tipos de datos
mi.lista <- list("letras" =  letters[1:4], opcion_2 =  opcion_2, numeros = c(1,2,3))

mi.lista_mal <- list("letras" <-   letters[1:4], opcion_2 <-  opcion_2, numeros <- c(1,2,3))

#acceso
mi.lista [[1]] 
mi.lista [[2]]
mi.lista [[3]]


mi.lista[["letras"]]
mi.lista[["opcion_2"]]

mi.lista$letras

class(mi.lista [[1]])
class(mi.lista [[2]])
class(mi.lista [[3]])


# 03. Procesamiento de base de datos --------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer) #Exploración rápida


#Importar el archivo y asignarlo en el environment----
pham <- read.xlsx(xlsxFile = "base/ALTO_MAIPO.xlsx")

#Explorar
glimpse(pham) #Una primera mirada de lo que hay en mis datos, la primera fila es extraña, dice "respuesta" o repite el nombre de la variable.

#quitar la primera fila a mis datos
pham <- pham[-1,]

names(pham) #observo que hay puntos, mayúsculas y minúsculas, etcétera. Está sucia


#limpieza inicial----
pham <- janitor::clean_names(pham) #con esto transformo todo a minúscula, quito tildes, saco signos, borro espacios

names(pham)



#observación de base
nrow(pham) #473 cantidad de casos
ncol(pham) #26 cantidad de variables
sapply(pham, FUN = class) # sapply: realiza un a función a varias variables 
str(pham) #estructura del objeto base de datos

# Renombrar variables -----------------------------------------------------

#extraigo el nombre de todas las variables
names (pham)

# [1] "comuna"                                                                                                                                                                                    
# [2] "localidad"                                                                                                                                                                                 
# [3] "sexo"                                                                                                                                                                                      
# [4] "otra_comuna"                                                                                                                                                                               
# [5] "hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0"                                                                                          
# [6] "la_vivienda_que_usted_ocupa_es_enc_leer_alternativas"                                                                                                                                      
# [7] "cual_es_el_valor_que_paga_mensualmente"                                                                                                                                                    
# [8] "parentezco_con_jedfe_de_hogar"                                                                                                                                                             
# [9] "edad"                                                                                                                                                                                      
# [10] "genero"                                                                                                                                                                                    
# [11] "situacion_ocupacional"                                                                                                                                                                     
# [12] "cual_es_la_ocupacion_principal_del_jefe_de_hogar_enc_respuesta_espontanea_clasificar_en_una_de_las_siguientes_categorias"                                                                  
# [13] "principales_traslados"                                                                                                                                                                     
# [14] "lugar_traslado"                                                                                                                                                                            
# [15] "temporalidad_traslado"                                                                                                                                                                     
# [16] "medio_transporte"                                                                                                                                                                          
# [17] "tiempo_traslado"                                                                                                                                                                           
# [18] "a_que_sistema_de_salud_pertenece_su_familia_enc_leer_alternativas_y_responder_todas_las_que_correspondan"                                                                                  
# [19] "cuando_usted_o_algun_familiar_necesita_atencion_de_salud_adonde_acude_principalmente_enc_leer_alternativas_y_responder_una"                                                                
# [20] "ha_cambiado_la_calidad_de_la_atencion_en_los_ultimos_seis_meses"                                                                                                                           
# [21] "si_nota_un_cambio_como_ha_cambiado_en_enc_leer_cada_tipo_de_cambio_y_anotar_1_si_2_no_todos_los_tipos_de_cambios_deben_quedar_con_una_respuesta_anotada"                                   
# [22] "en_los_ultimos_seis_meses_han_ocurrido_incidentes_que_alteren_el_orden_publico_en_la_localidad_donde_vive_enc_leer_alternativas_y_responder_una"                                           
# [23] "la_presencia_de_aes_gener_en_la_comuna_le_ha_traido_beneficios_a_usted_o_a_su_familia_enc_leer_cada_tipo_de_beneficio_todos_los_tipos_de_beneficios_deben_quedar_con_una_respuesta_anotada"
# [24] "usted_conoce_el_mecanismo_por_el_cual_opera_la_produccion_de_electricidad_en_la_central_hidroelectrica_alto_maipo"                                                                         
# [25] "solo_si_conoce_el_mecanismo_me_podria_indicar_cual_es_el_mecanismo_que_se_utiliza"                                                                                                         
# [26] "desde_el_inicio_de_las_actividades_de_alto_maipo_ha_notado_diferencias_en_el_nivel_del_caudal_del_rio_enc_leer_cada_uno_de_los_ambitos_elegir_una_alternativa"  


# genero un vector con todos los nombres de las columnas que quiero renombrar
nombres <- c(                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
"comuna",                                                                                                                                                                                    
"localidad",                                                                                                                                                                                 
"sexo",                                                                                                                                                                                      
"otra_comuna",                                                                                                                                                                               
"hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0",                                                                                          
"la_vivienda_que_usted_ocupa_es_enc_leer_alternativas",                                                                                                                                      
"cual_es_el_valor_que_paga_mensualmente",                                                                                                                                                    
"parentezco_con_jedfe_de_hogar",                                                                                                                                                             
"edad",                                                                                                                                                                                    
"genero",                                                                                                                                                                                    
"situacion_ocupacional",                                                                                                                                                                     
"cual_es_la_ocupacion_principal_del_jefe_de_hogar_enc_respuesta_espontanea_clasificar_en_una_de_las_siguientes_categorias",                                                                  
"principales_traslados",                                                                                                                                                                     
"lugar_traslado",                                                                                                                                                                            
"temporalidad_traslado",                                                                                                                                                                     
"medio_transporte",                                                                                                                                                                          
"tiempo_traslado",                                                                                                                                                                           
"a_que_sistema_de_salud_pertenece_su_familia_enc_leer_alternativas_y_responder_todas_las_que_correspondan",                                                                                  
"cuando_usted_o_algun_familiar_necesita_atencion_de_salud_adonde_acude_principalmente_enc_leer_alternativas_y_responder_una",                                                                
"ha_cambiado_la_calidad_de_la_atencion_en_los_ultimos_seis_meses",                                                                                                                           
"si_nota_un_cambio_como_ha_cambiado_en_enc_leer_cada_tipo_de_cambio_y_anotar_1_si_2_no_todos_los_tipos_de_cambios_deben_quedar_con_una_respuesta_anotada",                                   
"en_los_ultimos_seis_meses_han_ocurrido_incidentes_que_alteren_el_orden_publico_en_la_localidad_donde_vive_enc_leer_alternativas_y_responder_una",                                           
"la_presencia_de_aes_gener_en_la_comuna_le_ha_traido_beneficios_a_usted_o_a_su_familia_enc_leer_cada_tipo_de_beneficio_todos_los_tipos_de_beneficios_deben_quedar_con_una_respuesta_anotada",
"usted_conoce_el_mecanismo_por_el_cual_opera_la_produccion_de_electricidad_en_la_central_hidroelectrica_alto_maipo",                                                                         
"solo_si_conoce_el_mecanismo_me_podria_indicar_cual_es_el_mecanismo_que_se_utiliza",                                                                                                         
"desde_el_inicio_de_las_actividades_de_alto_maipo_ha_notado_diferencias_en_el_nivel_del_caudal_del_rio_enc_leer_cada_uno_de_los_ambitos_elegir_una_alternativa" ) 

 #genero un vector sólo con las 4 primeras letras:
nuevos_nombres <- str_sub(string = nombres, start = 1, end = 4 ) #muestro los argumentos

nuevos_nombres <- str_sub( nombres,  1, 4 )

#primer argumento - string = de donde saco los nombres: el vector creado
#segundo argumento - start = desde que posición extraigo (p)
#tercer argumento - end= hasta donde (1)


#renombro considerando todas las columnas elegidas asignando nuevos nombres
pham <- pham %>%
  rename_at(vars(nombres), ~ nuevos_nombres) #recodificación múltiples con un vector

#Problema! "cual" at locations 7 and 12.!

#renombro algunas variables en específico


#veo categorías de todas las variables
sapply(pham, FUN = unique) 

#posibilidad de renombrar uno por uno las variables de interés. # primero nuevo nombre y luego nombre antiguo

base_datos <- base_datos %>% dplyr::rename(nombre_antiguo=nombrenuevo,
                                           nombre_antiguo=nombrenuevo)

pham <- pham %>% dplyr::rename( anios_comuna = hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0,
                        propiedad_vivienda = la_vivienda_que_usted_ocupa_es_enc_leer_alternativas,
                        sistema_salud = a_que_sistema_de_salud_pertenece_su_familia_enc_leer_alternativas_y_responder_todas_las_que_correspondan,
                        pago_vivienda = cual_es_el_valor_que_paga_mensualmente,  
                        ocupacion_jefe_hogar = cual_es_la_ocupacion_principal_del_jefe_de_hogar_enc_respuesta_espontanea_clasificar_en_una_de_las_siguientes_categorias, 
                        servicio_salud = cuando_usted_o_algun_familiar_necesita_atencion_de_salud_adonde_acude_principalmente_enc_leer_alternativas_y_responder_una, 
                        calidad_atencion = ha_cambiado_la_calidad_de_la_atencion_en_los_ultimos_seis_meses, 
                        cambio_atencion = si_nota_un_cambio_como_ha_cambiado_en_enc_leer_cada_tipo_de_cambio_y_anotar_1_si_2_no_todos_los_tipos_de_cambios_deben_quedar_con_una_respuesta_anotada, 
                        ocurrencia_incidentes= en_los_ultimos_seis_meses_han_ocurrido_incidentes_que_alteren_el_orden_publico_en_la_localidad_donde_vive_enc_leer_alternativas_y_responder_una, 
                        beneficios_aes_gener = la_presencia_de_aes_gener_en_la_comuna_le_ha_traido_beneficios_a_usted_o_a_su_familia_enc_leer_cada_tipo_de_beneficio_todos_los_tipos_de_beneficios_deben_quedar_con_una_respuesta_anotada, 
                        mecanismo_produccion = usted_conoce_el_mecanismo_por_el_cual_opera_la_produccion_de_electricidad_en_la_central_hidroelectrica_alto_maipo, 
                        nombre_mecanismo = solo_si_conoce_el_mecanismo_me_podria_indicar_cual_es_el_mecanismo_que_se_utiliza, 
                        diferencia_caudal = desde_el_inicio_de_las_actividades_de_alto_maipo_ha_notado_diferencias_en_el_nivel_del_caudal_del_rio_enc_leer_cada_uno_de_los_ambitos_elegir_una_alternativa)



names(pham)


# selección y transformación de variables ---------------------------------
DataExplorer::create_report(pham) 

# sino funciona el DataExplorer
# install.packages("htmltools", version = "0.5.4")
# library(htmltools)
# library(DataExplorer)


#función para realizar un reporte general introductorio
#me permite ver posibles transformaciones o limpiezas



# Transformaciones/limpieza en variables categóricas---

names (pham)

#Situación Ocupacional
unique(pham$situacion_ocupacional)

#realizaremos un conjunto de transformaciones/limpiezas de nuestra BBDD. 

# para situación ocupacional (respuesta abierta)
#pham$situacion_ocupacional <- tolower(pham$situacion_ocupacional) #todas a minusculas
#pham$situacion_ocupacional  <- gsub(pattern = " ", replacement = "", x = pham$situacion_ocupacional) #elimino los espacios

# pham$situacion_ocupacional  <- gsub(" ", "", pham$situacion_ocupacional) (forma por default)

table(pham$situacion_ocupacional)

#Recodificar a 3 categorías 
#1. Trabajo remunerado; 2. Trabajo no remunerado; 3. No trabaja.

unique(pham$situacion_ocupacional)

#Recodificamos con mutate y case_when (para recodificar categoría de respuesta)

base <- base %>% mutate(nueva_variable=case_when(variable_original=="Valor de la condición"~"Nuevo valor de la categoría",
                                                 variable_original=="talvalor"~"nuevovalor",
                                                 variable_original=="x valor"~"nuevovalor"))

pham <- pham %>% mutate(situacion_ocupacional=case_when(situacion_ocupacional=="Trabajo remunerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajando"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajo independiente"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajando remunerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajador remunerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajaba"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Si"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Aseo"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Activo"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Garzona"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Ayudante de cocina"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajando municipla"~"Trabajo remunerado",
                                                              situacion_ocupacional=="trabaja"~"Trabajo remunerado",
                                                              situacion_ocupacional=="1"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabaja"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajo rumerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajo remerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajo renunerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajando desde casa"~"Trabajo remunerado",
                                                              situacion_ocupacional=="trabajo remunerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Trabajo renminerado"~"Trabajo remunerado",
                                                              situacion_ocupacional=="Dueña de casa"~"Trabajo no remunerado",
                                                              situacion_ocupacional=="Trabajo remunerdo"~"Trabajo no remunerado",
                                                              situacion_ocupacional=="2"~"Trabajo no remunerado",
                                                              situacion_ocupacional=="Trabajo no remunerado"~"Trabajo no remunerado",
                                                              situacion_ocupacional=="2?"~"Trabajo no remunerado",
                                                              situacion_ocupacional=="Cesante"~"No trabaja",
                                                              situacion_ocupacional=="No trabajaba"~"No trabaja",
                                                              situacion_ocupacional=="No"~"No trabaja",
                                                              situacion_ocupacional=="Reservas"~"No trabaja",
                                                              situacion_ocupacional=="Estudiando"~"No trabaja",
                                                              situacion_ocupacional=="No trabaja"~"No trabaja",
                                                              situacion_ocupacional=="no trabaja"~"No trabaja",
                                                              situacion_ocupacional=="Licencia por cirugía"~"No trabaja",
                                                              situacion_ocupacional=="3"~"No trabaja",
                                                              situacion_ocupacional=="Estudiante"~"No trabaja",
                                                              situacion_ocupacional=="Jubilada"~"No trabaja",
                                                              situacion_ocupacional=="No trajaba"~"No trabaja",
                                                              situacion_ocupacional=="3 jubilado"~"No trabaja",
                                                              situacion_ocupacional=="3 jubilada"~"No trabaja",
                                                              situacion_ocupacional=="1 - Situación Laboral junio 2018"~ NA,
                                                              situacion_ocupacional==" "~ NA,
                                                              TRUE~situacion_ocupacional))


# principales traslados

pham$principales_traslados <- tolower(pham$principales_traslados)
pham$principales_traslados  <- gsub(" ", "", pham$principales_traslados)

table(pham$principales_traslados)

#utilizando el siguiente ejemplo, elimine los tildes y la ñ
# categories <- gsub(" ", "_", categories)
# categories <- gsub("[áÁ]", "a", categories)
# categories <- gsub("[éÉ]", "e", categories)
# categories <- gsub("[íÍ]", "i", categories)
# categories <- gsub("[óÓ]", "o", categories)
# categories <- gsub("[úÚ]", "u", categories)
# categories <- gsub("ñ", "n", categories)

#saco tildes y ñ de comuna actual
pham$comuna_actual  <- gsub("[áÁ]", "a", x = pham$comuna_actual)
pham$comuna_actual  <- gsub("[éÉ]", "e", x = pham$comuna_actual)
pham$comuna_actual  <- gsub("[íÍ]", "i", x = pham$comuna_actual)
pham$comuna_actual  <- gsub("[óÓ]", "o", x = pham$comuna_actual)
pham$comuna_actual  <- gsub("[úÚ]", "u", x = pham$comuna_actual)
pham$comuna_actual  <- gsub("ñ", "n", x = pham$comuna_actual)
table(pham$comuna_actual)


#saco tildes y ñ de comuna previa
pham$comuna_pre  <- gsub("[áÁ]", "a", x = pham$comuna_pre)
pham$comuna_pre  <- gsub("[éÉ]", "e", x = pham$comuna_pre)
pham$comuna_pre  <- gsub("[íÍ]", "i", x = pham$comuna_pre)
pham$comuna_pre  <- gsub("[óÓ]", "o", x = pham$comuna_pre)
pham$comuna_pre  <- gsub("[úÚ]", "u", x = pham$comuna_pre)
pham$comuna_pre  <- gsub("ñ", "n", x = pham$comuna_pre)
table(pham$comuna_pre)


#otra opción para sacar tildes, ñ, transformar todo a minúscula y eliminar espacios
library(tidyverse)

names(pham)
pham <- pham %>%
  mutate(empleo_madre = stringi::stri_trans_general(empleo_madre, "Latin-ASCII"),
         empleo_padre = stringi::stri_trans_general(empleo_padre, "Latin-ASCII"), 
         empleo_madre = tolower (empleo_madre),
         empleo_padre = tolower (empleo_padre), 
         empleo_madre = gsub(" ", "_", empleo_madre),
         empleo_padre= gsub(" ", "_", empleo_padre))

table(pham$empleo_madre)
table(pham$empleo_padre)

      

#limpieza de pham
table(pham$puntaje)
class(pham$puntaje)
unique (pham$puntaje)


#Para cambiar datos específicos de la BBDD.

#uso de ifelse
#estructura: ifelse(test, yes=, no=)

#test= lo que se quiere probar [argumento 1]
#yes= que valor poner si se cumple [argumento 2]
#no= que valor poner si no se cumple [argumento 3]

?ifelse


table(pham$puntaje) # puntaje tiene un valor raro

pham <- pham %>%
  mutate(puntaje = ifelse(puntaje == "700+", yes= "700", no= puntaje))

#otra opción (sin nombrar los argumentos)
pham <- pham %>%
  mutate(puntaje = ifelse(puntaje =="700+", "700", puntaje))


table(pham$puntaje)
class(pham$puntaje) # observo que está en character, ¿Qué pasa si está en character?

pham$puntaje <- as.numeric(pham$puntaje) #lo transformo a numeric
class(pham$puntaje)


#realizar recodificaciones
#edad

table(pham$edad) #observo las frecuencias de edad

#Case_when----

#La función 'cases_when()' permite crear una nueva variable en base al valor(es) de otra(s) variable(s). Más cómodo que if_else() cuando la condición es complexa.


pham <- pham %>% 
  mutate (edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                            edad %in% c(21:23) ~ "21 a 23", 
                            edad %in% c(24:29) ~ "24 a 29", 
                            edad >= 30 ~ "30 o más"))

table(pham$edadr) # observo la recodificación
class(pham$edad)
class(pham$edadr)


# ¿Quiero saber si estudiante es o no primera generación universitaria?
#realizo una tipología 

unique (pham$educacion_madre)
unique (pham$educacion_padre)


pham <-  pham %>% 
  mutate(generacion = ifelse(educacion_madre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ) | 
                               educacion_padre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ), 
                             "No es primera generación", 
                             "Si es primera generación")) 


table (pham$generacion) #observo mi recodificación


#recordar operadores lógicos
# == (un valor), %in% c(más de un valor), <,>, >=, <=, != 
# & (and), | (or)



# Análisis exploratorio de datos ------------------------------------------


#observo la base
str(pham)
dplyr::glimpse(pham)
glimpse(pham)
dim (pham) 
 

# Exportar ----------------------------------------------------------------

write.xlsx(x = pham,file = "base/PHAM_Limpia")


# Ejemplos interesantes ---------------------------------------------------


#install.packages("chilemapas")

library(chilemapas)


poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 14) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)

# estos colores vienen del paquete colRoz
# https://github.com/jacintak/colRoz
paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")

ggplot(comunas_los_rios) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region de los Rios") +
  theme_minimal(base_size = 13)


