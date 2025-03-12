# Ayudantía 3
# Limpieza y transformación de datos

#Procesamiento de base de datos ------------------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer) #Exploración rápida


#IMPORTAR LA BASE DESDE EXCEL Y GUARDARLA EN EL ENVIROMENT----------------------
#vamos a importar la base de datos en escel a RStudio
#la vamos a llamar base_am
base_am <- read.xlsx(xlsxFile = "/Users/fran/Desktop/R ayudantías/mc2/ayudantias/ayudantia_03/ALTOMAIPO.xlsx", colNames = TRUE, detectDates = TRUE)

#colNames = TRUE: la primera fila del archivo se interpretará como nombres de columna. En FALSE, se generarán nombres de columna predeterminados. 
#detectDates = TRUE: intentará detectar las fechas en el archivo y las convertirá automáticamente. En FALSE, no se realizará esta detección automática y las fechas se tratarán como cadenas de texto


#EXPLORAR LA DATA
glimpse(base_am) 
#glimpse significa destello, sería como un vistazo rápido inicial de los datos en la consola
#Una primera mirada de lo que hay en mis datos: número de filas y columnas, además de las variables
#las variables están nombradas de manera poco práctica. Hay variables nombradas directamente con la pregunta

view(base_am) 
#al ver la data podemos notar la primera fila es extraña, dice "respuesta" o repite el nombre de la variable

#Vamos a eliminar la fila que repetía los nombres de las variables
base_am <- base_am[-1,]
#base_am <- base_am[-1,] elimina la primera fila del conjunto de datos. Selecciona todas las filas excepto la primera (`-1` indica la fila a eliminar)
#Ahora volvamos a ver la data a ver qué pasa...

#Vamos a ver nos nombres de las variables en la consola
names(base_am) 
#notamos que hay puntos, mayúsculas y minúsculas, preguntas muy largas. Incluso en algunas variables muestra las categorías de respuesta
#La data está "sucia", lo que dificultra trabajar en ella


#LIMPIEZA INICIAL---------------------------------------------------------------
base_am <- janitor::clean_names(base_am) 
#con esto transformo todo a minúscula, quito tildes, saco signos, borro espacios
#le vamos a decir al RStudio que nos guarde bajo el mismo nombre los datos limpios
#nos sobreescribió la data, pero ahora está limpia 

#Ahora veamos cómo nos quedan los nombres de las variables...
names(base_am)


#CARACTERISTICAS DE LA DATA
nrow(base_am) #473 cantidad de casos: filas
ncol(base_am) #26 cantidad de variables: columnas
sapply(base_am, FUN = class) # sapply: realiza una función a varias variables: se utiliza para aplicar la función `class()` a cada columna 
#y devolver una lista con las clases de cada columna (tipo de variable)
str(base_am) #muestra la estructura del objeto base de datos

# RENOMBRAR VARIABLES ----------------------------------------------------------
#vamos a cambiarle los nombres a las variables

##Pasos:
#1. Extraer el nombre de todas las variables desde la consola (lo copio)
names (base_am)

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


#2. Generar un vector con todas las columnas que quiero renombrar (concatenamos todas las variables o columnas)
#vamos a llamar a este vector cols_a_renombrar

cols_a_renombrar <- c(                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
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

#3. Generar un vector sólo con las 3 primeras letras: p01 (esto para simplificar)
#vamos a llamar a ese vector nuevos_nombres

#podemos hacerlo de estas dos formas distintas:
nuevos_nombres <- str_sub(string = cols_a_renombrar, start = 1, end = 4 ) #muestro los argumentos
#ó
nuevos_nombres <- str_sub(cols_a_renombrar, 1, 4) #no muestro los argumentos

#vamos a ver cómo nos quedarían nuestras variables
nuevos_nombres

#En la primera función:
#primer argumento string = de donde saco los nombres: en este caso el vector creado anteriormente 
#segundo argumento start = desde que posición extraigo (desde la posición 1)
#tercer argumento end= hasta donde (hasta la posición 4)


#4. Renombrar considerando todas las columnas elegidas asignando nuevos nombres
base_am <- base_am %>%
  rename_at(vars(cols_a_renombrar), ~ nuevos_nombres) #recodificación múltiples con un vector

#¿Qué estamos diciendo aquí?
#rename_at() permite renombrar columnas seleccionadas basándose en condiciones específicas: renombrame esto en 
#vars permite seleccionar columnas basadas en sus nombres 
#cols_a_renombrar es es el vector creado anteriormente que contiene los nombres de las columnas que se desean renombrar

#renombrame las columnas de la data original basadas en sus nombres desde el vector cols_a_renombrar                                                         

#Problema! "cual" at locations 7 and 12.!
#! Names must be unique, lo que quiere decir es que el nuevo nombre que se está intentando asignar ya existe en el dataframe 
#y los nombres de las columnas deben ser únicos 
#Para resolver este error, necesitamos asegurarnos de que los nuevos nombres que estamos asignando no estén duplicados en el dataframe
#el código distinct no nos sirve en este caso para eliminar duplicados, pues elimina filas 
#¿Qué podemos hacer?

#Podemos *renombrar algunas variables en específico*

#veo las categorías de todas las variables
sapply(base_am, FUN = unique)
#sapply() se utiliza para *aplicar* una función determinada a cada columna del dataframe 
#En este caso, se desea aplicar la función unique() a cada variable de la data base_am
#y devolver una lista con los valores únicos de cada variable

#Renombrar uno por uno las variables de interés, con las que se desee trabajar
#Utilizamos dplyr::rename()
#el operador :: se utiliza para acceder a funciones, métodos u objetos de un paquete
#primero colocamos el nuevo nombre = y luego el nombre antiguo
#queremos que este nombre lo tome la variable denominada ...

base_am <- base_am %>% dplyr::rename( anios_comuna = hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0,
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



#Veamos cómo nos quedan las variables
names(base_am)


# SELECCIÓN Y TRANSFORMACIÓN DE VARIABLES --------------------------------------
DataExplorer::create_report(base_am) 

#DataExplorer es una herramienta en R que proporciona funciones útiles para explorar y visualizar conjuntos de datos
#genera un reporte en R Markdown
#Este código utiliza la función create_report() del paquete DataExplorer::
#create_report() es una función que genera un informe automático sobre un dataframe dado
#En este caso, le estamos diciendo a RStudio que nos abra la función create_report( y la aplique a la data), tener una visión general de los datos
#función para realizar un reporte general introductorio
#me permite ver posibles transformaciones o limpiezas

# sino funciona el DataExplorer
# install.packages("htmltools", version = "0.5.4")
# library(htmltools)
# library(DataExplorer)


# Transformaciones/limpieza en *VARIABLES CATEGÓRICAS*

#Vamos a seleccionar la variable Situación Ocupacional
unique(base_am$situacion_ocupacional)
#con unique() vamos a ver los datos únicos para esa variable
#tal como he mostrado anteriormente, le estamos diciendio a RStudio que nos de los datos únicos de situacion_ocupacional (sus categorías)
#si se fijan, hay muchas categorías de respuesta
#esto complejiza el análisis, tenemos que ordenar los datos
#Hay muchas categorías que refieren a lo mismo, que se repiten escritas de otro modo, o que están mal escritas
#por ej aquellos que respondieron "trabaja" con "trabajando" 

#realizaremos un conjunto de transformaciones/limpiezas de nuestra base de datos 

# para situación ocupacional (respuesta abierta)
base_am$situacion_ocupacional <- tolower(base_am$situacion_ocupacional) #tolower() pasa todas a minusculas
base_am$situacion_ocupacional  <- gsub(pattern = " ", replacement = "", x = base_am$situacion_ocupacional) #elimino los espacios 

#tolower() tomará los valores de la columna "situacion_ocupacional" del dataframe base_am y los convertirá todos a minúscula
#gsub() se utiliza para eliminar todos los espacios en blanco de los elementos de la columna "situacion_ocupacional" del dataframe base_am
#gsub() es una función en R que se utiliza para reemplazar ocurrencias de un patrón específico en un vector de caracteres con un valor de reemplazo
#pattern = " " especifica el patrón que se buscará en los elementos de la columna, en este caso, el patrón es simplemente un espacio en blanco
#replacement = "" especifica el valor que se utilizará para reemplazar todas las ocurrencias del patrón encontrado
#En este caso, el valor de reemplazo es una cadena vacía, lo que significa que el espacio en blanco será eliminado
#x = es el vector de caracteres en el que se realizarán los reemplazos

# base_am$situacion_ocupacional  <- gsub(" ", "", base_am$situacion_ocupacional) (forma por default)

#vamos a solicitar los datos expuestos en forma de tabla
table(base_am$situacion_ocupacional)
#en este caso vemos que hay muchas categorías que se podrían juntar 
#y así simplificar el análisis

#RECODIFICAR a 3 categorías 
#1. Trabajo remunerado; 2. Trabajo no remunerado; 3. No trabaja.

#entonces vemos las categorías que hay y determinamos cuál se irá a cada grupo
unique(base_am$situacion_ocupacional)
#por ej: trabajando, trabajo remunerado, trabajo indep, trabajaba, si, activo, y similares, las vamos a categorizar como: Trabajo remunerado


#Recodificamos con MUTATE y CASE_WHEN-------------------------------------------

base_am <- base_am %>% mutate(situacion_ocupacional=case_when(situacion_ocupacional=="Trabajo remunerado"~"Trabajo remunerado",
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
                                                              TRUE~situacion_ocupacional))

#veamos cómo nos queda la recodificación
unique(base_am$situacion_ocupacional)


#Ejercicio----------------------------------------------------------------------
#Recodifiquemos la variable "parentezco_con_jedfe_de_hogar"
#veamos los datos de la variable

#¿Qué tenemos que hacer primero?
#1. Seleccionar la variable y ver los datos únicos para esta variable (con unique)

unique(base_am$parentezco_con_jedfe_de_hogar)

#2. Dejar todas las categorías en un mismo formato
#poner todo en minúscula y eliminar los espacios

base_am$parentezco_con_jedfe_de_hogar <- tolower(base_am$parentezco_con_jedfe_de_hogar) #pasa todas a minusculas
base_am$parentezco_con_jedfe_de_hogar  <- gsub(pattern = " ", replacement = "", x = base_am$parentezco_con_jedfe_de_hogar) #elimino los espacios

#3. Solicitar los datos expuestos en forma de tabla para determinar qué categorías construir y cuál dato dejar en cada categoría
#¿qué categorías podríamos establecer?

table(base_am$parentezco_con_jedfe_de_hogar)

#4. Recodificar

base_am <- base_am %>% mutate(parentezco_con_jedfe_de_hogar=case_when(parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefedehogar",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefadehogar",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefadefamilia",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefedefamilia",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefasehogar(viuda)",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefedehogar.(separado)",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jegadehogar",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefadejogar",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefasehogar",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefehogar",
                                                                      parentezco_con_jedfe_de_hogar=="Jefe/a de hogar"~"jefe",
                                                                      parentezco_con_jedfe_de_hogar=="Pareja"~"pareja",
                                                                      parentezco_con_jedfe_de_hogar=="Pareja"~"cónyuge",
                                                                      parentezco_con_jedfe_de_hogar=="Pareja"~"conyuge",
                                                                      parentezco_con_jedfe_de_hogar=="Pareja"~"conyugue",
                                                                      parentezco_con_jedfe_de_hogar=="Pareja"~"esposo",
                                                                      parentezco_con_jedfe_de_hogar=="Pareja"~"esposa",
                                                                      parentezco_con_jedfe_de_hogar=="Madre/Padre"~"madre",
                                                                      parentezco_con_jedfe_de_hogar=="Madre/Padre"~"mama",
                                                                      parentezco_con_jedfe_de_hogar=="Madre/Padre"~"padre",
                                                                      parentezco_con_jedfe_de_hogar=="Hijo/hija"~"hija",
                                                                      parentezco_con_jedfe_de_hogar=="Hijo/hija"~"hijo",
                                                                      parentezco_con_jedfe_de_hogar=="Hijo/hija"~"hijajefadehogar",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"conviviente",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"abuela",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"nieta",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"nieto",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"3",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"6",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"10",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"2",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"4",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"1",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"dueñodecasa",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"dueñadecasa",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"ahijado",
                                                                      parentezco_con_jedfe_de_hogar=="Otro"~"jubilada",
                                                                      TRUE~parentezco_con_jedfe_de_hogar))

table(base_am$parentezco_con_jedfe_de_hogar)

unique(base_am$parentezco_con_jedfe_de_hogar)

#2. Dejar todas las categorías en un mismo formato
#poner todo en minúscula y eliminar los espacios

base_am$parentezco_con_jedfe_de_hogar <- tolower(base_am$parentezco_con_jedfe_de_hogar) #pasa todas a minusculas
base_am$parentezco_con_jedfe_de_hogar  <- gsub(pattern = " ", replacement = "", x = base_am$parentezco_con_jedfe_de_hogar) #elimino los espacios

#3. Solicitar los datos expuestos en forma de tabla para determinar qué categorías construir y cuál dato dejar en cada categoría
#¿qué categorías podríamos establecer?

table(base_am$parentezco_con_jedfe_de_hogar)

#4. Recodificar

base_am <- base_am %>% mutate(parentezco_con_jedfe_de_hogar=case_when(parentezco_con_jedfe_de_hogar== "jefedehogar"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefadehogar"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefadefamilia"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefedefamilia"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefasehogar(viuda)"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefedehogar.(separado)"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jegadehogar"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefadejogar"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefasehogar"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefehogar"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "jefe"~"Jefe/a de hogar",
                                                                      parentezco_con_jedfe_de_hogar== "pareja"~"Pareja",
                                                                      parentezco_con_jedfe_de_hogar== "cónyuge"~"Pareja",
                                                                      parentezco_con_jedfe_de_hogar== "conyuge"~"Pareja",
                                                                      parentezco_con_jedfe_de_hogar== "conyugue"~"Pareja",
                                                                      parentezco_con_jedfe_de_hogar== "esposo"~"Pareja",
                                                                      parentezco_con_jedfe_de_hogar== "esposa"~"Pareja",
                                                                      parentezco_con_jedfe_de_hogar=="madre"~"Madre/Padre",
                                                                      parentezco_con_jedfe_de_hogar=="mama"~"Madre/Padre",
                                                                      parentezco_con_jedfe_de_hogar=="padre"~"Madre/Padre",
                                                                      parentezco_con_jedfe_de_hogar=="hija"~"Hijo/hija",
                                                                      parentezco_con_jedfe_de_hogar=="hijo"~"Hijo/hija",
                                                                      parentezco_con_jedfe_de_hogar=="hijajefadehogar"~"Hijo/hija",
                                                                      parentezco_con_jedfe_de_hogar=="conviviente"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar=="abuela"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "nieta"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "nieto"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "3"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "6"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "10"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "2"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "4"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "1"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "dueñodecasa"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "dueñadecasa"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "ahijado"~"Otro",
                                                                      parentezco_con_jedfe_de_hogar== "jubilada"~"Otro",
                                                                      TRUE~parentezco_con_jedfe_de_hogar))

table(base_am$parentezco_con_jedfe_de_hogar)

