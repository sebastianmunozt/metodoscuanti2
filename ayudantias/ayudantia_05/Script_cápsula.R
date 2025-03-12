
#Ayudantía 0.5
#Limpieza de datos: recodificación: cambio de nombres variables y sus categorías
#Análisis de datos categóricos: tablas de contingencia


# PROCESAMIENTO/LIMPIEZA DE DATOS:--------------------------------------------------------------------------
#Clase 5

#0. IMPORTAR LA BASE DE DATOS
# install.packages("pacman")
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer) #Exploración rápida


# Importar el archivo de la data y guardarlo en el environment con el nombre base_antropologia
base_antropologia <- read.xlsx("Encuesta-Estudiantes-Antropología-2023-(respuestas).xlsx")


3#1. EXPLORACIÓN DE LA BASE DE DATOS
glimpse(base_antropologia) 
#Una primera mirada de lo que hay en mis datos
#destello*

View(base_antropologia)
#Podemos ver que la primera fila es extraña, dice "respuesta" o repite el nombre de la variable.
#tenemos que eliminar la primera fila a mis datos, ya que no corresponde a un dato
#Cómo lo hacemos? (con R base)
base_antropologia <- base_antropologia[-1,]


#Miro los nombres de las variables de mi base de datos
#¿Con qué función hago esto?
names(base_antropologia) 
#observo que hay puntos, mayúsculas y minúsculas, y nombres muy largos.
#Data "sucia" y es compleja de trabajar 


#2. LIMPIEZA INICIAL DE LA BASE DE DATOS
#transformo todo a minúscula, quito tildes, saco signos, borro espacios
#¿Cómo lo hago? paquete::función janitor:clean_names
base_antropologia <- janitor::clean_names(base_antropologia) 


#Otra vez vemos los nombres de las variables, para verificar el cambio
names(base_antropologia)
#Vamos a ver que quedan mucho mejor los nombres 
#con _ que rreemplazan los espacios
#sin puntos ni mayúsculas


#3.OBSERVACIÓN DE LA BASE DE DATOS
##(número de filas, columnas, tipos de variables, estructura de los datos)
##La observación de la base de datos nos proporciona información valiosa que 
##nos permite realizar tareas de limpieza y recodificación de manera efectiva
##para preparar los datos para el análisis.
# dimensionar la magnitud de los datos
# identificar datos faltantes o valores atípicos


## ¿Con qué función averiguamos el NÚMERO DE FILAS?
nrow(base_antropologia) #15 cantidad de casos totales, 14 si descontamos la fila borrada


## ¿Con qué función averiguamos el NÚMERO DE COLUMNAS?
ncol(base_antropologia) #33 cantidad de variables


## ¿Cómo averiguamos el TIPO DE VARIABLE de todas las variables de la data?
#sapply: aplica una función a un conjunto de variables (que pueden ser todas)
#En este caso se está aplicando la función class a cada columna o variable de la data
##función class nos indica el tipo de variable (si son character, factor, int o numeric)

sapply(base_antropologia, FUN = class) 

#¿Para qué es importante que conozcamos esta información? (tipo de variable)
#El tipo de variable influye en cómo se pueden manipular los datos.
#Por ejemplo, los datos categóricos pueden necesitar ser recodificados o agrupados 
#de manera diferente a los datos numéricos.

## ¿Cómo averiguamos la ESTRUCTURA de la base de datos?
str(base_antropologia) 
#Nos indica el nombre de la variable, 
#el tipo (categórica: chr, o numérica), 
#y sus categorías de respuesta


# RECODIFICACIÓN/TRANSFORMACIÓN DE DATOS: 

# Renombrar ALGUNAS VARIABLES EN ESPECÍFICO: Elegir variables para renombrar---------------------------------------------------
#Posibilidad de renombrar una por una las variables de interés. 

# 1. Veo los nombress de todas las variables para seleccionar la(s) variable(s) que me interesa renombrar.
names(base_antropologia)
#Variables de interés: por ej, en el caso de que nos interese relacionar la educación con el empleo, 
#tendríamos que elegir las variables que nos permitan ver eso en particular.

# 2. Renombrar las variables elegidas con RENAME: 
#Estructura de ejemplo:
base_datos <- base_datos %>% dplyr::rename(nombre_nuevo=nombre_antiguo,
                                           nombre_nuevo=nombre_antiguo)

base_antropologia <- base_antropologia %>% dplyr::rename( edad = p02_edad_del_a_entrevistado, 
                                                          genero=p03_genero_del_a_entrevistado_a, 
                                                          anio_carrera=p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5,
                                                          comuna_actual=p05_comuna_actual_de_residencia,
                                                          comuna_previa=p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_u_otros_as_cuidadores_o_en_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia,
                                                          tipo_establecimiento=p07_ultimo_tipo_de_establecimiento_educativo_en_que_realizo_su_ensenanza_media,
                                                          puntaje=p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida,
                                                          situacion=p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes,
                                                          nivel_educativo_madre=p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre,
                                                          empleo_madre=p11_actualmente_su_madre_trabaja,
                                                          ocupacion_madre=p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre,
                                                          nivel_educativo_padre=p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre,
                                                          empleo_padre=p14_actualmente_su_padre_trabaja,
                                                          ocupacion_padre=p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre,
                                                          sostenedor=p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos)


# 3. Veo cómo quedaron los nuevos nombres de mis variables elegidas
names(base_antropologia)       
#si se fijan, sólo se cambian los nombres de las variables que elegí


# Renombrar TODAS LAS VARIABLES--------------------------------------------------------------------------------------------------
#Posibilidad de renombrar una por una todas las variables. 

# 1. Veo el nombre de todas las variables
names(base_antropologia)
#para ver qué nombre le voy a asignar a cada variable.

# 2. Renombrar con RENAME: (nombre_nuevo = nombre_antiguo)

________________ <- _________________ %>% _____::______( edad = edad, 
                                                          genero=genero, 
                                                          anio_carrera=anio_carrera,
                                                          comuna_actual=comuna_actual,
                                                          comuna_previa=comuna_previa,
                                                          tipo_establecimiento=tipo_establecimiento,
                                                          puntaje=puntaje,
                                                          situacion=situacion,
                                                          nivel_educativo_madre=nivel_educativo_madre,
                                                          empleo_madre=empleo_madre,
                                                          ocupacion_madre=ocupacion_madre,
                                                          nivel_educativo_padre=nivel_educativo_padre,
                                                          empleo_padre= empleo_padre,
                                                          ocupacion_padre=ocupacion_padre,
                                                          sostenedor=sostenedor,
                                                          clase_social=p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted,
                                                          acceso_computador=p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente,
                                                          acceso_computador=p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente,
                                                          acceso_celular=p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente,
                                                          frecuencia_música=p22_con_que_frecuencia_escucha_musica,
                                                          preferencia_música=p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche,
                                                          preferencia_música=p24_si_eligio_otra_cual,
                                                          preferencia_música=p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar,
                                                          preferencia_música=p26_si_eligio_otra_cual,
                                                          dispositivo_música=p27_con_que_dispositivo_suele_escuchar_mas_musica,
                                                          app_música=p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica,
                                                          app_música=p29_si_respondio_otro_cual,
                                                          red_social_tiempo=p30_cual_es_la_red_social_pasa_mas_tiempo,
                                                          red_social_tiempo=p31_si_respondio_otra_cual,
                                                          red_social_tiempo=p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo,
                                                          red_social_tiempo=p33_si_respondio_otra_cual)
                                            
base_antropologia <- base_antropologia %>% dplyr::rename( edad = edad, 
                                                         genero=genero, 
                                                         anio_carrera=anio_carrera,
                                                         comuna_actual=comuna_actual,
                                                         comuna_previa=comuna_previa,
                                                         tipo_establecimiento=tipo_establecimiento,
                                                         puntaje=puntaje,
                                                         situacion=situacion,
                                                         nivel_educativo_madre=nivel_educativo_madre,
                                                         empleo_madre=empleo_madre,
                                                         ocupacion_madre=ocupacion_madre,
                                                         nivel_educativo_padre=nivel_educativo_padre,
                                                         empleo_padre= empleo_padre,
                                                         ocupacion_padre=ocupacion_padre,
                                                         sostenedor=sostenedor,
                                                         clase_social=p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted,
                                                         acceso_computador=p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente,
                                                         acceso_computador=p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente,
                                                         acceso_celular=p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente,
                                                         frecuencia_música=p22_con_que_frecuencia_escucha_musica,
                                                         preferencia_música=p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche,
                                                         preferencia_música=p24_si_eligio_otra_cual,
                                                         preferencia_música=p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar,
                                                         preferencia_música=p26_si_eligio_otra_cual,
                                                         dispositivo_música=p27_con_que_dispositivo_suele_escuchar_mas_musica,
                                                         app_música=p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica,
                                                         app_música=p29_si_respondio_otro_cual,
                                                         red_social_tiempo=p30_cual_es_la_red_social_pasa_mas_tiempo,
                                                         red_social_tiempo=p31_si_respondio_otra_cual,
                                                         red_social_tiempo=p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo,
                                                         red_social_tiempo=p33_si_respondio_otra_cual)


#3. Nuevamente observamos los nombres para comprobar que se hizo la recodificación
names(base_antropologia)

                                                          
# En VARIABLES CATEGÓRICAS--------------------------------------------------------------------------

#Una vez renombradas las variables, debo ver si es necesario RENOMBRAR SUS CATEGORÍAS DE RESPUESTA.
#¿En qué me fijo para determinar esto? muchas respuestas muy variadas referentes a lo mismo.
#Voy a elegir las variables que deseo recodificar, pero estas se deben hacer POR SEPARADO, 
#ya que al enfocarnos en las categorías de respuesta, estamos hablando de una variable que tiene
#categorías diferentes a las otras variables. Todas tienen categorías distintas.

# 1. Elijo variables de interés y veo CUÁLES NECESITAN SER RECODIFICADAS. 
#no necesariamente todas las variables necesitan ser recodificadas.
#por lo general las preguntas con respuesta abierta necesitan posterior recodificación.
names(base_antropologia)
#para esto, puedo ver los nombres de las variables para sacar ideas previas de cruces, etc.

#Veo las categorías de todas las variables para seleccionar la(s) variable(s) a recodificar.
sapply(base_antropologia, FUN = unique) 
#¿Qué hacía el sapply? ¿y el unique?

#en este caso vamos a elegir solo una variable para ejemplificar.
## VARIABLE DE INTERÉS: Situación Ocupacional de la madre: ocupacion_madre

# 2. Obtengo de manera más directa los valores únicos de esta variable: sus CATEGORÍAS DE RESPUESTA.
#¿Qué función debo utilizar para ver los valores únicos?
unique(base_antropologia$ocupacion_madre)
#Esta variable se respondía a través de una respuesta abierta, 
#por lo que hay diversas respuestas que refieren a un mismo concepto: que trabaja remuneradamente o que no trabaja.

# 3. Dejar todas las categorías en un mismo formato
#poner todo en minúscula y eliminar los espacios
#para facilitar la recodificación
base_antropologia$ocupacion_madre <- tolower(base_antropologia$ocupacion_madre) #todas a minusculas
base_antropologia$ocupacion_madre  <- gsub(pattern = " ", replacement = "", x = base_antropologia$ocupacion_madre) #elimino los espacios


# 4. Ver cómo quedan nuestras categorías con las modificaciones anteriores 
table(base_antropologia$ocupacion_madre)
unique(base_antropologia$ocupacion_madre)

# 5. MUTATE Y CASE_WHEN para recodificar categorías de respuesta
base <- base %>% mutate(variable_elegida=case_when(variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                   variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                   variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                   TRUE ~ ocupacion_madre)) #Conserva el valor original para cualquier otra categoría no especificada

base_antropologia <- base_antropologia %>%
  mutate(ocupacion_madre = case_when(ocupacion_madre == "reponedaraenunsupermercado" ~ "Trabajo remunerado",
                                     ocupacion_madre == "docentedeyoga" ~ "Trabajo remunerado",
                                     ocupacion_madre == "reponedora" ~ "Trabajo remunerado",
                                     ocupacion_madre == "administraciónenempresadetransportedevalores" ~ "Trabajo remunerado",
                                     ocupacion_madre == "jubilada" ~ "Trabajo no remunerado",
                                     ocupacion_madre == "-" ~ NA,
                                     ocupacion_madre == "auxiliardeservicio" ~ "Trabajo remunerado",
                                     ocupacion_madre == "abogada" ~ "Trabajo remunerado",
                                     ocupacion_madre == "dueñadecasa" ~ "Trabajo no remunerado",
                                     ocupacion_madre == "paisajista,estáacargodesupervisarydirigirelmantenimientodelasáreasverdesenunacomuna." ~ "Trabajo remunerado",
                                     ocupacion_madre == "tíadefurgón,transportarniñosdebásica" ~ "Trabajo remunerado",
                                     ocupacion_madre == "dueñadecasa,cuidadosdelhogar,,repostera" ~ "Trabajo no remunerado",
                                     ocupacion_madre == "cocineradecasino" ~ "Trabajo remunerado",
                                     ocupacion_madre == "tens,trabajaenelpensionadodeunhospitalytienequeatenderpersonaspostoperatoriosocasospsiquiátricosderivadosdelestado." ~ "Trabajo remunerado",
                                     ocupacion_madre == "instructordeyogayactividadfísicaparaadultosyadultosmayores,tambiéntrabajaporunaempresalocalcomoorganizadoradecasas.comoorganizadora,limpiaprofundamenteyordenacasas,botacosasenmasa,etc"~"Trabajo remunerado",
                                     ocupacion_madre == "tíadefurgón,transportarniñosdebásica\n"~ "Trabajo remunerado",
                                     TRUE ~ ocupacion_madre))


# 6. Verificamos nuestra recodificación: nuevas categorías de respuesta.
table(base_antropologia$ocupacion_madre)


# IF ELSE--------------------------------------------------------------------------------------
#"si no", "si es que pasa esto, haz lo siguiente"
#se utiliza para realizar operaciones condicionales en vectores o columnas de datos
#entonces requiere crear vectores para aplicar la función.

#Utilizaremos la variable nivel_educativo_madre como ejemplo:

# 1. Veremos las categorías de respuesta. Están claras. 
table(base_antropologia$nivel_educativo_madre) 
#para el análisis,por ej, nos podría servir clasificar estas categorías en nivel educativo bajo, medio y alto
#entonces vamos a asignar las categorías que correspondan a cada uno de estos tres niveles que establecimos

# 2. Vamos a guardar la variable como objeto, para que el código la pueda encontrar
#en el objeto se guarda la concatenación de todas las categorías de respuesta de la variable, es decir, se crea un vector con estas categorías.
nivel_educativo_madre <- c("Ed. Media completa", "Ed. Tecnica superior (comp. o incomp.)", "Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)")

# 3. Aplicamos ifelse de la siguiente manera:
# -si el nivel educativo de la madre llega a Ed. Media completa, será Bajo,
# -si el nivel educativo de la madre llega a Ed. Tecnica superior (comp. o incomp.)" y "Ed. Universitaria (comp. o incomp.), será Medio,
# -si el nivel educativo de la madre llega a Ed. Posgrado (Magister, Doctorado), será Alto

#Estructura: (es para que se guien)
Nueva variable creada <- ifelse(Variable utilizada == "Categoría 1", "Clasificación designada A",
                          ifelse(Variable utilizada %in% c("Categoría 2", "Categoría 3"), "Clasificación designada B",
                            ifelse(Variable utilizada == "Categoría 4", "Clasificación designada C", NA))) #Se agregó NA como valor si ninguna de las condiciones se cumple.

#Ahora lo aplicamos...
nivel_educativo_madre_ordinal <- ifelse(nivel_educativo_madre == "Ed. Media completa", "Bajo",
                                      ifelse(nivel_educativo_madre %in% c("Ed. Tecnica superior (comp. o incomp.)", "Ed. Universitaria (comp. o incomp.)"), "Medio",
                                             ifelse(nivel_educativo_madre == "Posgrado (Magister, Doctorado)", "Alto", NA)))


table(nivel_educativo_madre_ordinal) 


#Eliminar tildes y quitar ñ-----------------------------------------------------
#Hay otras formas de igualar los formatos: eliminar tildes y quitar ñ

##Podemos ocupar gsub
#saco tildes y ñ de la variable comuna_actual
unique(base_antropologia$comuna_actual)

base_antropologia$comuna_actual  <- gsub("[áÁ]", "a", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[éÉ]", "e", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[íÍ]", "i", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[óÓ]", "o", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[úÚ]", "u", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("ñ", "n", x = base_antropologia$comuna_actual)
table(base_antropologia$comuna_actual)

#Verificamos
table(base_antropologia$comuna_actual)

##Podemos ocupar Latin-ASCII
#Latin-ASCII: carácteres especiales que no llevan tildes, ñ, etc. 
#saco tildes y ñ de las variables empleo_madre y empleo_padre
unique(base_antropologia$empleo_madre)
unique(base_antropologia$empleo_padre)

base_antropologia <- base_antropologia %>%
  mutate(
    empleo_madre = stringi::stri_trans_general(empleo_madre, "Latin-ASCII"),  # Convierte caracteres latinos en la columna `empleo_madre` a su equivalente en formato ASCII (lo mismo pero sin tildes, ñ, etc)
    empleo_padre = stringi::stri_trans_general(empleo_padre, "Latin-ASCII"),  # Convierte caracteres latinos en la columna `empleo_padre` a su equivalente en formato ASCII (lo mismo pero sin tildes, ñ, etc)
    empleo_madre = tolower(empleo_madre),  # Convierte todos los caracteres en la columna `empleo_madre` a minúsculas
    empleo_padre = tolower(empleo_padre),  # Convierte todos los caracteres en la columna `empleo_padre` a minúsculas
    empleo_madre = gsub(" ", "_", empleo_madre),  # Reemplaza espacios por guiones bajos en la columna `empleo_madre`
    empleo_padre = gsub(" ", "_", empleo_padre)  # Reemplaza espacios por guiones bajos en la columna `empleo_padre`
  )

#Verificamos
table(base_antropologia$empleo_madre)
table(base_antropologia$empleo_padre)





#ANÁLISIS DE DATOS CATEGÓRICOS:--------------------------------------------------------------------------
#Dstribución de frecuencias y Tablas de contingencia
#Clase 6

# 0. Instaladores y Apertura base
##INSTALAR PAQUETES
# Paquetes para presentación
install.packages("rlang")
library(rlang)

install.packages("xaringan")
library(xaringan)

install.packages ("xaringanthemer")
library(xaringanthemer)

install.packages("summarytools")
library(summarytools)


#Paquetes para clase
pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, 
               summarytools, ggthemes)
#knitr y gt me permitirán dar formato a las tablas

install.packages("webshot")
webshot::install_phantomjs() # Sino funcionan tablas, instalar para exportar tablas de knitr


## ABRIR BASE DE DATOS
datos <- datos::encuesta


# I. Distribución de frecuencias -----------------------------------------------
#00. Observo la base
glimpse(datos)

#¿Cuántos casos y cuántas variables?
#¿Cuáles son los tipos de variables?
#int: integer: numerica
#fct: factor: cualitativa

#¿Quiero saber cuántas personas son de cada religión?
table(datos$religion)


#Forma 1: Con freq de summarytools####
freq(datos$religion) #está medio desordenada: frecuencias válidas y acumuladas por default, se repiten.
freq(datos$religion, prop = TRUE, order = "freq", report.nas =  FALSE) #eliminar valores NA y ordenar por frecuencia

#Vemos que en la tabla se contemplan los NA: No aplica, Sin respuesta, No sabe (podría ser considerado).

#Buscamos eliminar valores perdidos NA (Sin respuesta y No aplica)
#básicamente porque no nos sirven en el análisis que queremos hacer de los datos
#y además se contemplan como datos en las tablas, lo que altera los %

#Eliminar NA con mutate
#tratamos de establecer que Sin respuesta y No aplica son NA
datos %>% 
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), NA, religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)

#¿Qué pasó? siguen apareciendo los valores, que no fueron categorizados como NA, ni eliminados.
class(datos$religion) #religión = factor.
#ifelse parece tener problemas para trabajar con variables factor 
#¿Qué hago? convierto la variable religión en variable de tipo character

datos %>% 
  mutate(religion = as.character(religion)) %>% #transformo a character
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)

#Destacar la importancia de tipo de dato (class()) para hacer ciertos procedimientos. 

#EJERCICIO 1: tabla de frecuencia con freq####
#hacer una tabla de frecuencia con freq() de la variable raza
#¿Qué deberíamos hacer primero?

#1. Chequear el tipo de variable, para anticipar errores como el anterior.
##¿Con qué función lo hacemos? ¿Cómo completamos? ¿Qué tipo de variable es?
____(datos$____)

#2. Si es factor, transformar la variable a character
##¿Con qué función debería hacerse?

_____ %>% 
  ______(raza = as.character(raza)) %>% #transformo a character
  freq(raza, prop = TRUE, order = "freq", report.nas =  FALSE)

#¿Cómo lo interpretamos?
#Con respecto a la raza de las personas que respondieron la encuesta, 
#16.395 personas son blancas, es decir, un 76,32% de la muestra, lo que representa una gran mayoría
#por otro lado, 3.129 personas encuestadas son negras, esto es, un 14,57% de la muestra. 
#Esto representaría una cantidad significativamente menor con respecto a las personas blancas.
#por otro lado, 1.959 personas se identifican con otra raza, lo que significaría un 9,12% de la muestra.

#Conclusiónes
#hay una diferencia significativa y una enorme distancia entre la cantidad de personas blancas y negras.
#las personas blancas de la muestra conforman una gran mayoría respecto a otras razas. 

#Reflexión
#Desde ahí se pueden sacar varias conclusiones: primero respecto a que una abismal mayoría de la muestra
#corresponde a gente "blanca", lo cual nos invita a reflexionar sobre la construcción de este cuestionario,
#en el sentido de ¿qué es ser blanco? ¿quiénes entran en esta categoría?
#hay muchas identidades que se invisibilizan y que posiblemente son encasilladas en lo blanco,
#y quizás a ciertas personas les pudo haber pasado que no supieron cómo categorizarse y terminaron
#diciendo blanco. Eso ocurre cuando las categorías de respuesta son muy polares, y son insuficientes para
#demostrar la realidad social. Al no existir otras categorías, posiblemente las personas se pueden haber 
#visto presionadas a votar por la opción de blanco, y quizás eso explique además la enorme cantidad.
#lo otro que se nos puede venir a la cabeza es el tema de los sesgos, pues en este caso podría haber un sesgo
#al contemplar principalmente la noción de las personas blancas (igualmente esto depende de muchas cosas: lugar, contexto, ...)
#para esto es interesante evaluar el propósito de la investigación.


#Forma 2: mediante tidyverse####
datos %>%
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # generar  porcentaje del total con la frecuencia relativa multiplicada x 100
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondear el porcentaje a 2 decimales 
  arrange(desc(Porcentaje)) %>%  #ordenar los porcentajes de mayor a menor
  rename(Frecuencia = n, Religion= religion)  #de mayor a menor.
rename(Frecuencia = n, Religion= religion) #les da los nombres nuevos a los encabezados de la tabla.

#si se fijan, esto es un código compuesto por varias funciones, una tras otra,
#todas enlazadas por el %>% 
#de este modo, este código permite hacer múltiples funciones: contar, generar %, redondear, ordenar y renombrar.

#Observemos que la categoría "Sin respuesta" aparece en el cálculo,
#preferimos eliminarla filtrando, entonces agregamos una función para filtrar al código:
datos %>%
  filter (!(religion =="Sin respuesta")) %>% #elimino "Sin respuesta"
  count(religion) %>% #cuenta
  mutate(Porcentaje = n / sum(n) * 100) %>% #saca el % 
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #round redondea el % a 2 decimales y mutate transforma los porcentajes.
  arrange(desc(Porcentaje)) %>% #ordena los % de mayor a menor.
  rename(Frecuencia = n, Religion= religion) # renombrar para otorgar nombres "Frecuencia" y "Religion".

# ! es un operador de negación: significa "no igual a". Los filtra eliminándolos.

#Se agrega el Total con bind_rows
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 1)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion) %>% 
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 


#EJERCICIO 2: tabla de frecuencia con tidyverse#### 
#a. pruebe hacer una tabla de frecuencia mediante tidyverse con la variable raza
#b. pruebe hacerlo con otra variable más

#Recordar que son una cadena de funciones asociadas con %>% 

#Comparemos esta tabla de raza con la que hicimos anteriormente (con freq de summarytools)
#¿Qué diferencias encontramos?


# II.Tablas de contingencia ----------------------------------------------------
#Vamos a hacer tablas de cntingencia con tidyverse
#las podemos hacer de dos maneras: con ctable o con prop.table

#Se utilizan para hacer un cruce entre dos variables, ya no sólo una como vimos anteriormente
#nos permite evaluar cómo se relacionan dos variables.

#1. Mediante ctable()####
summarytools::ctable( x = datos$religion, y = datos$raza)

# cruce de dos variables categóricas:
# en la X suele ir variable dependiente: religión (izquierda)
# en la Y la independiente: raza (arriba)
# la pregunta es: ¿cómo la independiente modifica a la dependiente?

#¿Cómo la raza influye en la religión?
#¿La religión depende de la raza?
#la mayoría de personas de raza negra son protestantes, es decir, 2.271 personas, 
#mientras que la minoría de esta raza, es decir, una persona, es hinduista.

#la mayoría de personas encuestadas blancas, es decir, 8.188 personas, son protestantes,
#la segunda mayoría en este caso es la religión católica (4.001 personas)
#la minoría, es decir, 7 personas, se identifican con la religión Nativa americana. 
#la segunda minoría para las personas blancas encuestadas es el hinduismo, con 8 personas. 


#igualmente esta tabla está un poco desordenada, así que la vamos a ordenar...

#Orden general de tabla ####
#La tabla se puede reordenar dependiendo de lo que uno estime conveniente
#en este caso, ordenamos la raza en Blanca, Negra, Otra, por orden de mayor a menor.
datos$raza <- datos$raza %>% fct_relevel(c("Blanca", "Negra", "Otra")) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica

#Tarea: ¿Para qué servían las funciones fct_relevel y fct_drop? Busquen en chat gpt*

#para religión me fijo en el orden de importancia en la distribución de frecuencias y ordeno la tabla de esa manera.
#La religión más seleccionada es protestante.
#ordenamos de mayor a menor.

# Protestante   10846    50.49    50.49
# Católica    5124    23.85    74.34
# Ninguna    3523    16.40    90.74
# Cristiana     689     3.21    93.94
# Judía     388     1.81    95.75
# Otra     224     1.04    96.79
# Budismo     147     0.68    97.48
# Inter o no confesional     109     0.51    97.98
# Musulmana/Islam     104     0.48    98.47
# Cristiana ortodoxa      95     0.44    98.91
# Sin respuesta      93     0.43    99.34
# Hinduismo      71     0.33    99.67
# Otra religión oriental      32     0.15    99.82
# Nativa americana      23     0.11    99.93
# No sabe      15     0.07   100.00
# No aplica       0     0.00   100.00

datos$religion <- datos$religion %>% fct_relevel(c("Protestante", 
                                                   "Católica", 
                                                   "Ninguna", 
                                                   "Cristiana",
                                                   "Judía", 
                                                   "Budismo", 
                                                   "Inter o no confesional", 
                                                   "Musulmana/Islam", 
                                                   "Cristiana ortodoxa", 
                                                   "Hinduismo", 
                                                   "Otra religión oriental", 
                                                   "Nativa americana", 
                                                   "Otra", 
                                                   "No sabe", 
                                                   "No aplica", 
                                                   "Sin respuesta")) %>% 
                                    fct_drop("No aplica")


#Uso de proporciones (%)####
#Esto nos va a permitir construir tablas con porcentajes diferentes,
#podemos calcular los porcentajes de distintas formas, 
#es decir, desde distintos puntos de vista:

## a) TABLA SIN PORCENTAJES
ctable( x = datos$religion, y = datos$raza, prop = "n", justify = "l")
#prop = "n" : es sin proporciones
#justify: "l": es ajustar la tabla a la izquierda

# b) TABLA CON PORCENTAJES DEL TOTAL
ctable( x = datos$religion, y = datos$raza, prop = "t", justify = "l")

#INTERPRETACIÓN TOTAL
#¿Qué se puede interpretar? (en general)
#38.11% de la muestra son personas protestantes y blancas a la vez.

## c) TABLA CON PORCENTAJES SEGÚN FILA
ctable( x = datos$religion, y = datos$raza, prop = "r", justify = "l") 
#prop = "r" : es proporciones en filas

#INTERPRETACIÓN POR FILAS
#¿Qué se puede interpretar? (por fila)
#De las personas protestantes encuestadas, un 75,5% son blancas.

## d) TABLA CON PORCENTAJES SEGÚN COLUMNA
ctable( x = datos$religion, y = datos$raza, prop = "c", justify = "l")

#prop = "c" : es proporciones en columnas

#INTERPRETACIÓN por COLUMNAS ####
#¿Qué se puede interpretar? (por columnas) *importante*
# comparar porcentaje total con los porcentajes de las columnas e ir viendo
#si hay grandes diferencias
#más allá de un 5% del total se suele considerar una diferencia importante. 

#¿Cómo se presenta el cruce entre estas dos variables?
#¿Cómo la raza influye en la religión?
#la raza negra correspondería al 72,58% de protestantes, lo que quiere decir que
#la raza negra estaría influyendo en la religión que se tiene, al tener una tendencia protestante
#¿qué generaría esta tendencia cultural?

#Ejemplo práctico* #### 
#Realice una tabla (con ctable) entre partido y raza

# 1.Seleccionar la posición en donde colocar las variables 
#x dependiente
#y independiente

#¿cuál es la variable dependiente y cuál es la independiente?
#¿Podemos decir que una influye en la otra? ¿Cuál influiría en cuál?
#la raza influiría en el partido que se tiene, en tanto condicionaría pertenecer a uno u otro partido.
#sería, en este caso, algo cultural. 

summarytools::ctable( x = datos$partido, y = datos$raza)

# 2.Filtramos NA y Ordenamos
unique(datos$partido)
datos %>%
  mutate(partido = as.character(partido)) %>%
  mutate(partido = if_else(partido %in% c("Sin respuesta", "No aplica"), as.character(NA), partido)) %>%
  freq(partido, prop = TRUE, order = "freq", report.nas =  FALSE)

# 3.Recodificamos las categorías para dejar menos cantidad de categorías de respuesta
datos <- datos %>%
  mutate(partido_r = case_when(partido ==  "No fuertemente demócrata" ~ "Demócrata",
                               partido ==  "Fuertemente demócrata" ~ "Demócrata",
                               partido ==  "Ind, pro dem" ~ "Demócrata",
                               partido ==  "Fuertemente republicano" ~ "Republicano",
                               partido ==  "No fuertemente republicano" ~ "Republicano",
                               partido ==  "Ind, pro rep" ~ "Republicano",
                               partido ==  "Otro partido" ~ "Otro partido",
                               partido ==  "Independiente" ~ "Independiente", 
                               partido ==  "No sabe" ~ "No sabe"))

class(datos$partido_r)
datos$partido_r <- as.factor(datos$partido_r) 
class(datos$partido_r)

datos$partido_r <- datos$partido_r %>% fct_relevel(c("Demócrata", "Republicano", 
                                                     "Independiente", "Otro partido", "No sabe"))

#EJERCICIO 3: tabla de contingencia ctable e interpretación####
#Realice una tabla con ctable, con las variables que estime conveniente, e interprete: 
#¿Cuál es la variable dependiente y cuál es la independiente?


                                                                                                                                                                                                                     





#2. Mediante prop.table() #### 

# a) seleccionamos las variables y luego hacemos la tabla
datos %>% 
  select(religion, raza) %>% #seleccionamos las variables religion y raza
  table() #nos permite hacer la tabla. no funciona sin los () 


# b) realizo la tabla y le saco proporciones
datos %>% 
  select(religion, raza) %>% 
  table(.) %>% #le ponemos un punto para representar el primer argumento que es el objeto que contiene los datos que se van a analizar.
  prop.table(.)*100 #el punto remite calcular la proporción para todos los datos a utilizar

# c) elimino categoría "Sin respuesta"
datos %>%
  filter(religion != "Sin respuesta") %>% #eliminamos la categoría con filter
  select(religion, raza) %>%
  droplevels() %>% #elimina las categorías que no se utilizan en la columna
  table(.) %>% 
  prop.table(.)*100


# d) redondeo
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% 
  table(.) %>% 
  prop.table(.) %>% 
  round(4)*100 # con esto primero redondeo en cuatro decimales y multiplico por 100

#hasta aquí se han calculado los porcentajes en total, no por filas ni columnas. 
#asi que abajo lo calsularemos por filas y columnas

#Uso de proporciones (%) #### 
# proporciones por filas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% 
  table(.) %>% 
  prop.table(.,1) %>% # el segundo argumento de prop.table es si se hace por filas (1)* o columnas (2)
  round(4)*100 

# proporciones por columnas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>% 
  table(.) %>% 
  prop.table(.,2) %>% # el segundo argumento de prop.table es si se hace por filas (1) o columnas (2)*
  round(4)*100 

#Agrego totales con addmargins #### 
#en tablas anteriores no están los totales, addmargins permite agregarlos

datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,1) #agrega total de filas 

datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) #agrega total de columnas

# INTERPRETAMOS
#Utilizamos la tabla por columnas para interpretar.
#¿Qué se puede interpretar de esta tabla?
#

#Combino addmargins y prop.table #### 

# 1. proporciones por filas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  prop.table(.,1) %>% # la tabla se hace por filas
  addmargins(.,2) %>% # agrega el total por columnas 
  round(4)*100

# 2. proporciones por columnas
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins        lo invertimos
  prop.table(.,2) %>% #segundo prop.table        a diferencia de lo anterior
  round(4)*100 


#EJERCICIO 4: tabla de contingencia prop.table####
#realice una tabla de contingencia con prop.table, 
#que sea por columnas entre las variables partido_r y raza




# III. Formatear y Guardar tablas -----------------------------------------

#1. Distribución de frecuencias####

#Exportarla a excel ####

#A. CON FREQ()
### Crear la tabla y guardarla como data frame
f_religion1 <- datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() #con esto lo convierto al formato data.frame, el que puede ser exportado a excel

### Crear el directorio donde se van a guardar las tablas
#si dentro de mi carpeta de proyecto no existe el directorio tablas, esta función lo crea:
if(!dir.exists("tablas")) dir.create("tablas") 

### Guardar la tabla en formato excel en el directorio creado
write.xlsx(f_religion1, "tablas/f_religion1.xlsx") 

#B. CON TIDYVERSE
### Se crea la tabla
f_religion2 <- datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion) %>% 
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 
#en este caso no es necesario agregarle tb() porque ya creamos el data frame con f_religion2

### Se guarda en excell en el directorio que creamos antes (llamado tablas)
write.xlsx(f_religion2, "tablas/f_religion2.xlsx") 

# Exportarla formateada con kable####

#A. CON FREQ()
### Creamos la tabla
#donde se está transformando la variable religión a tipo character
#donde se está dejando las categorías "Sin respuesta" y "No aplica" como NA
#donde se está sacando la frecuencia de religion y eliminando los NA
#además se está guardando como data frame
#y con kable se está formateando, para darle nombre a las columnas y a la tabla.
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión")

#Podemos instalar kablExtra que nos brinda mayores posibilidades para formatear
install.packages("kableExtra") 
library(kableExtra)

#Tabla en el Viewer como imagen:
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"), #nombre de las columnas del gráfico
        caption = "Distribución de frecuencias de Religión", #nombre del gráfico
        format = "html", digits = 2) %>%  #le doy formato con kable: html, y dos dígitos: decimales
  kable_classic(full_width = F, html_font = "Cambria") #le agrego tipo de letra

### Cambiar el formato de los decimales de punto a coma
#observo que los decimales están en punto (.) y los prefiero en coma (,)
options(OutDec= ",")

#y ahora corremos el código nuevamente para que quede lista la tabla para ser guardada
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Religión", 
        format = "html", digits = 2) %>%  
  kable_classic(full_width = F, html_font = "Cambria") 

#Guardar en una imagen
#agregamos save_kable() al código 
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Religión", 
        format = "html", digits = 2) %>%  
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/f_religion1.png", zoom = 3) #con esto la guardamos

# para ver las posibilidades de formato:
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

#B.CON TIDYVERSE
### Hago la tabla
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en dos decimales
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) %>% #renombro dos categorías
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) %>% 
  kable(col.names = c("Religion", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formato con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15)  #le doy formato 

#guardar en una imagen
#agregamos save_kable ()
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en dos decimales
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) %>% #renombro dos categorías
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) %>% 
  kable(col.names = c("Religion", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formato con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/f_religion2.png", zoom = 3) #file = donde la guardamos, zoom = cuan grande se guarda


#2. Tablas de contingencia####
#Mediante prop.table()####

#proporciones por columnas 
c_religionxraza1 <- 
  datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

write.xlsx(c_religionxraza1, "tablas/c_religionxraza1.xlsx") #guardalo en un excel



#para ver una opción 
c_religionxraza1 %>% 
  kable(., caption="Tabla de contingencia para religión y raza") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) 


#para guardarlo

c_religionxraza1 %>% 
  kable(., caption="Tabla de contingencia para religión y raza") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/c_religionxraza1.png", zoom = 2)




