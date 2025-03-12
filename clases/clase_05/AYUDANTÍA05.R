#Ayudantía 0.5
#Limpieza de datos: recodificación: cambio de nombres variables y sus categorías
#Análisis de datos categóricos: tablas de contingencia


# PROCESAMIENTO/LIMPIEZA DE DATOS:--------------------------------------------------------------------------

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


#1. EXPLORACIÓN DE LA BASE DE DATOS
glimpse(_________________) 
glimpse(base_antropologia)
#Una primera mirada de lo que hay en mis datos
#destello*

View(_________________)
View(base_antropologia)
#Podemos ver que la primera fila es extraña, dice "respuesta" o repite el nombre de la variable.
#tenemos que eliminar la primera fila a mis datos, ya que no corresponde a un dato
#Cómo lo hacemos? (con R base)
_______________ <- ________________[-1,]
base_antropologia <- base_antropologia[-1,]

#Miro los nombres de las variables de mi base de datos
#¿Con qué función hago esto?
_____(base_antropologia) 
names(base_antropologia)
#observo que hay puntos, mayúsculas y minúsculas, y nombres muy largos.
#Data "sucia" y es compleja de trabajar 


#2. LIMPIEZA INICIAL DE LA BASE DE DATOS
#transformo todo a minúscula, quito tildes, saco signos, borro espacios
#¿Cómo lo hago? ¿con qué paquete::función?
#pista: la función tiene que ver con las palabras limpieza_nombres y el paquete empieza con j
base_antropologia <- _______::___________(base_antropologia) 
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
____(base_antropologia) #15 cantidad de casos totales, 14 si descontamos la fila borrada


## ¿Con qué función averiguamos el NÚMERO DE COLUMNAS?
____(base_antropologia) #33 cantidad de variables


## ¿Cómo averiguamos el TIPO DE VARIABLE de todas las variables de la data?
#sapply: aplica una función a un conjunto de variables (que pueden ser todas)
#En este caso se está aplicando la función class a cada columna o variable de la data
##función class nos indica el tipo de variable (si son character o numeric)
#¿Qué debería poner ahí? 
#para que la función se aplique a cada variable de la base de datos?
sapply(_________________, FUN = class) 
sapply(base_antropologia, FUN = class)

#¿Para qué es importante que conozcamos esta información? (tipo de variable)
#El tipo de variable influye en cómo se pueden manipular los datos.
#Por ejemplo, los datos categóricos pueden necesitar ser recodificados o agrupados 
#de manera diferente a los datos numéricos.

## ¿Cómo averiguamos la ESTRUCTURA de la base de datos?
___(_________________) 
str(base_antropologia)
#Nos indica el nombre de la variable, 
#el tipo (categórica: chr, o numérica), 
#y sus categorías de respuesta


# RECODIFICACIÓN/TRANSFORMACIÓN DE DATOS: 

# RENOMBRAR ALGUNAS VARIABLES EN ESPECÍFICO: Elegir variables para renombrar---------------------------------------------------
#Posibilidad de renombrar una por una las variables de interés. 

# 1. Veo los nombress de todas las variables para seleccionar la(s) variable(s) que me interesa renombrar.
names(base_antropologia)
#Variables de interés: por ej, en el caso de que nos interese relacionar la educación con el empleo, 
#tendríamos que elegir las variables que nos permitan ver eso en particular.

# 2. Renombrar las variables elegidas con RENAME: 
#Estructura de ejemplo:
base_datos <- base_datos %>% dplyr::rename(nombre_nuevo=nombre_antiguo,
                                           nombre_nuevo=nombre_antiguo)

_________________ <- _________________ %>% dplyr::rename( edad = p02_edad_del_a_entrevistado, 
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


# RENOMBRAR TODAS LAS VARIABLES--------------------------------------------------------------------------------------------------
#Posibilidad de renombrar una por una todas las variables. 

# 1. Veo el nombre de todas las variables
names(base_antropologia)
#para ver qué nombre le voy a asignar a cada variable.

# 2. Renombrar con RENAME: (nombre_nuevo = nombre_antiguo)

________________ <- _________________ %>% _____::______( edad = p02_edad_del_a_entrevistado, 
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
                                                         ocupacion_padre=p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre
                                                         sostenedor=p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos,
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
                                                          sostenedor=p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos,
                                                          clase_social=p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted,
                                                          acceso_computador1=p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente,
                                                          acceso_computador2=p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente,
                                                          acceso_celular=p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente,
                                                          frecuencia_música=p22_con_que_frecuencia_escucha_musica,
                                                          preferencia_música1=p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche,
                                                          preferencia_música2=p24_si_eligio_otra_cual,
                                                          preferencia_música3=p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar,
                                                          preferencia_música4=p26_si_eligio_otra_cual,
                                                          dispositivo_música=p27_con_que_dispositivo_suele_escuchar_mas_musica,
                                                          app_música1=p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica,
                                                          app_música2=p29_si_respondio_otro_cual,
                                                          red_social_tiempo1=p30_cual_es_la_red_social_pasa_mas_tiempo,
                                                          red_social_tiempo2=p31_si_respondio_otra_cual,
                                                          red_social_tiempo3=p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo,
                                                          red_social_tiempo4=p33_si_respondio_otra_cual)


#3. Nuevamente observamos los nombres para comprobar que se hizo la recodificación
names(base_antropologia)


# En VARIABLES CATEGÓRICAS------------------------------------------------------

# 1.Veo las categorías de todas las variables para seleccionar la(s) variable(s) a renombrar.

sapply(base_antropologia, FUN = unique) 
#¿Qué hacía el sapply? ¿y el unique?

names(base_antropologia)

#Situación Ocupacional de la madre :
#- ocupacion_madre

unique(base_antropologia$ocupacion_madre)

#realizaremos un conjunto de transformaciones/limpiezas de nuestra BBDD. 

# para ocupación de la madre (respuesta abierta)
base_antropologia$ocupacion_madre <- tolower(base_antropologia$ocupacion_madre) #todas a minusculas
base_antropologia$ocupacion_madre  <- gsub(pattern = " ", replacement = "", x = base_antropologia$ocupacion_madre) #elimino los espacios

# base_antropologia$ocupacion_madre  <- gsub(" ", "", base_antropologia$ocupacion_madre) (forma por default)

table(base_antropologia$ocupacion_madre)

#Recodificar a 3 categorías 
#1. Trabajo remunerado; 2. Trabajo no remunerado; 3. No trabaja.

unique(base_antropologia$ocupacion_madre)

#Recodificamos con mutate y case_when (para recodificar categoría de respuesta)

base <- base %>% mutate(nueva_variable=case_when(variable_original=="Valor de la condición"~"Nuevo valor de la categoría",
                                                 variable_original=="talvalor"~"nuevovalor",
                                                 variable_original=="x valor"~"nuevovalor"))

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
                                     TRUE ~ ocupacion_madre)) # Conserva el valor original para cualquier otra categoría no especificada

#Verificamos la recodificación de la variable observando sus nuevos valores en una tabla según nuevas categorías
table(base_antropologia$ocupacion_madre)



#utilizando el siguiente ejemplo, elimine los tildes y la ñ
# categories <- gsub(" ", "_", categories)
# categories <- gsub("[áÁ]", "a", categories)
# categories <- gsub("[éÉ]", "e", categories)
# categories <- gsub("[íÍ]", "i", categories)
# categories <- gsub("[óÓ]", "o", categories)
# categories <- gsub("[úÚ]", "u", categories)
# categories <- gsub("ñ", "n", categories)

#saco tildes y ñ de comuna actual
base_antropologia$comuna_actual  <- gsub("[áÁ]", "a", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[éÉ]", "e", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[íÍ]", "i", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[óÓ]", "o", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("[úÚ]", "u", x = base_antropologia$comuna_actual)
base_antropologia$comuna_actual  <- gsub("ñ", "n", x = base_antropologia$comuna_actual)
table(base_antropologia$comuna_actual)


#saco tildes y ñ de comuna previa
base_antropologia$comuna_previa  <- gsub("[áÁ]", "a", x = base_antropologia$comuna_previa)
base_antropologia$comuna_previa  <- gsub("[éÉ]", "e", x = base_antropologia$comuna_previa)
base_antropologia$comuna_previa  <- gsub("[íÍ]", "i", x = base_antropologia$comuna_previa)
base_antropologia$comuna_previa  <- gsub("[óÓ]", "o", x = base_antropologia$comuna_previa)
base_antropologia$comuna_previa  <- gsub("[úÚ]", "u", x = base_antropologia$comuna_previa)
base_antropologia$comuna_previa  <- gsub("ñ", "n", x = base_antropologia$comuna_previa)
table(base_antropologia$comuna_previa)


#otra opción para sacar tildes, ñ, transformar todo a minúscula y eliminar espacios
library(tidyverse)

names(base_antropologia)
base_antropologia <- base_antropologia %>%
  mutate(
    empleo_madre = stringi::stri_trans_general(empleo_madre, "Latin-ASCII"),  # Convierte caracteres latinos en la columna `empleo_madre` a su equivalente ASCII
    empleo_padre = stringi::stri_trans_general(empleo_padre, "Latin-ASCII"),  # Convierte caracteres latinos en la columna `empleo_padre` a su equivalente ASCII
    empleo_madre = tolower(empleo_madre),  # Convierte todos los caracteres en la columna `empleo_madre` a minúsculas
    empleo_padre = tolower(empleo_padre),  # Convierte todos los caracteres en la columna `empleo_padre` a minúsculas
    empleo_madre = gsub(" ", "_", empleo_madre),  # Reemplaza espacios por guiones bajos en la columna `empleo_madre`
    empleo_padre = gsub(" ", "_", empleo_padre)  # Reemplaza espacios por guiones bajos en la columna `empleo_padre`
  )

table(base_antropologia$empleo_madre)
table(base_antropologia$empleo_padre)
