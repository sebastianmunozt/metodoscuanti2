#Práctica

# 01. Procesamiento de base de datos --------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, readxl,readr,
               janitor, forcats, writexl, DataExplorer) #dos formatos de excel xlsx y xl


#Importar el archivo y la asigno en el environment
datos <- read.xlsx(xlsxFile = "input/Encuesta Estudiantes Antropología 2023 (respuestas).xlsx", colNames = TRUE, detectDates = TRUE)

#Observe los datos mediante names y fijese cómo vienen los datos
names(datos) 


#limpieza inicial con janitor
#estructura: datos <- janitor::clean_names(base a limpiar)

datos <- janitor::clean_names(datos) 

#observe como quedaron con names ()
names(datos)

#observación de base
nrow(datos) # cantidad de casos: nrow(base)
ncol(datos) # cantidad de variables: ncol(base)
dim(datos) # casos y variables: dim(datos)
sapply(datos, FUN = class) # sapply: realiza un a función a varias variables: sapply(base, FUN = class)

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

#copie los nombres de todas las variables en el script
#borrele el indicador de posición

"marca_temporal"                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
"p01_nombre_del_entrevistado_a"                                                                                                                                                                                                                                                                                                                                                                                                                                                       
"p02_edad_del_a_entrevistado"                                                                                                                                                                                                                                                                                                                                                                                                                                                         
"p03_genero_del_a_entrevistado_a"                                                                                                                                                                                                                                                                                                                                                                                                                                                     
"p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5"                                                                                                                                                                                                                                                                                                                                                                                                                                 
"p05_comuna_actual_de_residencia"                                                                                                                                                                                                                                                                                                                                                                                                                                                     
"p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_u_otros_as_cuidadores_o_en_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia"                                                                                                                                                                                                                                                                                                                                 
"p07_ultimo_tipo_de_establecimiento_educativo_en_que_realizo_su_ensenanza_media"                                                                                                                                                                                                                                                                                                                                                                                                      
"p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida"                                                                                                                                                                                                                                                                                                                                                                                   
"p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes"                                                                                                                                                                                                                                                                                                                                                                                           
"p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre"                                                                                                                                                                                                                                                                                                                                                                                                                         
"p11_actualmente_su_madre_trabaja"                                                                                                                                                                                                                                                                                                                                                                                                                                                    
"p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre"                                                                                                                                                                                                                                                                                                                                         
"p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre"                                                                                                                                                                                                                                                                                                                                                                                                                         
"p14_actualmente_su_padre_trabaja"                                                                                                                                                                                                                                                                                                                                                                                                                                                    
"p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre"                                                                                                                                                                                                                                                                                                                                         
"p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos"                                                                                                                                                                                                                                                                                                                                                                                                 
"p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted"
"p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente"                                                                                                                                                                                                                                                                                                                                                                                          
"p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente"                                                                                                                                                                                                                                                                                                                                                                          
"p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente"                                                                                                                                                                                                                                                                                                                                                                                                                   
"p22_con_que_frecuencia_escucha_musica"                                                                                                                                                                                                                                                                                                                                                                                                                                               
"p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche"                                                                                                                                                                                                                                                                                                                                                                 
"p24_si_eligio_otra_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                             
"p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar"                                                                                                                                                                                                                                                                                                                                                                                                                             
"p26_si_eligio_otra_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                             
"p27_con_que_dispositivo_suele_escuchar_mas_musica"                                                                                                                                                                                                                                                                                                                                                                                                                                   
"p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica"                                                                                                                                                                                                                                                                                                                                                                                                      
"p29_si_respondio_otro_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
"p30_cual_es_la_red_social_pasa_mas_tiempo"                                                                                                                                                                                                                                                                                                                                                                                                                                           
"p31_si_respondio_otra_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
"p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo"                                                                                                                                                                                                                                                                                                                                                                                                                                 
"p33_si_respondio_otra_cual"



# genero un vector con todas las columnas que quiero renombrar
# utilice lo siguiente: cols_a_renombrar <- c()  
#copie los nombres de las columnas a renombrar desde la sección anterior
# entre cada nombre: ¿Qué debería poner?


cols_a_renombrar <- c(
  "marca_temporal",                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
  "p01_nombre_del_entrevistado_a"   ,                                                                                                                                                                                                                                                                                                                                                                                                                                                    
  "p02_edad_del_a_entrevistado"  ,                                                                                                                                                                                                                                                                                                                                                                                                                                                       
  "p03_genero_del_a_entrevistado_a"    ,                                                                                                                                                                                                                                                                                                                                                                                                                                                 
  "p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5"     ,                                                                                                                                                                                                                                                                                                                                                                                                                            
  "p05_comuna_actual_de_residencia"     ,                                                                                                                                                                                                                                                                                                                                                                                                                                                
  "p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_u_otros_as_cuidadores_o_en_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia"  ,                                                                                                                                                                                                                                                                                                                               
  "p07_ultimo_tipo_de_establecimiento_educativo_en_que_realizo_su_ensenanza_media"                ,                                                                                                                                                                                                                                                                                                                                                                                      
  "p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida"      ,                                                                                                                                                                                                                                                                                                                                                                             
  "p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes"  ,                                                                                                                                                                                                                                                                                                                                                                                         
  "p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre"     ,                                                                                                                                                                                                                                                                                                                                                                                                                    
  "p11_actualmente_su_madre_trabaja"    ,                                                                                                                                                                                                                                                                                                                                                                                                                                                
  "p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre"   ,                                                                                                                                                                                                                                                                                                                                      
  "p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre"    ,                                                                                                                                                                                                                                                                                                                                                                                                                     
  "p14_actualmente_su_padre_trabaja"    ,                                                                                                                                                                                                                                                                                                                                                                                                                                                
  "p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre"  ,                                                                                                                                                                                                                                                                                                                                       
  "p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos"                ,                                                                                                                                                                                                                                                                                                                                                                                 
  "p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted",
  "p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente",                                                                                                                                                                                                                                                                                                                                                                                          
  "p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente"  ,                                                                                                                                                                                                                                                                                                                                                                        
  "p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente",                                                                                                                                                                                                                                                                                                                                                                                                                   
  "p22_con_que_frecuencia_escucha_musica"  ,                                                                                                                                                                                                                                                                                                                                                                                                                                             
  "p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche" ,                                                                                                                                                                                                                                                                                                                                                                
  "p24_si_eligio_otra_cual"     ,                                                                                                                                                                                                                                                                                                                                                                                                                                                        
  "p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar"    ,                                                                                                                                                                                                                                                                                                                                                                                                                         
  "p26_si_eligio_otra_cual"    ,                                                                                                                                                                                                                                                                                                                                                                                                                                                         
  "p27_con_que_dispositivo_suele_escuchar_mas_musica"    ,                                                                                                                                                                                                                                                                                                                                                                                                                               
  "p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica"  ,                                                                                                                                                                                                                                                                                                                                                                                                    
  "p29_si_respondio_otro_cual"    ,                                                                                                                                                                                                                                                                                                                                                                                                                                                      
  "p30_cual_es_la_red_social_pasa_mas_tiempo"  ,                                                                                                                                                                                                                                                                                                                                                                                                                                         
  "p31_si_respondio_otra_cual"      ,                                                                                                                                                                                                                                                                                                                                                                                                                                                    
  "p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo"  ,                                                                                                                                                                                                                                                                                                                                                                                                                               
  "p33_si_respondio_otra_cual") 

cols_a_renombrar


#genero un vector sólo con las 3 primeras letras: p01
#forma: nuevos_nombres <- str_sub(string = cols_a_renombrar, start = desde donde, end = hasta donde )

#primer argumento - string = de donde saco los nombres: el vector creado
#segundo argumento - start = desde que posición extraigo (p)
#tercer argumento - end= hasta donde 

nuevos_nombres <- str_sub(string = cols_a_renombrar, start = 1, end = 3 ) #muestro los argumentos
nuevos_nombres


#renombro considerando todas las columnas elegidas asignando nuevos nombres

datos_r <- datos %>%
  rename_at(vars(cols_a_renombrar), ~ nuevos_nombres) #recodificación múltiples con un vector


datos_r <- datos %>%
  rename_at(vars(all_of(cols_a_renombrar)), ~ nuevos_nombres)


#para renombrar algunas variables en específico

#veo categorías de todas las variables
sapply(datos_r, FUN = unique)

#posibilidad de renombrar uno por uno las variables de interés. 
#renombre las 18 primeras variables: desde p01 a 018
# bbdd <- bbdd  %>% dplyr::rename(nombre_nuevo1 = nombre_viejo1, nombre_nuevo2 = nombre_viejo2, etc)

                                 
datos_r <- datos_r %>% dplyr::rename(nombre = p01,  # primero nuevo nombre y luego nombre antiguo
                        edad = p02, 
                        genero = p03, 
                        annio = p04, 
                        comuna_actual = p05, 
                        comuna_pre= p06, 
                        tipo_colegio = p07, 
                        puntaje = p08, 
                        estudio_trabajo = p09, 
                        educacion_madre = p10, 
                        trabaja_madre =p11, 
                        empleo_madre =p12, 
                        educacion_padre =p13, 
                        trabaja_padre =p14, 
                        empleo_padre = p15, 
                        psdhogar = p17, 
                        clase_social_subjetiva = p18)




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
datos_r <- datos_r %>%
  mutate(comuna_actual = stringi::stri_trans_general(comuna_actual, "Latin-ASCII"),
         comuna_pre = stringi::stri_trans_general(comuna_pre, "Latin-ASCII"), 
         comuna_actual = tolower (comuna_actual),
         comuna_pre = tolower (comuna_pre), 
         comuna_actual = gsub(" ", "_", comuna_actual),
         comuna_pre= gsub(" ", "_", comuna_pre))

table(datos_r$comuna_actual)
table(datos_r$comuna_pre)


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


datos_r <- datos_r %>%
  mutate(edad = ifelse(edad=="22 años", yes= 22, 
                          no=ifelse(edad=="30 años", yes=30, 
                                    no=edad)))
table(datos_r$edad)

datos_r$edad <- as.numeric(datos_r$edad) #lo transformo a numeric
class(datos_r$edad)



#realizar recodificaciones
#edad
#recodifico en 18 a 20, 21 a 23, mayores que 24

table(datos_r$edad) #observo las frecuencias de edad

datos_r <- datos_r %>% 
  mutate (edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                            edad %in% c(21:23) ~ "21 a 23", 
                            edad >= 24 ~ "Mayores que 24"))

table(datos_r$edadr) # observo la recodificación
class(datos_r$edad)
class(datos_r$edadr)


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





