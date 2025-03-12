# Clase 5 - Data Wrangling 2


# 03. Procesamiento de base de datos --------------------------------------

# install.packages("pacman")
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer) #Exploración rápida


#Importar el archivo y asignarlo en el environment----
base_antropologia <- read.xlsx("Encuesta-Estudiantes-Antropología-2023-(respuestas).xlsx")

#Explorar
glimpse(base_antropologia) #Una primera mirada de lo que hay en mis datos, la primera fila es extraña, dice "respuesta" o repite el nombre de la variable.

#quitar la primera fila a mis datos


names(base_antropologia) #observo que hay puntos, mayúsculas y minúsculas, etcétera. Está sucia


#limpieza inicial----
base_antropologia <- janitor::clean_names(base_antropologia) #con esto transformo todo a minúscula, quito tildes, saco signos, borro espacios

names(base_antropologia)# Queda mucho mejor



#observación de base
nrow(base_antropologia) #15 cantidad de casos
ncol(base_antropologia) #33 cantidad de variables
sapply(base_antropologia, FUN = class) # sapply: realiza un a función a varias variables 
str(base_antropologia) #estructura del objeto base de datos

# Renombrar variables -----------------------------------------------------

#extraigo el nombre de todas las variables
names (base_antropologia)
# [1] "marca_temporal"                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
# [2] "p01_nombre_del_entrevistado_a"                                                                                                                                                                                                                                                                                                                                                                                                                                                       
# [3] "p02_edad_del_a_entrevistado"                                                                                                                                                                                                                                                                                                                                                                                                                                                         
# [4] "p03_genero_del_a_entrevistado_a"                                                                                                                                                                                                                                                                                                                                                                                                                                                     
# [5] "p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5"                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [6] "p05_comuna_actual_de_residencia"                                                                                                                                                                                                                                                                                                                                                                                                                                                     
# [7] "p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_u_otros_as_cuidadores_o_en_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia"                                                                                                                                                                                                                                                                                                                                 
# [8] "p07_ultimo_tipo_de_establecimiento_educativo_en_que_realizo_su_ensenanza_media"                                                                                                                                                                                                                                                                                                                                                                                                      
# [9] "p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida"                                                                                                                                                                                                                                                                                                                                                                                   
# [10] "p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes"                                                                                                                                                                                                                                                                                                                                                                                           
# [11] "p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre"                                                                                                                                                                                                                                                                                                                                                                                                                         
# [12] "p11_actualmente_su_madre_trabaja"                                                                                                                                                                                                                                                                                                                                                                                                                                                    
# [13] "p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre"                                                                                                                                                                                                                                                                                                                                         
# [14] "p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre"                                                                                                                                                                                                                                                                                                                                                                                                                         
# [15] "p14_actualmente_su_padre_trabaja"                                                                                                                                                                                                                                                                                                                                                                                                                                                    
# [16] "p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre"                                                                                                                                                                                                                                                                                                                                         
# [17] "p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos"                                                                                                                                                                                                                                                                                                                                                                                                 
# [18] "p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted"
# [19] "p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente"                                                                                                                                                                                                                                                                                                                                                                                          
# [20] "p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente"                                                                                                                                                                                                                                                                                                                                                                          
# [21] "p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente"                                                                                                                                                                                                                                                                                                                                                                                                                   
# [22] "p22_con_que_frecuencia_escucha_musica"                                                                                                                                                                                                                                                                                                                                                                                                                                               
# [23] "p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche"                                                                                                                                                                                                                                                                                                                                                                 
# [24] "p24_si_eligio_otra_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                             
# [25] "p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar"                                                                                                                                                                                                                                                                                                                                                                                                                             
# [26] "p26_si_eligio_otra_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                             
# [27] "p27_con_que_dispositivo_suele_escuchar_mas_musica"                                                                                                                                                                                                                                                                                                                                                                                                                                   
# [28] "p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica"                                                                                                                                                                                                                                                                                                                                                                                                      
# [29] "p29_si_respondio_otro_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
# [30] "p30_cual_es_la_red_social_pasa_mas_tiempo"                                                                                                                                                                                                                                                                                                                                                                                                                                           
# [31] "p31_si_respondio_otra_cual"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
# [32] "p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo"                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [33] "p33_si_respondio_otra_cual"

# genero un vector con todos los nombres de las columnas que quiero renombrar
nombres <- c("marca_temporal", "p01_nombre_del_entrevistado_a", "p02_edad_del_a_entrevistado", "p03_genero_del_a_entrevistado_a", "p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5", "p05_comuna_actual_de_residencia", "p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_u_otros_as_cuidadores_o_en_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia", "p07_ultimo_tipo_de_establecimiento_educativo_en_que_realizo_su_ensenanza_media", "p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida", "p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes", "p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre", "p11_actualmente_su_madre_trabaja", "p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre", "p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre", "p14_actualmente_su_padre_trabaja", "p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre", "p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos", "p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted", "p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente", "p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente", "p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente", "p22_con_que_frecuencia_escucha_musica", "p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche", "p24_si_eligio_otra_cual", "p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar", "p26_si_eligio_otra_cual", "p27_con_que_dispositivo_suele_escuchar_mas_musica", "p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica", "p29_si_respondio_otro_cual", "p30_cual_es_la_red_social_pasa_mas_tiempo", "p31_si_respondio_otra_cual", "p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo", "p33_si_respondio_otra_cual")

#genero un vector sólo con las 4 primeras letras:
nuevos_nombres <- str_sub(string = nombres, start = 1, end = 4 ) #muestro los argumentos

nuevos_nombres <- str_sub( nombres,  1, 4 )

#primer argumento - string = de donde saco los nombres: el vector creado
#segundo argumento - start = desde que posición extraigo (p)
#tercer argumento - end= hasta donde (1)


#renombro considerando todas las columnas elegidas asignando nuevos nombres
base_antropologia <- base_antropologia %>%
  rename_at(vars(nombres), ~ nuevos_nombres) #recodificación múltiples con un vector

names(base_antropologia)

#renombro algunas variables en específico


#veo categorías de todas las variables
sapply(base_antropologia, FUN = unique) 

#posibilidad de renombrar uno por uno las variables de interés. # primero nuevo nombre y luego nombre antiguo

base_datos <- base_datos %>% dplyr::rename(nombrenuevo=nombre_antiguo,
                                           nombre_nuevo=nombre_antiguo)

base_antropologia <- base_antropologia %>% dplyr::rename( edad = p02_, 
                                                          genero=p03_, 
                                                          anio_carrera=p04_,
                                                          comuna_actual=p05_,
                                                          comuna_previa=p06_,
                                                          tipo_establecimiento=p07_,
                                                          puntaje=p08_,
                                                          situacion=p09_,
                                                          nivel_educativo_madre=p10_,
                                                          empleo_madre=p11_,
                                                          ocupacion_madre=p12_,
                                                          nivel_educativo_padre=p13_,
                                                          empleo_padre=p14_,
                                                          ocupacion_padre=p15_,
                                                          sostenedor=p17_,
                                                          clase_social=p18_)


#observamos los nombres
names(base_antropologia)


# selección y transformación de variables ---------------------------------
DataExplorer::create_report(base_antropologia) 

# sino funciona el DataExplorer
# install.packages("htmltools", version = "0.5.4")
# library(htmltools)
# library(DataExplorer)


#función para realizar un reporte general introductorio
#me permite ver posibles transformaciones o limpiezas



# Transformaciones/limpieza en variables categóricas---

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



#limpieza de base_antropologia
table(base_antropologia$puntaje)
class(base_antropologia$puntaje)
unique (base_antropologia$puntaje)


#Para cambiar datos específicos de la BBDD.

#uso de ifelse
#estructura: ifelse(test, yes=, no=)

#test= lo que se quiere probar [argumento 1]
#yes= que valor poner si se cumple [argumento 2]
#no= que valor poner si no se cumple [argumento 3]

?ifelse


table(base_antropologia$puntaje) # puntaje tiene un valor raro

base_antropologia <- base_antropologia %>%
  mutate(puntaje = ifelse(puntaje == "700+", yes= "700", no= puntaje))

#otra opción (sin nombrar los argumentos)
base_antropologia <- base_antropologia %>%
  mutate(puntaje = ifelse(puntaje =="700+", "700", puntaje))

#Mejor lo hago con case_when
base_antropologia <- base_antropologia %>%
  mutate(puntaje=case_when(puntaje=="500"~500,
                           puntaje=="590"~590,
                           puntaje=="610"~610,
                           puntaje=="630"~630,
                           puntaje=="650"~650,
                           puntaje=="670"~670,
                           puntaje=="680"~680,
                           puntaje=="700"~700,
                           puntaje=="no me acuerdo, pero creo que eran como 590/600"~590,
                           puntaje=="No se"~ NA,
                           puntaje=="No se aplica (ACT 30)"~NA,
                           TRUE ~puntaje))


table(base_antropologia$puntaje)
class(base_antropologia$puntaje) # observo que está en character, ¿Qué pasa si está en character?


base_antropologia$puntaje <- as.numeric(base_antropologia$puntaje) #lo transformo a numeric
class(base_antropologia$puntaje)


#realizar recodificaciones
#edad

table(base_antropologia$edad) #observo las frecuencias de edad

#Case_when----

#La función 'cases_when()' permite crear una nueva variable en base al valor(es) de otra(s) variable(s). Más cómodo que if_else() cuando la condición es complexa.



base_antropologia <- base_antropologia %>% 
  mutate (edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                            edad %in% c(21:23) ~ "21 a 23", 
                            edad %in% c(24:29) ~ "24 a 29", 
                            edad >= 30 ~ "30 o más"))

table(base_antropologia$edadr) # observo la recodificación
class(base_antropologia$edad)
class(base_antropologia$edadr)


# ¿Quiero saber si estudiante es o no primera generación universitaria?
#realizo una tipología 

unique (base_antropologia$nivel_educativo_madre)
unique (base_antropologia$nivel_educativo_padre)


base_antropologia <-  base_antropologia %>% 
  mutate(generacion = ifelse(nivel_educativo_madre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ) | 
                               nivel_educativo_padre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ), 
                             "No es primera generación", 
                             "Si es primera generación")) 



table (base_antropologia$generacion) #observo mi recodificación


#recordar operadores lógicos
# == (un valor),
# %in% 
# c(más de un valor),
# <,>, >=, <=, != 
# & (and), | (or)



# Análisis exploratorio de datos ------------------------------------------


#observo la base
str(base_antropologia)# Estructura base de datos
glimpse(base_antropologia) #Primer vistazo sobre la base de datos
dim (base_antropologia) #Dimensión de la base de datos


# Exportar ----------------------------------------------------------------

write.xlsx(x = base_antropologia,file = "base/base_antropologia_limpia")


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
