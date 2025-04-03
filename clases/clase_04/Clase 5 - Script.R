

# 01. Repaso: Actividad de subsetting ------------------------------------------
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


#02. Introducción
#flujo de trabajo
#orden del script


# 03. Tipos de datos ------------------------------------------------------

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
pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer) #dos formatos de excel xlsx y xl


#Importar el archivo y la asigno en el environment
datos <- read.xlsx(xlsxFile = "base/Encuesta Estudiantes Antropología (respuestas).xlsx", colNames = TRUE, detectDates = TRUE)

names(datos) #observo que hay puntos, mayúsculas y minúsculas, etcétera. Está sucia


#limpieza inicial
datos <- janitor::clean_names(datos) #con esto transformo todo a minúscula, quito tildes, saco signos, borro espacios

names(datos)



#observación de base
nrow(datos) # cantidad de casos
ncol(datos) # cantidad de variables
sapply(datos, FUN = class) # sapply: realiza un a función a varias variables 

# Renombrar variables -----------------------------------------------------

#extraigo el nombre de todas las variables
names (datos)

# [1] "marca_temporal"                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
# [2] "p01_nombre_del_entrevistado_a"                                                                                                                                                                                                                                                                                                                                                                                                                                                       
# [3] "p02_edad_del_a_entrevistado"                                                                                                                                                                                                                                                                                                                                                                                                                                                         
# [4] "p03_genero_del_a_entrevistado_a"                                                                                                                                                                                                                                                                                                                                                                                                                                                     
# [5] "p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5"                                                                                                                                                                                                                                                                                                                                                                                                                        
# [6] "p05_comuna_actual_de_residencia"                                                                                                                                                                                                                                                                                                                                                                                                                                                     
# [7] "p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia"                                                                                                                                                                                                                                                                                                                                                            
# [8] "p07_ultimo_tipo_de_establecimiento_educativo_al_realizar_su_ensenanza_media"                                                                                                                                                                                                                                                                                                                                                                                                         
# [9] "p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida"                                                                                                                                                                                                                                                                                                                                                                                   
# [10] "p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes"                                                                                                                                                                                                                                                                                                                                                                                           
# [11] "p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre"                                                                                                                                                                                                                                                                                                                                                                                                                         
# [12] "p11_actualmente_su_madre_trabaja"                                                                                                                                                                                                                                                                                                                                                                                                                                                    
# [13] "p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre"                                                                                                                                                                                                                                                                                                                                         
# [14] "p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre"                                                                                                                                                                                                                                                                                                                                                                                                                         
# [15] "p14_actualmente_su_padre_trabaja"                                                                                                                                                                                                                                                                                                                                                                                                                                                    
# [16] "p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre"                                                                                                                                                                                                                                                                                                                                         
# [17] "p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos"                                                                                                                                                                                                                                                                                                                                                                                                 
# [18] "p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted"
# [19] "p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente"                                                                                                                                                                                                                                                                                                                                                                                          
# [20] "p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente"                                                                                                                                                                                                                                                                                                                                                                          
# [21] "p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente"                                                                                                                                                                                                                                                                                                                                                                                                                   
# [22] "p22_con_que_frecuencia_escucha_musica"                                                                                                                                                                                                                                                                                                                                                                                                                                               
# [23] "p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar"                                                                                                                                                                                                                                                                                                                                                                                                                              
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

# genero un vector con todas las columnas que quiero renombrar
cols_a_renombrar <- c(                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    "p01_nombre_del_entrevistado_a",                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    "p02_edad_del_a_entrevistado",                                                                                                                                                                                                                                                                                                                                                                                                                                                         
    "p03_genero_del_a_entrevistado_a",                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    "p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5",                                                                                                                                                                                                                                                                                                                                                                                                                         
    "p05_comuna_actual_de_residencia",                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    "p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia",                                                                                                                                                                                                                                                                                                                                                            
    "p07_ultimo_tipo_de_establecimiento_educativo_al_realizar_su_ensenanza_media",                                                                                                                                                                                                                                                                                                                                                                                                         
    "p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida",                                                                                                                                                                                                                                                                                                                                                                                   
    "p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes",                                                                                                                                                                                                                                                                                                                                                                                           
    "p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre",                                                                                                                                                                                                                                                                                                                                                                                                                         
    "p11_actualmente_su_madre_trabaja",                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    "p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre",                                                                                                                                                                                                                                                                                                                                         
    "p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre",                                                                                                                                                                                                                                                                                                                                                                                                                         
    "p14_actualmente_su_padre_trabaja",                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    "p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre",                                                                                                                                                                                                                                                                                                                                         
    "p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos",                                                                                                                                                                                                                                                                                                                                                                                                 
    "p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted",
    "p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente",                                                                                                                                                                                                                                                                                                                                                                                          
    "p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente",                                                                                                                                                                                                                                                                                                                                                                          
    "p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente",                                                                                                                                                                                                                                                                                                                                                                                                                   
    "p22_con_que_frecuencia_escucha_musica",                                                                                                                                                                                                                                                                                                                                                                                                                                               
    "p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar",                                                                                                                                                                                                                                                                                                                                                                                                                              
    "p24_si_eligio_otra_cual",                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    "p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar",                                                                                                                                                                                                                                                                                                                                                                                                                             
    "p26_si_eligio_otra_cual",                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    "p27_con_que_dispositivo_suele_escuchar_mas_musica",                                                                                                                                                                                                                                                                                                                                                                                                                                   
    "p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica",                                                                                                                                                                                                                                                                                                                                                                                                      
    "p29_si_respondio_otro_cual",                                                                                                                                                                                                                                                                                                                                                                                                                                                          
    "p30_cual_es_la_red_social_pasa_mas_tiempo",                                                                                                                                                                                                                                                                                                                                                                                                                                           
    "p31_si_respondio_otra_cual",                                                                                                                                                                                                                                                                                                                                                                                                                                                          
    "p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo",                                                                                                                                                                                                                                                                                                                                                                                                                                 
    "p33_si_respondio_otra_cual") 

#genero un vector sólo con las 3 primeras letras: p01
nuevos_nombres <- str_sub(string = cols_a_renombrar, start = 1, end = 3 ) #muestro los argumentos

nuevos_nombres <- str_sub(cols_a_renombrar, 1, 3)  #no muestro los argumentos
nuevos_nombres

#primer argumento - string = de donde saco los nombres: el vector creado
#segundo argumento - start = desde que posición extraigo (p)
#tercer argumento - end= hasta donde (1)


#renombro considerando todas las columnas elegidas asignando nuevos nombres
datos <- datos %>%
  rename_at(vars(cols_a_renombrar), ~ nuevos_nombres) #recodificación múltiples con un vector


#renombro algunas variables en específico

#veo categorías de todas las variables
sapply(datos, FUN = unique)

#posibilidad de renombrar uno por uno las variables de interés. 
datos <- datos %>% dplyr::rename(nombre = p01,  # primero nuevo nombre y luego nombre antiguo
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
DataExplorer::create_report(datos) 

# sino funciona el DataExplorer
# install.packages("htmltools", version = "0.5.4")
# library(htmltools)
# library(DataExplorer)


#función para realizar un reporte general introductorio
#me permite ver posibles transformaciones o limpiezas



# Transformaciones/limpieza en variables categóricas

names (datos)
unique(datos$comuna_actual)

#realizaremos un conjunto de transformaciones/limpiezas de nuestras BBDD. 

# para comuna actual
datos$comuna_actual <- tolower(datos$comuna_actual) #todas a minusculas
datos$comuna_actual  <- gsub(pattern = " ", replacement = "", x = datos$comuna_actual) #elimino los espacios

# datos$comuna_actual  <- gsub(" ", "", datos$comuna_actual) 
#tal es la forma por default. 

table(datos$comuna_actual)

# para comuna pre
datos$comuna_pre <- tolower(datos$comuna_pre)
datos$comuna_pre  <- gsub(" ", "", datos$comuna_pre)

table(datos$comuna_pre)

#utilizando el siguiente ejemplo, elimine los tildes y la ñ
# categories <- gsub(" ", "_", categories)
# categories <- gsub("[áÁ]", "a", categories)
# categories <- gsub("[éÉ]", "e", categories)
# categories <- gsub("[íÍ]", "i", categories)
# categories <- gsub("[óÓ]", "o", categories)
# categories <- gsub("[úÚ]", "u", categories)
# categories <- gsub("ñ", "n", categories)

#saco tildes y ñ de comuna actual
datos$comuna_actual  <- gsub("[áÁ]", "a", x = datos$comuna_actual)
datos$comuna_actual  <- gsub("[éÉ]", "e", x = datos$comuna_actual)
datos$comuna_actual  <- gsub("[íÍ]", "i", x = datos$comuna_actual)
datos$comuna_actual  <- gsub("[óÓ]", "o", x = datos$comuna_actual)
datos$comuna_actual  <- gsub("[úÚ]", "u", x = datos$comuna_actual)
datos$comuna_actual  <- gsub("ñ", "n", x = datos$comuna_actual)
table(datos$comuna_actual)


#saco tildes y ñ de comuna previa
datos$comuna_pre  <- gsub("[áÁ]", "a", x = datos$comuna_pre)
datos$comuna_pre  <- gsub("[éÉ]", "e", x = datos$comuna_pre)
datos$comuna_pre  <- gsub("[íÍ]", "i", x = datos$comuna_pre)
datos$comuna_pre  <- gsub("[óÓ]", "o", x = datos$comuna_pre)
datos$comuna_pre  <- gsub("[úÚ]", "u", x = datos$comuna_pre)
datos$comuna_pre  <- gsub("ñ", "n", x = datos$comuna_pre)
table(datos$comuna_pre)


#otra opción para sacar tildes, ñ, transformar todo a minúscula y eliminar espacios
library(tidyverse)

names(datos)
datos <- datos %>%
  mutate(empleo_madre = stringi::stri_trans_general(empleo_madre, "Latin-ASCII"),
         empleo_padre = stringi::stri_trans_general(empleo_padre, "Latin-ASCII"), 
         empleo_madre = tolower (empleo_madre),
         empleo_padre = tolower (empleo_padre), 
         empleo_madre = gsub(" ", "_", empleo_madre),
         empleo_padre= gsub(" ", "_", empleo_padre))

table(datos$empleo_madre)
table(datos$empleo_padre)

      

#limpieza de datos
table(datos$puntaje)
class(datos$puntaje)
unique (datos$puntaje)


#Para cambiar datos específicos de la BBDD.

#uso de ifelse
#estructura: ifelse(test, yes=, no=)

#test= lo que se quiere probar [argumento 1]
#yes= que valor poner si se cumple [argumento 2]
#no= que valor poner si no se cumple [argumento 3]

?ifelse



table(datos$puntaje) # puntaje tiene un valor raro

datos <- datos %>%
  mutate(puntaje = ifelse(puntaje == "700+", yes= "700", no= puntaje))

#otra opción (sin nombrar los argumentos)
datos <- datos %>%
  mutate(puntaje = ifelse(puntaje =="700+", "700", puntaje))


table(datos$puntaje)
class(datos$puntaje) # observo que está en character, ¿Qué pasa si está en character?

datos$puntaje <- as.numeric(datos$puntaje) #lo transformo a numeric
class(datos$puntaje)


#realizar recodificaciones
#edad

table(datos$edad) #observo las frecuencias de edad

datos <- datos %>% 
  mutate (edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                            edad %in% c(21:23) ~ "21 a 23", 
                            edad %in% c(24:29) ~ "24 a 29", 
                            edad >= 30 ~ "30 o más"))

table(datos$edadr) # observo la recodificación
class(datos$edad)
class(datos$edadr)


# ¿Quiero saber si estudiante es o no primera generación universitaria?
#realizo una tipología 

unique (datos$educacion_madre)
unique (datos$educacion_padre)


datos <-  datos %>% 
  mutate(generacion = ifelse(educacion_madre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ) | 
                               educacion_padre %in% c("Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)" ), 
                             "No es primera generación", 
                             "Si es primera generación")) 


table (datos$generacion) #observo mi recodificación


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

write.xlsx(x = datos,file = "base/Encuesta_Antropología_Limpia")


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


