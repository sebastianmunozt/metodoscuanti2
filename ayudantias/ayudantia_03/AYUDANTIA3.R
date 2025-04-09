# AYUDANTÍA 3
# DATA WRANGLING y Repaso Tidyverse

# Data wrangling es el proceso de limpiar, transformar y organizar datos crudos
# para que estén listos para su análisis.

# En el proceso de data wrangling realizaremos:
# 1. Exploración de la base de datos: glimpse, summary y names
# 2. Limpieza de datos: limpiar y renombrar variables
# 3. Transformación de datos: recodificación de categorías


# Instalar y cargar paquetes necesarios-----------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               readxl,
               janitor, # limpieza de datos
               writexl, # guardar tablas formato excel
               DataExplorer) # exploración rápida


# Importación de la base de datos-----------------------------------------------
base_antropologia <- read.xlsx("Encuesta-Estudiantes-Antropología-2023-(respuestas).xlsx") %>%
  dplyr::select(3:ncol(.)) # seleccionar desde la columna 3 en adelante


# DATA WRANGLING----------------------------------------------------------------

# 1. Explorar la base de datos--------------------------------------------------

# Vistazo inicial
glimpse(base_antropologia)

# Sumario general
summary(base_antropologia)

# Ver nombres de las variables
names(base_antropologia)


# 2. Limpieza de datos: Renombrar variables-------------------------------------

# a) Limpiar nombres de variables
base_antropologia <- janitor::clean_names(base_antropologia)
names(base_antropologia)

# b) Renombrar variables
base_antropologia <- base_antropologia %>%
  dplyr::rename(
    edad = p02_edad_del_a_entrevistado,
    genero = p03_genero_del_a_entrevistado_a,
    anio_carrera = p04_ano_en_que_se_encuentra_de_la_carrera_1_2_3_4_5,
    comuna_actual = p05_comuna_actual_de_residencia,
    comuna_previa = p06_comuna_de_residencia_de_su_familia_nuclear_padres_hermanos_as_u_otros_as_cuidadores_o_en_la_que_vivio_la_mayor_parte_de_infancia_y_adolescencia,
    tipo_establecimiento = p07_ultimo_tipo_de_establecimiento_educativo_en_que_realizo_su_ensenanza_media,
    puntaje = p08_puntaje_final_obtenido_en_la_prueba_de_seleccion_universitaria_poderado_segun_carrera_elegida,
    situacion = p09_cual_de_estas_situaciones_describe_mejor_su_actividad_principal_durante_el_ultimo_mes,
    nivel_educativo_madre = p10_indique_el_maximo_nivel_educativo_obtenido_por_su_madre,
    empleo_madre = p11_actualmente_su_madre_trabaja,
    ocupacion_madre = p12_cual_es_la_ocupacion_u_oficio_actual_de_su_madre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_madre,
    nivel_educativo_padre = p13_indique_el_maximo_nivel_educativo_obtenido_por_su_padre,
    empleo_padre = p14_actualmente_su_padre_trabaja,
    ocupacion_padre = p15_cual_es_la_ocupacion_u_oficio_actual_de_su_padre_describa_las_principales_tareas_y_funciones_en_el_puesto_de_trabajo_actual_de_su_padre,
    sostenedor = p17_quien_es_el_principal_sostenedor_a_de_su_hogar_actual_quien_aporta_mas_ingresos,
    clase_social = p18_en_la_sociedad_comunmente_existen_distintos_grupos_o_clases_sociales_las_personas_de_clase_social_alta_son_las_que_tienen_los_ingresos_mas_altos_el_mayor_nivel_de_educacion_y_los_trabajos_mas_valorados_las_personas_de_clase_social_baja_son_las_que_tienen_los_ingresos_mas_bajos_el_menor_nivel_de_educacion_y_los_trabajos_menos_valorados_entre_estas_clases_existen_otras_intermedias_segun_su_opinion_a_cual_de_los_siguientes_grupos_o_clases_sociales_pertenece_usted,
    acceso_computador_hogar = p19_podria_decirme_si_su_casa_tiene_computador_ya_sea_notebook_o_de_escritorio_actualmente,
    acceso_computador_personal = p20_podria_decirme_si_usted_tiene_computador_para_uso_personal_ya_sea_notebook_o_de_escritorio_actualmente,
    acceso_celular = p21_podria_decirme_si_usted_tiene_smartphone_personal_actualmente,
    frecuencia_música = p22_con_que_frecuencia_escucha_musica,
    preferencia_música_1 = p23_que_tipo_de_musica_es_la_que_mas_prefiere_escuchar_aun_cuando_escuche_mas_de_un_estilo_elija_el_que_mas_escuche,
    preferencia_música_otra_1 = p24_si_eligio_otra_cual,
    preferencia_música_2 = p25_cual_es_la_segunda_musica_que_mas_prefiere_escuchar,
    preferencia_música_otra_2 = p26_si_eligio_otra_cual,
    dispositivo_música = p27_con_que_dispositivo_suele_escuchar_mas_musica,
    app_música = p28_cual_es_principal_sitio_programa_o_aplicacion_para_bajar_o_escuchar_musica,
    app_música_otra = p29_si_respondio_otro_cual,
    red_social_tiempo_1 = p30_cual_es_la_red_social_pasa_mas_tiempo,
    red_social_tiempo_otra_1 = p31_si_respondio_otra_cual,
    red_social_tiempo_2 = p32_y_cual_es_la_segunda_red_social_pasa_mas_tiempo,
    red_social_tiempo_otra_2 = p33_si_respondio_otra_cual
  )
names(base_antropologia)


# 3. Transformación de datos: Recodificación de variables-----------------------

# a) Variables cualitativas
sapply(base_antropologia, FUN = unique)

# Exploramos la variable de interés: ocupación de la madre
table(base_antropologia$ocupacion_madre)

# Homogeneizar valores
base_antropologia <- base_antropologia %>%
  mutate(
    ocupacion_madre = stringi::stri_trans_general(ocupacion_madre, "Latin-ASCII"),
    ocupacion_madre = tolower(ocupacion_madre),
    ocupacion_madre = gsub(" ", "_", ocupacion_madre)
  )

# Verificar categorías
table(base_antropologia$ocupacion_madre)
unique(base_antropologia$ocupacion_madre)

# Recodificar categorías
base_antropologia <- base_antropologia %>%
  mutate(
    ocupacion_madre = recode(ocupacion_madre,
                             "reponedara_en_un_supermercado_" = "Servicios",
                             "docente_de_yoga" = "Servicios",
                             "reponedora" = "Servicios",
                             "paisajista,_esta_a_cargo_de_supervisar_y_dirigir_el_mantenimiento_de_las_areas_verdes_en_una_comuna." = "Servicios",
                             "tia_de_furgon,_transportar_ninos_de_basica\r\n" = "Servicios",
                             "duena_de_casa,_cuidados_del_hogar,,_repostera_" = "Servicios",
                             "cocinera_de_casino" = "Servicios",
                             "instructor_de_yoga_y_actividad_fisica_para_adultos_y_adultos_mayores,_tambien_trabaja_por_una_empresa_local_como_organizadora_de_casas._como_organizadora,_limpia_profundamente_y_ordena_casas,_bota_cosas_en_masa,_etc" = "Servicios",
                             "tens,_trabaja_en_el_pensionado_de_un_hospital_y_tiene_que_atender_personas_post_operatorios_o_casos_psiquiatricos_derivados_del_estado." = "Salud y cuidado",
                             "auxiliar_de_servicio" = "Salud y cuidado",
                             "abogada" = "Administrativo / Profesional",
                             "administracion_en_empresa_de_transporte_de_valores" = "Administrativo / Profesional",
                             "duena_de_casa" = "Trabajo doméstico / Inactiva",
                             "jubilada" = "Trabajo doméstico / Inactiva",
                             "-" = "Desconocido"
    )
  )
table(base_antropologia$ocupacion_madre)

# b) Variables numéricas: recodificación con case_when
table(base_antropologia$puntaje)

base_antropologia <- base_antropologia %>%
  mutate(puntaje = case_when(
    puntaje %in% c("650", "670", "680", "700+") ~ "Alto",
    puntaje %in% c("610", "630") ~ "Medio",
    puntaje %in% c("500", "590", "no me acuerdo, pero creo que eran como 590/600") ~ "Bajo",
    puntaje %in% c("No se", "No se aplica (ACT 30)") ~ "NA",
    TRUE ~ NA_character_
  ))
table(base_antropologia$puntaje)

# 🔶**Ejercicio:** Completa el código para recodificar la variable 'edad' utilizando mutate y case_when

#1. Recodificar 

base_antropologia <- _______________ %>%
  ______(
    edad = _________(
      grepl("años", edad) ~ as.numeric(gsub(" años", "", edad)), # Elimina " años" y convierte a numérico
      TRUE ~ as.numeric(edad) 
    )
  )

#2. Verificar el resultado

_____(base_antropologia$edad_recodificada)


# 4. Guardar base de datos limpia-----------------------------------------------
dir.create(path = "base limpia")
write.xlsx(x = base_antropologia, file = "base limpia/Encuesta_Antropología_Limpia.xlsx")

# 🔶**Ejercicio:** Completa el código para guardar la base con el nombre Datos, en la carpeta llamada Output, ¿cómo sería el código? 

#1. Crear carpeta llamada 'Output'
 ______ (path = "______")

#2. Guardar base limpia con el nombre 'Datos' en la carpeta 'Output'
 ______ (x = _____, file = "______/Datos.xlsx")


# Conclusión--------------------------------------------------------------------
# Las herramientas tidyverse como mutate, case_when o rename son útiles para realizar
# data wrangling. También podemos usar filter para eliminar valores perdidos o
# select para trabajar con variables específicas.
