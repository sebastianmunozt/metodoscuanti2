                                                  ### PRUEBA 1 ###

#                                       ***********Instrucciones************

# a. Ir a File y crear un Rproject denominelo: apellido_prueba1.Rproj, por ejemplo: "Ocampo_prueba1.Rproj"
# b. Guardar este script (prueba_1_A.R) y base de datos de Alto Maipo (ALTO MAIPO.XLSX) en la carpeta Rproject
# c. Comenzar y desarrollar la prueba en este script
# d. Al finalizar debe guardar este script en la carpeta del proyecto denominada apellido_prueba1.Rproj, 
# comprimir la carpeta del proyecto y enviarla a ginowrs@gmail.com y semunoz@uahurtado.cl Con el asunto
# apellido prueba 1

#parte 0: Muestreo. Debe elegir 2 de las 4 preguntas y responderlas en este script

# 1- Nombre el elemento clave para que la selección de una muestra cuantitativa no esté sesgada. Dé un ejemplo de una muestra sesgada y cómo se evita esto.
# 2- Nombre las diferencias entre muestra y población, entre estadístico y parámetro. A partir de ello, explique qué significa hacer una inferencia.
# 3- Algunos errores se asumen debido a la técnica de muestreo misma. Otros son ajenos al muestreo. Nombre y explique un error de muestreo y un error ajeno al muestreo.
# 4- ¿Qué es un marco muestral y para qué sirve dentro del proceso de muestreo?

# Parte 1: R base ---------------------------------------------------------

#0.1 Crear un VECTOR llamado EDAD que contenga las edades de los/as miembros/as de su familia. (1 pto)
# -Deben ser almenos 4 integrantes, en caso de que su familia cuente con menos miembros debe inventar uno.
# -El vector debe contener sólo datos numéricos.


#0.2 Crear 3 vectores (llamarlos NOMBRE, nivel_educacional, comida_favorita)  (3 pto)
# con la misma cantidad de elementos que EDAD, pero con variables de tipo character (texto).
# Comida favorita: asigne los valores sin espacios de la comida favorita de cada integrante de su familia ej: ("lasagna", "asado","cazuela","pastel_de_choclo",etc)

#0.3 Modificar el tercer elemento de nivel_educacional, asignandole el valor "superior". (1 pto)

#0.4 Crear un dataframe (llamarla: BASE_DATOS) que contenga los 4 vectores creados previamente (1,5 pto)



# Parte 2: Abrir, guardar e indexar bases de datos ---------------------------------------

#01. Cree una carpeta en el escritorio (eg.  apellido_Prueba1), cree un proyecto e importen la base de datos de la
#Encuesta de Monitoreo de Indicadores Sociales del Proyecto Hidroeléctrico Alto Maipo (PHAM)  
#detallando la ruta desde el punto de partida del proyecto y asignandola a Alto Maipo
#recuerden que dicha base es un fragmento de la encuesta sobre los indicadores sociales del PHAM

read_excel(file = "ruta/archivo.xlsx") #recuerde asignar a un objeto ()


# 01.1 Elimine la primera fila de la base de datos
____ <- ____[-1,]

# 01.2 Indique la cantidad de observaciones y de variables utilizando las funciones 
# correspondientes (código).


# 02. limpieza inicial
library(janitor)
____ <- janitor::clean_names(____) #transforma todo a minúscula, sin tildes, ni signos, ni espacios


# 02.1 Para renombrar las variables ejecute el siguiente código rellenando con el 
# nombre que asignó a su base de datos.

____ <- ____ %>% dplyr::rename( anios_comuna = hace_cuantos_anos_vive_en_esta_comuna_enc_anotar_cantidad_de_anos_si_es_menos_de_un_ano_anotar_0,
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




# 03. Supongan que sólo les interesa trabajar con las variables edad, sexo y  temporalidad_traslado
# para saber los nombres utilice función names ()#esta función también ayuda con la posición de la variable

# Utilice el método de acceso de los corchetes [] para crear un nuevo objeto que contenga: 
# a) todas las filas de la base, 
# b) pero solo esas columnas

# 03.1 realicelo con el nombre de las variables (0.5 pto)

# 03.2 realicelo con la posición de las variables (0.5 pto)

# 03.3 observe si son iguales (0.5 pto)

#surespuesta en 03.1 == surespuesta en 03.2
   
#04. Tome esa misma base (ALTO MAIPO) y con el método de acceso de corchetes [] conserve  los primeros 200 casos/filas
#guardela en pham_200 (1 pto)
pham_200 <- pham [ 1:200,] #debe rellenar el siguiente código



# Parte 3: tidyverse: select, filter, mutate ---------------------------------------------------

#01. Cargar tidyverse (0.5 ptos)


#02. Usando la función select() seleccione las variables edad, sexo y  temporalidad_traslado con:
# El nombre de las variables (0.5 pto)
# La posición de las variables (0.5 pto)
# para observar el nombre de las variables puede hacerlo con names ()


#03. Recodifique las categorías de respuesta de la variable temporalidad_traslado 
#utilizando mutate junto con case_when() (2 ptos)

____<- ____ %>%
  mutate(temporalidad_traslado = case_when(
    temporalidad_traslado %in% c("1", "6", "1 dia", "1 día", "diaria", "Diaria",
                                 "Diaria diaria", "Diaria semanal", "Diariamente",
                                 "Diario", "7 días", "Todos los dias") ~ "1. Diaria",
    temporalidad_traslado %in% c("2","1 vez a la semana", "1 vez por semana", "Fin de semana",
                                 "Turno semanal", "3 veces al mes") ~ "2. Una vez por semana",
    temporalidad_traslado %in% c("Turnos 3er turno","Turno  1x3","3", "2 a 3 veces de la semana", "2 a 3 veces por semana",
                                 "2 dias", "2 o 3 veces por semana", "2 o más veces a la semana",
                                 "2 veces a la semana", "2 veces o más por semana",
                                 "2 veces por semana", "3 dias", "3 días", "3 Por semana",
                                 "3 veces a la semana", "3 veces por semana", "4 dias",
                                 "5 a 6 dias", "5 dias", "5 días", "6 dias", "6 días",
                                 "Dos veces por semana", "Más de 2 veces por semana",
                                 "Más de 2 veces x semana") ~ "3. Más de dos veces por semana",
    temporalidad_traslado %in% c("4", "2 veces al mes", "3° turno", "cada 15 dias",
                                 "Cada 15 dias", "cada 15 días", "Cada 15 días",
                                 "Más de 1 vez al mes", "Más de una vez al mes",
                                 "Turnos 10x10", "Turnos 20x10") ~ "4. Cada 15 días",
    temporalidad_traslado %in% c("1 vez al mes", "1 vez mes", "1 vez x mes",
                                 "1:vez al mes", "Una vez al mes") ~ "5. Una vez al mes",
    TRUE ~ temporalidad_traslado
  ))

# 0.4 Compruebe con laa función table que la recodificación esté correcta (1 pto)


# Parte 4: Exportar datos -------------------------------------------------------
# Guarde su base recodificada con sólo 200 casos y tres variables en la carpeta del proyecto con la siguiente función 


write.xlsx(x = ____ ,file = ____) #(2 ptos)