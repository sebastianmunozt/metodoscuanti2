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



#0.2 Crear 3 vectores (NOMBRE, posicion_politica, comuna) (3 ptos)
# con la misma cantidad de elementos que EDAD, pero con variables de tipo character (texto).
# posicion_politica: asigne los valores: "extrema_izquierda", "izquierda", "centro", "derecha", "extrema_derecha", etc.

#0.3 Modificar el tercer elemento de posicion_politica, asignandole el valor "extrema derecha" (1 pto)

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



#03. Supongan que sólo les interesa trabajar con las variables edad_2, localidad y lugar_traslado

# para saber los nombres utilice función names ()#esta función también ayuda con la posición de la variable

# Utilice el método de acceso de los corchetes [] para crear un nuevo objeto que contenga: 
# a) todas las filas de la base, 
# b) pero solo esas columnas

# 03.1 realicelo con el nombre de las variables (0.5 pto)

# 03.2 realicelo con la posición de las variables (0.5 pto)

# 03.3 observe si son iguales (0.5 pto)

#surespuesta en 03.1 == surespuesta en 03.2

# 04. Tome esa misma base (ALTO MAIPO) y con el método de acceso de corchetes [] conserve  los primeros 200 casos/filas
#guardela en pham_200 (1 pto)
pham_200 <- ____ [ _:___,] #debe rellenar el siguiente código




# Parte 3: tidyverse: select, filter, mutate ---------------------------------------------------

#01. Cargar tidyverse (0.5 ptos)


#02. Usando la función select() seleccione las variables edad_2, localidad y lugar_traslado con:
# El nombre de las variables
# La posición de las variables
# para observar el nombre de las variables puede hacerlo con names ()



unique(pham_200$lugar_traslado)
#0.3 Recodifique las categorías de respuesta de la variable lugar_traslado 
#utilizando mutate junto con case_when() (2 ptos)

____ <- ____ %>%
  mutate(
    lugar_traslado = case_when(
      lugar_traslado %in% c("1", "Canelo", "El canelo", "El ingenio", "El Ingenio", "El melocoton", "El tuyo", 
                            "En la localidad ", "En la misma localidad", "Localidad donde vive", "Maitenes", "Manzano",
                            "Misma localidad", "Misma localidad donde vive", "Misma loclidad", "Romeral", "Volcan",
                            "Hacia otra localidad", "Lo valdes", "Los maitenes", "Los valdes", "Melocotón",
                            "otra localidad", "Otra localidad", "San alfonso", "San gabril", "San Gabriel", 
                            "San jise de maipo", "San José d maipo", "San jose de maipo", "San José de maipo", 
                            "San José de Maipo", "San jose de maipo ( hopsital)", "San jose de maipo ( la ennec )", 
                            "San jose de maipo (entre localidades )", "San jose de maipo (hospital)", 
                            "San jose de maipo 4 sur", "San jose de maipo, puente alto", "San jose de maipo, stgo", 
                            "Vertientes", "1 - Lugar") ~ "1. En la localidad donde vive",
      
      lugar_traslado %in% c("2", "Dentro de la comuna", "El canelo", "El ingenio", "El melocoton", "El tuyo", 
                            "fuera de la localidad", "Fuera de la localidad", "Guayacan", "Lo valdes", "Los maitenes",
                            "Los valdes", "Maitenes", "Manzano", "Melocotón", "Queltehue", "Vertientes") ~ "2. Hacia otra localidad",
      
      lugar_traslado %in% c("3", "Hospital san jose de maipo", "Jardín infantil san jose de maipo", "San José centro",
                            "San jose de maipo", "San José de maipo", "San José de Maipo", "San jose de maipo ( hopsital)",
                            "San jose de maipo centro", "San jose de maipo municipio", "San jose de maipo, puente alto stgo",
                            "San jose de maipo, santiago", "San jose de maipo, stgo") ~ "3. Al centro de San José de Maipo ",
      
      lugar_traslado %in% c("4", "Colina", "Fuera  de la comuna", "Fuera de la común", "fuera de la comuna", "Fuera de la comuna",
                            "Hacia otra comuna", "Las condes", "Las vizcachaz", "Lo espejo", "Otra comuna", 
                            "Otra comuna de la región metropolitana", "Otra conuna", "Otras comunas", "Pte alto", 
                            "Pte alto, stgo, san jose de maipo", "Puente alto", "Puente alto, san jose de maipo", 
                            "San joaquin", "San Tiago centro", "Santiago", "Santiago centro", "Vicuña makena", "Vitacura") ~ "4. Hacia otra comuna",
      
      lugar_traslado %in% c("5", "Calama", "Hacia otra region", "Norte (calama)", "Otra region", "Otra región", "Rancagua") ~ "5. Hacia otra región",
      
      TRUE ~ lugar_traslado
    )
  )

# 0.4 Compruebe con la función table que la recodificación esté correcta (1 pto)

# Parte 4: Exportar datos -------------------------------------------------------
# Guarde su base recodificada con sólo 200 casos y tres variables en la carpeta del proyecto con la siguiente función 


write.xlsx(x = ____ ,file = ____) #(2 ptos)
