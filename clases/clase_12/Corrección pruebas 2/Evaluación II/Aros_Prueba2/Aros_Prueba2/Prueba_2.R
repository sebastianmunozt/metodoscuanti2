                                         ###         PRUEBA 2        ###

#                                       ***********Instrucciones************


# a. Ir a File y crear un Rproject denominelo: apellido_prueba2.Rproj, por ejemplo: "Ocampo_prueba2.Rproj" 

# b. Guardar este script (prueba_2.R) y base de datos de la encuesta del curso (base_antropologia_limpia.xlsx) en la carpeta en que se encuentra su proyecto apellido_prueba2.Rproj

# c. Comenzar y desarrollar la prueba en este script.

# d. Al finalizar debe guardar este script en la carpeta donde se encuentra el proyecto denominado apellido_prueba2.Rproj,
# el script se debe denominar igual que el archivo del proyecto, es decir, apellido_prueba2.R (sólo cambia su extensión de .Rproject a .R) 
# comprimir la carpeta del proyecto (esta carpeta debe contener al menos 3 archivos: apellido_prueba2.Rproj, apellido_prueba2.R y base_antropologia_limpia.xlsx) 
# y enviarla a ginowrs@gmail.com y semunoz@uahurtado.cl Con el asunto: apellido prueba 2

# La prueba consta de 2 partes, la primera de desarrollo.
# Y la segunda de código en R. 

# Mucha Suerte, que les vaya bien!!

#                                                 Primera parte

# Seleccione 3 de las siguientes 8 preguntas y responda debajo de la pregunta correspondiente,
# su respuesta debe ser clara y responder específicamente lo que se consulta, (mínimo un párrafo por respuesta, máximo 2). (6 ptos en total) cada pregunta tiene 2 ptos.

# 1. Describa la diferencia entre la hipótesis nula y la hipótesis alternativa en un contraste de hipótesis.
#La hipotesis nula es basicamente lo contrario de la hipotesis alternativa. Por lo tanto, se crea una hipotesis alternativa/especfiica
#para trabajarla con la evidencia muestral, para poder revisar si la hipotesis puede ser mantenida o rechazada, a partir, de la hipotesis nula

### Bien!

# 2. ¿cuál es la manera en que se debe interpretar el valor-p y cómo repercute este valor sobre la decisión acerca del contraste de hipótesis?
# 3. Describa qué son los estadísticos de tendencia central, de 2 ejemplos de este tipo de estadísticos y cuáles son sus diferencias con respecto a las medidas de dispersión
# 
# 4. Cuál es la diferencia entre la estadística descriptiva y la estadística inferencial?
#   
# 5. ¿cuál es la diferencia entre las pruebas de contraste de hipótesis de Chi cuadrado con ANOVA?
#la diferencia entre las pruebas de contraste de hipotesis de Chi cuadrado es que aquella se utiliza cuando 
#contamos con dos variables categóricas (nominales u ordinales).  Y Anova se utiliza cuando contamos con una variable cuantitativa y otra categórica (variable de agrupación). 
#Esta última debe diferencia desde tres grupos. 

### La principal diferencia entre las pruebas de Test de hipótesis Chi cuadrado y ANOVA, es el método de cada una, ya que la prueba Chi cuadrado compara frecuencias observadas con frecuencias esperadas,
### mientras que el test de ANOVA compara variabilidad entre y dentro de los grupos. De igual modo, se presenta una diferencia en el tipo de variables que utilizan. BIEN!!! 

# 6. ¿a qué se refiere con que la correlación no establece una dependencia entre variables?
# Es por  que no es causalidad, porque se definen por fuerza y linealibilidad de dos variables. Por lo tanto, la correlación es una medida estadística que describe la relación entre 
#dos variables cuantitativas. Que va desde un número que va desde -1 hasta 1. Por lo que es limitada. 

### Bien :)

# 7. ¿cuáles son las diferencias entre correlación y covarianza? y ¿cuáles son sus similitudes
# 8. ¿Por qué se dice que el proceso de inferencia sigue una lógica inductiva?


#                                                 Segunda parte

# Código en R 
  
# 00. Carga de Paquetes --------------------------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               summarytools,
               kableExtra,#Tablas elegantes
               webshot2,#exportar tablas
               chromote,
               viridis, #temas de gráficos
               hrbrthemes)#temas de gráficos


# 01. Cargar datos-------------------------------------------------------- 
base <- read.xlsx("base_antropologia_limpia.xlsx")

  
# 01.1 ¿cuántas filas y columnas tienen los datos? (0.5 ptos)
glimpse(base)
Rows: 147
Columns: 61
#(debe escribir código que entregue la información, no se aceptan respuestas sin código)

#02. Renombrar variables
#02.01: observe las variables de la base de datos con names(0.5)
names(base)
#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa(2 ptos)
# Para designar nuevos nombres a sus variables se puede apoyar en las siguientes etiquetas de las preguntas (use sólo las de su grupo):
base <- base %>% dplyr::rename(informacion_basura = ma_01,
                               gestion_residuos = ma_02,
                               participacion_cuidado = ma_03,
                               participacion_actividades = ma_04,
                               frecuencia_productos = ma_05) 
# "MA_01 En una escala del 1-10 siendo el 10 el mayor nivel de información y el 1 el menor ¿Qué tan informado se encuentra con respecto a la problemática del exceso de basura en las calles?"
# "MA_02 En una escala del 1 al 10, ¿Qué tan interesado estaría en participar en actividades educativas que amplíen su conocimiento sobre gestión de residuos y problemas relacionados?"
# "MA_03 ¿Ha participado en algún grupo, organización o proyecto ecológicos relacionados con el cuidado y preservación del medio ambiente?"
# "MA_04 ¿Ha participado en eventos o actividades comunitarias relacionadas a la difusión de información con respecto a los problemas medioambientales dentro de los últimos tres meses meses? Tales como charlas, ferias ecológicas, talleres prácticos o campañas específicas?"
# "MA_05 ¿Con qué frecuencia adquiere productos sostenibles o amigables con el medio ambiente, teniendo en cuenta la reducción de residuos generados por sus envases? Ttales como botellas reciclables, productos a granel, empaques minimalistas o envases reciclables"

# Puede utilizar el siguiente formato

# base <- base %>% ______::_____(Nombre nuevo = nombre antiguo,  
#                                ______ = ______,
#                                ______ = ______, 
#                                ______ = ______)
names(base)

# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos
base <- base %>% dplyr::rename(lugar_musica = cm_05) 
names(base)

# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)
table(base$lugar_musica)

# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.

# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)
respuestas <- strsplit(base$lugar_musica, ",")

# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

respuestas <- unlist(respuestas) 

# 03.5 observe las respuestas con freq (1 pto)
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub


 respuestas <- gsub(" En el transporte público", "En el transporte público", respuestas) #(1 pto)

 respuestas <- gsub(" En otros lugares", "En otros lugares", respuestas ) #(1 pto)



# 03.7 vuelva a observar sus respuestas con freq (1 pto)
 freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
   tb()

 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
 isidora_aros <- freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# 04. Ahora recodifique la variable re_02 (¿Cuál es su afiliación religiosa o creencia espiritual?)
# 04.1 observe las categorías de respuesta de esta variable (0.5 pto)
 names(base)
 table(base$re_02)
 
# 04.2 recodifique las categorías de esta variable en una nueva variable denominada "religion_rec" de la siguiente forma (3 ptos)
# Hint:  debe utilizar la función case_when()
 

# Las nuevas 3 categorías

# 1. Espiritualidad sin afiliación religiosa:
  
- "No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías)."
- "seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre"
- "deísmo"

# 2. Religión específica:

- "Catolico"
- "Cristianismo Protestante (Evangélico, anglicano, etcétera)"
- "Grecorromana "
- "Yoruba "
- "Ortodoxo"

# 3. Sin religión:

- "Pagana"    
- "Ateo"
- "Agnóstico"
- "No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria"
- "Ninguno "

# Utilice la siguiente estructura de código 

 base<- base%>%  mutate(religion_rec= case_when( re_02 %in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías).","seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", "deísmo") ~ "espiritualidad sin afiliacion religiosa",
                                               re_02 %in% c("Catolico","Cristianismo Protestante (Evangélico, anglicano, etcétera)","Grecorromana ","Yoruba ","Ortodoxo") ~"religion especifica",
                                              re_02 %in% c("Pagana","Ateo","Agnóstico","No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria","Ninguno ") ~"sin religion",
                                               TRUE~ religion_rec))


# 04.3 observe la nueva variable "religion_rec" con la función table (0.5 ptos)

table(base$religion_rec)

# 05.1 Ahora recodifique la variable to_01  (tiempo libre) en 2 categorías en la misma variable (2 ptos) 
# "Tiempo disponible": Bastante tiempo.
#                      Suficiente tiempo.

# "Tiempo limitado": No tengo tiempo.
#                    Poco tiempo.


base <- base %>% mutate(to_01=case_when(to_01 %in% c("Bastante tiempo.", "Suficiente tiempo.")~"tiempo disponible",
                                        to_01 %in% c("No tengo tiempo.", "Poco tiempo.")~ "tiempo limitado",
                                        TRUE~to_01))
table(base$to_01)

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"
summary(base)

# Media (0.5 ptos)
5.691
# Mediana (0.5 ptos)
5.800 
# desviación estándar (0.5 ptos)

# varianza (0.5 ptos)

# 07. Grafique los cruces de la variable  "ea_04_notas_ultimo_semestre" con las siguientes variables:

# Hint: La variable "ea_04_notas_ultimo_semestre" es la variable dependiente. 


# 07.1 ea_04_notas_ultimo_semestre vs religion_rec (3 ptos)

ggplot(base, aes( x= religion_rec, y= ea_04_notas_ultimo_semestre, fill = religion_rec  )) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Promedio de notas según tipo de religión", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "religion_rec",
       fill = "religion_rec")+
  theme(legend.position="none")

# 07.2 ea_04_notas_ultimo_semestre vs to_01 (3 ptos)

ggplot(base, aes(x= to_01, y=ea_04_notas_ultimo_semestre, fill=to_01)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Notas último semestre según tiempo", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "ea_04_notas_ultimo_semestre",
       fill = "to_01")+
  theme(legend.position="none")


# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"


ggplot(isidora_aros, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Lugares donde escuchan música", 
       subtitle = "Estudiantes de antropología", 
       caption = "Fuente: Encuesta Estudiantes Antropología 2024")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")
