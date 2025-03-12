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
# 
# 2. ¿cuál es la manera en que se debe interpretar el valor-p y cómo repercute este valor sobre la decisión acerca del contraste de hipótesis?
#   
# 3. Describa qué son los estadísticos de tendencia central, de 2 ejemplos de este tipo de estadísticos y cuáles son sus diferencias con respecto a las medidas de dispersión
# 
# 4. Cuál es la diferencia entre la estadística descriptiva y la estadística inferencial?
#Respuesta: La estadistica descriptiva resume y describe los datos de un conjunto específico usando medidas como media  y desviación estandar. La estadística inferencial, en cambio, hace inferencias sobre una población más amplia basándose en una muestra, utilizando métodos como pruebas de hipótesis y estimación de intervalos de confianza.

### Bien

#   
# 5. ¿cuál es la diferencia entre las pruebas de contraste de hipótesis de Chi cuadrado con ANOVA?
#   Respuesta: La prueba Chi cuadrado evalúa la asociación entre  variables categóricas comparando frecuencias observadas y esperadas de tablas de contingencia. En cambio la prueba ANOVA compara las medias de tres o más grupos para determinar si al menos uno es significativamente diferente analizando la variación dentro de los grupos.

### Bien

# 6. ¿a qué se refiere con que la correlación no establece una dependencia entre variables?
# Respuesta:   decir que la correlación no establece una dependencia entre variables significa que aunque dos variables puedan mostrar una relación estadística (correlación) esto no implica que una variable cause o dependa directamente de la otra; la corrrelación simplemente indica que hay una asociación, no una causalidad.

### Súper!

# 7. ¿cuáles son las diferencias entre correlación y covarianza? y ¿cuáles son sus similitudes?
#   
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
147 columnas y 60 filas #
num_filas <- nrow(base)
num_columnas <- ncol(base)

### Esta respuesta es exactamente la misma a la de otra compañera. Ojo con los plagios. 
### Hay una confusión entre las filas y columnas. En realidad la base tiene 147 filas y 60 columnas. 
### Por otro lado, no era necesario guardar los resultados en objetos

#(debe escribir código que entregue la información, no se aceptan respuestas sin código)
libro_codigos<- read.xlsx("base_antropologia_limpia.xlsx") #Esto es para guardar el libro de códigos. No se solicitaba. Si hubieras querido ponerlo, era más lógico ponerlo arriba, cuando cargas los datos y abres la base.

#02. Renombrar variables
#02.01: observe las variables de la base de datos con names(0.5)
glimpse(base)
names(base)
#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa(2 ptos)
# Para designar nuevos nombres a sus variables se puede apoyar en las siguientes etiquetas de las preguntas (use sólo las de su grupo):

# "EA_01 ¿Cuántas horas dedica aproximadamente al estudio y a la realización de trabajos universitarios fuera del aula por cada día una semana hábil? (esto es: de lunes a viernes) Por ejemplo, si lunes y martes suelo estudiar más o menos 3 horas, miércoles 5 horas y, finalmente jueves y viernes suelo estudiar 2 horas, el total de horas es 15 Dividido por 15 por 5, me da 3 Debería marcar la alternativa b (3 o 4 horas)"                                         
# "EA_02 ¿Cuántas horas dedica aproximadamente al estudio cada día, a lo largo del fin de semana? (sábado y domingo)"
# "EA_03 ¿Cómo describiría su carga académica actual?"
# "EA_04 En el último semestre finalizado ¿Aproximadamente qué promedio de notas ha obtenido? Considere el último semestre finalizado como el segundo semestre del año 2023."
# "EA_05 ¿Qué tan satisfecho está con tu rendimiento académico en el último semestre finalizado? Considere el último semestre finalizado como el segundo semestre del año 2023."
# "EA_06 En una escala del 1 al 5, donde 1 es "mínimo estrés" y 5 es "máximo estrés", ¿Cómo calificaría su nivel de estrés en la universidad en el último semestre finalizado?"
# "EA_07 ¿En qué medidas el estrés afecta su rendimiento académico?"
# "EA_08 ¿Puede identificar por sí mismo cuando se siente estresado debidos a factores relacionados con el ámbito universitario?"
# "EA_09 Cuando está en periodos de evaluaciones académicas ¿ha tenido alguno de estos síntomas? (Seleccione TODAS las alternativas que correspondan con su caso)"
# "EA_10 ¿Qué estrategias utiliza con mayor frecuencia para manejar el estrés académico? (Seleccione TODAS las alternativas que correspondan con su caso)"
# "RE_01 ¿Cómo califica su creencia de un ser supremo o deidades?"
# "RE_02 ¿Cuál es su afiliación religiosa o creencia espiritual? (Selecciona una opción)"
# "RE_03 ¿Con qué frecuencia acude a su religión o a instancias en dónde conectes con tu espiritualidad? (rezo, oración, meditación u otro)"
# "RE_04 ¿Asiste con regularidad a algún lugar destinado al culto religioso?"
# "RE_05 ¿Consideras que hay una influencia de la religión o la espiritualidad en SUS decisiones éticas y morales que toma en su vida cotidiana?"
# "RS_01 ¿Cuántas horas al día pasa en redes sociales? (Debes considerar la suma de todas las redes sociales que utilices)"
# "RS_02 ¿Cuáles son las principales razones por las que utiliza redes sociales? (Seleccione TODAS las alternativas que correspondan con su caso)"
# "RS_03 ¿Experimenta una sensación de “necesidad” o “expectativa social” de mantener una presencia activa en sus redes sociales mediante la publicación de fotos, videos, etcétera?"
# "RS_04 ¿Cómo gestiona el estrés relacionado con el uso de redes sociales?"
# "RS_05 ¿Qué red social sueles ocupar con mayor frecuencia?"
# "RS_06 ¿Cuál es la SEGUNDA red social que sueles ocupar con frecuencia?"
# "TO_01 ¿Cuánto tiempo libre considera que ha tenido por semana para dedicarse a actividades fuera del ámbito universitario? (Considere actividades fuera del ámbito universitario tales como deporte, entretenimiento, salidas, fiestas, etc)"
# "TO_02 Considere esta definición de ocio antes de contestar ésta y las siguientes preguntas: “El ocio se puede entender como el tiempo del que dispone una persona para distraerse e idealmente disfrutar de un momento agradable en su agenda personal o tiempo libre” Tales como salir tener vida social, hacer deporte o prácticas de consumo o participación cultural (leer, escuchar o tocar música, estar en internet) Excluya trabajo y estudio Ahora que ya comprende a qué se refiere el ocio en esta encuesta, responda: ¿Cuántos días a la semana (incluyendo semana y fin semana) realiza actividades de ocio?"
# "TO_03 ¿Administra su tiempo de forma óptima para equilibrar tus obligaciones académicas con las actividades de ocio?"
# "TO_04 ¿Cuáles de estas actividades prefiere realizar en su tiempo de ocio?"
# "TO_05 ¿Forma parte de un club, colectivo o taller donde se realicen actividades las actividades previamente mencionadas?"
# "TO_06 ¿Qué tan importante es para usted el tiempo dedicado a actividades de ocio?"
# "MA_01 En una escala del 1-10 siendo el 10 el mayor nivel de información y el 1 el menor ¿Qué tan informado se encuentra con respecto a la problemática del exceso de basura en las calles?"
# "MA_02 En una escala del 1 al 10, ¿Qué tan interesado estaría en participar en actividades educativas que amplíen su conocimiento sobre gestión de residuos y problemas relacionados?"
# "MA_03 ¿Ha participado en algún grupo, organización o proyecto ecológicos relacionados con el cuidado y preservación del medio ambiente?"
# "MA_04 ¿Ha participado en eventos o actividades comunitarias relacionadas a la difusión de información con respecto a los problemas medioambientales dentro de los últimos tres meses meses? Tales como charlas, ferias ecológicas, talleres prácticos o campañas específicas?"
# "MA_05 ¿Con qué frecuencia adquiere productos sostenibles o amigables con el medio ambiente, teniendo en cuenta la reducción de residuos generados por sus envases? Ttales como botellas reciclables, productos a granel, empaques minimalistas o envases reciclables"
# "CM_01 Aproximadamente ¿Con qué frecuencia suele escuchar música durante la semana?"
# "CM_02 Aproximadamente ¿Cuántas horas al día escucha música?"
# "CM_03 ¿Qué género musical suele escuchar con frecuencia?"
# "CM_04 ¿Cuál es el segundo género musical que suele escuchar con frecuencia?"
# "CM_05 ¿Dónde suele escuchar música habitualmente?"
# "CM_06 ¿Qué tan importante es la música para ti en su día a día?" 

# Puede utilizar el siguiente formato


base <- base %>% dplyr::rename(calificacion_creencias = re_01)
base <- base %>% dplyr::rename(afiliacion_religiosa = re_02)
base <- base %>% dplyr::rename(frecuencia_religiosa = re_03)
base <- base %>% dplyr::rename(asistencia_religiosa = re_04)

#                                ______ = ______,
#                                ______ = ______, 
#                                ______ = ______)

### Se renombran por separado, no se utiliza el formato del ejemplo, pero aún así se logra hacer la recodificación. 
### Hubiera sido bueno verificar la recodificación con names nuevamente

# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos

base <- base %>% rename(lugar_musica=cm_05)


# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)
table(base$lugar_musica)




# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.

# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)

respuestas <- strsplit(base$lugar_musica, ",")
table(base$lugar_musica)
# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

respuestas <- unlist(respuestas)
print(respuestas)

# 03.5 observe las respuestas con freq (1 pto)

freq (respuestas, prop = TRUE, order = "freq", report.nas = FALSE) %>% tb()

# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub


 respuestas <- gsub(" En el transporte público", "En el transporte público",  respuestas ) #(1 pto)

 respuestas <- gsub(" En otros lugares", "En otros lugares", respuestas ) #(1 pto)



# 03.7 vuelva a observar sus respuestas con freq (1 pto)
 freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
   tb()

 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
 
gonzalez_valentina <-  freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# 04. Ahora recodifique la variable re_02 (¿Cuál es su afiliación religiosa o creencia espiritual?)


# 04.1 observe las categorías de respuesta de esta variable (0.5 pto)
table(base$afiliacion_religiosa_recodificada)
# 04.2 recodifique las categorías de esta variable en una nueva variable denominada "religion_rec" de la siguiente forma (3 ptos)
# Hint:  debe utilizar la función case_when()
base <- base %>% 
  mutate(religion_rec = case_when(
    afiliacion_religiosa_recodificada %in% c("Católica", "Protestante") ~ "Cristiana",
    re_02 %in% c("Budista", "Hinduista", "Taoísta") ~ "Otras religiones",
    re_02 == "Agnóstico" ~ "Espiritual no religioso",
    re_02 == "Ateo" ~ "No religioso",
    TRUE ~ "Otro"
  ))
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

base <- base %>%  mutate(religion_rec= case_when(afiliacion_religiosa_recodificada %in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías).","seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", "deísmo") ~"_ _ _ _",
                                               afiliacion_religiosa_recodificada %in% c("Catolico","Cristianismo Protestante (Evangélico, anglicano, etcétera)","Grecorromana ","Yoruba ","Ortodoxo") ~"_ _ _ _",
                                               afiliacion_religiosa_recodificada %in% c("Pagana","Ateo","Agnóstico","No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria","Ninguno ") ~"_ _ _ _",
                                               TRUE~afiliacion_religiosa_recodificada))

table(base$afiliacion_religiosa_recodificada)


base <- base %>%
  mutate(afiliacion_religiosa_recodificada = case_when(
    afiliacion_religiosa %in% c("Católica", "Protestante") ~ "Cristiana",
    afiliacion_religiosa %in% c("Ateo", "Agnóstico") ~ "No religioso",
    afiliacion_religiosa == "Otra" ~ "Otra",
    TRUE ~ afiliacion_religiosa))

# 04.3 observe la nueva variable "religion_rec" con la función table (0.5 ptos)
table(base$afiliacion_religiosa_recodificada)



# 05.1 Ahora recodifique la variable to_01  (tiempo libre) en 2 categorías en la misma variable (2 ptos) 
# "Tiempo disponible": Bastante tiempo.
#                      Suficiente tiempo.

# "Tiempo limitado": No tengo tiempo.
#                    Poco tiempo.


base <- base %>% mutate(to_01=case_when(to_01%in% c("Bastante tiempo.", "Suficiente tiempo.")~"tiempo_disponible",
                                        to_01 %in% c("No tengo tiempo.", "Poco tiempo.")~ "tiempo_limitado",
                                        TRUE~to_01))

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"

# Media (0.5 ptos)

# Mediana (0.5 ptos)

# desviación estándar (0.5 ptos)

# varianza (0.5 ptos)
mean_ea_04 <- mean(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
median_ea_04 <- median(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
sd_ea_04 <- sd(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
var_ea_04 <- var(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)

# 07. Grafique los cruces de la variable  "ea_04_notas_ultimo_semestre" con las siguientes variables:

# Hint: La variable "ea_04_notas_ultimo_semestre" es la variable dependiente.


# 07.1 ea_04_notas_ultimo_semestre vs religion_rec (3 ptos)

ggplot(base, aes(x = religion_rec, y = ea_04_notas_ultimo_semestre, fill = religion_rec)) + 
  geom_boxplot(alpha = 0.3) +
  labs(
    title = "Promedio de notas según tipo de religión",
    y = "Promedio de notas último semestre",
    fill = "Religión"
  ) +
  theme_minimal()
names(base)

# 07.2 ea_04_notas_ultimo_semestre vs to_01 (3 ptos)

ggplot(base, aes(x= afiliacion_religiosa, y=ea_04_notas_ultimo_semestre, fill=afiliacion_religiosa)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Notas último semestre según tiempo", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "Promedio de notas último semestre",
       fill = "Tiempo")+
  theme(legend.position="none")

# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"

ggplot(gonzalez_valentina, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +
  
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
  
