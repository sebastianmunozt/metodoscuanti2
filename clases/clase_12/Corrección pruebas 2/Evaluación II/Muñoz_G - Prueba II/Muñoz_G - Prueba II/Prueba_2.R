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
#   
# 5. ¿cuál es la diferencia entre las pruebas de contraste de hipótesis de Chi cuadrado con ANOVA?
#   La diferencia entre las pruebas de contraste de hipótesis de Chi cuadrado y ANOVA radica principalmente en el tipo de datos que analizan y en sus propósitos específicos.

#La prueba de Chi cuadrado se utiliza para analizar datos categóricos. Esta prueba evalúa si hay una asociación significativa entre dos variables categóricas. 
#Por ejemplo, podríamos usar una prueba de Chi cuadrado para determinar si hay una relación entre el género de los estudiantes y su elección de especialización en antropología. La prueba compara las frecuencias observadas en cada categoría con las frecuencias esperadas si no hubiera asociación entre las variables. 
#Si la diferencia entre las frecuencias observadas y esperadas es grande, podemos concluir que hay una asociación significativa entre las variables.
#Por otro lado, el ANOVA (Análisis de Varianza) se utiliza para comparar las medias de tres o más grupos y determinar si al menos una de las medias es significativamente diferente de las demás. Esta prueba es útil cuando tenemos una variable continua y queremos ver si su media difiere entre diferentes categorías o grupos.
#Por ejemplo, podríamos usar ANOVA para comparar las notas promedio de estudiantes de antropología que pertenecen a diferentes programas de estudio. ANOVA analiza la variabilidad dentro de los grupos y entre los grupos para determinar si las diferencias observadas en las medias son más grandes de lo que cabría esperar por azar.
#En resumen, la prueba de Chi cuadrado se utiliza para analizar la relación entre variables categóricas, mientras que ANOVA se utiliza para comparar las medias de una variable continua entre tres o más grupos. Ambas pruebas son esenciales en la investigación, pero se aplican en contextos diferentes según el tipo de datos y el objetivo del análisis.

### Muy buena respuesta. El detector indica que fue elaborada con un 100% de probabilidad de uso de IA. 

# 6. ¿a qué se refiere con que la correlación no establece una dependencia entre variables?

#   Cuando decimos que la correlación no establece una dependencia entre variables, nos referimos a que una correlación entre dos variables no implica que una variable cause cambios en la otra. 
#La correlación simplemente mide la fuerza y la dirección de una relación lineal entre dos variables.
#Por ejemplo, podríamos encontrar una correlación entre las horas de estudio y las calificaciones de los estudiantes de antropología. 
#Una alta correlación positiva indicaría que, en general, a más horas de estudio, mejores calificaciones. Sin embargo, esto no significa necesariamente que estudiar más cause mejores calificaciones. 
#Podrían estar involucrados otros factores, como la calidad del estudio, la dificultad de los cursos o incluso la inteligencia de los estudiantes.
#La clave aquí es recordar que la correlación no implica causalidad. Dos variables pueden estar correlacionadas debido a una coincidencia, a la influencia de una tercera variable, o a otras razones que no impliquen una relación causal directa. 
#Por ejemplo, podríamos encontrar una correlación entre el consumo de helado y la cantidad de personas que se ahogan en piscinas. Ambas variables pueden aumentar durante el verano, pero eso no significa que comer helado cause ahogamientos.
#En resumen, cuando decimos que la correlación no establece una dependencia, estamos diciendo que la correlación muestra una relación entre variables pero no prueba que una variable influya o dependa de la otra. Es fundamental considerar otros análisis y evidencias para determinar cualquier relación causal.

### Muy buena respuesta. El detector indica que fue elaborada en un 100% con IA

# 7. ¿cuáles son las diferencias entre correlación y covarianza? y ¿cuáles son sus similitudes?
# La correlación y la covarianza son dos medidas estadísticas que describen la relación entre dos variables, pero difieren en su interpretación y escala. La covarianza mide la dirección de la relación lineal entre dos variables, pero su valor depende de las unidades de las variables, lo que puede dificultar su interpretación.
#En cambio, la correlación es una versión normalizada de la covarianza, con valores entre -1 y 1, lo que facilita su interpretación: 1 indica una relación lineal positiva perfecta, -1 una relación lineal negativa perfecta, y 0 indica ausencia de relación lineal.
#Mientras que la covarianza muestra si dos variables tienden a aumentar o disminuir juntas sin indicar la fuerza de esta relación, la correlación proporciona información tanto sobre la dirección como sobre la fuerza de la relación. 
#A pesar de estas diferencias, ambas medidas tienen el propósito común de describir cómo se relacionan dos variables y se utilizan para identificar y cuantificar relaciones lineales.

### Muy buena respuesta. El detector indica que fue elaborada en un 100% con IA. Hay detalles que ni siquera fueron vistos en clases.


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
#(debe escribir código que entregue la información, no se aceptan respuestas sin código)

nrow(base)
ncol(base)

### Se ocupan las funciones correctas pero no se responde a la pregunta, por lo que no se puede saber si se identifica la diferencia entre casos y variables.

#02. Renombrar variables
#02.01: observe las variables de la base de datos con names(0.5)
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
library(dplyr)

 base <- base %>% dplyr::rename(informacion_basura = ma_01,  
                                interes_actividades_educativas = ma_02,
                                participacion_grupo_ambiental = ma_03, 
                                frecuencia_adquisicion_productos = ma_05)


# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos

base <- base %>% rename(lugar_musica=cm_05)


# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)

unique(base$lugar_musica)


# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.

# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)

respuestas <- strsplit(base$lugar_musica, ",")
print(respuestas)

# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

respuestas <- unlist (respuestas) 

# 03.5 observe las respuestas con freq (1 pto)
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub


 respuestas <- gsub(" En el transporte público", "En el transporte público",  respuestas) #(1 pto)

 respuestas <- gsub(" En otros lugares", "En otros lugares", respuestas ) #(1 pto)



# 03.7 vuelva a observar sus respuestas con freq (1 pto)
 freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
   tb()

 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
 
gonzalo_muñoz <-  freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# 04. Ahora recodifique la variable re_02 (¿Cuál es su afiliación religiosa o creencia espiritual?)
# 04.1 observe las categorías de respuesta de esta variable (0.5 pto)

unique(base$re_02)

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

base <- base %>%  mutate(religion_rec= case_when(re_02 %in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías).","seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", "deísmo") ~"espiritualidad_sin_afiliacion_religiosa",
                                               re_02 %in% c("Catolico","Cristianismo Protestante (Evangélico, anglicano, etcétera)","Grecorromana ","Yoruba ","Ortodoxo") ~"religion_especifica",
                                               re_02 %in% c("Pagana","Ateo","Agnóstico","No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria","Ninguno ") ~"sin_religion",
                                               TRUE~re_02))


# 04.3 observe la nueva variable "religion_rec" con la función table (0.5 ptos)

table(base$religion_rec)


# 05.1 Ahora recodifique la variable to_01  (tiempo libre) en 2 categorías en la misma variable (2 ptos) 
# "Tiempo disponible": Bastante tiempo.
#                      Suficiente tiempo.

# "Tiempo limitado": No tengo tiempo.
#                    Poco tiempo.


base <- base %>% mutate(to_01=case_when(to_01 %in% c("Bastante tiempo.", "Suficiente tiempo.")~"Tiempo disponible",
                                        to_01 %in% c("No tengo tiempo.", "Poco tiempo.")~ "Tiempo limitado",
                                        TRUE~to_01))

table(base$to_01)

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"

# Media (0.5 ptos)
media <- mean(base$ea_04_notas_ultimo_semestre,na.rm = TRUE)

# Mediana (0.5 ptos)
mediana <- median(base$ea_04_notas_ultimo_semestre,na.rm = TRUE)

# desviación estándar (0.5 ptos)
desviacion <- sd(base$ea_04_notas_ultimo_semestre,na.rm = TRUE)

# varianza (0.5 ptos)
varianza <- var(base$ea_04_notas_ultimo_semestre,na.rm = TRUE)


# 07. Grafique los cruces de la variable  "ea_04_notas_ultimo_semestre" con las siguientes variables:

# Hint: La variable "ea_04_notas_ultimo_semestre" es la variable dependiente.

# 07.1 ea_04_notas_ultimo_semestre vs religion_rec (3 ptos)

ggplot(base, aes(x=religion_rec, y=ea_04_notas_ultimo_semestre, fill=religion_rec)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Promedio de notas según tipo de religión", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "Promedio de Notas",
       fill = "Tipo de Religion")+
  theme(legend.position="none")

# 07.2 ea_04_notas_ultimo_semestre vs to_01 (3 ptos)

ggplot(base, aes(x=to_01, y=ea_04_notas_ultimo_semestre, fill=to_01)) + 
  geom_boxplot(alpha=0.3) +
  labs(
    title = "Notas último semestre según tiempo disponible",
    caption = "Fuente: Encuesta de Estudiantes UAH 2024",
    y = "Promedio de Notas",
    fill = "Tiempo Disponible") +
  theme(legend.position="none")


# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"


ggplot(gonzalo_muñoz, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +
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
