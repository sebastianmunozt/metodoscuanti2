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
# En un contraste de hipótesis, la hipótesis nula (H₀) y la hipótesis alternativa (H₁) son proposiciones complementarias utilizadas para realizar pruebas estadísticas. La hipótesis nula representa una afirmación de no efecto o no diferencia, es decir, sostiene que cualquier diferencia observada en los datos es atribuible al azar. Por ejemplo, en un estudio clínico, la hipótesis nula podría afirmar que un nuevo medicamento no tiene efecto sobre una enfermedad en comparación con un placebo. Por otro lado, la hipótesis alternativa propone que existe un efecto o diferencia real, sugiriendo que las observaciones son el resultado de una causa específica más allá del azar. Siguiendo el ejemplo anterior, la hipótesis alternativa podría afirmar que el nuevo medicamento tiene un efecto significativo en el tratamiento de la enfermedad. En esencia, la hipótesis nula es el punto de partida que se somete a prueba, mientras que la hipótesis alternativa es la afirmación que se intenta demostrar mediante la evidencia estadística. La validación de cualquiera de estas hipótesis se realiza a través de pruebas estadísticas que determinan la probabilidad de que los datos observados hayan ocurrido bajo el supuesto de la hipótesis nula. Si esta probabilidad es suficientemente baja, se rechaza la hipótesis nula en favor de la hipótesis alternativa.

### La respuesta está muy buena realmente, pero tuve que evaluar el uso de ia y el resultado fue que se utilizó 100% ia para responder.

# 2. ¿cuál es la manera en que se debe interpretar el valor-p y cómo repercute este valor sobre la decisión acerca del contraste de hipótesis?
#   El valor-p es una medida utilizada en estadística para determinar la significancia de los resultados obtenidos en un contraste de hipótesis. Representa la probabilidad de obtener un resultado igual o más extremo que el observado, asumiendo que la hipótesis nula (H₀) es verdadera. En otras palabras, el valor-p indica cuán compatibles son los datos con la hipótesis nula. Un valor-p bajo sugiere que los datos observados son poco probables bajo el supuesto de que H₀ es cierta, lo que proporciona evidencia en contra de la hipótesis nula. Generalmente, se establece un nivel de significancia (α), comúnmente 0.05, como umbral para tomar decisiones. Si el valor-p es menor o igual a α, se rechaza la hipótesis nula en favor de la hipótesis alternativa (H₁), indicando que los resultados son estadísticamente significativos. Por el contrario, si el valor-p es mayor que α, no se rechaza la hipótesis nula, sugiriendo que no hay suficiente evidencia para soportar la hipótesis alternativa. En resumen, el valor-p ayuda a determinar si los resultados observados son suficientemente inusuales para cuestionar la hipótesis nula, influyendo directamente en la decisión de rechazar o no rechazar dicha hipótesis en el contexto del contraste de hipótesis.

### Se utilizó ia para responder

# 3. Describa qué son los estadísticos de tendencia central, de 2 ejemplos de este tipo de estadísticos y cuáles son sus diferencias con respecto a las medidas de dispersión
# Los estadísticos de tendencia central son medidas que describen el punto central o típico de un conjunto de datos, proporcionando una idea de la ubicación central de los valores. Dos ejemplos de estos estadísticos son la media y la mediana. La media, o promedio, se calcula sumando todos los valores y dividiéndolos entre el número total de observaciones, proporcionando una medida del valor central considerando todos los datos. La mediana, en cambio, es el valor que divide el conjunto de datos en dos partes iguales, de modo que la mitad de los valores son menores y la otra mitad son mayores, siendo menos sensible a valores atípicos y sesgos en la distribución de los datos. 
#Estas medidas de tendencia central difieren de las medidas de dispersión, que describen la variabilidad o el grado de dispersión de los datos alrededor del centro. Ejemplos de medidas de dispersión son la varianza y el rango. La varianza mide la media de las desviaciones al cuadrado de cada valor respecto a la media, proporcionando una idea de cuánto varían los valores en promedio. El rango, por otro lado, es la diferencia entre el valor máximo y el mínimo del conjunto de datos, indicando la extensión total de los valores. Mientras que los estadísticos de tendencia central se centran en identificar el valor representativo del conjunto de datos, las medidas de dispersión se enfocan en describir cuán extendidos o concentrados están los valores en torno a ese centro.

### Se utilizó ia para responder


#                                                 Segunda parte

# Código en R 
  
# 00. Carga de Paquetes --------------------------------------------------------
install.packages("pacman")
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
# 147 columnas y 60 filas  ###Esto está erróneo.

#(debe escribir código que entregue la información, no se aceptan respuestas sin código)

num_filas<- nrow(base_antropologia)
num_columnas <- ncol(base_antropologia)

### Esta respuesta es exactamente la misma a la de otra compañera. Ojo con los plagios. 
### Por otro lado, hay un error en la respuesta, pues se confunden la cantidad de filas con las columnas
### Además, no se solicitaba que se guardaran los resultados en objetos

#02. Renombrar variables
#02.01: observe las variables de la base de datos con names(0.5)
names(base_antropologia)

#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa(2 ptos)
# Para designar nuevos nombres a sus variables se puede apoyar en las siguientes 
#etiquetas de las preguntas (use sólo las de su grupo):


# "RE_01 ¿Cómo califica su creencia de un ser supremo o deidades?"

# "RE_03 ¿Con qué frecuencia acude a su religión o a instancias en dónde conectes con tu espiritualidad? (rezo, oración, meditación u otro)"

# "RE_04 ¿Asiste con regularidad a algún lugar destinado al culto religioso?"

# "RE_05 ¿Consideras que hay una influencia de la religión o la espiritualidad en SUS decisiones éticas y morales que toma en su vida cotidiana?"
# 

# Puede utilizar el siguiente formato
install.packages("dplyr")
library(dplyr)

base <- base %$% 
  dplyr::rename(calificacion_creencias=re_01)
base <- base %$% 
  dplyr::rename(afiliacon_religiosa = re_02)
base <- base %$%
  dplyr::rename(frecuencia_religiosa= re_03)
base <- base %$%
  dplyr::rename(asistencia_religiosa=re_04)

base <- base %>%
  rename(calificacion_creencias = re_01) %>%
  rename(afiliacion_religiosa = re_02) %>%
  rename(frecuencia_religiosa = re_03) %>%
  rename(asistencia_religiosa = re_04)

### Primero, se realiza el proceso dos veces, y de ambas formas ninguna coincide con el formato entregado en el ejemplo
### Por otro lado, esta respuesta es exactamente la misma que la de su compañera, utilizando las mismas variables y poniendoles exactamente el mismo nombre
### Además se colocaron en el mismo orden
### Puede ser que sean del mismo grupo de trabajo, pero es muy notorio el plagio pues todas las personas le pueden colocar nombres distintos a las variables
### Existen múltiples posibilidades de nombres para las variables, ya que esos dependen de lo que uno decida, es decir, uno le inventa los nombres. 

# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos

base <- base %>% rename(Lugar_musica=cm_05)


# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)

table(base$Lugar_musica)


# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.

# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)

respuestas <- strsplit(base$Lugar_musica, ",")


# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

respuestas <- unlist (respuestas) 

# 03.5
freq <- frequency(respuestas, prop = TRUE, order = "freq", report.nas = FALSE)

### No se realiza correctamente
### se escribe frequency y no se agrega tb()
### Corrección:  
# freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
# tb()

# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub

### No se logra ver la tabla, y por ende no se logra evidenciar que las respuestas se contabilizan 2 veces.

 # Reemplaza las respuestas con espacios precedentes utilizando la función gsub
 respuestas <- gsub("^\\s+En el transporte público", "En el transporte público", respuestas)
 respuestas <- gsub("^\\s+En otros lugares", "En otros lugares", respuestas)
 
### Donde se escribió  ^\\s+  debería haber ido un espacio
### Igualmente funciona al parecer 
 
# 03.7 vuelva a observar sus respuestas con freq (1 pto)

 freq(respuestas,prop=TRUE, order="freq", report.nas=FALSE)

 ### Se logra sacar la tabla esta vez, a diferencia de la anterior
 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
 Javiera_herrera <- freq(respuestas, prop=TRUE,order="freq",report.nas=FALSE) %$%
   tb()

 ### Error in freq(respuestas, prop = TRUE, order = "freq", report.nas = FALSE) %$%  : 
 #  could not find function "%$%"
 ### Está mal escrito el pipe %>% 

# 04. Ahora recodifique la variable re_02 (¿Cuál es su afiliación religiosa o creencia espiritual?)
# 04.1 observe las categorías de respuesta de esta variable (0.5 pto)
 table(base$afiliacion_religiosa)

# 04.2 recodifique las categorías de esta variable en una nueva variable denominada "religion_rec" de la siguiente forma (3 ptos)
# Hint:  debe utilizar la función case_when()
 library(dplyr)
 
 base <- base %>%
   mutate(religion_rec = case_when(
     afiliacion_religiosa %in% c("católica", "protestante") ~ "cristiana",
     afiliacion_religiosa %in% c("Budista", "Hinduista", "Taoísta") ~ "otras religiones",
     afiliacion_religiosa == "Agnostico" ~ "Espiritual no religioso",
     afiliacion_religiosa == "ateo" ~ "no religioso",
     TRUE ~ "otro"
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




# 04.3 observe la nueva variable "religion_rec" con la función table (0.5 ptos)


 base <- base %>%
   mutate(religion_rec = case_when(
     afiliacion_religiosa %in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías).",
                                 "seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", 
                                 "deísmo") ~ "Espiritual no religioso",
     afiliacion_religiosa %in% c("Catolico", "Cristianismo Protestante (Evangélico, anglicano, etcétera)", "Grecorromana ", "Yoruba ", "Ortodoxo") ~ "Cristiana",
     afiliacion_religiosa %in% c("Pagana", "Ateo", "Agnóstico", "No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria", "Ninguno ") ~ "No religioso",
     TRUE ~ "Otro"
   ))
 
 table(base$religion_rec)

# 05.1 Ahora recodifique la variable to_01  (tiempo libre) en 2 categorías en la misma variable (2 ptos) 
# "Tiempo disponible": Bastante tiempo.
#                      Suficiente tiempo.

# "Tiempo limitado": No tengo tiempo.
#                    Poco tiempo.


 base <- base %>% 
   mutate(to_01 = case_when(
     to_01 %in% c("Bastante tiempo.", "Suficiente tiempo.") ~ "Tiempo suficiente",
     to_01%in% c("No tengo tiempo.", "Poco tiempo.") ~ "Falta de tiempo",
     TRUE ~ "otros" 
   ))
 
 

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"

# Media (0.5 ptos)

 mean_ea_04 <- mean(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
# Mediana (0.5 ptos)
 median_ea_04 <- median(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)

# desviación estándar (0.5 ptos)
 sd_ea_04 <- sd(base$ea_04_notas_ultimo_semestre, na.rm = TRUE)
# varianza (0.5 ptos)
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

 library(ggplot2)
 
 ggplot(base, aes(x = to_01, y = ea_04_notas_ultimo_semestre, fill = to_01)) + 
   geom_boxplot(alpha = 0.3) +
   labs(
     title = "Notas último semestre según tiempo disponible",
     y = "Promedio de notas último semestre",
     fill = "Tiempo"
   ) +
   theme_minimal()
 


# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"


 library(ggplot2)
 library(scales) 
 
 ggplot(base, aes(x = fct_reorder(lugar_musica, base), y = ..count../sum(..count..), fill = lugar_musica)) +
   geom_col() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   xlab("") + ylab("") +
   labs(
     title = "Lugares donde escuchan música",
     subtitle = "Estudiantes de antropología",
     caption = "Fuente: Encuesta Estudiantes Antropología 2024"
   ) +
   geom_text(aes(label = percent(..count../sum(..count..))), 
             stat = "count", 
             vjust = -0.5, 
             size = 3, 
             fontface = "bold") +
   scale_fill_viridis_d(option = "C", guide = "none") + Theoph
 
