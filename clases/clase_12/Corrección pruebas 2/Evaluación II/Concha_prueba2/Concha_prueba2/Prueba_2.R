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

### Ojo con las instrucciones. Se respondían sólo 3 preguntas. Tomaré en cuenta las mejores para poner los puntos. 

# 1. Describa la diferencia entre la hipótesis nula y la hipótesis alternativa en un contraste de hipótesis.
# La hipótesis nula cosiste en la media poblacional que toma un valor especifico y es la que se busca contrastar, por otra parte la hiótesis alternativa sirve para comparar sólo si la hipótesis nula no se cumple y es especfiica 

### Las hipótesis corresponden a afirmaciones que buscan probar si las variables o grupos evaluados se relacionan o no.
### Por ejemplo, en el test de hipótesis Chi Cuadrado, la hipótesis nula indica que las variables no se relacionan, es decir, son independientes, 
### mientras que la hipótesis alternativa indica que sí hay relación entre las variables, es decir, son dependientes entre sí. 
### Y en el test de ANOVA, la hipótesis nula indica que las medias (promedios) de los grupos evaluados son iguales, 
### mientras que la hipótesis alternativa indica que no lo son.

# 2. ¿cuál es la manera en que se debe interpretar el valor-p y cómo repercute este valor sobre la decisión acerca del contraste de hipótesis?
#   el valor p e sla probailidad de obtener un valor estadísitico al menos tan adverso para la hipótesis nula (que es verdadera o se cumple) como el calculado en la muestra de la hipótesis nula, si esta se calcule permite entender cual es la probabilidad (entre 0 a 1) de que ocurra, repercutiendo en como entendemos la frecuencia de que obtengamos ese dato y que nos lleve a esa conclusión errónea de la hipótesis alternativa

### Falta claridad y precisión en la respuesta, además de que hay ciertas apreciaciones que son incorrectas. El valor -p no repercute en la frecuencia que tengamos del dato (los datos)
### y la hipótesis alternativa no es una conclusión errónea, sino que es la afirmación que se aprueba si se rechaza la hipótesis nula. 
### Es correcto que el valor -p es una probabilidad, pero no es correcto que sea la probabilidad de obtener un valor estadísitico al menos tan adverso para la hipótesis nula. 
### No se entiende a qué se refiere con valor adverso. 
### El valor -p se obtiene a partir del nivel de confianza, por lo que su interpretación dependerá de esto.
### El nivel de confianza puede ser de 99%, 95% o 90%, siendo el valor -p distinto para cada nivel, por lo que la interpretación puede variar en cada caso.
### También es importante mencionar que en el contrtaste o test de hipótesis se observa si se supera o no el valor -p para determinar si se aprueba o rechaza la hipótesis nula.
### Por ejemplo, si el nivel de confianza es del 99%, el valor -p correspondería a 0,01, por lo que se observaría si se supera este valor para ver si se aprueba o rechaza la hipótesis nula.
### En el caso del 95% de confianza, el valor -p corresponderá a 0,05; y en el caso del 90% de confianza, el valor -p correspondería a 0,1

# 3. Describa qué son los estadísticos de tendencia central, de 2 ejemplos de este tipo de estadísticos y cuáles son sus diferencias con respecto a las medidas de dispersión
# la estaaistica de tendencia entral son las medidad que describen el centro del conjunto de datos, por ejemplo está la media que es el promedio aritmetoc y la mediana que es el valor que divide en dos partes iguales, por otra parte parte de las diferencias en las medidas de dispersión correpsonden que la medidia de tendencia central indica el punto central de lso datos, las medidas de dispersión indican qué tan dispersos esta´n esos datos alrededor del centro

### En general está bien

# 4. Cuál es la diferencia entre la estadística descriptiva y la estadística inferencial?
#   la estadística descriptiva corresponde al resumen descriptivo del conjunto de los datos con medidas como al media, mediana, desviacióne standar, etc. mientras que al estdistica inferencial, como su nombr elo idica, se enfoca en la prediccón o inferencia sobre la muestar de datos (la población)

### En general está bien, pero hay que tener ojo con confundir la muestra de los datos con la población. La muestra son los casos estudiados, mientras que la población es todo el universo de casos posibles.
### La muestra sería un subconjunto de la población. 
### Esta diferencia es importante para entender la estadística inferencial, porque con esta se va a poder extrapolar los resultados de la muestra a la población,
### es decir, se van a poder generalizar los resultados de la muestra, infiriendo que la población entera presenta dichos resultados. 

# 5. ¿cuál es la diferencia entre las pruebas de contraste de hipótesis de Chi cuadrado con ANOVA?
#   la prueba ANOVA se preocupa de comparar los medias diferentes de más de 3 grupos mientras que Chi cuadrado permite probar la independecia  o comparar la distribucipon observada con una distribución esperada

### En general está bien, pero falta precisión en la respuesta

# 6. ¿a qué se refiere con que la correlación no establece una dependencia entre variables?
#   se refiere a que no necesariamente dos variables están relacionadas cusalmente

### Falta precisión y claridad en la respuesta

# 7. ¿cuáles son las diferencias entre correlación y covarianza? y ¿cuáles son sus similitudes?
#   entre sus disferencias se encuentra que el rango de la correlación se encuntra entre los rango de -1 a 1 mientras que la covarianza no está limitado, mientras que en al escala la correlación no presenta unidades estandarizadas mientras que alcovarianza si depende de las unidades de variable. Por otra parte, sus similitudes es que ambas miden cómo van variando juntas dos variables o que sus correlaciones positivas indican que sus variables cambian juntas mientras que la negativa es que van a la inersa

### En general está bien :)

# 8. ¿Por qué se dice que el proceso de inferencia sigue una lógica inductiva?
#   se dice eso pues en base a la observación se puede inferir que puede existir una relación o probabilidad, entonces esas predicciones se refierre a observaciones generalizadas

### Falta precisión y claridad en la respuesta


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

str(base) #147 filas y  60 columnas
#02. Renombrar variables
#02.01: observe las variables de la base de datos con names(0.5)
names(base)
#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa(2 ptos)
# Para designar nuevos nombres a sus variables se puede apoyar en las siguientes etiquetas de las preguntas (use sólo las de su grupo):

# "MA_01 En una escala del 1-10 siendo el 10 el mayor nivel de información y el 1 el menor ¿Qué tan informado se encuentra con respecto a la problemática del exceso de basura en las calles?"
# "MA_02 En una escala del 1 al 10, ¿Qué tan interesado estaría en participar en actividades educativas que amplíen su conocimiento sobre gestión de residuos y problemas relacionados?"
# "MA_03 ¿Ha participado en algún grupo, organización o proyecto ecológicos relacionados con el cuidado y preservación del medio ambiente?"
# "MA_04 ¿Ha participado en eventos o actividades comunitarias relacionadas a la difusión de información con respecto a los problemas medioambientales dentro de los últimos tres meses meses? Tales como charlas, ferias ecológicas, talleres prácticos o campañas específicas?"
# "MA_05 ¿Con qué frecuencia adquiere productos sostenibles o amigables con el medio ambiente, teniendo en cuenta la reducción de residuos generados por sus envases? Ttales como botellas reciclables, productos a granel, empaques minimalistas o envases reciclables"

# Puede utilizar el siguiente formato

 base <- base %>% dplyr::rename(interes_participar = ma_02,  
                                participacion_grupo = ma_03,
                                participacion_evento = ma_04, 
                                frecuencia_consumo_sostenible = ma_05)


# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos

base <- base %>% rename(lugar_musica=cm_05)
names(base)

### Hubiera sido bueno utilizar el mismo código de arriba dplyr::rename, aunque igual resultó la recodificación así, pero no siempre resuta de esa manera.

# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)

table(base$lugar_musica)
freq(base$lugar_musica)
unique(base$lugar_musica)

# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.

# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)

respuestas <- strsplit(base$lugar_musica, ",")

# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

respuestas <- unlist (respuestas) 

# 03.5 observe las respuestas con freq (1 pto)
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub


 respuestas <- gsub(" En el transporte público", "En el transporte público",  respuestas ) #(1 pto)

 respuestas <- gsub(" En otros lugares", "En otros lugares", respuestas ) #(1 pto)



# 03.7 vuelva a observar sus respuestas con freq (1 pto)
 freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
   tb()

 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
 
gabriel_concha <-  freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
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

base <- base %>%  mutate(religion_rec= case_when(re_02 %in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías).","seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", "deísmo") ~"Espiritualidad sin afiliación religiosa",
                                               re_02 %in% c("Catolico","Cristianismo Protestante (Evangélico, anglicano, etcétera)","Grecorromana ","Yoruba ","Ortodoxo") ~"Religión específica",
                                               re_02 %in% c("Pagana","Ateo","Agnóstico","No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria","Ninguno ") ~"Sin religión",
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

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"
table(base$ea_04_notas_ultimo_semestre)
# Media (0.5 ptos)
media_notas <- mean(base$ea_04_notas_ultimo_semestre, na.rm = TRUE) #media de 5.691379
# Mediana (0.5 ptos)

# desviación estándar (0.5 ptos)
sd_notas <- sd(base$ea_04_notas_ultimo_semestre, na.rm = TRUE) #desviación estándar de notas 0.472103
# varianza (0.5 ptos)

# 07. Grafique los cruces de la variable  "ea_04_notas_ultimo_semestre" con las siguientes variables:
ea_04_notas_ultimo_semestre <- base$ea_04_notas_ultimo_semestre
unique(ea_04_notas_ultimo_semestre)
# Hint: La variable "ea_04_notas_ultimo_semestre" es la variable dependiente.

# 07.1 ea_04_notas_ultimo_semestre vs religion_rec (3 ptos)
religion_rec <- base$religion_rec
ggplot(base, aes(x=ea_04_notas_ultimo_semestre, y=religion_rec, fill=ea_04_notas_ultimo_semestre)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Promedio de notas según tipo de religión", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "religion_rec",
       fill = "religion_rec")+
  theme(legend.position="none")

# 07.2 ea_04_notas_ultimo_semestre vs to_01 (3 ptos)

ggplot(base, aes(x=ea_04_notas_ultimo_semestre, y=base$to_01, fill=ea_04_notas_ultimo_semestre)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Notas último semestre según tiempo", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "base$to_01",
       fill = "base$to_01")+
  theme(legend.position="none")


# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"


ggplot(gabriel_concha, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Lugares donde escuchan música", 
       subtitle = "Estudiantes de antropología", 
       caption = "Fuente: Encuesta Estudiantes Antropología 2024")+
  geom_text(aes(label = paste0(round(pct, 1), "3")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")
