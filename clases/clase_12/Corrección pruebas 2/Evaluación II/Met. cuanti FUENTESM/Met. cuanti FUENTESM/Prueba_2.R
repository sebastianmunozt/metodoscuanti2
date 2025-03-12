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

                                                 Primera parte
#Cuál es la diferencia entre la estadística descriptiva y la estadística inferencial?
                                                 
Mientras que la estadística descriptiva se encarga de Describir, y Su objetivo principal es resumir y organizar los datos de manera comprensible.
Utiliza medidas como la media, mediana y moda para describir el centro de los datos.Tambien Evalúa la variabilidad de los datos mediante medidas como el rango, la desviación estándar y la varianza.
Emplea gráficos como histogramas, gráficos de barras, diagramas de dispersión y diagramas de caja para visualizar los datos. La estadística inferencial se centra en: hacer inferencias y predicciones sobre una población basada en una muestra de datos.
Utiliza estimaciones puntuales y por intervalos para hacer inferencias sobre parámetros poblacionales.
 Evalúa hipótesis sobre los datos mediante pruebas estadísticas, como las pruebas t, ANOVA y chi-cuadrado.
Desarrolla modelos para predecir futuros eventos o comportamientos basados en datos muestrales.
La estadística descriptiva se limita a describir los datos recolectados, mientras que la inferencial va más allá para hacer predicciones y generalizaciones sobre una población mayor. La descriptiva usa herramientas como tablas, gráficos y medidas de resumen. La inferencial utiliza técnicas más complejas como el muestreo, las pruebas de hipótesis y la construcción de intervalos de confianza.
La descriptiva busca proporcionar una visión clara y organizada de los datos, mientras que la inferencial busca tomar decisiones y hacer predicciones basadas en los datos.

# 5. ¿cuál es la diferencia entre las pruebas de contraste de hipótesis de Chi cuadrado con ANOVA?
 En una prueba de contraste de hipotesis Chi cuadrado se utiliza cuando contamos con dos variables
categóricas (nominales u ordinales) y Anova se utiliza cuando contamos con una variable cuantitativa y otra categórica 
(variable de agrupación).

# 6. ¿a qué se refiere con que la correlación no establece una dependencia entre variables?
Cuando se dice que la correlación no establece una dependencia entre variables, se refiere a que la correlación solo mide la fuerza y la dirección de una relación lineal entre dos variables, 
pero no implica que una variable cause cambios en la otra.

# 7. ¿cuáles son las diferencias entre correlación y covarianza? y ¿cuáles son sus similitudes?
La Covarianza Mide el grado en que dos variables cambian juntas. Si las variables tienden a aumentar y disminuir al mismo tiempo, la covarianza será positiva. Si una aumenta mientras la otra disminuye, la covarianza será negativa.
Por otro lado la Correlación Es una medida estandarizada de la relación lineal entre dos variables. La correlación convierte la covarianza en una escala sin unidades que varía entre -1 y 1.
Sus similitudes son: Ambas medidas se utilizan para evaluar la relación lineal entre dos variables. Si las variables no tienen una relación lineal, ambas medidas pueden ser menos informativas.Tanto la covarianza como la correlación pueden ser positivas o negativas, 
indicando la dirección de la relación entre las dos variables.



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
library(tidyverse)
library(openxlsx)
library(summarytools)
library(kableExtra)
library(webshot2)
library(chromote)
library(viridis)
library(hrbrthemes)

# 01. Cargar datos-------------------------------------------------------- 
base <- read.xlsx("base_antropologia_limpia.xlsx")


  
  
# 01.1 ¿cuántas filas y columnas tienen los datos? (0.5 ptos)
Tiene 60 columnas y 167 filas.
#(debe escribir código que entregue la información, no se aceptan respuestas sin código)
glimpse(base)
str(base)
summary(base)
view(base) #Se utiliza con mayúscula

#02. Renombrar variables
#02.01: observe las variables de la base de datos con names(0.5)
names(base)

#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa(2 ptos)
# Para designar nuevos nombres a sus variables se puede apoyar en las siguientes etiquetas de las preguntas (use sólo las de su grupo):

# "TO_02 Considere esta definición de ocio antes de contestar ésta y las siguientes preguntas: “El ocio se puede entender como el tiempo del que dispone una persona para distraerse e idealmente disfrutar de un momento agradable en su agenda personal o tiempo libre” Tales como salir tener vida social, hacer deporte o prácticas de consumo o participación cultural (leer, escuchar o tocar música, estar en internet) Excluya trabajo y estudio Ahora que ya comprende a qué se refiere el ocio en esta encuesta, responda: ¿Cuántos días a la semana (incluyendo semana y fin semana) realiza actividades de ocio?"
# "TO_04 ¿Cuáles de estas actividades prefiere realizar en su tiempo de ocio?"
# "TO_05 ¿Forma parte de un club, colectivo o taller donde se realicen actividades las actividades previamente mencionadas?"
# "TO_06 ¿Qué tan importante es para usted el tiempo dedicado a actividades de ocio?"


# Puede utilizar el siguiente formato

base <- base %>% dplyr::rename(ocio_y_dias = to_02,                              
                                  Seleccion_actividad_ocio =to_04,                          
                                Pertenencia_agrupación_ocio= to_05,
                                  importancia_activ_ocio = to_06)
names(base)

# 03. Suponga que está trabajando para una investigación interesada en los habitos de escucha y consumo de música.
# Su jefe le solicita un gráfico que indique la frecuencia de los lugares donde los estudiantes de antropología escuchan música.
# Para lo cual debe realizar las siguientes acciones

# 03.1 Cambie el  nombre de la variable cm_05 por lugar_musica (0.5 ptos) en la misma base de datos

base <- base %>% rename(lugar_musica=cm_05)
names(base)

# 03.2 observe las categorías de la variable lugar_musica, puede utilizar funciones como freq, table, unique, etc. (0.5 ptos)


unique(base$lugar_musica)

# 03.3 Dado que la respuesta viene en una variable que a su vez contiene respuestas múltiples usted se da cuenta de que debe separar las respuestas.

# Para ello separe las respuestas de la variable lugar_musica por coma (,). 
# Utilice la función strsplit() y guardelo en un objeto llamado "respuestas" (1.5 ptos)

lu
### No hay respuesta
### Este paso condicionaba todos los demás

# 03.4 Transforme el objeto respuestas a un vector con la función unlist (0.5 pto)

lugar_musica <- unlist(lugar_musica) 

### Se hizo la lista, pero falta el paso anterior, por lo que no se logra realizar correctamente
### El código es correcto, pero no se logra realizar el objetivo

# 03.5 observe las respuestas con freq (1 pto)
freq(lugar_musica, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

### Se hizo la tabla, pero falta el paso anterior, por lo que no se logra realizar correctamente
### El código es correcto, pero no se logra realizar el objetivo

# Hay respuestas que se contabilizan 2 veces como "En el transporte público" y "En otros lugares", 
# 03.6 homologue ambas categorías de respuesta eliminando el espacio que antecede utilizando la función gsub


 lugar_musica <- gsub(" En el transporte público", "En el transporte público", lugar_musica ) #(1 pto)

 lugar_musica <- gsub(" En otros lugares", "En otros lugares", lugar_musica ) #(1 pto)

### Los códigos son correctos, pero no se logra el objetivo del ejercicio

# 03.7 vuelva a observar sus respuestas con freq (1 pto)
 freq(lugar_musica, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
   tb()

### La tabla no sale como debería, debido a que no se realizó el punto 3.03
 ### no se logra el objetivo del ejercicio.
 
# 03.8 Ahora, utilizando la misma función de la última pregunta (03.7) guarde el resultado en un objeto
# el nombre del objeto debe ser su "nombre_apellido", (0.5 pto)
 
 
lugar_musica <-  freq(lugar_musica, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

### No se guarda el objeto con el formato solicitado

# 04. Ahora recodifique la variable re_02 (¿Cuál es su afiliación religiosa o creencia espiritual?)
# 04.1 observe las categorías de respuesta de esta variable (0.5 pto)
names(base)
# 04.2 recodifique las categorías de esta variable en una nueva variable denominada "religion_rec" de la siguiente forma (3 ptos)
# Hint:  debe utilizar la función case_when()
# Las nuevas 3 categorías

base <- base %>%dplyr::rename(religion_rec=re_02)
str(base$religion_rec)
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
unique(base$religion_rec)
base <- base %>%  mutate(religion_rec= case_when(religion_rec%in% c("No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías)","seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre", "deísmo")~"Espiritualidad_sin_afiliación_religiosa",
                                                religion_rec%in% c("Catolico","Cristianismo Protestante (Evangélico, anglicano, etcétera)","Grecorromana ","Yoruba ","Ortodoxo")~"religion_especifica",
                                               religion_rec %in% c("Pagana","Ateo","Agnóstico","No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria","Ninguno ")~"sin_religion",
                                               TRUE~religion_rec))
#lo hice al reves y no me salio :p

# 04.3 observe la nueva variable "religion_rec" con la función table (0.5 ptos)


table(base$religion_rec)

# 05.1 Ahora recodifique la variable to_01  (tiempo libre) en 2 categorías en la misma variable (2 ptos) 
# "Tiempo disponible": Bastante tiempo.
#                      Suficiente tiempo.

# "Tiempo limitado": No tengo tiempo.
#                    Poco tiempo.


base <- base %>% mutate(to_01=case_when(to_01%in% c("Bastante tiempo.", "Suficiente tiempo.")~"tiempo_disponible",
                                        to_01 %in% c("No tengo tiempo.", "Poco tiempo.")~ "tiempo_limitado",
                                        TRUE~to_01))

# 06. Obtenga los siguientes estadísticos descriptivos de la variable "ea_04_notas_ultimo_semestre"

# Media (0.5 ptos)media <- mean(df$ea_04_notas_ultimo_semestre, na.rm = TRUE)


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

ggplot(base, aes(x = to_01, y = ea_04_notas_ultimo_semestre, fill = to_01)) + 
  geom_boxplot(alpha = 0.3) +
  labs(
    title = "Notas último semestre según tiempo disponible",
    y = "Promedio de notas último semestre",
    fill = "Tiempo"
  ) +
  theme_minimal()
names(base)

# 07.3 ahora grafique la distribución de los resultados guardados en el punto 03.8 con el siguiente código (3 ptos)
# recuerde que el nombre era su "nombre_apellido"


ggplot(base, aes(x = base, y = fct_reorder(lugar_musica, base), fill=lugar_musica)) +
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

