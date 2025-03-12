#Ayudantía 06
#Repaso Tablas de frecuencia y contingencia
#Gráficos 

# 1. Instalo y abro paquetes ---------------------------------------------------
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               readr,
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer, #Exploración rápida 
               knitr,
               summarytools,
               webshot)

webshot::install_phantomjs()  # Instalar phantomjs si no está instalado

# 2. Importo base de datos y la asigno a environment ---------------------------
base_antropologia <- read.xlsx("base_antropologia_limpia.xlsx")
libro_codigos<- read.xlsx("base_antropologia_limpia.xlsx") 

#Explorar
glimpse(base_antropologia) #Una primera mirada de lo que hay en mis datos (variables, tipo, respuesta)
names(base_antropologia) #observo los nombres de las variables (que nos permite evaluar si renombrar)
View(base_antropologia) #Veo la base de datos

# 3. Data Wrangling ------------------------------------------------------------
#preparar los datos de tal manera que sean adecuados y listos para los análisis

# *Limpieza inicial ####
# base_antropologia <- janitor::clean_names(base_antropologia) 
# En este caso la base de datos está limpia, por lo que NO es necesario hacer la limpieza
# Pero si tenemos una base de datos que tenga variables con formatos muy distintos(muchas mayúsculas y minúsculas, puntos, espacios y guiones)
#podemos realizar el proceso de limpieza transformando todo a minúscula, quitando tildes, borrando espacios

# Observación general de base ####
nrow(base_antropologia) #147 cantidad de casos
ncol(base_antropologia) #60 cantidad de variables
sapply(base_antropologia, FUN = class) # sapply: realiza función class a todas las variables 
str(base_antropologia) #estructura del objeto base de datos

# Renombrar variables ####
#En este caso no es necesario cambiar el nombre de las variables, pues ya están listos los nombres.
#Esto se realiza en el caso de que los nombres de las variables sean muy largos o sean las preguntas mismas.

### Renombrar ALGUNAS VARIABLES EN ESPECÍFICO: Elegir variables para renombrar
#Posibilidad de renombrar una por una las variables de interés. 
#Pasos: 
# a) Veo los nombres de todas las variables para seleccionar la(s) variable(s) que me interesa renombrar. 
names(base_antropologia)
# b) Renombrar las variables elegidas con RENAME: 
base_antropologia <- base_antropologia %>% dplyr::rename(variable_nombre_nuevo=nombre_antiguo,
                                                         variable_nombre_nuevo=nombre_antiguo)
# c) Veo cómo quedaron los nuevos nombres de mis variables elegidas con names(base_antropologia)       

### Renombrar TODAS LAS VARIABLES
#Pasos:
# a) Veo el nombre de todas las variables con names(base_antropologia) para ver qué nombre le voy a asignar a cada variable.
# b) Renombrar con RENAME:  base_antropologia <- base_antropologia %>% dplyr::rename(nombre_nuevo = nombre_antiguo, con todas las variables)
# c) Nuevamente observamos los nombres para comprobar que se hizo la recodificación, con names(base_antropologia)


# Recodificación categorías variables -----------------------------------------------------
# Categorías de respuesta
# Pasos:
# a) Selecciono mis variables de interés 
# b) Veo si es necesario recodificar las variables (sus categorías de respuesta) *¿Cómo yo decido esto? Preguntas con respuesta abierta o categorías con nombres que deseo cambiar 
# c) Obtengo de manera más directa los valores únicos de la variable a recodificar: sus CATEGORÍAS DE RESPUESTA.
unique(base_antropologia$variable_seleccionada)
# d) Dejar todas las categorías en un mismo formato: poner todo en minúscula y eliminar los espacios
base_antropologia$variable_seleccionada <- tolower(base_antropologia$variable_seleccionada) #todas a minusculas
base_antropologia$variable_seleccionada  <- gsub(pattern = " ", replacement = "", x = base_antropologia$variable_seleccionada) #elimino los espacios
# e) Aplico MUTATE Y CASE_WHEN para recodificar categorías de respuesta
base <- base %>% mutate(variable_elegida=case_when(variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                   variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                   variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                   TRUE ~ variable_elegida)) 


# 4. Construcción de tablas ----------------------------------------------------
# Crear y guardar tablas
### Primero tenemos que crear el directorio donde se van a guardar las tablas
#si dentro de mi carpeta de proyecto no existe el directorio tablas, esta función lo crea:
if(!dir.exists("tablas")) dir.create("tablas") 

# TABLAS DE FRECUENCIA (1 variable)####

#Forma 1: Con freq (En variables categóricas) ####

#Pasos:
# a) Chequear el tipo de variable, para anticipar errores con variables tipo factor
class(base_antropologia$ultimo_colegio)

# b) Si la variable es factor, transformar a character
base_antropologia %>% 
  mutate(ultimo_colegio = as.character(ultimo_colegio)) #transformo a character
#en este caso no es necesario, porque es character, pero les dejo el código de cómo sería
  
 # c)  construir la tabla con freq y guardarla con kable
base_antropologia %>% 
  freq(base_antropologia$ultimo_colegio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  kable(col.names = c("Tipo de colegio", "Frecuencia", "%", "% Acumulado"),
        caption = "Último colegio en el que estudió", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/ultimo_colegio.png", zoom = 3) #se va a guardar en el file llamado "tablas"

# c)  Interpretar y sacar conclusiones 


#Forma 2: Con tidyverse (En variables numéricas) ####

library(dplyr)
library(kableExtra)

# Utilizo tidyverse para realizar múltiples funciones y sacar la tabla

# a) Creo la tabla y la guardo en el enviroment
tabla_notas <- base_antropologia %>%
  select(ea_04_notas_ultimo_semestre) %>% 
  mutate(ea_04_notas_ultimo_semestre = as.numeric(ea_04_notas_ultimo_semestre)) %>%  # Transformar a variable numérica
  filter(!is.na(ea_04_notas_ultimo_semestre)) %>%  # Eliminar NA filtrando correctamente
  group_by(ea_04_notas_ultimo_semestre) %>%  # Agrupar por las notas (todas las notas 6.0 en una misma categoría)
  summarise(Frecuencia = n()) %>%  # Contar la frecuencia de cada nota
  mutate(Porcentaje = Frecuencia / sum(Frecuencia) * 100) %>%  # Calcular el porcentaje
  mutate(Porcentaje = round(Porcentaje, 1)) %>%  # Redondear el porcentaje a 1 decimal
  arrange(desc(Porcentaje)) %>%  # Ordenar los porcentajes de mayor a menor
  rename(Notas = ea_04_notas_ultimo_semestre)  # Renombrar la columna de las notas

# b) Guardar la tabla en formato png
tabla_notas %>%
  kable(col.names = c("Notas", "Frecuencia", "Porcentaje"), 
        caption = "Distribución de frecuencias de Notas del último semestre", 
        format = "html", digits = 2) %>%  # Dar formato con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/Notas_últimosemestre.png", zoom = 3)  # Guardar la tabla

#Por otro lado, para hacer análisis univariado de variables numéricas como ea_04_notas_ultimo_semestre
#podemos sacar MTC como la media

#¿Cuál es el promedio de notas?
  
# Paso 1: Transformar y filtrar
base_antropologia <- base_antropologia %>%
  mutate(ea_04_notas_ultimo_semestre = as.numeric(ea_04_notas_ultimo_semestre)) %>%
  filter(!is.na(ea_04_notas_ultimo_semestre))

# Paso 2: Calcular la media
mean_notas <- mean(base_antropologia$ea_04_notas_ultimo_semestre, na.rm = TRUE)

# Mostrar la media
mean_notas #5.691379


# TABLAS DE CONTINGENCIA (cruce 2 variables) ####
# Cargar las bibliotecas necesarias
library(knitr)
library(kableExtra)
library(webshot2)
library(gmodels)

#Forma 1: Mediante ctable() ####

# a) Elegir nuestras variables para el cruce y hacer la tabla con estas
# variables ultimo_colegio y re_02: afiliación religiosa
# X: variable dependiente religión (izquierda)
# Y: variable independiente: raza (arriba)
  summarytools::ctable( x = base_antropologia$re_02, y = base_antropologia$ultimo_colegio)
  
  ctable( x = base_antropologia$re_02, y = base_antropologia$ultimo_colegio, prop = "c", justify = "l")
  
  summarytools::ctable( x = base_antropologia$re_02, y = base_antropologia$ultimo_colegio)
  
  # b) Filtramos NA y Ordenamos
# En este caso no será necesario pues no hay valores NA
  
  # c) Recodificamos las categorías para dejar menos cantidad de categorías de respuesta
  
  ##Primero con re_02
 unique( base_antropologia$re_02)
 
   afiliacion_rel <- base_antropologia %>%
    mutate(re_02 = case_when(re_02 ==  "Yoruba " ~ "Otra",
                             re_02 ==  "Grecorromana " ~ "Otra",
                             re_02 ==  "Grecorromana" ~ "Otra",
                             re_02 ==  "Ortodoxo" ~ "Otra",
                             re_02 ==  "Cristianismo Protestante (Evangélico, anglicano, etcétera)" ~ "Cristianismo protestante",
                             re_02 ==  "No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria" ~ "Ninguna",
                             re_02 ==  "Catolico" ~ "Catolicismo",
                             re_02 ==  "deísmo" ~ "Otra",
                             re_02 ==  "Agnóstico" ~ "Agnóstico/a", 
                             re_02 ==  "Ateo" ~ "Ninguna",
                             re_02 ==  "Pagana" ~ "Otra", 
                             re_02 ==  "No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías)." ~ "Otra",
                             TRUE ~ re_02))
  
  afiliacion_rel$re_02
  
  #Ordenamos transformando a factor
  afiliacion_rel <-   afiliacion_rel %>%
    mutate(re_02 = as.factor(re_02)) 
  afiliacion_rel$re_02 <- afiliacion_rel$re_02 %>% fct_relevel(c("Catolicismo", "Cristianismo protestante", 
                                                       "Agnóstico/a", "Ninguna", "Otra"))
  
  unique(afiliacion_rel$re_02)
  
  ##Segundo con ultimo_colegio
  recod_ultimo_colegio <- afiliacion_rel %>%
    mutate(ultimo_colegio = case_when(ultimo_colegio ==  "Particular" ~ "Privado",
                                       ultimo_colegio ==  "Particular subvencionado" ~ "Particular Subvencionado",
                                       ultimo_colegio ==  "Público" ~ "Público",
                                       ultimo_colegio ==  "Total" ~ "Total",
                                       TRUE ~ ultimo_colegio))
  
  #Definirla como factor para que se guarde correctamente en el data frame
  recod_ultimo_colegio <- recod_ultimo_colegio %>%
    mutate(ultimo_colegio = as.factor(ultimo_colegio))
  
  unique(recod_ultimo_colegio$ultimo_colegio)
  
  # Crear la tabla cruzada con ctable
  tabla_rel_coleg <- ctable(x = recod_ultimo_colegio$re_02,
                            y = recod_ultimo_colegio$ultimo_colegio,
                            prop = "c", #proporciones por columna
                            justify = "l")
  
  # Ver el resultado
  print(tabla_rel_coleg)
  
  # e) Interpretar y sacar conclusiones
  
  
  # Cargar la librería necesaria
  library(summarytools)
  
  # Asegúrate de que las variables están en formato factor
  recod_ultimo_colegio <-  recod_ultimo_colegio %>%
    mutate(re_02 = as.factor(re_02),
           ultimo_colegio = as.factor(ultimo_colegio))
  
  # Crear la tabla de contingencia con ctable
  tabla_rel_coleg <- ctable(x =  recod_ultimo_colegio$re_02,
                               y =  recod_ultimo_colegio$ultimo_colegio,
                               prop = "c",
                               useNA = "no")
  
  # Mostrar la tabla de contingencia
  print( tabla_rel_coleg)
#  ¿Cuál de las afiliaciones religiosas tradicionales predomina en establecimiento educativo?
# ¿Cuáles son minorías para cada caso?
  #Un 0% de personas (de la muestra) provenientes de colegios públicos son cristianas
  #Por otro lado, un 31% de las personas agnósticas de la muestra son provenientes de colegios públicos, 
  #Por otro lado, un 31% de las personas provenientes de colegios públicos de la muestra son agnósticas, **
  #lo que representaría un mayor % del total de personas agnósticas de la muestra. (cifra que llama la atención)
  
  
  
  # Forma 2: Mediante prop.table() #### 

  # a) Renombrar y ordenar las variables
  tabla_rel_clase <- base_antropologia %>%
    mutate(clase_social = case_when(clase_social ==  "Clase social baja" ~ "Clase Baja",
                                    clase_social==  "Clase social media" ~ "Clase Media",
                                    clase_social ==  "Clase social media - baja" ~ "Clase Baja",
                                    clase_social ==  "Clase social media - alta" ~ "Clase Alta",
                                          TRUE ~ clase_social))
  
  tabla_rel_clase <- as.data.frame(tabla_rel_clase)   
  
  tabla_rel_clase <- tabla_rel_clase %>%
    mutate(clase_social = factor(clase_social, levels = c("Clase Baja", "Clase Media", "Clase Alta"))) %>%
    group_by(clase_social) %>%
    mutate(porcentaje = n() / nrow(tabla_rel_clase) * 100) %>%
    arrange(desc(porcentaje))
  
  unique(tabla_rel_clase$clase_social)
  
  # b) Crear la tabla y la guardo en el enviroment
  tabla_rel_clase <- tabla_rel_clase  %>%
    select(Religión = re_01, `Clase Social` = clase_social) %>%  # Seleccionar y renombrar las variables
    droplevels() %>% # Eliminar las categorías que no se utilizan en la columna
    table(.) %>%# Hacer una tabla con todos los datos
    addmargins(.,2) %>% # Calcular los porcentajes por columnas
    prop.table(.) %>% # Agregar total de columnas
    round(4)*100  # Formatear y redondear con símbolo de porcentaje
  
  print(tabla_rel_clase)
  
  # c) Interpretar y sacar conclusiones
  #En la clase baja, el 13.33% representa a las personas con creencia firme, 
  #mientras que el 25.00% de las personas encuestadas no se identifican con ninguna creencia en la clase baja.
  #En la clase media, el 31.75% de las personas encuestadas afirma presentar dudas ocasionales respecto a sus creencias religiosas
  #es decir, un porcentaje considerable de las personas encuestadas que son de clase media, presentan dudas.
  #Finalmente, un 41,67% de las personas de clase alta encuestadas no presentan creencias, lo cual representa una cifra considerable
  
  # d) Guardar en un excel
  write.xlsx(tabla_rel_clase, "tablas/tabla_rel_clase.xlsx") 
  
  # e) Guardar en PNG
  tabla_rel_clase %>% 
    kable(., caption="Tabla de contingencia para religión y clase social (% por columnas)") %>% 
    kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
    save_kable(file = "tablas/tabla_rel_clase.png", zoom = 2)
  
  
  # 5. CONSTRUCCIÓN DE GRÁFICOS ------------------------------------------------
  #Paquetes 
  pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,  
                 knitr, gt, summarytools, ggthemes, hrbrthemes, foreign, DescTools, ineq, haven)
  
  #Cargo base
  # https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Casen_en_Pandemia_2020_revisada202209.sav.zip
  Casen_en_Pandemia_2020_revisada202209 <- read.spss("Casen_en_Pandemia_2020_revisada202209/Casen_en_Pandemia_2020_revisada202209.sav", to.data.frame = TRUE)
  setwd("/Users/fran/Desktop/Base_antropo")
  
  library(haven)
  Casen_en_Pandemia_2020_revisada202209 <- read_sav("Base Casen /Casen_en_Pandemia_2020_revisada202209.sav")
  View(Casen_en_Pandemia_2020_revisada202209)
  
  #Creo la carpeta de gráficos
  if(!dir.exists("gráficos")) dir.create("gráficos") 
  
  #Estructura código guía ggplot
  ggplot(base_de_datos, aes(y = eje y, x = "eje x")) +
    geom_tipodegráfico(del tipo de gráfico dependen los argumentos, fill= "color del gráfico") +
    depende del tipo de gráfico()
  
  # ggplot es la función que va a crear el gráfico
  # aes define la estética del gráfico (los ejes x e y)
  # fill define el color de relleno del gráfico
  # theme_ define el tema del gráfico, su aspecto
  
  # Gráfico de caja y bigotes - geom_boxplot ####
  
  # Gráfico para UNA SÓLA VARIABLE (y1:ingresos) #
  g_box_ingresos <-
  ggplot(Casen_en_Pandemia_2020_revisada202209, aes(y = y1, x="")) + #gráfico de la base de datos Casen2020, con un sólo eje (y = y1)
    geom_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6,  fill= "pink") + #outlier son los casos atípicos que se dejan como NA y luego se eliminan. Extensión de los bigotes y ancho de la caja.
    scale_y_continuous(limits = c(0, quantile(Casen_en_Pandemia_2020_revisada202209$y1, 0.95, na.rm = TRUE))) + #La escala del eje y se ajusta para que vaya desde 0 hasta el percentil 95 de la variable y1, excluyendo valores NA
    theme_minimal()+ #tema minimalista
    ylab("Ingresos") #etiqueta ariable utilizada
  
  # Guardamos
  ggsave("gráficos/g_box_ingresos.png", plot = g_box_ingresos, width = 10, height = 7, dpi = 300)
  
  # Interpretación
  # La caja representa el 50% de los casos de la distribución, 
  # en su límite superior presenta el tercer cuartil
  # en su limite inferior presenta el segundo cuartil
  # porque es entre ambos cuartiles donde se concentra la mitad de la distribución
  # La mediana está siempre situada dentro de esta caja, representada en forma de línea (la línea negra)
  #En este caso, la mediana serían aprox 400.000 pesos, es decir, 
  #el dato que está en medio de la distribución de los datos ordenados de menor a mayor
  #divide en dos partes iguales la distribución
  #¿Qué significa esto? que hay un 50% de las personas que percibe menos de $400.000 en ingresos, mientras que 
  #la otra mitad de personas percibe más de $400.000
  #También podemos decir que los ingresos se concentran entre el cuartil 2 y 3, al rededor de los 300.000 y los 600.000 mil pesos.
  
  # Calculamos la media de ingresos
  mean_ingresos <- mean(Casen_en_Pandemia_2020_revisada202209$y1, na.rm = TRUE)
  
  mean_ingresos #653891.6
  #En promedio, las personas de la muestra ganan 653.892
  #El problema es que no se están contemplando los casos extremos
  
  # Calculamos la desviación estándar
  sd_ingresos <- sd(Casen_en_Pandemia_2020_revisada202209$y1, na.rm = TRUE)
  
  sd_ingresos #709222.5
  #La distancia promedio de los ingresos con respecto a la media es de 709222.5
  #Los ingresos se desvían mucho de la media, lo que quiere decir que hay bastantes casos extremos hacia la parte superior
  #ingresos muy altos que corresponden a casos aislados, los cuales modifican la distribución de los datos
  
# ---
  
  # Gráfico de caja y bigote con DOS VARIABLES #
  #Gráfico por sexo (para hombre 1 y mujer 2) 
  #cambiar la variable a factor para que la contemple y haga la diferencia entre hombres y mujeres
  Casen_en_Pandemia_2020_revisada202209$sexo <- as.factor(Casen_en_Pandemia_2020_revisada202209$sexo)
  
  #hago el gráfico
  g_ingresos_sexo <-
  ggplot(Casen_en_Pandemia_2020_revisada202209, aes(x= sexo, y = y1, fill=sexo)) + #en x pongo la variable de corte
    geom_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6) +
    scale_y_continuous(limits = c(0, quantile(Casen_en_Pandemia_2020_revisada202209$y1, 0.95, na.rm = TRUE))) + 
    guides(fill = "none")+
    theme_minimal() +
    ylab("Ingresos") 
  
  # Guardamos
  ggsave("gráficos/g_ingresos_sexo.png", plot =   g_ingresos_sexo, width = 10, height = 7, dpi = 300)
  
  #¿Cómo interpreto?
  #Para el caso de los hombres (rojos), la mediana de ingresos es levemente mayor que para el caso de las mujeres(verdes)
  #Las cajas demuestran igualmente que los ingresos se concentran en valores menores para el caso de las mujeres, 
  #mientras que para el caso de los hombres se concentran en valores mayores.
  
  
  # Histograma - geom_histogram ####
  # variable edad
  # cambiar los binwidth
  g_histogram_edad <- 
  ggplot(Casen_en_Pandemia_2020_revisada202209, aes(x = edad)) +
    geom_histogram(binwidth = 5, color = "purple", fill = "skyblue") +
    labs(x = "Edad", y = "Frecuencia") +
    theme_classic()
  #fijense en el binwidth, es algo que hay que cambiar dependiendo de la avariable que vamos a graficar y los valores que contemple.
  #Podemos ver la distribución de las edades: mayoría de personas de entre 20 y 30 años, y luego de 60 años.
  
  # Guardamos
  ggsave("gráficos/g_histogram_edad.png", plot = g_histogram_edad, width = 10, height = 7, dpi = 300)
  
  # Gráfico de barras - geom_bar ####
  ## Ingresos por región
  # Convertir la variable 'region' a factor si no lo es
  Casen_en_Pandemia_2020_revisada202209$region <- as.factor(Casen_en_Pandemia_2020_revisada202209$region)
  
  # Crear gráfico
  g_ingresos_region <- 
  ggplot(Casen_en_Pandemia_2020_revisada202209, aes(x = region, y = y1, fill = region)) + 
    geom_bar(stat = "identity") + # Usar geom_bar con stat="identity" para especificar las alturas de las barras basadas en los valores de y1
    theme_minimal() + # Aplicar el tema minimalista
    xlab("Región") + # Etiqueta del eje x
    ylab("Ingresos") + # Etiqueta del eje y
    ggtitle("Distribución de Ingresos por Región") + # Título del gráfico
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotar las etiquetas del eje x para mejorar la legibilidad
  
  # Guardar
  ggsave("gráficos/g_ingresos_region.png", plot = g_ingresos_region, width = 10, height = 7, dpi = 300)
  
  #Regiones:
  # 13: RM
  # 5: Región de Valparaíso
  # 8: Región del Biobío
  
  #Interpretación:
  #Hay una distribución de ingresos por región que es sumamente desigual, concentrándose la mayoría en la RM
  
  
  # Proceso completo ejemplo------------------------------------------------------------
  #Recodificación y análisis
  # Preguntas de respuesta múltiple 
  # se puede tener más de una categoría de respuesta por cada caso
  # esto implica realizar un proceso un poco más largo
  
  #EJ: (realizado por ustedes mismos)
  # ea_10
  # ¿Qué estrategias utiliza con mayor frecuencia para manejar el estrés académico?
  # seleccione todas las alternativas que correspondan con su caso

  unique(base_antropologia$ea_10)
  class(base_antropologia$ea_10)
  
  # 1. Cambiar todas las comas que se utilicen dentro de las mismas categorías
  #se cambió una categoría que tenía una "," y al seperar las opciones dentro de la respuesta
  #tambien cortaba un parantesis que tenia comas, así que se cambió a un "/"
  #Esto se hace para poder separar las mismas categorías de respuesta unas de otras (con ,), en los casos donde se hayan respondido varias 
  
  base_antropologia <- base_antropologia %>%
    mutate(ea_10 = case_when(
      grepl("Participar en otras actividades creativas \\(música, arte, escritura\\)", ea_10) ~ #buscar un patrón específico 
        gsub("Participar en otras actividades creativas \\(música, arte, escritura\\)", #eliminar , y cambiarlas por /
             "Participar en otras actividades creativas (música/arte/escritura)", 
             ea_10), TRUE ~ ea_10))
  
  # 2. Separar las respuestas y crear un vector que las ponga en una lista
  respuestas_ea_10 <- unlist(strsplit(base_antropologia$ea_10, ", ")) # separo las respuestas que tienen coma (,)
  
  #veo el vector con la lista de categorías
  unique(respuestas_ea_10)
  #se separan todas las categorías
  
  #elimino espacio antes de primera letra y guardo este vector limpio 
  respuestas_ea_10_limpio <- trimws(respuestas_ea_10, which = "left")
  
  # 3. Realizar una tabla de frecuencias y observar las respuestas 
  #¿cuál es la respuesta más frecuente? es decir ¿cuál es la estrategia que más se utiliza para manejar el estrés académico?
  freq(respuestas_ea_10_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
    tb()
  
  # 4. Guardo en otro vector para crear la tabla y para graficar
  ea_10_graf <- freq(respuestas_ea_10_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
    tb()
  
  ea_10_tabla <- freq(respuestas_ea_10_limpio, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
    tb() %>%
    kable(col.names = c("Estrategias", "Frecuencia", "%", "% Acumulado"),
          caption = "Estrategias del manejo del estres", 
          format = "html", digits = 2) %>%  #le doy formate con kable
    kable_classic(full_width = F, html_font = "Cambria") %>% 
    save_kable(file = "tablas/ea_10_tabla.png", zoom = 3)
  
  # Renombro value y pct
  ea_10_graf <-  ea_10_graf %>% 
    rename(Problema = value, Porcentaje= pct)
  
# Realizo un gráfico de columnas
  g_ea_10_graf <- ggplot(ea_10_graf, aes(x = Porcentaje, y = fct_reorder(Problema, Porcentaje), fill= Problema)) +
    geom_col() +
    labs(title = "Estrategias del manejo del estres",
         subtitle = "según datos de Encuestas Estudiantes Antropología 2024",
         x = "%",
         y = "Estrategia") +
    geom_text(data = ea_10_graf %>% filter(rank(-Porcentaje) <= 12), # Solo añadir texto a las primeras 8 categorías
              aes(label = ifelse(rank(-Porcentaje) <= 12, paste0(round(Porcentaje, 1), "%"), "")),
              hjust = 1, size = 3, nudge_x = -.9, fontface= "bold", color = "white") +
    scale_fill_viridis_d(option = "C", guide = "none") +
    theme_ipsum()
  
  print(g_ea_10_graf)
  
  # Guargo el gráfico
  ggsave("gráficos/g_ea_10_graf.png", plot = g_ea_10_graf, width = 10, height = 7, dpi = 300)
  
  
