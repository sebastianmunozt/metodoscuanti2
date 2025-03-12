
# Trabajo en clase --------------------------------------------------------
##00. Lectura de paquetes a utilizar 

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)

#01. Pasos básicos --------------------------------------------------------------------
#01.01 Apertura de base

#Base a trabajar
base <- read.xlsx(xlsxFile = "base/EncuestaAntropologia_2023.xlsx")

#Libro de códigos
libro <- read.csv(file = "base/Encuesta antropología 2023.csv")

#ojo hay una que está en xlsx y otra en csv: por eso uso dos librerías distintas
#observen que la forma de llamar a la base es distinta


#Pregunta: ¿Qué es un libro de códigos en una encuesta?
# https://www.ine.gob.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/libro-de-codigos/codigos-ene-2021.pdf?sfvrsn=54753851_38

#01.02 Primera mirada de la base
glimpse(base)


#02. Renombrar variables
names(base) #ver variables
names(libro) # ver libro de códigos

#renombro un conjunto de variables
base <- base %>% dplyr::rename(tiempo_libre = le01,  
                                 frutas = hs01,
                                 limite_alcohol = al01, 
                                 rapport= eta1)

#Práctica: renombre usted 4 variables que utilizará su grupo
# base <- base %>% dplyr::rename(_______ = _______,  
#                                 _______ = _______,
#                                 _______ = _______, 
#                                 _______ = _______)


#03. Limpieza de valores/categorías
#observación preliminar de base de datos
DataExplorer::create_report(base)

#¿Qué valores se ven extraños y podríamos cambiar?


#03.01. Limpio: ciudad_actual: donde lee libros
names(libro)
freq(base$ciudad_actual, prop = TRUE, order = "freq", report.nas =  FALSE)
unique(base$ciudad_actual)

# observo categorías a modificar

#cambio de categorías
base <- base %>%
  mutate(ciudad_actual = ifelse(ciudad_actual=="Donde pueda encontrar espacio en mi casa", yes= "Otros lugares de la casa", 
                          no=ifelse(ciudad_actual %in% c("donde sea", "Todos los anteriores ", "Todos", "Todas las anteriores "), yes="Indefinido", 
                                    no=ciudad_actual)))
# compruebo con una tabla de frecuencias
freq(base$ciudad_actual, prop = TRUE, order = "freq", report.nas =  FALSE)


#Práctica: cambie los resultados de ciudad actual
# freq(base$________, prop = TRUE, order = "freq", report.nas =  FALSE)
# unique(base$_______)

base <- base %>%
  mutate(ciudad_actual = stringi::stri_trans_general(ciudad_actual, "Latin-ASCII"),
         ciudad_actual= gsub(" ", "_", ciudad_actual))

unique(base$ciudad_actual)

# base <- base %>%
#   mutate(ciudad_actual = ifelse(ciudad_actual %in% c("__________", "_________", "__________", "__________", "__________"), yes="Santiago", 
#                                  no=ciudad_actual))

#¿Qué hacer con Valparaiso y Paine?



# #03.02. Limpieza de respuestas múltiples
#limpio: le11: porqué dispositivo: múltiples respuestas (multiple response)
unique(base$le11)

# cada persona, contesta más de una respuesta y aparece en la base separada por punto y coma (;)

#separo las respuestas y creo un vector que las lista
respuestas <- strsplit(base$le11, ";") # separo las respuestas que tienen punto y coma (;)
respuestas <- unlist(respuestas) #las unlisto, las saco de una lista

#observo las respuestas
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#cambios quienes contestaron: libgen y wattpad (donde lo ubicarían?)
#este método de recodificación sirve para vectores

respuestas <- gsub("libgen", "A través de sitios web no oficiales (por ejemplo: Lectulandia, Thepiratebay, Sci-hub, etc)", respuestas)
respuestas <- gsub("wattpad", "Wattpad", respuestas)

#Observo
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#Guardo para graficar
le11f <- freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()


#Práctica: haremos lo mismo para hs07: actividades físicas que realizó####
unique(base$hs07)

respuestas <- strsplit(base$hs07, ";")
respuestas <- unlist(respuestas)

#Observo
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() 

freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()%>% 
  print(n = 26)

#cambios quienes contestaron distintos tipos de danza, a danza. 
#Danza contemporánea·
#zumba
respuestas <- gsub("Danza contemporánea·", "Danza", respuestas)
respuestas <- gsub("zumba", "Danza", respuestas)

#Práctica: cambie quienes contestaron: salir a caminar·, Caminatas Rutinarias a Caminar




##04. Preguntas abiertas
# preguntarle a chatgpt: qué recomiendas para codificar preguntas abiertas en una encuesta?
unique(base$etb2a)
unique(base$etb2b)
unique(base$etb2c)

#¿Qué tipo de palabras son?
#adjetivos: características: positivos/negativos?
#sustantivos comunes/ propios: en qué área/ámbito?
#tendrán que codificar y observar tendencias


##05. Recodificaciones
#Edad
base <- base %>%
  mutate(edad = str_extract(edad, "\\d+")) %>% # solo números de la variable edad
  mutate(edad = as.numeric(edad)) %>%  #la transformo en variable numérica numérico
  mutate(edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                           edad %in% c(21:23) ~ "21 a 23", 
                           edad %in% c(24:29) ~ "24 a 29", 
                           edad >= 30 ~ "30 o más"))

freq(base$edadr, prop=TRUE,  report.nas = FALSE) %>% 
  tb() 

#práctica recodifique: anio_de_ingreso en: pre_pandemicos, pandemicos, pos_pandemia
unique(base$anio_de_ingreso)
freq(base$anio_de_ingreso, prop=TRUE,  report.nas = FALSE) %>% 
  tb() 


## 06. Análisis univariado de variables cualitativas ---------------------------
# 06.1. Distribución de frecuencias
names(base)

#genero


base %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

base %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") 

options(OutDec= ",") # que el separador de decimales sea coma (,)

#cómo haría para eliminar una categoría: "prefiero no responder"
class(base$genero)
unique(base$genero)

base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") 

#para guardarla
if(!dir.exists("tablas")) dir.create("tablas")

base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/dfgenero1.png", zoom = 3)


#Práctica: 
#realice dos distribuciones de frecuencias de las variables que utilizará
#guardelas en tabla

names(base)
names(libro)



# # 06.2. Gráficos categóricos univariados 
#Para geom_col(): gráfico de barras

#Realizo una tabla
dfgenero <- base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() 

names(dfgenero)

#opción 1
ggplot(dfgenero, aes(x = pct, y = fct_reorder(genero, pct), fill=genero)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Género Antropología UAH", 
       subtitle = "Encuesta Estudiantes 2023",
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

#opción 2
ggplot(dfgenero, aes(x =fct_reorder(genero, pct), y = pct, fill=genero)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Género Antropología UAH", 
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")


#Práctica: realice dos gráficos de barras, con dos variables de su grupo


# gráfico de tortas
ggplot(dfgenero, aes(x = "", y = pct, fill = genero)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Género Estudiantes UAH", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "Género") +
  theme_ipsum() +
  scale_fill_viridis_d( guide = "legend", limits = dfgenero$genero)

#por default: geom_bar cuenta las frecuencias, esto sería geom_bar(stat = "count")
#stat = "identity": cada barra representa el porcentaje de la categoría.

#Práctica: realice dos gráficos de torta.


# 07. Análisis bivariados variables cualitativas --------------------------
# ¿La realización de actividad física se realicionará con el género en los/as estudiantes de antropología?
# hs08
unique(base$hs08)
table(base$hs08)

base %>%
  filter(genero != "Prefiero no responder") %>%
  select(hs08, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

#recodifico para tener menos categorías 
base <- base %>% 
  mutate(hs08 = fct_recode(hs08,
                           "Entre 1 y 4 horas" = "Entre 1 y 4 horas",
                           "Más de 5" = "Entre 5 y 8 horas", 
                           "Más de 5" = "Más de 8 horas", 
                           "No realizo actividad física o deportiva" = "No realizo ninguna actividad física o deportiva durante la semana"))


#lo guardo
ct_acfisgenero <- base %>%
  filter(genero != "Prefiero no responder") %>%
  select(hs08, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

#graficar
#elijo mi paleta: VaporWave Color Palette
#elijo ciertos colores
colors <- c("#ff71ce", "#01cdfe", "#05ffa1", "#b967ff", "#fffb96") 


#Elija su propia paleta de colores
#https://www.color-hex.com/

ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = hs08)) + #subset: elimino (!-) cateogía "Prefiero no responder"
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)

#Rercodifico: cuando en hs07 dice que no realizó actividad física, tampoco debería haber realizado en hs08
base <- base %>% 
  mutate(hs08 = case_when(
    hs07 == "No realicé ninguna actividad física o deportiva" ~ "No realizo actividad física o deportiva",
    TRUE ~ hs08
  ))

ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = hs08)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)

#me sigue quedando un valor NA en Femenino
#uso sólo los complete.cases(hs08)
ggplot(data = subset(base, genero != "Prefiero no responder" & complete.cases(hs08)), aes(x = genero, fill = hs08)) + #solo utilizo filas completas
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)

#Práctica: ¿La preferencia de ficción o no ficción dependerá del género?
names(base)
names(libro)
unique(base$le06)


#acorto los nombres para que aparezcan bien en la tabla
base <- base %>% 
  mutate(le06 = fct_recode(le06,
                           "Ficción" = "Ficción (novelas de romance, acción, suspenso, fanfics, ciencia ficción, policiales, poesía, etc)",
                           "No ficción" = "No ficción (documentos científicos, académicos, revistas, biografías, etc)"))

#donde se pone la independiente: y
#donde se pone la dependiente: x


ctable( x = base$le06, y = base$genero, prop = "c", justify = "l")


#Realizo tabla de contingencia con tidyverse
base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le06, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

#guarde la tabla como ct_lecgenero
_________ <- base %>% 
  filter(genero != "Prefiero no responder") %>%
  select(______, _______) %>% #selecciono mis variables
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 


#graficó la tabla
#subset: de la base= base, no seleccione el género

ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = le06)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  xlab("Género")+
  theme_ipsum() +
  scale_fill_manual(values = colors)


ggplot(data = subset(________, __________ != "Prefiero no responder"), aes(x = genero, fill = le06)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  xlab("Género")+
  theme_ipsum() +
  scale_fill_manual(values = colors)+
  labs (title = "Preferencia Literaria por Género", 
         caption = "Encuesta Estudiantes Antropología UAH", 
         fill= "Tipo de literatura")


#Análisis
#¿Por qué los hombres leerán más ficción que las mujeres?
#¿Qué dice la bilbiografía?
#¿Qué dice la investigacación cualitativa?
#¿Cómo complemento lo que aparece en el "dato duro" y lo que señalan otras fuentes?






