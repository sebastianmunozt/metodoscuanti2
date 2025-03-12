####Carga de paquetes y bases de datos####

install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)
library(pacman)


####carga de bases y libro de codigos####

base <- readxl::read_excel("EncuestaAntropologia_2023.xlsx")
libro <- readr::read_csv("Encuesta antropología 2023.csv")

####Limpieza y transformación#####

####le10####

names(libro)
freq(base$le10, prop = TRUE, order = "freq", report.nas =  FALSE)
unique(base$le10)

base <- base %>%
  mutate(le10 = ifelse(le10=="Donde pueda encontrar espacio en mi casa", yes= "Otros lugares de la casa",
                       no=ifelse(le10 %in% c("donde sea", "Todos los anteriores ", "Todos", "Todas las anteriores ", "Todos los anteriores"), yes="Todas las anteriores",
                                 no=le10)))

freq(base$le10, prop = TRUE, order = "freq", report.nas =  FALSE)

####le11####

unique(base$le11)

Pregunta_le11 <- strsplit(base$le11, ";")
Pregunta_le11 <- unlist(Pregunta_le11)

Pregunta_le11 <- gsub("libgen", "A través de sitios web no oficiales (por ejemplo: Lectulandia, Thepiratebay, Sci-hub, etc)", Pregunta_le11)
Pregunta_le11 <- gsub("wattpad", "A través de sitios web no oficiales (por ejemplo: Lectulandia, Thepiratebay, Sci-hub, etc", Pregunta_le11)
Pregunta_le11 <- gsub("Wattpad", "A través de sitios web no oficiales (por ejemplo: Lectulandia, Thepiratebay, Sci-hub, etc", Pregunta_le11)


freq(Pregunta_le11, prop=TRUE, order = "freq", report.nas = FALSE) %>%
  tb()

le11f <- freq(Pregunta_le11, prop=TRUE, order = "freq", report.nas = FALSE) %>%
  tb()

####edad####
base <- base %>%
  mutate(edad = str_extract(edad, "\\d+")) %>%
  mutate(edad = as.numeric(edad)) %>%
  mutate(edadr= case_when (edad %in% c(18:20) ~ "18 a 20",
                           edad %in% c(21:23) ~ "21 a 23",
                           edad %in% c(24:29) ~ "24 a 29",
                           edad >= 30 ~ "30 o más"))

####ciudad####
base <- base %>%
  mutate(ciudad_actual = ifelse(ciudad_actual %in% c("donde sea", "Todos los anteriores ", "Todos", "Todas las anteriores "), yes="Santiago",
                                no=ciudad_actual))


####Realización de tablas de frecuencias para todas las variables sociodemográficas####

webshot::install_phantomjs()

if(!dir.exists("tablas")) dir.create("tablas")

####edad####
base %>% 
  freq(edadr, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Rango de edad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en rangos de edad", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/edad1.png", zoom = 3)

####genero#####



base %>% 
  mutate(genero = if_else(genero =="Prefiero no responder", as.character(NA), genero)) %>%
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/genero1.png", zoom = 3)

if(!dir.exists("tablas")) dir.create("tablas")



####Ciudad####



base %>% 
  freq(ciudad_actual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Ciudad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Ciudad", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/ciudad_actual1.png", zoom = 3)



####comuna####


base %>% 
  freq(comuna_actual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Comuna", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de comunas", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/comuna_actual1.png", zoom = 3)


####Año de ingreso####


base %>% 
  freq(anio_de_ingreso, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Año de ingreso", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en los años de ingreso", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/anio_de_ingreso1.png", zoom = 3)





####notas####


base %>% 
  freq(avr_notas, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Notas avr", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en las notas avr", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/avr_notas1.png", zoom = 3)



####Educación####



base %>% 
  freq(educacion, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Educación", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en la educación", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/educacion1.png", zoom = 3)


#####clase social####



base %>% 
  freq(clase_social, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Clase social", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en la clase social", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/clase_social1.png", zoom = 3)



####educación madre#####

base %>% 
  freq(educacion_madre, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Educacion de la madre", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en la educacion de la madre", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/educacion_madre1.png", zoom = 3)


####educación padre####

base %>% 
  freq(educacion_madre, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Educacion del padre", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en la educacion del padre", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/educacion_padre1.png", zoom = 3)

####Realización de tablas de frecuencias para todas las variables de su grupo####
####le01#####


base %>% 
  freq(le01, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Tiempo libre semanal", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en el tiempo libre semanal", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le01.png", zoom = 3)




####le2#####



base %>% 
  freq(le02, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Se utiliza para leer", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en el uso para la", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le02.png", zoom = 3)






####le3#####



base %>% 
  freq(le03, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Tiempo que utiliza para leer", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en el tiempo de la lectura", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le03.png", zoom = 3)

####le04####

base %>% 
  freq(le04, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Planificado el tiempo", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en si está el tiempo planificado", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le04.png", zoom = 3)







####le05####


base %>% 
  freq(le05, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Dedicarle más tiempo a la lectura personal, además de la obligatoria", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en dedicar más tiempo a la lectura personal, además de la obligatoria", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le05.png", zoom = 3)





####le06####


base %>% 
  freq(le06, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género preferido", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias del género preferido", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le06.png", zoom = 3)




####le07####
base %>% 
  freq(le07a, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Cuento", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en cuento_le07a", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07a.png", zoom = 3)



base %>% 
  freq(le07b, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Drama", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en drama_le07b", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07b.png", zoom = 3)


base %>% 
  freq(le07c, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Suspenso", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en suspenso_le07c", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07c.png", zoom = 3)


base %>% 
  freq(le07d, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Comedia", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en comedia_le07d", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07d.png", zoom = 3)


base %>% 
  freq(le07e, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Leyenda", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en leyenda_le07e", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07e.png", zoom = 3)



base %>% 
  freq(le07f, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Fanfic", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en fanfic", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07f.png", zoom = 3)



base %>% 
  freq(le07g, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Ciencia Ficción", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en ciencia ficción", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le07g.png", zoom = 3)

####le08####
base %>% 
  freq(le08a, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Documentos científicos y papers ", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en documentos científicos y papers", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le08a.png", zoom = 3)


base %>% 
  freq(le08b, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Ensayos ", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en ensayos", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le08b.png", zoom = 3)


base %>% 
  freq(le08c, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Textos periodísticos ", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en textos periodísticos ", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le08c.png", zoom = 3)


base %>% 
  freq(le08d, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Revistas especializadas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en revistas especializadas  ", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le08d.png", zoom = 3)



base %>% 
  freq(le08e, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Crítica literaria y de arte ", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias en crítica literaria y de arte   ", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le08e.png", zoom = 3)

####le09####



base %>% 
  freq(le09, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Nivel de acuerdo con la frase Mi carrera universitaria influye en mis preferencias de lectura personal
", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias del nivel de acuerdo con la frase Mi carrera universitaria influye en mis preferencias de lectura personal", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le09.png", zoom = 3)


####le10####



base %>% 
  freq(le10, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Lugar para la lectura personal", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias del lugar para la lectura personal", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le10.png", zoom = 3)


####le11####


base %>% 
  freq(le11, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Medios", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de los medios", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le11.png", zoom = 3)


####le012####



base %>% 
  freq(le12, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Medios por los sitios web oficiales o no oficiales", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de los medios por los sitios web oficiales o no oficiales", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/le12.png", zoom = 3)

####Realización de gráficos de barra para las 3 variables más destacadas de su grupo####
####Realización de 2 gráficos de tortas de variables más destacadas de su grupo####
####Realización 3 tablas de contingencia donde pueda utilizar variables independientes y dependientes considerando los problemas centrales de su grupo####


####Realización de gráficos de barra para las 3 variables más destacadas de su grupo####
####creación de tabla####

####le02####
tabla2 <-table(base$le02)
dfle02 <-as.data.frame(tabla2)

grafico2 <- ggplot(dfle02, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  labs(x = "", y = "Frecuencia", fill = "", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023") +
  ggtitle("Tiempo para lecturas personales de estudiantes UAH")

print(grafico2)

####le03####

tabla3 <-table(base$le03)
dfle03 <-as.data.frame(tabla3)

grafico3 <- ggplot(dfle03, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  labs(x = "Horas", y = "Frecuencia", fill = "", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023") +
  ggtitle("Cantidad de tiempo para Lectura Personal estudiantes UAH")

print(grafico3)


####le06####

tabla6 <-table(base$le06)
dfle06 <-as.data.frame(tabla6)

dfle06$Var1 <- ifelse(dfle06$Var1 == "Ficción (novelas de romance, acción, suspenso, fanfics, ciencia ficción, policiales, poesía, etc)", "Ficción",
                      ifelse(dfle06$Var1 == "No ficción (documentos científicos, académicos, revistas, biografías, etc)", "No Ficción", dfle06$Var1))

grafico6 <- ggplot(dfle06, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  labs(x = "Género literario", y = "Frecuencia", fill = "", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023") +
  ggtitle("Preferencia en éneros literarios de estudiantes UAH")

print(grafico6)

####Realización de 2 gráficos de tortas de variables más destacadas de su grupo####

####dfle03####

ggplot(dfle03, aes(x = "", y = "", fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Cantidad de tiempo para Lectura Personal estudiantes UAH",
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "Horas") +
  theme_ipsum() +
  scale_fill_manual(values = c("blue", "red", "green", "yellow", "purple"))


####dle06####

ggplot(dfle06, aes(x = "", y = "", fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Preferencia en géneros literarios de estudiantes UAH",
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "Géneros literarios") +
  theme_ipsum() +
  scale_fill_manual(values = c("blue", "orange"))

#Los gráficos de torta presentan un error, se gráfican las categorías y se dividen en igual magnitud simplemente
#graficando las categorías pero no su magnitud. Para ello deben de asignar el nombre de la variable en aes(x= nombre_variale).
#


####Realización 3 tablas de contingencia donde pueda utilizar variables independientes y dependientes considerando los problemas centrales de su grupo####

####tablac01####
base %>%
  select(edad, le06) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

####Guardo la tablale02####
tablac01 <- base %>% 
  select(edad, le06) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>%
  prop.table(.,2) %>%
  round(4)*100

####tablac02####
base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le03, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

####Guardo la tablale03####

tablac02 <- base %>% 
  filter(genero != "Prefiero no responder") %>%
  select(le03, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>%
  prop.table(.,2) %>%
  round(4)*100 

####tablac03####
base %>%
  select(comuna_actual, le06) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

####Se guarda  la tabla####

tablac03 <- base %>% 
  select(comuna_actual, le06) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>%
  prop.table(.,2) %>%
  round(4)*100 

####Realización de 3 gráficos que incluyan la información de dos variables (pueden ser los mismos de la tabla de contingencia)####

####Grafico de Género de lectura preferido####
ggplot(dfle06, aes(x = factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = c("#FFD600", "#00BFFF")) +
  labs(title = "Género de lectura preferido", x = "Géneros de lectura", y = "Número de respuestas") +
  scale_fill_manual(values = c("#FFD600", "#00BFFF"))

####Grafico de horas dedicadas a las lecturas personales####
ggplot(dfle03, aes(x = factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", aes(fill = factor(Var1))) +
  labs(title = "Cantidad de horas dedicadas a las lecturas personales", x = "Cantidad de Horas", y = "Número de respuestas") +
  scale_fill_manual(values = c("#00FFF4", "#FF00FF", "#00FF00", "#FFFF00", "#0000FF"))

####Grafico de ¿Utilizas parte de tu tiempo libre para lecturas personales?####
ggplot(dfle02, aes(x = factor(Var1), y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity") +
  labs(title = "¿Utilizas parte de tu tiempo libre para lecturas personales", x = "Respuestas", y = "Número de respuestas") +
  scale_fill_manual(values = c("#FF19C8", "#00BFFF", "#FFD600", "#00FF00"))




####Parte 2####

####5 tablas para los análisis univariados####

base_filtrada <- subset(base, genero != "Prefiero no responder")
colores <- c("#469E10", "#172FEB", "#EB5217")
tabla_frecuencia <- table(base_filtrada$genero)
pie(tabla_frecuencia,
    main = "Distribución por Género",
    col = colores)
etiquetas <- paste(names(tabla_frecuencia), "\n", tabla_frecuencia)
legend("topright", legend = etiquetas, bty = "n", cex = 0.8)

##################

tabla_frecuencia <- table(base$le03)
num_barras <- length(tabla_frecuencia)
colores <- sample(colors(), num_barras)
barplot(tabla_frecuencia,
        main = "Horas de tiempo libre dedicado a la lectura personal",
        xlab = "Horas",
        ylab = "Frecuencia",
        col = colores,
        cex.names = 0.7,
        las = 1,
        ylim = c(0, max(tabla_frecuencia) + 5),  # Ajusta los límites del eje Y
        breaks = seq(0, max(tabla_frecuencia) + 5, by = 5))  # Define los intervalos de frecuencia en el eje Y
text(x = 1:num_barras, y = tabla_frecuencia, labels = tabla_frecuencia,
     pos = 3, cex = 0.8)

###################

tabla_frecuencia2 <- table(base$le02)
colores <- sample(colors(), length(tabla_frecuencia2))
total_respuestas <- sum(tabla_frecuencia2)
porcentajes <- round(tabla_frecuencia2 / total_respuestas * 100, 1)
etiquetas <- paste(names(tabla_frecuencia2), "(", tabla_frecuencia2, ")", sep = " ")
barplot(tabla_frecuencia2,
        main = "Respuesta a la pregunta:\n¿Utilizas de tu tiempo libre para realizar lecturas personales?",
        col = colores,
        ylim = c(0, max(tabla_frecuencia2) + 10),
        ylab = "Cantidad de respuestas",
        xlab = "Categorías")
text(x = 1:length(tabla_frecuencia2), y = tabla_frecuencia2, labels = etiquetas, pos = 3, cex = 0.8)


####################
tabla_frecuencia3 <- table(base$le05)
colores <- sample(colors(), length(tabla_frecuencia3))
total_respuestas <- sum(tabla_frecuencia3)
porcentajes <- round(tabla_frecuencia3 / total_respuestas * 100, 1)
etiquetas <- paste(names(tabla_frecuencia3), "(", tabla_frecuencia3, ")", sep = " ")
pos_max <- which.max(tabla_frecuencia3)
barplot(tabla_frecuencia3,
        main = "Te gustaría dedicarle más tiempo a la lectura personal 
        aparte de la lectura obligatoria o complementaria",
        col = colores,
        ylim = c(0, max(tabla_frecuencia3) + 10),
        ylab = "Cantidad de respuestas",
        xlab = "Categorías")
text(x = 1:length(tabla_frecuencia3), y = tabla_frecuencia3, labels = etiquetas, pos = 3, cex = 0.8)
text(x = pos_max, y = tabla_frecuencia3[pos_max], labels = tabla_frecuencia3[pos_max], pos = 3, cex = 0.8)

######################

colores <- rainbow(length(tabla_frecuencia4))
total_personas <- sum(tabla_frecuencia4)
etiquetas <- paste(names(tabla_frecuencia4), "(", tabla_frecuencia4, ")", sep = "")
pie(tabla_frecuencia4,
    main = "Distribución por Género",
    col = colores)
legend("topright", legend = etiquetas, bty = "n", cex = 0.8)

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)

base <- read.xlsx(xlsxFile = "base/EncuestaAntropologia_2023.xlsx")
libro <- read.csv(file = "base/Encuesta antropología 2023.csv")



####4 gráficos bivariados y tablas de contingencia####



#####genero con el género que más se lee####


base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le06, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 


genero_y_lectura_g <- base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le06, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

kable(genero_y_lectura_g) %>% 
  kable_classic() 

colors <- c("#ff71ce", "#01cdfe", "#05ffa1", "#b967ff", "#fffb96") 


ggplot(data = subset(base, genero != "Prefiero no responder" & complete.cases(le06)), aes(x = genero, fill = le06)) + 
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)+
  labs(title = "Género de la persona y género que más se lee")



#####género y dedicar tiempo para la lectura#####


base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le02, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

genero_y__si_se_dedica_tiempo_para_leer <- base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le02, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

kable(genero_y__si_se_dedica_tiempo_para_leer) %>% 
  kable_classic() 


colors <- c("#ff71ce", "#01cdfe", "#05ffa1", "#b967ff", "#fffb96") 




ggplot(data = subset(base, genero != "Prefiero no responder" & complete.cases(le02)), aes(x = genero, fill = le02)) + 
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)+
  labs(title = "Género de la persona y si se dedica tiempo a la lectura")




####género y tiempo libre que se utiliza para leer#### 


base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le03, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

genero_y_tiempo_en_horas <- base %>%
  filter(genero != "Prefiero no responder") %>%
  select(le03, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

kable(genero_y_tiempo_en_horas) %>% 
  kable_classic() 


colors <- c("#ff71ce", "#01cdfe", "#05ffa1", "#b967ff", "#fffb96") 


ggplot(data = subset(base, genero != "Prefiero no responder" & complete.cases(le03)), aes(x = genero, fill = le03)) + 
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)+
  labs(title = "Género y el tiempo que se utiliza para leer")



#####género y edad####

base %>%
  filter(le06 != "Prefiero no responder") %>%
  select(le12, le06) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 

genero_y_tiempo_de_leer_personal <- base %>%
  filter(le06 != "Prefiero no responder") %>%
  select(edadr, le06) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100 


kable(genero_y_tiempo_de_leer_personal) %>% 
  kable_classic() 

colors <- c("#ff71ce", "#01cdfe", "#05ffa1", "#b967ff", "#fffb96") 


ggplot(data = subset(base, le06 != "Prefiero no responder" & complete.cases(edadr)), aes(x = le06, fill = edadr)) + 
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors)  +
  labs(title = "Preferencia de lectura y rango de edad")



#Comentario final: A no ser por el problema de los gráficos de torta, su trabajo está perfecto!, felicitaciones!

