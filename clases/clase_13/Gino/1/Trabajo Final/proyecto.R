

# Carga de paquetes -------------------------------------------------------
install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)

  #i.	Carga bases de datos


base <- read.xlsx(xlsxFile = "base/EncuestaAntropologia_2023.xlsx")

libro <- read.csv(file = "base/Encuesta antropología 2023.csv")


# ii. Limpieza y transformación de las variables --------------------------

      #Limpieza variables de la ciudad de origen


base <- base %>%
  mutate(ciudad_actual = stringi::stri_trans_general(ciudad_actual, "Latin-ASCII"),
         ciudad_actual = gsub("\\s+", "_", ciudad_actual))

unique(base$ciudad_actual)

     #Santiago

base <- base %>%
  mutate(ciudad_actual = ifelse(ciudad_actual %in% c("Santiago_", "Santiago", "Metropolitana_", "Santiago_centro", "Santiagl", "santiago_"), yes="Santiago", 
                                no=ciudad_actual))
    #Paine

base <- base %>%
  mutate(ciudad_actual = ifelse(ciudad_actual == "Paine_", yes="Paine", 
                                no=ciudad_actual))
    #Valparaiso

base <- base %>%
  mutate(ciudad_actual = ifelse(ciudad_actual == "Valparaiso_", yes="Valparaiso", 
                                no=ciudad_actual))

#Creación de variable de pre-pandemia y post-pandemia

base <- base %>% 
  mutate(pandemia_o_no_pandemia =
           case_when(anio_de_ingreso <= 2019 ~ "pre-pandemia",
                     anio_de_ingreso >= 2020 ~ "post-pandemia"))

#Recodificación de los nombres de nuestras variables

base <- base %>% rename(concorde_con_rapport = eta1,
                        concorde_con_etica_en_tercero = eta2,
                        claridad_de_rapport = eta3,
                        caso_etico = eta4,
                        valor_colaborativo = etb1,
                        valor_horizontal = et01,
                        profesores_promueven_colaboracion = etb3d,
                        contacto_post_terreno = etb4,
                        importancia_confidencialidad = etb5,
                        realizar_retribucion = etb6,
                        consentimiento_informado = etb7,
                        post_ramo_etica = etb8)

#Corregir concorde_con_rapport

base <- base %>% 
  mutate(concorde_con_rapport = ifelse(concorde_con_rapport >= 3, yes = "De acuerdo",
                                       no = "No estoy de acuerdo"))

#Corregir variable consentimiento_informado 
respuestas_consentimiento_informado <- strsplit(base$consentimiento_informado, ";") # separo las respuestas que tienen punto y coma (;)
respuestas_consentimiento_informado <- unlist(respuestas_consentimiento_informado)

freq(respuestas_consentimiento_informado, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()


# iii.	Realización de tablas de frecuencias sociodemograficas -------------


names(base)

base %>% 
  freq(edad, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Edad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de edades", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(ciudad_actual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Ciudad actual", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Ciudad actual", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(comuna_actual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Comuna actual", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de comunas", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")
base %>% 
  freq(anio_de_ingreso, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Año de ingreso", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de año de ingreso", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(avr_notas, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Promedio notas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de notas enseñanza media", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(educacion, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Tipo de educación", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de tipos de educación", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(clase_social, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Clase social", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de clases sociales", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(educacion_madre, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Educación de la madre", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de la educación de la madre", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(educacion_padre, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Educación del padre", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de la educación del padre", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")


# iv.	Realización de tablas de frecuencias del grupo ----------------------

names(base)

base %>% 
  freq(concorde_con_rapport, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("¿Estás de acuerdo con definición de rapport?", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de la definición de rapport", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(concorde_con_etica_en_tercero, prop = TRUE, report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("¿Qué tan de acuerdo estás?", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de preferencia de ética en tercer año", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(claridad_de_rapport, prop = TRUE, report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("¿Qué tan claro estás?", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias sobre la claridad sobre definición de rapport", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(caso_etico, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Preferencias", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de caso ético", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(valor_colaborativo, prop = TRUE, report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de importancia de colaboración con el informante", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(valor_horizontal, prop = TRUE, report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de importancia de horizontalidad con el informante", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(profesores_promueven_colaboracion, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de cuánto promueven la colaboración con informante los profesores", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(contacto_post_terreno, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de contacto después del terreno", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(importancia_confidencialidad, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Educación de la madre", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de importancia de la confidencialidad con el informante", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(realizar_retribucion, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de realización de devolución/retribución", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

freq(respuestas_consentimiento_informado, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de conformidad con el ramo de ética en tercer año", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

base %>% 
  freq(post_ramo_etica, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>% 
  kable(col.names = c("Respuestas", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de conformidad con el ramo de ética en tercer año", 
        digits = c(0, 0, 1, 1))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria")

# v.	Realización de gráficos de barra para las 3 variables  ---------------
#variable 1
df_variable_1<- base %>% 
  freq(realizar_retribucion, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

ggplot(df_variable_1, aes(x = pct, y = fct_reorder(realizar_retribucion, pct), fill= realizar_retribucion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Retribucion informantes", 
       subtitle = "Encuesta estudiantes antropologia",
       caption = "FUENTE: Encuesta Estudiantes Antropología 2023")

#variable 2
df_variable_2<- base %>% 
  freq(concorde_con_etica_en_tercero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

ggplot(df_variable_2, aes(x = pct, y = fct_reorder(concorde_con_etica_en_tercero, pct), fill= concorde_con_etica_en_tercero)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "De acuerdo con ramo etica en tercero año", 
       subtitle = "Encuesta estudiantes antropologia",
       caption = "FUENTE: Encuesta Estudiantes Antropología 2023")

#variable 3
df_variable_3<- base %>% 
  freq(valor_colaborativo, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

ggplot(df_variable_3, aes(x = pct, y = fct_reorder(valor_colaborativo, pct), fill= valor_colaborativo)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "colaboracion con informantes", 
       subtitle = "Encuesta estudiantes antropologia",
       caption = "FUENTE: Encuesta Estudiantes Antropología 2023")


# vi. Realización de 2 gráficos de tortas  --------------------------------

df_grafico_torta_1 <- base %>% 
  freq(profesores_promueven_colaboracion, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

ggplot(df_grafico_torta_1, aes(x = "", y = pct, fill = profesores_promueven_colaboracion)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Docentes promueven relaciones colaborativas", 
       caption = "FUENTE: Encuesta de Estudiantes UAH 2023",
       fill = "Respuestas",
       x = "", y = "")+
  theme_ipsum()


df_grafico_torta_2 <- base %>% 
  freq(contacto_post_terreno, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

ggplot(df_grafico_torta_2, aes(x = "", y = pct, fill = contacto_post_terreno)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Contacto post terreno", 
       caption = "FUENTE: Encuesta de Estudiantes UAH 2023",
       fill = "Respuestas",
       x = "", y = "")+
  theme_ipsum()


# vii.	Realización 3 tablas de contingencia -------------------------------

base %>%
  select(concorde_con_rapport, contacto_post_terreno) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

base %>% 
  select(profesores_promueven_colaboracion, valor_colaborativo) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(., 2) %>% 
  prop.table(., 2) %>% 
  round(4) * 100

base %>%
  select(concorde_con_etica_en_tercero, caso_etico) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100


# viii.	Realización de 4 gráficos bivariados ------------------------------

ggplot(data = subset(base, pandemia_o_no_pandemia != "NA"), aes(x = concorde_con_rapport, fill = pandemia_o_no_pandemia)) +
  geom_bar(stat = "count") +
  labs(
    title = "Relación concorde con rapport y pre/post pandemia",
    subtitle = "Encuesta Estudiantes Antropología UAH",
    caption = "Fuente: Encuesta Antropologia 2023",
    x="", y= "Frecuencia", 
    fill= "Respuestas"
    )+
  theme_ipsum()

ggplot(data = subset(base, contacto_post_terreno != "NA"), aes(x = concorde_con_rapport, fill = contacto_post_terreno)) +
  geom_bar(stat = "count") +
  labs(
    title = "Relación concorde con rapport y contacto post terreno",
    subtitle = "Encuesta Estudiantes Antropología UAH",
    caption = "Fuente: Encuesta Antropologia 2023",
    x="¿De acuerdo con la definición de rapport?", y= "Frecuencia", 
    fill= "¿Has visitado o contactado con tu informante clave post terreno?"
  )+
  theme_ipsum()

ggplot(data = subset(base, pandemia_o_no_pandemia != "NA"), aes(x = concorde_con_rapport, fill = pandemia_o_no_pandemia)) +
  geom_bar(stat = "count") +
  labs(
    title = "Relación concorde con rapport y pre/post pandemia",
    subtitle = "Encuesta Estudiantes Antropología UAH",
    caption = "Fuente: Encuesta Antropologia 2023",
    x="", y= "Frecuencia", 
    fill= "Respuestas"
    )+
  theme_ipsum()

    #Recodificar Respuestas valor_horizontal

unique(base$valor_horizontal)

base <- base %>% 
  mutate(r_valor_horizontal = case_when(
    valor_horizontal %in% c(1, 2, 3) ~ "Poco horizontal",
    valor_horizontal %in% c(4, 5, 6) ~ "Medianamente horizontal",
    valor_horizontal %in% c(7, 8, 9, 10) ~ "Muy horizontal"
  ))

ggplot(data = subset(base, r_valor_horizontal != "NA"), aes(x = r_valor_horizontal, fill = genero)) +
  geom_bar(stat = "count") +
  labs(
    title = "Relacion horizontal y genero",
    subtitle = "Encuesta Estudiantes Antropología UAH",
    caption = "Fuente: Encuesta Antropologia 2023",
    x="Relación con el informante clave", y= "Frecuencia", 
    fill= "Respuestas"
  )+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

names(base)

ggplot(data = subset(base, profesores_promueven_colaboracion != "NA"), aes(x = profesores_promueven_colaboracion, fill = anio_de_ingreso)) +
  geom_bar(stat = "count") +
  labs(
    title = "Año de ingreso y promover colaboracion docentes",
    subtitle = "Encuesta Estudiantes Antropología UAH",
    caption = "Fuente: Encuesta Antropologia 2023",
    x="", y= "Frecuencia", 
    fill= "Respuestas"
  )+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ix.	Realización de un índice aditivo ------------------------------------



# x.	Cruce del índice aditivo con otras dos variables ---------------------




