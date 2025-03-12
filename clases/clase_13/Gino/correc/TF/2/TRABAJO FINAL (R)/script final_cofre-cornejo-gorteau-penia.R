
#### GRUPO: CONSUMO DE ALCOHOL EN ESTUDIANTES DE ANTROPOLOGÍA ####

## Carolina Cofré, Pablo Cornejo, Marithé Gorteau, Octavio Peña ##

## 1. INSTALACIONES ####

# instalación paquetes

install.packages("tidyverse")

install.packages("pacman")

install.packages("dplyr")

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)


# instalación base de datos

base_antropo <- read.xlsx(xlsxFile = "base/EncuestaAntropologia_2023.xlsx")


# instalación libro de códigos

libro_antropo <- read.csv(file = "base/Encuesta antropología 2023.csv")



## 2. LIMPIEZA Y TRANSFORMACIÓN DE DATOS ####

names(base_antropo)
names(libro_antropo)

# renombrar variables

base_antropo <- base_antropo %>% dplyr::rename(nobeber_limitante = al01,
                               alcohol_fueraclases = al02,
                               alcohol_compartir = al03,
                               presion_social = al04,
                               alcohol_facilita_interacciones = al05,
                               compañero_alcohol = al06,
                               anio2022_consumo_mensual = al07,
                               salud_mental = al08,
                               desempenio_academico = al09,
                               erasmo = al10)


DataExplorer::create_report(base_antropo)

# limpieza variables

unique(base_antropo$ciudad_actual)

base_antropo <- base_antropo %>%
  mutate(ciudad_actual = ifelse (ciudad_actual %in% c("Santiago·","Metropolitana·","Santiago centro","santiago·","Santiagl", "Santiago ", "santiago ", "Metropolitana "), 
                                                      yes="Santiago", no=ciudad_actual))
                                 
# recodificar variables

# edad

base_antropo <- base_antropo %>%
  mutate(edad = str_extract(edad, "\\d+")) %>% 
  mutate(edad = as.numeric(edad)) %>%  
  mutate(edadr= case_when (edad %in% c(18:20) ~ "18 a 20", 
                           edad %in% c(21:23) ~ "21 a 23", 
                           edad %in% c(24:29) ~ "24 a 29", 
                           edad >= 30 ~ "30 o más"))
# año de ingreso

base_antropo <- base_antropo %>%
  mutate(anio_de_ingreso = str_extract(anio_de_ingreso, "\\d+")) %>% 
  mutate(anio_de_ingreso = as.numeric(anio_de_ingreso)) %>% 
  mutate(anio_de_ingresor= case_when (anio_de_ingreso %in% c(2017:2019) ~ "Estudiantes prepandemicos", 
                                      anio_de_ingreso %in% c(2020:2021) ~ "Estudiantes pandemicos",
                                      anio_de_ingreso %in% c(2022:2023) ~ "Estudiantes postpandemicos"))
         
                                 
## 3. TABLAS DE FRECUENCIA PARA VARIABLES SOCIODEMOGRÁFICAS ####

# para guardar las tablas

webshot::install_phantomjs()

if(!dir.exists ("tablas")) dir.create ("tablas")

# 3.1 variable de género

base_antropo %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla genero

base_antropo %>% 
  freq(genero, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")%>%
  save_kable(file = "tablas/genero.png", zoom=3) 

# 3.2 variable de edad

base_antropo %>% 
  freq(edadr, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Edad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencia de edades", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar variable edad

base_antropo %>% 
  freq(edadr, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Edad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencia de edades", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")%>%
  save_kable(file = "tablas/edadr.png", zoom=3)


# 3.3 variable de ciudad actual

base_antropo %>% 
  freq(ciudad_actual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Ciudad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de ciudades de residencia actual", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")


# guardar variable ciudad actual

base_antropo %>% 
  freq(ciudad_actual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Ciudad", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de ciudades de residencia actual", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")%>%
  save_kable(file = "tablas/ciudad.png", zoom=3)

## 4. TABLAS DE FRECUENCIA PARA TODAS LAS VARIABLES DEL GRUPO ####


# tabla 01

base_antropo %>% 
  freq(nobeber_limitante, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Consumo", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumir alcohol  limita interacciones sociales", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 01

base_antropo %>%
  freq(nobeber_limitante, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Consumo", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumir alcohol  limita interacciones sociales", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")%>%
  save_kable(file = "tablas/nobeberlimitante.png", zoom=3)


# tabla 02

base_antropo %>% 
  freq(alcohol_fueraclases, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Se incorpora", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de consumo fuera del espacio universitario", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 02

base_antropo %>% 
  freq(alcohol_fueraclases, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Se incorpora", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de consumo fuera del espacio universitario", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
save_kable(file = "tablas/alcoholfueradeclases.png", zoom=3)


# tabla 03

base_antropo %>% 
  freq(alcohol_compartir, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Del 1 al 5", "Frecuencia", "%", "% Acumulado"),
        caption = "Del 1 al 5 que tan de acuerdo estás con que beber ayuda a compartir entre compañerxs (siendo 1 muy en desacuerdo y 5 muy de acuerdo)", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 03
base_antropo %>% 
  freq(alcohol_compartir, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Del 1 al 5", "Frecuencia", "%", "% Acumulado"),
        caption = "Del 1 al 5 que tan de acuerdo estás con que beber ayuda a compartir entre compañerxs (siendo 1 muy en desacuerdo y 5 muy de acuerdo)", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/alcoholcompartir.png", zoom=3)


# tabla 04

base_antropo %>% 
  freq(presion_social, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Te sientes", "Frecuencia", "%", "% Acumulado"),
        caption = "Presión al consumo de alcohol al compartir con amistades de universidad después de clases", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 04

base_antropo %>% 
  freq(presion_social, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Te sientes", "Frecuencia", "%", "% Acumulado"),
        caption = "Presión al consumo de alcohol al compartir con amistades de universidad después de clases", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/presionalcohol.png", zoom=3)


# tabla 05

base_antropo %>% 
  freq(alcohol_facilita_interacciones, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Facilita la relación", "Frecuencia", "%", "% Acumulado"),
        caption = "Alcohol como facilitador para relacionarse con compañerxs fuera del horario de clases", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 05

base_antropo %>% 
  freq(alcohol_facilita_interacciones, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Facilita la relación", "Frecuencia", "%", "% Acumulado"),
        caption = "Alcohol como facilitador para relacionarse con compañerxs fuera del horario de clases", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/alcoholfacilitarelacion.png", zoom=3)


# tabla 06

base_antropo %>% 
  freq(compañero_alcohol, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Conoce a alguien que beba alcohol para relacionarse fuera de la universidad", "Frecuencia", "%", "% Acumulado"),
        caption = "Conocimiento de alguien que recurra al alcohol para socializar fuera del horario de clases", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 06

base_antropo %>% 
  freq(compañero_alcohol, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Conoce a alguien que beba alcohol para relacionarse fuera de la universidad", "Frecuencia", "%", "% Acumulado"),
        caption = "Conocimiento de alguien que recurra al alcohol para socializar fuera del horario de clases", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/alcoholparasocializar.png", zoom=3)


# tabla 07

base_antropo %>% 
  freq(anio2022_consumo_mensual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Cantidad de veces", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumo fuera de clases en el último semestre de 2022", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 07

base_antropo %>% 
  freq(anio2022_consumo_mensual, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Cantidad de veces", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumo fuera de clases en el último semestre de 2022", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/consumomensual2022.png", zoom=3)


# tabla 08

base_antropo %>% 
  freq(salud_mental, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Afecta la salud mental", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumo de alcohol en contexto universitario y salud mental", 
        digits = c(0, 0, 2, 2))%>%    
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 08

base_antropo %>% 
  freq(salud_mental, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Afecta la salud mental", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumo de alcohol en contexto universitario y salud mental", 
        digits = c(0, 0, 2, 2))%>%    
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/saludmental.png", zoom=3)


# tabla 09

base_antropo %>% 
  freq(desempenio_academico, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Afecta el desempeño académico", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumo de alcohol constante durante el contexto universitario afecta el desempeño académico", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

# guardar tabla 09

base_antropo %>% 
  freq(desempenio_academico, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Afecta el desempeño académico", "Frecuencia", "%", "% Acumulado"),
        caption = "Consumo de alcohol constante durante el contexto universitario afecta el desempeño académico", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/desempenioacademico.png", zoom=3)


# tabla 10

base_antropo %>% 
  freq(erasmo, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Del 1 al 5", "Frecuencia", "%", "% Acumulado"),
        caption = "Erasmo Escala como un espacio de consumo de alcohol, ¿está bien? Del 1 al 5 (siendo 1 muy en desacuerdo y 5 muy de acuerdo)", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman")

#guardar tabla 10

base_antropo %>% 
  freq(erasmo, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Del 1 al 5", "Frecuencia", "%", "% Acumulado"),
        caption = "Erasmo Escala como un espacio de consumo de alcohol, ¿está bien? Del 1 al 5 (siendo 1 muy en desacuerdo y 5 muy de acuerdo)", 
        digits = c(0, 0, 2, 2))%>%  
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable(file = "tablas/erasmo.png", zoom=3)



## 5. GRÁFICOS DE BARRA VERTICALES PARA VARIABLES DESTACADAS ####

# 5.1 variable: alcohol_facilita_interacciones

unique(base_antropo$alcohol_facilita_interacciones)

dfalcohol_facilita_interacciones <- base_antropo %>% 
  freq(alcohol_facilita_interacciones, prop = TRUE, order = "freq", report.nas = FALSE) %>%
  tb()

# gráfico

ggplot(dfalcohol_facilita_interacciones, aes(x =fct_reorder(alcohol_facilita_interacciones, pct), y = pct, fill= alcohol_facilita_interacciones)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "No beber alcohol limita la relación entre compañerxs", 
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

# 5.2 variable: salud_mental

unique(base_antropo$salud_mental)

dfsalud_mental <- base_antropo %>% 
  freq(salud_mental, prop = TRUE, order = "freq", report.nas = FALSE) %>%
  tb()

# gráfico

ggplot(dfsalud_mental, aes(x =fct_reorder(salud_mental, pct), y = pct, fill= salud_mental)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Alcohol afecta la salud mental", 
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

# 5.3 variable: desempenio_academico

unique(base_antropo$desempenio_academico)

dfdesempenio_academico <- base_antropo %>% 
  freq(desempenio_academico, prop = TRUE, order = "freq", report.nas = FALSE) %>%
  tb()

# gráfico

ggplot(dfdesempenio_academico, aes(x =fct_reorder(desempenio_academico, pct), y = pct, fill= desempenio_academico)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Alcohol afecta el desempeño académico", 
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")



## 6. GRÁFICOS DE TORTA PARA VARIABLES DESTACADAS ####

# 6.1 variable: salud_mental
# gráfico

ggplot(dfsalud_mental, aes(x = "", y = pct, fill = salud_mental)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + 
  labs(title = "Alcohol influye en la salud mental de estudiantes de Antropología UAH", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "salud_mental") +
  theme_ipsum() +
  scale_fill_viridis_d( guide = "legend", limits = dfsalud_mental$salud_mental)

# 6.2 variable: desempenio_academico
# gráfico

ggplot(dfdesempenio_academico, aes(x = "", y = pct, fill = desempenio_academico)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + 
  labs(title = "Alcohol influye en la salud mental de estudiantes de Antropología UAH", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "desempenio_academico") +
  theme_ipsum() +
  scale_fill_viridis_d( guide = "legend", limits = dfdesempenio_academico$desempenio_academico)



## 7. TABLAS DE CONTINGENCIA ####

# 7.1 variable: alcohol_facilita_interacciones y genero

base_antropo %>%
  filter(genero != "Prefiero no responder") %>%
  select(alcohol_facilita_interacciones, genero) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

#GUARDARLA
ct_alinteracciongenero <- base_antropo %>% 
  filter(genero != "Prefiero no responder") %>%
  select(alcohol_facilita_interacciones, genero) %>% 
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

# 7.2 variable: salud_mental y anio_de_ingreso

base_antropo %>%
  select(salud_mental, anio_de_ingresor) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

#GUARDARLA
ct_saludmentalanio <- base_antropo %>% 
  select(salud_mental, anio_de_ingresor) %>% 
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

# 7.3 variable: desempenio_academico y edad

base_antropo %>%
  filter(desempenio_academico != "NA") %>%
  select(desempenio_academico, edadr) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

#GUARDARLA
ct_desempenioedad <- base_antropo %>% 
  filter(desempenio_academico != "NA") %>%
  select(desempenio_academico, edadr) %>% 
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100

## 8. GRÁFICOS DE BARRA PARA TABLAS DE CONTINGENCIA ####

colors <- c("#96ceb4", "#ff6f69", "#ffcc5c") 

# 8.1 variables: alcohol_facilita_interacciones y genero

ggplot(data = subset(base_antropo, genero != "Prefiero no responder"), aes(x = genero, fill = alcohol_facilita_interacciones)) +
  geom_bar(position = "fill") +
  ylab("Alcohol Interacciones") +
  xlab("Género")+
  theme_ipsum() +
  scale_fill_manual(values = colors)+
  labs (title = "Relación entre género y quienes creen que el alcohol facilita interacciones sociales", 
        caption = "Encuesta Estudiantes Antropología UAH", 
        fill= "Alcohol facilita relaciones sociales")

# 8.2 variables: salud_mental y anio_de_ingreso

ggplot(data = subset(base_antropo), aes(x = anio_de_ingresor, fill = salud_mental)) +
  geom_bar(position = "fill") +
  ylab("Afecta salud mental") +
  xlab("Año de ingreso")+
  theme_ipsum() +
  scale_fill_manual(values = colors)+
  labs (title = "Relación entre año de ingreso e influencia de alcohol en salud mental ", 
        caption = "Encuesta Estudiantes Antropología UAH", 
        fill= "Alcohol afecta salud mental")

#este gráfico hubiera quedado de mejor manera con las barras laterales, pero está bien.

#8.3 variables: desempenio_academico y edad

ggplot(data = subset(base_antropo, desempenio_academico != "NA"), aes(x = edadr, fill = desempenio_academico)) +
  geom_bar(position = "fill") +
  ylab("Afecta desempeño académico") +
  xlab("Edad")+
  theme_ipsum() +
  scale_fill_manual(values = colors)+
  labs (title = "Relación entre edad e influencia del alcohol en desempeño académico", 
        caption = "Encuesta Estudiantes Antropología UAH", 
        fill= "Alcohol afecta desempeño académico")



## 9. ÍNDICE ADITIVO ####

#Pregunta 1


base_antropo <- mutate(base_antropo, nobeber_limitante = car::recode(base_antropo$nobeber_limitante,
                                                                     "Creo que no beber alcohol NO LIMITA las posibilidades de relacionarse entre compañeras/os" = 1,
                                                                     "Creo que no beber alcohol SI LIMITA las posibilidades de relacionarse entre compañeras/os" = 0))


#Pregunta 2


base_antropo <- mutate(base_antropo, alcohol_fueraclases = car::recode(base_antropo$alcohol_fueraclases,
                                                                       'Nunca' = 0, 'A veces' = 1, 'Rara vez' = 1, 'Generalmente' = 2, 'Casi siempre' = 3, 'Siempre' = 5))



#Pregunta 3


base_antropo <- mutate(base_antropo, alcohol_compartir = recode(alcohol_compartir, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5)) 




#Pregunta 4

base_antropo <- mutate(base_antropo, presion_social = car::recode(base_antropo$presion_social,
                                                                  'Nada presionada/o' = 0, 'Poco presionada/o' = 1, 'Demasiado presionada/o/e, llegando a consumir alcohol por aquella presión' = 4, 'Muy presionada/o' = 5))



#Pregunta 5

base_antropo <- mutate(base_antropo,alcohol_facilita_interacciones=car::recode(base_antropo$alcohol_facilita_interacciones,
                                                                               "'Si'=5; 'No'=0"))


#Pregunta 6

base_antropo <- mutate(base_antropo,compañero_alcohol=car::recode(base_antropo$compañero_alcohol,
                                                                  "'Si'=2; 'No'=0"))                                                                


#Pregunta 7

base_antropo <- mutate(base_antropo, anio2022_consumo_mensual = car::recode(base_antropo$anio2022_consumo_mensual,
                                                                            "No bebí alcohol el último semestre después de clases con mis compañeras/os" = 0,
                                                                            "Una vez al mes" = 1,
                                                                            "Dos veces al mes" = 2,
                                                                            "Cuatro veces o más" = 3,
                                                                            "Tres veces al mes" = 5,
                                                                            "NA" = 0))



#Pregunta 8

base_antropo <- mutate(base_antropo,salud_mental=car::recode(base_antropo$salud_mental,
                                                             "'Si'=5; 'No'=0"))  


#Pregunta 9

base_antropo <- mutate(base_antropo,desempenio_academico=car::recode(base_antropo$desempenio_academico,
                                                                     "'Si'=5; 'No'=0")) 

#Pregunta 10

base_antropo <- mutate(base_antropo, erasmo = car::recode(erasmo, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5))



## 10. CRUCE DE ÍNDICE ADITIVO CON DOS VARIABLES ####


#Comentario final= excelente trabajo, sólo que en algunas variables las categorías se hubieran recodificado para que no sean
#tan largas, lo que dificulta la lectura en tablas y gráficos. También les recomiendo que usen el código coord_flip en sus gráficos
#para que sean más fáciles de leer sus resultados. Felicitaciones
                                 