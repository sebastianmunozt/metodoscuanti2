# Ayudantía 07
#Repaso Prueba 2

# 00 Paquetes ------------------------------------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               summarytools,
               kableExtra,#Tablas elegantes
               webshot2,#exportar tablas
               chromote,
               hrbrthemes)#temas de gráficos


# 01 Cargar datos---------------------------------------------------------------
base <- read.xlsx("base_antropologia_limpia.xlsx")

# 02 explorar base--------------------------------------------------------------
glimpse(base)
str(base)
summary(base)
names(base)
view(base)

# 03 Renombrar variables
base <- base %>% dplyr::rename(actividad_ocio = to_04) #TO_04. ¿Cuáles de estas actividades prefiere realizar en su tiempo de ocio? 
names(base) #[37]                             

#Recodificar preguntas con RESPUESTAS MÚLTIPLES --------------------------------
# Separar las respuestas de actividad_ocio porque es de respuesta múltiple

# Veo las respuestas de la variable
table(base$actividad_ocio)

# Separar las respuestas que tienen coma (,) y guardarlas en el enviroment como strsplit
respuestas <- strsplit(base$actividad_ocio, ",") 

# Convertimos la lista en un vector
respuestas <- unlist(respuestas) # sacar de una lista y convertir en un vector

# Observo las respuestas en una tabla 
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# Elimino caracteres invisibles y espacios adicionales
respuestas <- gsub("[·]", "", respuestas) # eliminar el carácter "·"
respuestas <- trimws(respuestas) # eliminar espacios en blanco al inicio y al final

# Normalizar respuestas para eliminar diferencias menores 
respuestas <- tolower(respuestas) # convierto todo a minúsculas (opcional)
respuestas <- gsub("[[:space:]]+", " ", respuestas) # reemplazo múltiples espacios con un solo espacio

#Vuelvo a revisar como quedaron las respuestas
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()


#cambio categorías parecidas y reduzco categorías de respuestas con gsub

respuestas <- gsub("costura y diseño de ropa", "Artesanía y vestuario", respuestas)
respuestas <- gsub("tejer", "Artesanía y vestuario", respuestas)

respuestas <- gsub("ver videos en youtube de temas variados.", "Ver contenido audiovisual", respuestas)
respuestas <- gsub("ver programas de humor", "Ver contenido audiovisual", respuestas)
respuestas <- gsub("ver series y / o películas.", "Ver contenido audiovisual", respuestas)

respuestas <- gsub("escuchar podcast", "Socializar", respuestas)
respuestas <- gsub("salir con amigos / familia.", "Socializar", respuestas)

respuestas <- gsub("Música.", "Música", respuestas)
respuestas <- gsub("tocar música", "Música", respuestas)
respuestas <- gsub("escuchar música", "Música", respuestas)
respuestas <- gsub("bailar y/o cantar", "Música", respuestas)


#Reviso nuevamente
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#Guardo para graficar
actividad_ocio <- freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

#Gráficos ----------------------------------------------------------------------
##Gráficos de columna 
#opción 1: 
ggplot(actividad_ocio, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) + #fct_reorder reordena niveles de variables factor
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Actividades de Ocio estudiantes Antropología", 
       subtitle = "Encuesta Estudiantes 2024", #(opcional)
       caption = "fuente: Encuesta Estudiantes Antropología 2024")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #redondea a 1 decimal y saca el porcentaje, concatenando "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+ #hjust texto horizontal, taamaño 3, hacia la izquierda(-9) y en negrita(bold)
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

#opción 2
ggplot(actividad_ocio, aes(x =fct_reorder(respuestas, pct), y = pct, fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Actividades de Ocio estudiantes Antropología", 
       caption = "fuente: Encuesta Estudiantes Antropología 2024")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

#ejes:
#horizontal x
#vertical y

# Distribución de frecuencias --------------------------------------------------
#identidad de género recodificada _r
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

#Tabla formateada con kable
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"), #nombro las columnas
        caption = "Distribución de frecuencias de Género", #nombro la tabla
        digits = c(0, 0, 2, 2))%>%  #establezco decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") #la tabla no debe ocupar todo el ancho, fuente cambria

# Tablas de contingencia (cruce 2 variables) ####
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
summarytools::ctable( x = base$re_02, y = base$ultimo_colegio)

ctable( x = base$re_02, y = base$ultimo_colegio, prop = "c", justify = "l")

summarytools::ctable( x = base$re_02, y = base$ultimo_colegio)

# b) Filtramos NA y Ordenamos
# En este caso no será necesario pues no hay valores NA

# c) Recodificamos las categorías para dejar menos cantidad de categorías de respuesta

##Con re_02
unique(base$re_02)

base <- base %>%
  mutate(re_02 = case_when(re_02 ==  "Yoruba " ~ "Otra",
                           re_02 ==  "Grecorromana " ~ "Otra",
                           re_02 ==  "Grecorromana" ~ "Otra",
                           re_02 ==  "Ortodoxo" ~ "Ortodoxa",
                           re_02 ==  "Cristianismo Protestante (Evangélico, anglicano, etcétera)" ~ "Cristianismo protestante",
                           re_02 ==  "No tengo afiliaciones religiosas y tampoco me adscribo a ningún tipo de corriente de pensamiento de tipo clasificatoria" ~ "Ninguna",
                           re_02 ==  "Catolico" ~ "Catolicismo",
                           re_02 ==  "deísmo" ~ "Deísmo",
                           re_02 ==  "Agnóstico" ~ "Agnóstico/a", 
                           re_02 ==  "Ateo" ~ "Ninguna",
                           re_02 ==  "Ninguno " ~ "Ninguna",
                           re_02 ==  "Pagana" ~ "Otra", 
                           re_02 ==  "No tengo una afiliación religiosa, pero si me considero una persona espiritual (por ejemplo, creo en las energías)." ~ "Otra",
                           re_02 ==  "seria en creer en alguien superior un tipo de energía que se interpreta de diversas formas en la religión pero no es alguien al cual se le pueda poner un nombre" ~ "Otra",
                           TRUE ~ re_02))

unique(base$re_02)

#Ordenamos transformando a factor
base <- base %>%
  mutate(re_02 = as.factor(re_02)) 
base$re_02 <- base$re_02 %>% fct_relevel(c("Catolicismo", "Cristianismo protestante", 
                                                               "Agnóstico/a", "Ninguna", "Otra"))
unique(base$re_02)

# Crear la tabla cruzada con ctable
ctable(x = base$re_02,
      y = base$ultimo_colegio,
      prop = "c", #proporciones por columna
      justify = "l")


# Forma 2: Mediante prop.table() #### 

# Crear la tabla y la guardo en el enviroment
base  %>%
  select(Religión = re_01, `Clase Social` = clase_social) %>%  # Seleccionar y renombrar las variables
  droplevels() %>% # Eliminar las categorías que no se utilizan en la columna
  table(.) %>%# Hacer una tabla con todos los datos
  addmargins(.,2) %>% # Calcular los porcentajes por columnas
  prop.table(.) %>% # Agregar total de columnas
  round(4)*100  # Formatear y redondear con símbolo de porcentaje

# c) Interpretar y sacar conclusiones

# d) Guardar en un excel
write.xlsx(tabla_rel_clase, "tablas/tabla_rel_clase.xlsx") 

# e) Guardar en PNG
tabla_rel_clase %>% 
  kable(., caption="Tabla de contingencia para religión y clase social (% por columnas)") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/tabla_rel_clase.png", zoom = 2)


# 05 Gráficos categóricos univariados ----------------------------------------
#Gráfico de columnas
ggplot(actividad_ocio, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Actividades de Ocio estudiantes Antropología", 
       subtitle = "Encuesta Estudiantes 2024", #(opcional)
       caption = "fuente: Encuesta Estudiantes Antropología 2024")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

# Gráfico de tortas
ggplot(actividad_ocio, aes(x = "", y = pct, fill = respuestas)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Actividades de Ocio estudiantes Antropología", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "Religión") +
  theme_ipsum() +
  scale_fill_viridis_d(guide = "legend", limits = actividad_ocio$respuestas)

#por default: geom_bar cuenta las frecuencias, esto sería geom_bar(stat = "count")
#stat = "identity": cada barra representa el porcentaje de la categoría.

