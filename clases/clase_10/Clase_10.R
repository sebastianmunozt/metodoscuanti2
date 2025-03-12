
# 00 Paquetes --------------------------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               summarytools,
               kableExtra,#Tablas elegantes
               webshot2,#exportar tablas
               chromote,
               hrbrthemes)#temas de gráficos


# 01 Cargar datos--------------------------------------------------------
base <- read.xlsx("base/base_antropologia_limpia.xlsx")

# 02 explorar base--------------------------------------------------------


str(base)
summary(base)
view(base)


# 03 Renombrar variables--------------------------------------------------------


base <- base %>% dplyr::rename(interes_participar_ma = ma_02,  
                               nivel_info_basura = ma_01, 
                               actividad_ocio = to_04,#TO_04. ¿Cuáles de estas actividades prefiere realizar en su tiempo de ocio? 
                               lugar_musica= cm_05)#CM_05.  ¿Dónde suele escuchar música habitualmente? Seleccione más de una alternativa si corresponde. 



# Separar las respuestas de actividad_ocio porque es de respuesta múltiple

# Cómo lo hago para ver rápidamente la variable que me interesa?
# (por 0.2 décimas) 

table(base$actividad_ocio)

respuestas <- strsplit(base$actividad_ocio, ",") # separar las respuestas que tienen coma (,)

# Convertimos la lista en un vector
respuestas <- unlist(respuestas) # sacar de una lista y convertir en un vector

# observo las respuestas
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

#Necesito cambiar categorías de respuesta pero
#No puedo Recodificar directamente en distintas categorías porque no es una variable!
#Utilizo gsub


#cambio categorías parecidas y reduzco categorías de respuestas


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



# Distribución de frecuencias --------------------------------------------
#identidad de género recodificada
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

#Tabla fancy
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género", 
        digits = c(0, 0, 2, 2))%>%  #decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") 


#Práctica: realice dos distribuciones de frecuencias



# Gráficos categóricos univariados ----------------------------------------
#opción 1
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

#opción 2
ggplot(actividad_ocio, aes(x =fct_reorder(respuestas, pct), y = pct, fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Actividades de Ocio estudiantes Antropología", 
       caption = "fuente: Encuesta Estudiantes Antropología 2024")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")


#Práctica: realice dos gráficos de barras, con dos variables distintas


# gráfico de tortas
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

#Práctica: realice dos gráficos de torta.


# Análisis bivariados  (entre una variable numérica y una categórica)--------------------------------------------------

names(libro)
names(base)

#Variable cuantitativa: nivel_info_basura
table(base$nivel_info_basura)# va del 1 al 10
str(base$nivel_info_basura)#es numérica


#¿El nivel de información de basura en las calles dependerá de la identidad de género?

ggplot(base, aes(x=identidad_genero_r, y=nivel_info_basura, fill=identidad_genero_r)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Interés en gestión de residuos según género", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "Interés en gestión de residuos",
       fill = "Identidad de género")+
  theme(legend.position="none")


#¿El nivel de información de basura en las calles dependerá de la clase social?

ggplot(base, aes(x=clase_social, y=interes_participar_ma, fill=clase_social)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Interés en gestión de residuos según clase social", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "Interés en gestión de residuos",
       fill = "Clase Social")+
  theme(legend.position="none")



#¿El interés en gestión de residuos dependerá del sector donde vivo?

ggplot(base, aes(x=comuna_r1, y=interes_participar_ma, fill=comuna_r1)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Interés en gestión de residuos según zona de la capital", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "Interés en gestión de residuos",
       fill = "Zona")+
  theme(legend.position="none")


#Prueba T de student-----------------------(sirve sólo con un variable dicotómica)-------------------------------------

#usaré género dicotómico (sólo para efectos de la explicación)

base<-base %>% mutate(genero_dicotomico= case_when(identidad_genero_r=="Hombre cisgenero" ~ "Hombre" ,
                                             identidad_genero_r=="Mujer cisgenero" ~ "Mujer"))

#H0: media de interés en gestión de residuos no depende del género (Media de hombres es igual a media de mujeres)
#H1: Media de interés en gestión de residuos si depende del género (la media es distinta)

t_test_result <-t.test(interes_participar_ma ~ genero_dicotomico, data=base)
 # 95 percent confidence interval:
 #   -1.3322584  0.5941631
 # sample estimates:
 #   mean in group Hombre  mean in group Mujer 
 # 5.619048             5.988095 
 
t_test_result$p.value #p-value es mayor a 0.05 por lo que no hay evidencia que permita rechazar la hipótesis nula.
#Si bien los datos reflejan una diferencia de medias del interés en participar en gesttión de residuos entre ambos géneros,
# No existe evidencia significativa que permita rechazar la hipótesis nula 

#y si quiero hacer la misma prueba de hipótesis con otra variable que tenga más de dos grupos?

#Prueba anova-------------------------------------------------------------------

# analysis of variance
 
class(base$identidad_genero_r) #variable es caracter
 
# La variable de grupo debe ser factor 
 
base$identidad_genero_r <- as.factor(base$identidad_genero_r)

# Análisis Anova
anova_result <- aov(interes_participar_ma ~ identidad_genero_r, data = base)

# Ver resultado
summary(anova_result)
 
#Ver probabilidad, fijarse que sea menor/mayor al valor de corte 
