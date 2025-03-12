
# I. Elementos iniciales -----------------------------------------------------

#cargar las librerías a utilizar
pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, summarytools, ggthemes, hrbrthemes)

# webshot::install_phantomjs() # para exportar tablas de knitr

#abrir la base a trabajar
datos <- datos::encuesta

# II. Repaso Clase Anterior ---------------------------------------------------


# 1. Análisis univariado ####

#Frecuencia de religión

#01. forma (A)
#01. Realizo la tabla (mediante freq de summarytools:: )

datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() # la convierte en tabla


#haga una tabla de frecuencias con otra variable
#utilice names() para observarlas
names (____)

#utilice unique() para ver categorías a eliminar
unique(datos$_____)


datos %>% 
  mutate(____________ = as.character(____________)) %>%
  mutate(____________ = if_else(____________ %in% c("Sin respuesta", "No aplica"), as.character(NA), ____________)) %>% 
  freq(____________, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() # la convierte en tabla

# para qué sirve prop=TRUE
# para qué sirve order = "freq"
# para qué sirve report.nas = FALSE



#0.2. Guardo la tabla en un objeto: tb()
f_religion1 <- datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() #con esto lo convierto al formato data.frame, el que puede ser exportado a excel

class(f_religion1)


#práctica: guarde su tabla en un objeto


#03. Guardo la tabla en un excel (puedo pasarla a un ppt)
if(!dir.exists("tablas")) dir.create("tablas") 
#si dentro de mi carpeta de proyecto no existe el directorio tablas, crealo.

#la guardo en excel
write.xlsx(f_religion1, "tablas/f_religion1.xlsx") #guardalo en un excel

#guarde su tabla en un excel
write.xlsx(__________, "tablas/_________.xlsx")


#04. Guardo la tabla en imagen (formato Kable)
#kablExtra: mayores posibilidades para formatear
#install.packages("kableExtra")
library(kableExtra)


#04.1 Observo la tabla en formato imagen
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") 


#realice su tabla en formato imagen
datos %>% 
  mutate(____________ = as.character(____________)) %>%
  mutate(____________ = if_else(____________ %in% c("Sin respuesta", "No aplica"), as.character(NA), ____________)) %>% 
  freq(____________, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("____________", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") 


#los decimales están con punto, queremos transformarlo a coma. 
options(OutDec= ",") # me permite que los resultados estén con coma, en vez de punto

#04.2 Guardo la tabla en una imagen usar save_kable
datos %>% 
  mutate(religion = as.character(religion)) %>%
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)  %>% 
  tb() %>%
  kable(col.names = c("Religion", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tablas/f_religion1.png", zoom = 3)


#02. forma (B): tidyverse

#Construyo la tabla
f_religion2 <- datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion) %>% 
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 

#Guardo la tabla en excel
write.xlsx(f_religion2, "tablas/f_religion2.xlsx") #guardalo en un excel


#Veo tabla en Kable
#tabla de Kable
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en dos decimales
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) %>% #renombro dos categorías
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) %>% 
  kable(col.names = c("Religion", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formate con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15)

#guardo la tabla en formato kable
datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>% # contar las frecuencias de cada religión
  mutate(Porcentaje = n / sum(n) * 100) %>% # genero un porcentaje
  mutate(Porcentaje = round (Porcentaje, 2)) %>% #redondeo en dos decimales
  arrange(desc(Porcentaje)) %>%  #ordeno de mayor a menor
  rename(Frecuencia = n, Religion= religion) %>% #renombro dos categorías
  bind_rows(list(Religion = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) %>% 
  kable(col.names = c("Religion", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Relgión", 
        format = "html", digits = 2) %>%  #le doy formato con kable
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/f_religion2.png", zoom = 3)


#ejercicio para la casa. Haga lo mismo con tidyverse para otra variable. 



#2. Análisis bivariado ####
#0.1 Ordenar las variables a utilizar (raza y religión): 
datos$raza <- datos$raza %>% fct_relevel(c("Blanca", "Negra", "Otra")) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica

datos$religion <- datos$religion %>% fct_relevel(c("Protestante", 
                                                   "Católica", 
                                                   "Ninguna", 
                                                   "Cristiana",
                                                   "Judía", 
                                                   "Budismo", 
                                                   "Inter o no confesional", 
                                                   "Musulmana/Islam", 
                                                   "Cristiana ortodoxa", 
                                                   "Hinduismo", 
                                                   "Otra religión oriental", 
                                                   "Nativa americana", 
                                                   "Otra", 
                                                   "No sabe", 
                                                   "No aplica", 
                                                   "Sin respuesta"
)) %>% 
  fct_drop("No aplica")

#02. Realización de tabla
#poner proporciones en columna (más utilizada)
# en la X suele ir variable dependiente: religión
# en la y la independiente: raza
# la pregunta es: ¿cómo la dependiente modifica a la independiente?

#con ctable (de paquete summarytools::)
ctable( x = datos$religion, y = datos$raza, prop = "c", justify = "l")

#puede interpretarlo?

# realice un cruce con estado_civil
names(datos)
ctable( x = datos$religion, y = datos$_________, prop = "c", justify = "l")

#ven algo interesante?



#con tidyverse 
datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 


#03. guardo lo realizado
#03.1: A excel
c_religionxraza1 <- 
  datos %>%
  filter(religion != "Sin respuesta") %>%
  select(religion, raza) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

write.xlsx(c_religionxraza1, "tablas/c_religionxraza1.xlsx") #guardalo en un excel


#03.1: formato imagen
c_religionxraza1 %>% 
  kable(., caption="Tabla de contingencia para religión y raza") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) 

#para guardarlo
c_religionxraza1 %>% 
  kable(., caption="Tabla de contingencia para religión y raza") %>% 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% 
  save_kable(file = "tablas/c_religionxraza1", zoom = 2)





# III. Gráficar con {ggplot} ---------------------------------------------------

#lógica de cebolla anidando las capas con el signo +

ggplot(data= base_a_utilizar, aes(x = variable_1, y = variable_2, 
                                  color =variable_que distingue_color, 
                                  fill=variable_que_rellena,)) + #de donde salen los datos, cuales son las variables
geom_algo()+ #qué tipo de gráfico se va a realizar?
labs () + #cuales serán los títulos?
guides()+ #cuadrado donde se mostrarán una descripción de categorías
theme_() #se utilizará un tema global?

#0.0. Realizo la tabla a graficar

#tabla de religión
f_religion <- datos %>%
  filter (!(religion =="Sin respuesta")) %>% 
  count(religion) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Religion= religion)

#tabla de raza
f_raza <- datos %>%
  filter (!(raza =="Sin respuesta")) %>% 
  count(raza) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>%
  mutate(Porcentaje = round (Porcentaje, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, raza= raza)



#a qué queremos llegar?
ggplot(f_religion, aes(x = Porcentaje, y = fct_reorder(Religion, Porcentaje))) +
  geom_col(fill = "tomato") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Religión en Estados Unidos", 
       subtitle = "según datos de CENSO",
       caption = "fuente: Libreria Datos")+
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()




#1. Gráficos para Univariados####

# 1.1. geom_col()####
#gráfico de columnas
names(f_religion)

#0.1.A: primera versión del gráfico
ggplot(data= f_religion, aes(x = Religion, y = Frecuencia)) +
  geom_col()

#0.1.B:Ordenar gráfico

#Alinear nombres de las etiquetas
ggplot(f_religion, aes(x = Religion, y = Frecuencia)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#práctica: cambie el angulo de las etiquetas: por ejemplo: 45
ggplot(f_religion, aes(x = Religion, y = Frecuencia)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = _________, hjust = 1))



#Ordenar de menor a mayor frecuencia 
#y = fct_reorder(variable_del_eje, variable_por_la_que_Se_ordena)

ggplot(f_religion, aes(x = fct_reorder(Religion, Frecuencia), y = Frecuencia)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #ajusto etiquetas


#Ordenar de mayor a menor frecuencia
ggplot(f_religion, aes(x = fct_reorder(Religion, Frecuencia, desc), y = Frecuencia)) + #con desc
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#práctica: 
#haga un gráfico geom_col() de f_raza
#cambie el orden en raza: 
#ponga el texto en 45 grados
#observe las variables de f_raza con names()
names(_____)

ggplot(_______, aes(x = fct_reorder(_______, Frecuencia), y = Frecuencia)) +
  geom_col() +
  theme(axis.text.x = element_text(angle =_______, hjust = 1)) #ajusto etiquetas



#Cambiar la orientación de los ejes: cambio el que estaba en x a y.
ggplot(f_religion, aes(x = Frecuencia, y = fct_reorder(Religion, Frecuencia))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Utilizo coord_flip ()
ggplot(f_religion, aes(x = fct_reorder(Religion, Frecuencia), y = Frecuencia)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

#Cambio de etiquetas: via labs()
names(f_religion)
ggplot(f_religion, aes(x = Porcentaje, y = fct_reorder(Religion, Porcentaje))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs (title = "Religión en Estados Unidos", 
        subtitle = "según datos de CENSO",
        x = "%",
        y = "Religiones",
        caption = "fuente: Libreria Datos") 


#Elimino las etiquetas de X e y
names(f_religion)
ggplot(f_religion, aes(x = Porcentaje, y = fct_reorder(Religion, Porcentaje))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs (title = "Religión en Estados Unidos", 
        subtitle = "según datos de CENSO",
        caption = "fuente: Libreria Datos")+
  xlab("") + ylab("") # se fue lo que decía Religiones y el %


# Le pongo los porcentajes al final con geom_text()
ggplot(f_religion, aes(x = Porcentaje, y = fct_reorder(Religion, Porcentaje))) +
  geom_col() +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Religión en Estados Unidos", 
       subtitle = "según datos de CENSO",
       caption = "fuente: Libreria Datos")


#le agrego color "tomato"
ggplot(f_religion, aes(x = Porcentaje, y = fct_reorder(Religion, Porcentaje))) +
  geom_col(fill = "tomato") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Religión en Estados Unidos", 
       subtitle = "según datos de CENSO",
       caption = "fuente: Libreria Datos")+
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3)


#le agrego un tema, hago que esté en negrita y con nudge que las barras se separen
ggplot(f_religion, aes(x = Porcentaje, y = fct_reorder(Religion, Porcentaje))) +
  geom_col(fill = "tomato") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Religión en Estados Unidos", 
       subtitle = "según datos de CENSO",
       caption = "fuente: Libreria Datos")+
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()
  
unique(f_religion$Religion)



#1.2. geom_bar()####
#es una suerte de atajo, abrevia, no obliga a dar todos los detalles
#cuanta la frecuencia de una variable en particular directamente desde la base de datos
#no es necesario hacer una tabla con una variable y que indique la frecuencia

names(datos)

#observen voy directo a datos!

ggplot(datos, aes(x = religion)) +
  geom_bar()

#hagalo con raza
ggplot(datos, aes(x = ______)) +
  geom_bar()


#pero es más complejo ordenar y transformar
ggplot(datos, aes(x = fct_reorder(religion, religion, function(x) sum(datos$religion == x)), y = ..count..)) +
  geom_bar()


#gráfico de torta
f_religion$Religion <- factor(f_religion$Religion, levels = f_religion$Religion[order(f_religion$Porcentaje)])

ggplot(f_religion, aes(x = "", y = Porcentaje, fill = Religion)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Religión en Estados Unidos", 
       subtitle = "según datos de CENSO",
       caption = "fuente: Libreria Datos",
       fill = "Religión") +
  theme_ipsum() +
  scale_fill_discrete( guide = "legend", limits = f_religion$Religion)


#por default: geom_bar cuenta las frecuencias, esto sería geom_bar(stat = "count")
#stat = "identity": cada barra representa el porcentaje de la categoría.


#práctica para la casa: puede hacer un gráfico de torta de raza?


#II. Análisis bivariado ####
#00. gráfico raza por religión

#ordeno las variables raza y variables religión: 

datos$raza <- datos$raza %>% fct_relevel(c("Blanca", "Negra", "Otra")) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica

datos$religion <- datos$religion %>% fct_relevel(c("Protestante", 
                                                   "Católica", 
                                                   "Ninguna", 
                                                   "Cristiana",
                                                   "Judía", 
                                                   "Budismo", 
                                                   "Inter o no confesional", 
                                                   "Musulmana/Islam", 
                                                   "Cristiana ortodoxa", 
                                                   "Hinduismo", 
                                                   "Otra religión oriental", 
                                                   "Nativa americana", 
                                                   "Otra", 
                                                   "No sabe", 
                                                   "No aplica", 
                                                   "Sin respuesta"
)) %>% 
  fct_drop("No aplica")


#Forma 1: Integrar las proporciones al gráfico de barras
ggplot(data = datos, aes(x = raza, fill = religion)) +
  geom_bar(position = "fill") + ylab("Proporciones")+ # con "fill"
  theme_classic()


#Forma 1.1.: elegir colores en específico
# Vector de colores contrastantes
colors <- c("#440154", "#b2df8a", "#365A8C", "#277E8E", "#1FA088", "#44A96C", "#7FBC41", 
                     "#B3CC2A", "#FDE725", "#46337E", "#b07aa1", "#ff9da7", "#9c755f", 
                     "#bab0ac", "#5c5c5c") 
                     
# defino mi paleta de colores, deben ser al menos la cantidad de mis categorías. 

# los busqué con chatgpt: 
# dame 15 colores contrastantes similares a vidiris para hacer un gráfico de barras

ggplot(data = datos, aes(x = raza, fill = religion)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  theme_classic() +
  scale_fill_manual(values = colors) #poner los colores manualmente 



#páginas para ver códigos de los colores: 
#https://www.color-hex.com/
#https://www.w3schools.com/colors/colors_picker.asp


#Ejemplo 2
#raza y partido

#hago recodificación anterior

datos %>%
  mutate(partido = as.character(partido)) %>%
  mutate(partido = if_else(partido %in% c("Sin respuesta", "No aplica"), as.character(NA), partido)) %>%
  freq(partido, prop = TRUE, order = "freq", report.nas =  FALSE)

datos <- datos %>%
  mutate(partido_r = case_when(partido ==  "No fuertemente demócrata" ~ "Demócrata",
                               partido ==  "Fuertemente demócrata" ~ "Demócrata",
                               partido ==  "Ind, pro dem" ~ "Demócrata",
                               partido ==  "Fuertemente republicano" ~ "Republicano",
                               partido ==  "No fuertemente republicano" ~ "Republicano",
                               partido ==  "Ind, pro rep" ~ "Republicano",
                               partido ==  "Otro partido" ~ "Otro partido",
                               partido ==  "Independiente" ~ "Independiente", 
                               partido ==  "No sabe" ~ "No sabe"))

class(datos$partido_r)
datos$partido_r <- as.factor(datos$partido_r) #por qué se transforma a factor?
class(datos$partido_r)

datos$partido_r <- datos$partido_r %>% fct_relevel(c("Demócrata", "Republicano", 
                                                     "Independiente", "Otro partido", "No sabe"))

#Práctica realice el gráfico anterior considerando partido_r
#cambiele las etiquetas con:
#ylab ("Orientación Política"): para eje y
#labs(fill = "Orientación Política"): para para el recuerdo que indica los colores de fill

ggplot(data = datos, aes(x = ________, fill = ________)) +
  geom_bar(position = "fill") +
  ylab("Orientación Política") +
  theme_classic() +
  scale_fill_manual(values = colors) +
  labs(fill = "Orientación Política")

  
#¿Qué hacer con NA?
#para eliminar datos NA: na.omit(datos)
# x= raza
# y= partido_r

ggplot(data = na.omit(datos), aes(x = _____, fill = _______)) + #esto elimina datos perdidos
  geom_bar(position = "fill") +
  ylab("Orientación Política") +
  theme_classic() +
  scale_fill_manual(values = colors) +
  labs(fill = "Orientación Política")


#Forma dos: facet_wrap
#hace varios gráficos con una variable en particular 

ggplot(data = na.omit(datos), aes(x = partido_r)) +
  geom_bar() +
  ylab("Frecuencias") +
  theme_classic()+
  facet_wrap (~ raza) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = na.omit(datos), aes(x = partido_r, fill=partido_r)) + #fill=partido: cambia colores por partido
  geom_bar() +
  ylab("Frecuencias") +
  xlab("Orientación Política") +
  theme_classic()+
  facet_wrap (~ raza) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = "Orientación Política")



# Práctica ----------------------------------------------------------------
#01. recodifique con mutate en tres grupos de edad: 18 a 35, 36 a 64 y 65 y más (clase 5)
#pongale edad_r



#02. realice una distribución de frecuencias con freq (clase 6)


#03. realice una tabla de contingencia con porcentaje en columnas edadr y partido_r  (clase 6)
# ¿Cuál sería su variable independiente y su variable dependiente?


#04. realice una gráfico de barra par la variable edad_r


#05. realice un gráfico de barras de las distintas edades llenado (fill= partido_r)
#donde el fill sean las proporciones: position = "fill"





# Pruebas de significación estadística ------------------------------------
# El Chi cuadrado

datos$raza <- datos$raza %>% fct_relevel(c("Blanca", "Negra", "Otra")) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica

datos$religion <- datos$religion %>% fct_relevel(c("Protestante", 
                                                   "Católica", 
                                                   "Ninguna", 
                                                   "Cristiana",
                                                   "Judía", 
                                                   "Budismo", 
                                                   "Inter o no confesional", 
                                                   "Musulmana/Islam", 
                                                   "Cristiana ortodoxa", 
                                                   "Hinduismo", 
                                                   "Otra religión oriental", 
                                                   "Nativa americana", 
                                                   "Otra", 
                                                   "No sabe", 
                                                   "No aplica", 
                                                   "Sin respuesta"
)) %>% 
  fct_drop("No aplica")


# Chi cuadrado: Forma 1
#tengo que hacer tabla de contingencia sólo de frecuencias absolutas

datos %>% 
  select(religion, raza) %>% 
  table() %>% 
  chisq.test()

#Chi cuadrado: Forma 2
ctable( x = datos$religion, y = datos$raza, prop = "c", justify = "l", 
        chisq = T)

# Práctica: realice el test de Chi cuadrado para partido_r y raza
datos %>% 
  select(partido_r, raza) %>% 
  table() %>% 
  chisq.test()



