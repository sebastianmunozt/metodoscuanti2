# Ayudantía 6
# Análisis de datos categóricos
# Análisis univariado: Distribución de frecuencia: Tablas de frecuencia 
# Análisis bivariado: Cruce de variables: Tablas de contingencia

# Cargar Paquetes 
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#Leer archivos xlsx
               readxl,#Leer archivos xl      
               writexl,#Guardar tablas formato excel
               DataExplorer,#Exploración rápida
               summarytools,#Contiene la función freq() que muestra las frecuencias absolutas y relativas de una variable
               kableExtra,#Tablas elegantes
               webshot2,#Exportar tablas
               knitr,#Dar formato a las tablas
               gt)#Dar formato a las tablas


# Importar base de datos
base <- read.xlsx("base_antropologia_limpia.xlsx")

# Explorar base
glimpse(base)
str(base)
summary(base)
names(base)

# Distribución de frecuencias---------------------------------------------------
# Quiero saber ¿cuántas personas se identifican con cada género?
# variable: identidad de género recodificada _r 

# Tabla simple con table()
table(base$identidad_genero_r)

# Distintas formas de hacer tablas de frecuencia:

## Forma 1: Con freq() de summarytools ####
freq(base$identidad_genero_r) 
# La tabla está algo desordenada: incluye frecuencias válidas y acumuladas por default, y aparecen los NA

# Eliminamos valores NA y ordenamos por frecuencia
freq(base$identidad_genero_r, prop = TRUE, order = "freq", report.nas = FALSE) 

# Si queremos además visualizar una tabla más elegante y formateada 
# Tabla formateada con kable (para formato HTML)
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"), #nombro las columnas
        caption = "Distribución de frecuencias de Género", #nombro la tabla
        digits = c(0, 0, 2, 2))%>%  #establezco decimales por columna
  kable_classic(full_width = F, html_font = "Cambria") #la tabla no debe ocupar todo el ancho, fuente cambria
# tb() hac que se visualice

# PARA GUARDAR LA TABLA
### En formato HTML
# Cargar librerías necesarias
library(knitr)
library(kableExtra)

# Crear tabla
tabla_html <- base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género",
        digits = c(0, 0, 2, 2),
        format = "html") %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Creamos carpeta si no existe
if(!dir.exists("tablas")) dir.create("tablas")

# Guardar como archivo HTML
save_kable(tabla_html, "tablas/f_genero1.html")

### En formato Excell 
# Guardamos la tabla sin utilizar Kable
# Creamos la tabla como data.frame
f_genero1_df <- base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  rename(Género = 1, Frecuencia = 2, Porcentaje = 3, Porcentaje_acumulado = 4) %>%
  mutate(Porcentaje = round(Porcentaje, 2),
         Porcentaje_acumulado = round(Porcentaje_acumulado, 2))

# Guardamos como archivo Excel correctamente
write.xlsx(f_genero1_df, "tablas/f_genero1.xlsx")

# Si quieres mostrarla bonita en pantalla ahora sí usas kable:
f_genero1_df %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género",
        digits = c(0, 0, 2, 2)) %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Forma 2: con tidyverse ####
#Con este código vamos a generar una tabla de frecuencias y porcentajes de la variable identidad de género.
#Este proceso incluye: filtrar valores perdidos, contar frecuencias, calcular porcentajes,
#redondear decimales, ordenar los resultados, renombrar columnas y agregar una fila de Total.

f_genero2 <- base %>%
  filter(!is.na(identidad_genero_r)) %>% # filtramos para eliminar los casos con NA (sin respuesta)
  count(identidad_genero_r) %>% # contamos cuántas veces aparece cada categoría de identidad de género
  mutate(Porcentaje = n / sum(n) * 100) %>% # generamos el porcentaje respecto del total
  mutate(Porcentaje = round(Porcentaje, 2)) %>% # redondeamos los porcentajes a dos decimales
  arrange(desc(Porcentaje)) %>% # ordenamos los resultados de mayor a menor porcentaje
  rename(Frecuencia = n, Genero = identidad_genero_r) %>% # renombramos las columnas para mayor claridad
  bind_rows(list(Genero = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 
# agregamos una fila final que indica el total de respuestas, con 100% como porcentaje
# La función bind_rows nos permite agregar una fila de Total al final, lo que facilita la lectura.

#Exportamos la tabla a Excel en la carpeta "tablas"
write.xlsx(f_genero2, "tablas/f_genero2.xlsx")

#Exportamos la tabla a HTML en la carpeta "tablas"
library(kableExtra)

# Crear la tabla HTML
tabla_html <- f_genero2 %>%
  kable(format = "html",
        col.names = c("Género", "Frecuencia", "Porcentaje"),
        caption = "Distribución de identidades de género",
        digits = c(0, 0, 2)) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Guardar como archivo HTML
save_kable(tabla_html, file = "tablas/f_genero2.html")


# Tablas de contingencia (cruce 2 variables)------------------------------------
# Cargar las bibliotecas necesarias
library(knitr)
library(kableExtra)
library(webshot2)
library(gmodels)

#Forma 1: Mediante ctable() ####

# a) Elegir nuestras variables para el cruce y hacer la tabla con estas
# Si nos preguntamos 
# ¿Qué religiones son más frecuentes en ciertos colegios?
# Esta tabla de contingencia presenta la distribución de las afiliaciones religiosas según el último colegio 
# cómo varía o se mantiene la proporción de religiones en los diferentes colegios.
# variables: ultimo_colegio y re_02: afiliación religiosa

# Tabla de contingencia simple
summarytools::ctable( x = base$re_02, y = base$ultimo_colegio)
# Nos entrega las frecuencias absolutas (conteo de casos por combinación), las proporciones marginales (por fila y columna),
# y los totales por fila y por columna 
# Podemos ver que queda una tabla muy desordenada y compleja de entender

# Obtener proporciones por columna:
ctable( x = base$re_02, y = base$ultimo_colegio, prop = "c", justify = "l")
# Indica que las proporciones deben calcularse por columna ("column-wise proportions")
# Esto es útil cuando quieres analizar cómo se distribuyen las respuestas dentro de cada columna 
# por ejemplo, qué proporción de estudiantes de colegio particular pertenece a cada religión.
# justify = "l" alinea el texto a la izquierda dentro de la tabla ("l" de left).
# Nos damos cuenta de que la tabla, si bien es entendible, tiene mucha información, muchas categorías, 
# por lo que es compleja de entender

# b) Recodificamos las categorías para dejar menos cantidad de categorías de respuesta con MUTATE
## Para la variable re_02
#vemos categorías de respuesta de re_02
unique(base$re_02)

#recodificamos categorías con mutate
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

#verificamos 
unique(base$re_02)

# c) Filtramos NA y Ordenamos
# En este caso no será necesario pues no hay valores NA

# PARA ORDENAR CATEGORÍAS
#Observamos el tipo de variable
class(base$re_02)
# es una variable character, pero debemos transformarla a variable factor para poder ordenarla

#Transformamos a factor
base <- base %>%
  mutate(re_02 = as.factor(re_02)) 

#Ordenamos categorías 
base$re_02 <- base$re_02 %>% fct_relevel(c("Catolicismo", "Cristianismo protestante", 
                                           "Agnóstico/a", "Ninguna", "Otra"))
#verificamos
unique(base$re_02)
# fíjense en el levels

# d) Crear la tabla cruzada con ctable
ctable(x = base$re_02,
       y = base$ultimo_colegio,
       prop = "c", #proporciones por columna
       justify = "l") #texto hacia la izquierda

# e) Interpretar
#El grupo "Otra" es el más común en todos los tipos de colegio, especialmente en el subvencionado (45%).
#Los colegios particulares tienen más personas sin religión ("Ninguna"): 28,6%.
#Los colegios públicos tienen un poco más de agnósticos (25,6%).
#Las religiones institucionales (católica y protestante) no superan juntas el 12% en ningún tipo de colegio.


# Forma 2: Mediante prop.table() #### 
# a) Elegir nuestras variables para el cruce y hacer la tabla con estas
# Si nos preguntamos ¿Qué religiones predominan en ciertos estratos de clase?
# la tabla muestra cómo se distribuyen las afiliaciones religiosas según la clase social
# variables: clase_social y re_01

# b) Crear la tabla y guardarla en el enviroment
tabla_rel_clase <- base  %>%
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

# III. Formatear tablas --------------------------------------------------------
# Tablas de frecuencia:
# Exportar formateado con kable ####
# Formato para Viewer con kableExtra y kable
base %>%
  mutate(identidad_genero_r = as.character(identidad_genero_r)) %>%
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas = FALSE) %>%
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género",
        format = "html", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "tablas/f_genero1.png", zoom = 3)

# Alternativa con Tidyverse
f_genero2 %>%
  kable(col.names = c("Género", "Frecuencia", "Porcentaje"),
        caption = "Distribución de frecuencias de Género",
        format = "html", digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>%
  save_kable(file = "tablas/f_genero2.png", zoom = 3)

# 2. Tablas de contingencia:
# Tabla de contingencia entre afiliación religiosa y tipo de colegio

# Creamos tabla de contingencia con proporciones por columnas (con prop.table en este caso)
c_religionxcolegio1 <- base %>%
  filter(!is.na(re_02) & !is.na(ultimo_colegio)) %>%
  select(re_02, ultimo_colegio) %>%
  droplevels() %>%
  table() %>%
  addmargins(2) %>%
  prop.table(2) %>%
  round(4) * 100

# Guardamos en Excel
write.xlsx(c_religionxcolegio1, "tablas/c_religionxcolegio1.xlsx")

# Formateamos y visualizamos con kable
c_religionxcolegio1 %>%
  kable(caption = "Tabla de contingencia: Afiliación religiosa y Tipo de Colegio", #asigna título
        format = "html") %>% #asigma formato html 
  kable_classic(full_width = F, html_font = "Cambria", font_size = 15) %>% #ancho tabla, fuente Cambria y tamaño de fuente
  save_kable(file = "tablas/c_religionxcolegio1.png", zoom = 2) #zoom = 2: aumenta la resolución de la imagen a 2

#full_width = FALSE: la tabla no ocupará todo el ancho del documento, se ajustará a su contenido.
#html_font = = "Cambria": usa la fuente Cambria.
#font_size = 15: asigna tamaño de fuente 15, para mejorar la legibilidad.
