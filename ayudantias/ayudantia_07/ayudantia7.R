# Ayudantía 7
# Análisis de datos categóricos 
# Visualización de datos cualitativos en ggplot

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
               gt,#Dar formato a las tablas
               dplyr,#Manipulación de datos (%>% , count, mutate)
               ggplot2,#Sistema de gráficos
               forcats,#Manipulación de factores (fct_reorder)
               scales,#Para mostrar porcentajes correctamente
               viridis,#Paletas de colores 
               hrbrthemes)#Permite aplicar temas (theme_ipsum)

# Importar base de datos
base <- read.xlsx("base_antropologia_limpia.xlsx")

# Explorar base
glimpse(base)
str(base)
summary(base)
names(base)

# ANÁLISIS DE DATOS CATEGÓRICOS (CUALITATIVOS): TABLAS Y GRÁFICOS ---------------------------

# Tabla de frecuencias para identidad de género (con freq)
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>% #transforma la tabla que entrega freq() en una tabla "normal" que R pueda leer y presentar bonita con kable().
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género",
        digits = c(0, 0, 2, 2),
        format = "html") %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Guardar tabla en HTML
#Crear objeto para guardar tabla
tabla_html <- base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas = FALSE) %>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Distribución de frecuencias de Género",
        digits = c(0, 0, 2, 2),
        format = "html") %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria")

#Creamos carpeta si no existe
if(!dir.exists("output")) dir.create("output")

#Guardar como archivo HTML
save_kable(tabla_html, "output/f_genero1.html")


# Tabla de frecuencias para identidad de género (con tidyverse)
base %>%
  filter(!is.na(identidad_genero_r)) %>% # filtramos para eliminar los casos con NA (sin respuesta)
  count(identidad_genero_r) %>% # contamos cuántas veces aparece cada categoría de identidad de género
  mutate(Porcentaje = n / sum(n) * 100) %>% # generamos el porcentaje respecto del total
  mutate(Porcentaje = round(Porcentaje, 2)) %>% # redondeamos los porcentajes a dos decimales
  arrange(desc(Porcentaje)) %>% # ordenamos los resultados de mayor a menor porcentaje
  rename(Frecuencia = n, Genero = identidad_genero_r) %>% # renombramos las columnas para mayor claridad
  bind_rows(list(Genero = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 
# agregamos una fila final que indica el total de respuestas, con 100% como porcentaje
# La función bind_rows nos permite agregar una fila de Total al final, lo que facilita la lectura.

# Guardar tabla en formato excell
#Crear objeto para guardar tabla
f_genero2 <- base %>%
filter(!is.na(identidad_genero_r)) %>% 
  count(identidad_genero_r) %>% 
  mutate(Porcentaje = n / sum(n) * 100) %>% 
  mutate(Porcentaje = round(Porcentaje, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  rename(Frecuencia = n, Genero = identidad_genero_r) %>% 
  bind_rows(list(Genero = "Total", Frecuencia = sum(.$Frecuencia), Porcentaje = 100)) 

#Exportamos la tabla a Excel en la carpeta "tablas"
write.xlsx(f_genero2, "output/f_genero2.xlsx")


## GRÁFICOS de tablas de frecuencias con ggplot ####
library(ggplot2) #sistema de gráficos
library(dplyr) #manipulación de datos (%>% , count, mutate)
library(forcats) #manipulación de factores (fct_reorder)
library(scales) #para mostrar porcentajes correctamente
library(viridis) #paletas de colores 
library(hrbrthemes) #permite aplicar temas (theme_ipsum)

## Gráfico de barras simples (frecuencia absoluta)
ggplot(base, aes(x = identidad_genero_r)) + #definimos la estética (aes) donde el eje x será identidad_genero_r
  geom_bar(fill = "steelblue") + #elaboramos un gráfico de barras (geom_bar) y rellenamos de color azul acero (fill)
  labs(title = "Distribución según identidad de género", #colocamos etiquetas del gráfico (título)
       x = "Identidad de género", #etiqueta del eje x 
       y = "Frecuencia") + #etiqueta del eje y
  theme_minimal() #tema (apariencia)

## Frecuencia absoluta simple con barras verticales mejoradas
ggplot(base, aes(x = identidad_genero_r)) +
  geom_bar(fill = "steelblue") +                          
  geom_text(stat = "count", aes(label = ..count..), #agrega los números de frecuencia sobre cada barra
            vjust = -0.5, fontface = "bold", size = 3.5) + #
  labs(title = "Distribución según identidad de género",
       subtitle = "Frecuencia absoluta por categoría",
       caption = "Fuente: Base de datos propia",
       x = "Identidad de género",
       y = "Frecuencia") +
  theme_ipsum() + #tema visual atractivo y limpio
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) #gira etiquetas para que no se sobrepongan y se lean mejor
#'stat = "count"' le dice a ggplot que calcule automáticamente el número de observaciones por categoría
#'..count..' es una variable interna de ggplot que representa esa cuenta automática
#vjust = -0.5 ajusta verticalmente el texto (negativo = más arriba de la barra)
#fontface = "bold" pone el texto en negrita
#size = 3.5) + ajusta el tamaño de letra del texto


## Gráfico de barras ordenadas (en caso de querer ordenar por frecuencia)
base %>%
  count(identidad_genero_r) %>% #cuenta la cantidad de casos por cada categoría de identidad_genero_r, creando columna n=frecuencias
  ggplot(aes(x = fct_reorder(identidad_genero_r, n), y = n)) + #fct_reorder() para ordenar las categorías según la frecuencia (de menor a mayor). Eje x recibe las categorías ordenadas, y el eje y las frecuencias
  geom_col(fill = "darkorange") + #gráfico de columnas: usa geom_col() en lugar de geom_bar() porque ya tenemos los conteos listos. Se rellenan las columnas de color naranjo (fill)
  coord_flip() + #se invierten los ejes para que las barras sean horizontales
  labs(title = "Frecuencia por identidad de género (ordenado)", #se establecen etiquetas
       x = "Identidad de género",
       y = "Frecuencia") +
  theme_minimal()

## Frecuencia absoluta ordenada y horizontal con etiquetas
base %>%
  count(identidad_genero_r) %>%
  ggplot(aes(x = fct_reorder(identidad_genero_r, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +                                           
  geom_text(aes(label = n),                                
            hjust = -0.1, fontface = "bold", size = 3.5) +
  labs(title = "Frecuencia por identidad de género (ordenado)",
       subtitle = "Ordenado de menor a mayor frecuencia",
       caption = "Fuente: Base de datos propia",
       x = "Identidad de género",
       y = "Frecuencia") +
  theme_ipsum() +
  theme(plot.title = element_text(face = "bold"))

## Gráfico de proporciones (porcentajes)
base %>%
  count(identidad_genero_r) %>% #cuenta la cantidad de casos por cada categoría de identidad_genero_r, creando columna n=frecuencias
  mutate(porc = n / sum(n)) %>% #crea una nueva columna llamada "porc" que contiene la proporción de cada categoría respecto del total 
  ggplot(aes(x = identidad_genero_r, y = porc)) + #eje x variable identidad_genero_r, eje y columna porc de proporciones
  geom_col(fill = "purple") + #se rellenan las columnas de color morado (fill)
  scale_y_continuous(labels = scales::percent_format()) + #transforma el eje y para que las proporciones se muestren como porcentajes (0.25 → 25%)
  labs(title = "Proporción según identidad de género", #se establecen etiquetas
       x = "Identidad de género",
       y = "Porcentaje") +
  theme_minimal()

## Gráfico de proporciones con etiquetas porcentuales
base %>%
  count(identidad_genero_r) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = porc, y = fct_reorder(identidad_genero_r, porc), fill = identidad_genero_r)) +
  geom_col() +
  geom_text(aes(label = paste0(round(porc * 100, 1), "%")), #muestra porcentaje con un decimal
            hjust = -0.1, size = 3.5, fontface = "bold") + #alinea el texto, le da tamaño y fuente de letra
  scale_x_continuous(labels = percent_format()) + #formatea el eje X (en este caso, de proporciones) para que se vea como porcentaje
  scale_fill_viridis_d(option = "C", guide = "none") + #agrega otros colores según paleta de color atractiva
  labs(title = "Proporción según identidad de género", #agrega título, subtítulo y etiquetas de ejes
       subtitle = "Cada barra representa el porcentaje del total",
       caption = "Fuente: Base de datos propia",
       x = "Porcentaje",
       y = "Identidad de género") +
  theme_ipsum()
#hjust = -0.1 mueve el texto un poco fuera de la barra hacia la izquierda (más separado)
#size = 3.5 define el tamaño del texto de las etiquetas en puntos (tamaño estándar de 3 a 4)
#fontface = "bold" cambia el estilo de fuente del texto y lo pone en "bold", es decir, negrita

# Tablas de contingencia 
library(knitr)
library(kableExtra)
library(webshot2)
library(gmodels)

# Tabla de contingencia para clase_social y re_01 (con prop.table)
base  %>%
  select(Religión = re_01, `Clase Social` = clase_social) %>%  # Seleccionar y renombrar las variables
  droplevels() %>% # Eliminar las categorías que no se utilizan en la columna
  table(.) %>%# Hacer una tabla con todos los datos
  addmargins(.,2) %>% # Calcular los porcentajes por columnas
  prop.table(.) %>% # Agregar total de columnas
  round(4)*100  # Formatear y redondear con símbolo de porcentaje

# Guardamos como objeto la tabla 
tabla_rel_clase <- base  %>%
  select(Religión = re_01, `Clase Social` = clase_social) %>%  
  droplevels() %>% 
  table(.) %>%
  addmargins(.,2) %>% 
  prop.table(.) %>% 
  round(4)*100  

## GRÁFICOS de tablas de contingencia con ggplot####
# Cargar librerías necesarias
library(tidyverse)
library(forcats)

# Análisis bivariado: Relación entre religión y clase social
# 1. Observo los levels de las variables (sus categorías de respuesta) 
levels(base$re_01) 
levels(base$clase_social)
#Esto es importante para colocar abajo las categorías correctas al aplicar fct_relevel (para ordenar categorías)

# 2. Ordeno las variables religión y clase social
base <- base %>%
  mutate(re_01 = fct_relevel(re_01, 
                             "Creencia firme", "Creencia moderada", "Dudas ocasionales", "Escéptico", "No creencias") %>% 
           fct_drop("No aplica"),
         clase_social = fct_relevel(clase_social, 
                                    "Clase social baja", "Clase social media - baja", "Clase social media", "Clase social media - alta") %>% 
           fct_drop("No aplica"))

# 3. Creo un vector de colores personalizado
colors <- c("#440154", "#365A8C", "#277E8E", "#1FA088", "#44A96C", "#7FBC41", 
            "#B3CC2A", "#FDE725", "#46337E", "#b07aa1", "#ff9da7", "#9c755f", 
            "#bab0ac", "#5c5c5c", "#b2df8a")

# 4. Creo un gráfico de barras apiladas por proporción: clase social por religión
ggplot(data = na.omit(base), aes(x = clase_social, fill = re_01)) +
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  xlab("Clase social") +
  theme_classic() +
  scale_fill_manual(values = colors) +
  labs(fill = "Religión")

### Otras forma de graficar 
# 1. Preparar la tabla para graficar
tabla_rel_clase_df <- as.data.frame(tabla_rel_clase)

# 2. Renombrar columnas para claridad
colnames(tabla_rel_clase_df) <- c("Religion", "Clase_Social", "Porcentaje")

# 3. Eliminar la fila de suma total (Religion == "Sum")
tabla_rel_clase_df <- tabla_rel_clase_df %>% 
  filter(Clase_Social != "Sum")

# 4. Crear gráficos
## Gráfico de barras verticales (porcentajes)
ggplot(tabla_rel_clase_df, aes(x = Clase_Social, y = Porcentaje, fill = Religion)) +
  geom_col(position = "dodge") +  # Barras lado a lado
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Distribución de creencias religiosas según clase social",
       x = "Clase social", y = "Porcentaje") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
#Cómo lo vemos? saturado

#Lo ajustamos
ggplot(tabla_rel_clase_df, aes(x = Clase_Social, y = Porcentaje, fill = Religion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, check_overlap = TRUE) +
  labs(title = "Distribución de creencias religiosas según clase social",
       x = "Clase social", y = "Porcentaje") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")
#Se ve mejor que antes, pero en mi opinión sigue siendo mucha información para este tipo de gráfico

## Gráfico de barras horizontales (porcentajes)
ggplot(tabla_rel_clase_df, aes(x = Porcentaje, y = Religion, fill = Clase_Social)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_dodge(width = 0.9),
            hjust = -0.1, size = 3) +
  labs(title = "Porcentaje de creencias religiosas por clase social",
       x = "Porcentaje", y = "Religión") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

## Gráfico de columnas apiladas
ggplot(tabla_rel_clase_df, aes(x = Clase_Social, y = Porcentaje, fill = Religion)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Distribución apilada de creencias religiosas por clase social",
       x = "Clase social", y = "Porcentaje") +
  theme_minimal()
#Se ve mucho mejor con este estilo de gráfico


# Pruebas de significación estadística ------------------------------------
# El Chi cuadrado
#Cruces: Creencias religiosas y Clase social

## Tabla de contingencia: porcentajes por columnas (Creencias religiosas y Clase social)
tabla_rel_clase <- base %>%
  select(Religión = re_01, Clase_Social = clase_social) %>%  
  droplevels() %>% 
  table() %>%
  addmargins(2) %>%
  prop.table(2) %>%
  round(4)*100

tabla_rel_clase

# Interpretación tabla 
#Con respecto a la clase social baja, las Creencia firme y moderada están igualadas (25% cada una), 
#lo que indica una religiosidad relativamente fuerte en este grupo. 
#Por otro lado, un 33.33% de personas de esta clase declara no tener creencias.
#Con respecto a la clase social media baja, hay mayor diversidad de posturas religiosas, con aumento del escepticismo.
#Un 25% de las personas de clase social media es esceptica, y un 22.92% presenta dudas ocasionales. 
#En cuanto a las personas de clase media, en ellas predomina una religiosidad más cuestionadora; 
#y decrecen significativamente las creencias firmes. Aumentan las dudas ocasionales (31.75%) y el escepticismo (23.81%),
#y bajan las creencias firmes (9.52%) y moderada (12.7%).

#La tabla muestra que a medida que aumenta la clase social, se observa una disminución sostenida de las creencias religiosas firmes o moderadas, 
#y un aumento de posturas más escépticas o no creyentes. Las personas de clases más altas tienden a mostrar una mayor distancia frente a la religión tradicional, 
#mientras que en la clase baja, la religiosidad firme o moderada aún conserva peso.

# TEST DE HIPÓTESIS: PRUEBA DEL CHI CUADRADO -------------------------------------------------------------
# ¿Qué nos dice la prueba de CHI CUADRADO?
#La prueba de Chi-cuadrado evalúa si existe una asociación significativa entre dos variables categóricas

# Hipótesis del test
# H₀ (hipótesis nula): No hay relación entre religión y clase social. Son independientes.

# H₁ (hipótesis alternativa): Sí hay relación entre religión y clase social.

# Prueba de Chi cuadrado
chisq.test(table(base$re_01, base$clase_social))

#El valor de p = 0.3858 es mucho mayor que 0.05, lo cual no permite rechazar la hipótesis nula.
#Por tanto, no se observa una asociación estadísticamente significativa entre la religión y la clase social en tu muestra.
#Es decir, las variables no se asocian significativamente.
#la distribución de creencias religiosas no varía significativamente entre los distintos niveles de clase social. 
#por lo que las diferencias observadas podrían deberse al azar.

