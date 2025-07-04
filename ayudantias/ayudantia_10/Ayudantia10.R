# Ayudantía final
# Repaso general

# Cargar Paquetes ------------------------------------------------------------------------------------------------- 
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

# Importar base de datos -------------------------------------------------------
base <- read.xlsx("base_antropologia_limpia.xlsx") %>% 
  dplyr::select(4:ncol(.)) # seleccionar desde la columna 4 en adelante

# Observar si la base está cargada con ls()
ls(base)

# Explorar base ----------------------------------------------------------------
glimpse(base)
str(base)
summary(base)
names(base)

# ¿Cuántos casos y cuántas variables tiene la base?

# Limpieza de datos ------------------------------------------------------------
## `mutate` + `case_when`: Recodificar: cambiar nombres de categorías de variables

# Vamos a recodificar la variable comuna_actual, pues tiene muchas categorías y algunas de estas se repiten
# Vemos las categorías de la variable elegida con unique o table
unique(base$comuna)
table(base$comuna)

# Recodificaremos por zona de la comuna:
# Zona Norte: Quilicura, Huechuraba, Conchalí, Recoleta, Independencia, Renca
# Zona Oriente: Providencia, Las Condes, Vitacura, Lo Barnechea, Ñuñoa, La Reina, Peñalolén, Macul
# Zona Sur: La Pintana, San Joaquín, San Ramón, La Granja, El Bosque, La Cisterna, San Miguel, Lo Espejo, Pedro Aguirre Cerda
# Zona Poniente: Maipú, Pudahuel, Cerro Navia, Lo Prado, Quinta Normal, Estación Central
# Zona Centro: Santiago Centro
# Zona Periurbana: Puente Alto, San Bernardo, Paine, Buin, Calera de Tango, Talagante, Melipilla, Peñaflor, Padre Hurtado, Lampa, Til Til, Curacaví
# Fuera de Santiago: San Felipe, Llay Llay, Los Andes, Rancagua, La Serena

# Recodificamos con mutate case_when
# Alguien quisiera explicar lo que estamos haciendo en este código?
base <- base %>%
  mutate(comuna = case_when(
    comuna %in% c("quilicura", "huechuraba", "conchali", "recoleta", "renca") ~ "Zona Norte",
    comuna %in% c("providencia", "las_condes", "vitacura", "nunoa", "ñuñoa", "la_reina", "macul", "penalolen") ~ "Zona Oriente",
    comuna %in% c("la_pintana", "san_joaquin", "san_ramon", "la_granja", "el_bosque",
                  "la_cisterna", "san_miguel", "lo_espejo", "pedro_aguirre_cerda") ~ "Zona Sur",
    comuna %in% c("maipu", "pudahuel", "cerro_navia", "lo_prado", "quinta_normal", "estacion_central", "estación_central") ~ "Zona Poniente",
    comuna %in% c("santiago_centro") ~ "Zona Centro",
    comuna %in% c("puente_alto", "san_bernardo", "paine", "buin", "calera_de_tango", 
                  "talagante", "melipilla", "penaflor", "lampa", "til_til", "curacavi") ~ "Zona Periurbana",
    comuna %in% c("san_felipe", "llay_llay", "los_andes", "rancagua", "la_serena") ~ "Fuera de Santiago",
    TRUE ~ "Otra"
  ))

# Comprobamos la recodificación realizada
unique(base$comuna)
table(base$comuna)

# Realización de tablas y gráficos ---------------------------------------------

# Análisis univariado: distribución de frecuencias ####

# Tabla de frecuencia con %
# Creamos una tabla con porcentajes para la variable comuna_actual
t_comuna <- base %>%
  count(comuna) %>%  # cuenta casos de la variable comuna_actual
  mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%  # calcula y redondea %
  arrange(desc(Porcentaje)) %>% # ordena de mayor a menor (descendiente)
  rename(Frecuencia = n) # renombra los datos de n como frecuencia 

t_comuna

# Gráficos
# Gráfico de barras
ggplot(t_comuna, aes(x = comuna, y = Porcentaje )) +
  geom_col(fill = "Pink") +
  theme_minimal() +
  labs(
    x     = "Comunas",
    y     = "%",
    title = "Tabla sobre zona comunal del estudiante" ,
    caption = "Elaboracion propia 2025") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) +
      theme_minimal())

# Gráfico de torta
ggplot(t_comuna, aes(x =comuna, y = Porcentaje, fill = comuna)) +
  geom_col(width = 55) + # genera los sectores de la torta
  coord_polar(theta = "y") + # convierte en gráfico de torta
  geom_text(aes(label = paste0(Porcentaje, "%")), # coloca etiquetas de porcentaje
            position = position_stack(vjust = 0.5)) +
  theme_void() + # elimina ejes y fondo
  labs(
    fill    = "Comunas", # etiqueta genérica de leyenda
    title   = "Tabla sobre zona comunal del estudiante", # título genérico
    caption = "Elaboracion propia 2025"  # pie de gráfico genérico
    )

ggplot(t_comuna, aes(x = "", y = Porcentaje, fill = reorder(comuna, -Porcentaje))) +
  geom_col(width = 1, color = "white") +  # sector blanco entre segmentos
  coord_polar(theta = "y") +  # gráfico circular
  geom_text(aes(label = paste0(Porcentaje, "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +  # texto centrado y legible
  scale_fill_brewer(palette = "Set3") +  # colores suaves y diferenciados
  theme_void() +  # elimina ejes, líneas y fondo
  labs(
    fill    = "Zona Comunal",
    title   = "Distribución por zona comunal del estudiante",
    caption = "Elaboración propia, 2025") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(face = "bold"))

# Análisis bivariado: Cruce entre variables cualitativas ####
# Variables 

base %>%
  filter(!is.na(ea_06_nivel_estres_ultimo_semestre_r), !is.na(comuna)) %>%
  select(comuna, ea_06_nivel_estres_ultimo_semestre_r) %>%  # primera variable = filas, segunda = columnas
  droplevels() %>%
  table(.) %>%
  addmargins(., margin = 2) %>% # suma márgenes por columna (edad_r)
  prop.table(., margin = 2) %>% # proporciones dentro de cada edad
  round(4) * 100

# Interpretamos: 
# Zona Periurbana tiene el porcentaje más alto de estudiantes con estrés alto (29%) 
# y ningún estudiante con estrés bajo (0%)
# Zona Oriente tiene un 18% de estudiantes con estrés alto, pero también tiene el mayor 
# porcentaje con estrés bajo (50%)
# Zona Centro y Zona Poniente tienen porcentajes considerables de estrés bajo (25%), 
# lo que puede indicar menor estrés en esas zonas 
# Fuera de Santiago y Zona Norte presentan porcentajes bajos de estrés alto y moderado, 
# y ningún estrés bajo reportado, aunque también son las zonas con menor representación 
# en la muestra

# Pruebas de hipótsis - variables cualitativas ---------------------------------
library(dplyr) 
library(janitor) 

# Tabla de frecuencias ABSOLUTAS
# tabla de frecuencias absolutas para 'comuna' y 'ea_06_nivel_estres_ultimo_semestre_r'

tabla_freq <- base %>%
  filter(!is.na(comuna), !is.na(ea_06_nivel_estres_ultimo_semestre_r)) %>%
  select(comuna, ea_06_nivel_estres_ultimo_semestre_r) %>%
  droplevels() %>% # Elimina niveles de factores que no están presentes después de posibles filtros
  table() %>%
  addmargins(., margin = 2) 

print(tabla_freq)

# Tabla de frecuencias ESPERADAS para observar si hay casillas con menos de 5

# Eliminar la columna de totales temporalmente para el cálculo de expected si addmargins se aplicó al objeto antes del chisq.test
# Si la tabla se construye directamente con table() y luego se pasa a chisq.test, este maneja la tabla correctamente.
# Para el cálculo de esperados, chisq.test usa la tabla sin las sumas.
expected_freq <- chisq.test(tabla_freq[,-ncol(tabla_freq)])$expected
print(expected_freq)

# Las frecuencias esperadas son bajas
# hay múltiples celdas con frecuencias esperadas muy bajas
# Un porcentaje muy alto de celdas tienen frecuencias esperadas menores a 5

# ¿Considerando este dato, debería realizar un chi-2, un test de fisher o un chi-2 (montecarlo)?

# Si hay más del 20% de las celdas con frecuencias esperadas menores a 5, o cualquier celda con frecuencia esperada menor a 1,
# se recomienda no usar la prueba de chi-cuadrado estándar sin corrección.
# En esos casos, si la tabla es 2x2, se prefiere la Prueba Exacta de Fisher.
# Si la tabla es mayor a 2x2 y las condiciones de chi-cuadrado no se cumplen,
# se puede optar por la prueba de chi-cuadrado con simulación de Monte Carlo.

# Calcular el porcentaje de celdas con frecuencias esperadas < 5
num_cells_lt_5 <- sum(expected_freq < 5) #15 celdas de la tabla de frecuencias esperadas tienen un valor menor a 5
total_cells <- length(expected_freq) #el 62.5% de las celdas de la tabla de frecuencias esperadas tienen un valor menor a 5
percentage_lt_5 <- (num_cells_lt_5 / total_cells) * 100 #la taabla tiene 25 celdas en total

# Comprobamos que hay muchas frecuencias esperadas muy bajas
#  Dado que el 62.5% de tus celdas tienen frecuencias esperadas menores a 5 
# la prueba de Chi-cuadrado con simulación de Monte Carlo es la elección correcta

# Realizamos chi-cuadrado con simulación de Monte Carlo:

# Para hacer test chi-2 (con la modificación de montecarlo)

# Paso 1: Eliminar la columna y fila de totales generadas por addmargins()
# chisq.test requiere la tabla de contingencia pura, sin los totales.
# Si la tabla_freq tiene los totales en la última fila y última columna,
# los eliminamos de esta manera:
tabla_sin_totales <- tabla_freq[-nrow(tabla_freq), -ncol(tabla_freq)]

# Paso 2: Aplica la prueba chi-cuadrado con simulación de Monte Carlo
# con 10.000 repeticiones (B = 10000).
# 'simulate.p.value = TRUE' activa la simulación de Monte Carlo.
# 'B' especifica el número de replicaciones para la simulación.
chisq_montecarlo_res <- chisq.test(tabla_sin_totales, simulate.p.value = TRUE, B = 10000)

# Paso 3: Muestra los resultados de la prueba de Chi-cuadrado Monte Carlo
print(chisq_montecarlo_res)

# Paso 4: Interpretación de los resultados (esto es un esqueleto, debes completarlo con los valores obtenidos)
# HAGA SU INTERPRETACIÓN:
# ¿Se acepta o rechaza hipótesis nula?
# ¿Cómo se interpreta lo anterior? Mirando el valor p y el nivel de significancia

# La hipótesis nula (H0) es que no hay asociación entre 'comuna' y 'ea_06_nivel_estres_ultimo_semestre_r'
# (es decir, son independientes).
# La hipótesis alternativa (H1) es que sí hay una asociación (no son independientes).

# Dado que 0.90080.05, el p-value es mayor que el nivel de significancia (0,05), NO se rechaza la hipótesis nula.
# no hay evidencia estadística significativa para concluir que existe una asociación entre la comuna de residencia 
# y el nivel de estrés reportado en el último semestre

