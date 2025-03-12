
# Ayudantía 2
# TIDYVERSE: seleccionar, filtrar, mutar, agrupar y resumir

## Carga de paquetes tidyverse con pacman
# install.packages("pacman")
pacman::p_load(tidyverse)

# Carga del conjunto de datos
datos_anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

#Le estamos pidiendo a Rstudio que nos lea este conjunto de datos desde el link
#y lo guarde como datos_anime

## Ejecutar esta limpieza para evitar casos repetidos
datos_anime <- datos_anime %>%
  distinct(animeID, .keep_all = TRUE)

#¿Qué le estamos diciendo a Rstudio?
#guárdame en datos_anime, sobreescribe los datos de la data eliminando las filas duplicadas
#la función distinct() se utiliza para eliminar las filas duplicadas de un dataframe
#le indicamos a RStudio que tome el dataframe datos_anime, aplique la función distinct() sobre la columna animeID, 
#y mantenga todas las columnas de cada fila (keep_all = TRUE)

## 0. Observar las variables con names() 
names (datos_anime)

#vamos a ver la data
view(datos_anime)


# Select ------------------------------------------------------------------

# 1. Seleccionar las columnas name, genre y score. Guarda esta selección como base: columnas_seleccionadas
columnas_seleccionadas <- datos_anime %>%
  select(name, genre, score)
#seleccionamos 3 variables

#veamos como nos queda
view(columnas_seleccionadas) 

# 2. Seleccionar las columnas name hasta studio. Guarda esta selección como base: seleccion_rango
seleccion_rango <- datos_anime %>%
  select(name:studio)
#seleccionamos 9 variables 

# 3. Excluir las columnas related y background. Guarda esta selección como base: excluir_columnas
excluir_columnas <- datos_anime %>%
  select(-related, -background)
#excluimos las variables related y blackground, quedandonos con 26 de 28 variables totales
#lo podemos hacer tal como una operación matematica, restando las variables que queremos excluir

#si se fijan, las observaciones continuan siendo las mismas, misma cantidad
#porque seleccionamos variables, la cantidad de observaciones no se altera
#13.631 observaciones en las variables seleccionadas


# Filter ------------------------------------------------------------------

# 1. Filtrar para quedarse solo con el Studio Ghibli, primero observar cuáles son los estudios en la variable studio con unique(). 
#Guarda este filtro como base: ghibli
ghibli <- datos_anime %>%
  filter(studio == "Studio Ghibli")
#nos quedamos solo con los datos del Studio Ghibli
#disminuyen las observaciones, ¿por que?
#no sacamos ninguna variable, pero si el resto de datos de animes de otros studios 

# 2. Para el Studio Ghibli, quedarse solo con los géneros "Action", "Adventure", "Fantasy". 
#Guarda este filtro como base: ghibli_generos
ghibli_generos <- datos_anime %>%
  filter(studio == "Studio Ghibli" & 
           (genre %in% c("Action", "Adventure", "Fantasy")))

view(ghibli_generos) 

# 3. Filtrar animes que aún se están emitiendo. Guarda este filtro como base: emitiendose
# variable si se están o no emitiendo: status
#en estatus las que se están emitiendo son: Currently Airing

emitiendose <- datos_anime %>%
  filter(status == "Currently Airing")

#de los animes en general filtramos 
#solo nos quedan los que estan actualmente al aire 

#¿cuáles son los animes que están actualmente en emisión?
View(emitiendose) 


# Mutate ------------------------------------------------------------------
#mutate() se utiliza para agregar nuevas columnas o modificar las existentes
#crear nuevas variables utilizando los datos que deseemos

#En este caso vamos a...

# Añadir una columna de clasificación según la puntuación: 
# guardando base nueva como: etiqueta_puntuacion
# generando una variable nueva llamada: categoria_puntuacion
# recodificando con case_when, score cuando:
# es mayor o igual a 8 es "Alto"
# menor a 8 y mayor o igual a 6 es "Medio"
# el resto "Bajo"

unique(datos_anime$score)
etiqueta_puntuacion <- datos_anime %>%
  mutate(categoria_puntuacion = case_when(
    score >= 8 ~ "Alto",
    score >= 6 & score < 8 ~ "Medio",
    TRUE ~ "Bajo"
  ))

#unique() proporciona una lista de todas las puntuaciones únicas presentes en la columna "score"
#el signo $ asocia y llama la variable score de la data datos_anime

#case_when() significa "en el caso de que sea" o "cuando sea..." 
#ej: muta o modifica esta categoría cuando el puntaje sea >= 8, >= 6 & < 8, TRUE (el resto)
#permite definir condiciones y establecer sus correspondientes resultados

#En este caso se está creando una nueva columna llamada "categoria_puntuacion" 
#basada en los valores de la columna "score"
#con las siguientes condiciones:
#Si la puntuación es mayor o igual a 8, la categoría de puntuación será "Alto"
#Si la puntuación es mayor o igual a 6 y menor que 8, la categoría de puntuación será "Medio"
#Para todos los demás casos, la categoría de puntuación será "Bajo"

View(etiqueta_puntuacion)
#podremos ver la categoría de la puntuación según las categorías que acabamos de definir
#cambio va a quedar guardado en etiqueta_puntuacion

table(etiqueta_puntuacion$categoria_puntuacion)
#¿Cuántos animes tenemos con puntuación alta? ¿Qué puntuación tienen la mayoría de los animes? 
#son considerablemente menos los casos de animes con puntuación alta
#los animes con puntuación baja superan considerablemente en cantidad a los animes con puntuación alta, casi 8 veces su valor

#podemos ver nuevamente etiqueta_puntuación para sacar más conclusiones: ¿qué animes tienen puntuaciones altas?


# Group By y Summarise -----------------------------------------------------
#Agrupar por y resumir

# 1. Contar películas por estudio y encontrar el estudio con más películas. Guarda este conteo como base: conteo_peliculas_estudio
conteo_peliculas_estudio <- datos_anime %>%
  filter(type == "Movie") %>%
  group_by(studio) %>%
  summarise(cantidad_peliculas = n()) %>%
  arrange(desc(cantidad_peliculas))

#Filtramos los datos según la categoría Movie, es decir, dejamos solo las películas de anime
#Agrupamos las películas por studio de animación
#Summarise: resumimos la cantidad de películas
#la función n() cuenta el número de observaciones en cada grupo: cuántas películas tiene cada estudio
#Arrange: ordenamos los resultados en orden descendente según la cantidad de películas 
#Esto significa que los estudios con más películas estarán en la parte superior del resultado

#¿Qué estudio animó la mayor cantidad de películas?
View(conteo_peliculas_estudio)
