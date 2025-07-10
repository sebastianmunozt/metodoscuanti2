
# Cápsula Trabajos finales
# 1) Recodificación de variables de preguntas con respuestas múltiples
# 2) Construcción de un índice

# 0. Paquetes ------------------------------------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               summarytools,
               kableExtra,#Tablas elegantes
               webshot2,#exportar tablas
               chromote,
               hrbrthemes)#temas de gráficos


# I. Cargar datos---------------------------------------------------------------
base <- read.xlsx("base_antropologia_limpia.xlsx")

# II. Explorar base
glimpse(base)
names(base)

# III. Limpieza y transformación de datos: Renombrar variables

# Renombramos la variable to_04 en actividad_ocio con rename 
base <- base %>% dplyr::rename(actividad_ocio = to_04) #TO_04. ¿Cuáles de estas actividades prefiere realizar en su tiempo de ocio? 
names(base) #[37]      

# Recodificar preguntas con RESPUESTAS MÚLTIPLES -------------------------------
# Separar las respuestas de actividad_ocio porque es de respuesta múltiple

# 1. Vemos las respuestas de la variable
table(base$actividad_ocio)

# 2. Separamos las respuestas que tienen coma (,) y guardarlas en el enviroment como "respuestas"
respuestas <- strsplit(base$actividad_ocio, ",") 

# 3. Obtenemos las respuestas en una lista y convertimos en un vector
respuestas <- unlist(respuestas) 

# 4. Observamos las respuestas en una tabla (con freq)
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# 5. Eliminamos caracteres invisibles y espacios adicionales
respuestas <- gsub("[.]", "", respuestas) # eliminar el carácter "·"
respuestas <- trimws(respuestas) # eliminar espacios en blanco al inicio y al final

# Normalizamos respuestas para eliminar diferencias menores 
respuestas <- tolower(respuestas) # convierto todo a minúsculas (opcional)
respuestas <- gsub("[[:space:]]+", " ", respuestas) # reemplazo múltiples espacios con un solo espacio

# Vuelvo a revisar como quedaron las respuestas
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# 6. Cambiamos categorías parecidas y reducimos categorías de respuestas con gsub

# Nuevas categorías:
  # Arte y vestuario
  # Ver contenido audiovisual
  # Socializar
  # Música
  # Deporte
  # Juegos
  # Descanso

respuestas <- gsub("costura y diseño de ropa", "Arte y vestuario", respuestas)
respuestas <- gsub("tejer", "Arte y vestuario", respuestas)
respuestas <- gsub("dibujar / pintar", "Arte y vestuario", respuestas)
respuestas <- gsub("leer y/o escribir", "Arte y vestuario", respuestas)

respuestas <- gsub("ver videos en youtube de temas variados", "Ver contenido audiovisual", respuestas)
respuestas <- gsub("ver programas de humor", "Ver contenido audiovisual", respuestas)
respuestas <- gsub("ver series y / o películas", "Ver contenido audiovisual", respuestas)

respuestas <- gsub("escuchar podcast", "Socializar", respuestas)
respuestas <- gsub("salir con amigos / familia", "Socializar", respuestas)

respuestas <- gsub("Música", "Música", respuestas)
respuestas <- gsub("tocar música", "Música", respuestas)
respuestas <- gsub("escuchar música", "Música", respuestas)
respuestas <- gsub("bailar y/o cantar", "Música", respuestas)

respuestas <- gsub("practicar deporte / ejercitarse", "Deporte", respuestas)

respuestas <- gsub("jugar videojuegos", "Juegos", respuestas)

respuestas <- gsub("dormir", "Descanso", respuestas)


# Reviso nuevamente
freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()

# Guardo para graficar
actividad_ocio <- freq(respuestas, prop=TRUE, order = "freq", report.nas = FALSE) %>% 
  tb()


# Construcción de índices con MUTATE--------------------------------------------
# Indices se construyen para variables numéricas
names(base)

# INDICE DE DESEMPEÑO ACADÉMICO PERCIBIDO
# a mayor horas de estudio semanales, notas más altas y satisfacción con el rendimiento, 
# mayor desempeño académico percibido.
# a más horas de estudio, mejores notas y mejor satisfacción = mejor desempeño.

# Seleccionamos variables de interés
unique(base$horas_estudio_semana)
unique(base$notas_ultimo_semestre_intervalo)
unique(base$ea_05_satisfaccion_rendimiento_academico_r)

# PASO 1: RECODIFICAMOS VARIABLES
# Recodificamos las categorías de respuesta para hacerlas numéricas
datos_recodificados <- base %>%
  mutate(
    # Más horas de estudio = mayor puntuación
    horas_estudio_num = case_when(
      horas_estudio_semana == "1 o 2 horas al día" ~ 1,
      horas_estudio_semana == "3 o 4 horas al día" ~ 2,
      horas_estudio_semana == "5 o 6 horas por días" ~ 3,
      horas_estudio_semana == "Más de 7 horas por día" ~ 4,
      TRUE ~ NA_real_
    ),
    # Más alta la nota = mayor puntuación
    notas_ultimo_semestre_num = case_when(
      notas_ultimo_semestre_intervalo == "4.0 a 4.9" ~ 1,
      notas_ultimo_semestre_intervalo == "5.0 a 5.9" ~ 2,
      notas_ultimo_semestre_intervalo == "6.0 a 7.0" ~ 3,
      TRUE ~ NA_real_
    ),
    # Satisfacción con rendimiento (1 insatisfecho, 2 satisfecho)
    satisfaccion_rendimiento_num = case_when(
      ea_05_satisfaccion_rendimiento_academico_r == "Insatisfecho" ~ 1,
      ea_05_satisfaccion_rendimiento_academico_r == "Satisfecho" ~ 2,
      TRUE ~ NA_real_
    )
  )

# Vemos cómo quedaron las categorías de las nuevas variables recodificadas
unique(datos_recodificados$horas_estudio_num)
unique(datos_recodificados$notas_ultimo_semestre_num)
unique(datos_recodificados$satisfaccion_rendimiento_num)

# Verificamos que las variables sean numericas
class(datos_recodificados$horas_estudio_num)
class(datos_recodificados$notas_ultimo_semestre_num)
class(datos_recodificados$satisfaccion_rendimiento_num)

# PASO 2: IMPUTAR VALORES FALTANTES
 # Para cada fila se calcula el promedio de las respuestas disponibles y se reemplazan los valores faltantes (NA) por este promedio
datos_imputados <- datos_recodificados %>%
  rowwise() %>%
  mutate(
    promedio = mean(c(horas_estudio_num, notas_ultimo_semestre_num, satisfaccion_rendimiento_num), na.rm = TRUE),
    horas_estudio_num = if_else(is.na(horas_estudio_num), promedio, horas_estudio_num),
    notas_ultimo_semestre_num = if_else(is.na(notas_ultimo_semestre_num), promedio, notas_ultimo_semestre_num),
    satisfaccion_rendimiento_num = if_else(is.na(satisfaccion_rendimiento_num), promedio, satisfaccion_rendimiento_num)
  ) %>%
  ungroup()

# PASO 3: CALCULAR EL INDICE
# Se crea el índice sumando las 3 variables, lo que genera un puntaje máximo de 9.
datos_indice <- datos_imputados %>%
  mutate(indice = horas_estudio_num + notas_ultimo_semestre_num + satisfaccion_rendimiento_num)

glimpse(datos_indice)

# PASO 4: GENERAR TABLA DE PROMEDIOS
# Agruparemos a las personas por su edad y calcularemos el promedio de ese índice para cada grupo
unique(base$edad_r)

datos_indice$edad_r <- datos_indice$edad_r %>% fct_relevel(c("21 a 23", "18 a 20", "24 a 29", "30 o más"))

tabla_promedios <- datos_indice %>%
  group_by(edad_r) %>%
  summarise(promedio_indice = mean(indice, na.rm = TRUE)) %>%
  ungroup()

print(tabla_promedios)

# Tanto los estudiantes más jóvenes (18 a 20) como los mayores (30 o más) tienen promedios más bajos en el índice académico (~5.1)
# Esto podría sugerir mayores desafíos en estudiantes más jóvenes (recien entrando, menor experiencia)
# o menor nivel de engagement académico en estos grupos.
# Por otro lado, los grupos de 21 a 23 y 24 a 29 muestran los mayores niveles en el índice (~5.6)
# Estos tramos de edad podrían corresponder a estudiantes más consolidados o adaptados a la vida universitaria, 
# con mayores recursos o estrategias de estudio.

# PASO 5: CREAR GRÁFICO
# Genera un gráfico de barras con la base datos_indice, para entender los valores en promedio_indice por la variable edad_r

ggplot(tabla_promedios, aes(x = edad_r, y = promedio_indice)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(x = "Edad", y = "Promedio del Índice", title = "Promedio del Índice de desempeño académico percibido por edad") +
  theme_minimal()


