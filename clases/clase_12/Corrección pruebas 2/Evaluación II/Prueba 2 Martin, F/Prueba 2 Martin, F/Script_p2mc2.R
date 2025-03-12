#####INICIO 
pacman::p_load(tidyverse,# Universo de paquetes : tidyr, dplyr, ggplot2,readr,purrr,tibble, stringr, forcats
               openxlsx,#leer archivos xlsx
               readxl,# leer archivos xl      #dos formatos de excel xlsx y xl
               readr,
               janitor,#limpieza de datos
               writexl,#Guardar tablas formato excel
               DataExplorer, #Exploración rápida 
               knitr,
               summarytools,
               webshot)

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, summarytools, ggthemes, hrbrthemes, foreign, DescTools, ineq)


base_antropologia <- read.xlsx("base_antropologia_limpia.xlsx")
libro_codigos<- read.xlsx("base_antropologia_limpia.xlsx") 


#Explorar
glimpse(base_antropologia) #Una primera mirada de lo que hay en mis datos (variables, tipo, respuesta)
names(base_antropologia) #observo los nombres de las variables (que nos permite evaluar si renombrar)
View(base_antropologia) #Veo la base de datos

#si esq tenemos que eliminar la primera fila a mis datos, ya que no corresponde a un dato lo hacemos con
_______________ <- ________________[-1,]
###LIMPIEZA DE VARIABLE DE DATOS ···· MINUSCULA Y GUION BAJO
base_antropologia <- janitor::clean_names(base_antropologia) 



#LIMPIAR Y ORDENAR LA BASE DE DATOS
_______________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________
# RENAME ------------------------------------------------------------------
_________________________________________________________________________________________________________________________________________
___________________________________________________________________________________________________________________________________________

# a) Veo los nombres de todas las variables para seleccionar la(s) variable(s) que me interesa renombrar. 
names(base_antropologia)
# b) Renombrar las variables elegidas con RENAME: 
base_antropologia <- base_antropologia %>% dplyr::rename(variable_nombre_nuevo=nombre_antiguo_VARIABLE,
                                                         variable_nombre_nuevo=nombre_antiguo_VARIABLE )

___________________________________________________________________________________________________________________________________
____________________________________________________________________________________________________________________________________
# MUTATE ------------------------------------------------------------------
____________________________________________________________________________________________________________________________________
___________________________________________________________________________________________________________________________________

##### MUTATE ··· DPLYR,  - RECODIFICAR categorias de respuestas de variable

# a) Selecciono mis variables de interés 
# b) Veo si es necesario recodificar las variables (sus categorías de respuesta) *¿Cómo yo decido esto? Preguntas con respuesta abierta o categorías con nombres que deseo cambiar 
# c) Obtengo de manera más directa los valores únicos de la variable a recodificar: sus CATEGORÍAS DE RESPUESTA.
unique(base_antropologia$variable_seleccionada)
table(base_antropologia$variable_seleccionada) #PARA VER LAS RESPUESTAS 

          #en el caso que las variables no esten en el formato ordenado (es decir, sin minuscula y con espacios) lo corregimos usando
        base_antropologia$variable_seleccionada <- tolower(base_antropologia$variable_seleccionada) #todas a minusculas
        base_antropologia$variable_seleccionada  <- gsub(pattern = " ", replacement = "", x = base_antropologia$variable_seleccionada) #quitamos los espacios
        #es CASI lo mismo q el L JANITOR pero esta es para recoficcar LAS CATERGORIAS DE RESPUESTA, es más especifico 
        
# e) Aplico MUTATE Y CASE_WHEN para recodificar categorías de respuesta
 base <- base %>% mutate(variable_elegida=case_when(variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                           variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                           variable_elegida=="valor respuesta dada"~"Nuevo valor de la categoría",
                                                           TRUE ~ variable_elegida))         
        
#OJITO, dependiendo de la varianle con la que se trabaja, se debe revisar si supone o no supone un orden en sus respuestas, 
 #Hay 2 tipos de orden: CHARACTER NO SUPONE ORDEN mientras que FACTOR SI SUPONDE UN ORDEN
#en el caso que SI SUPONGA UN ORDEN, se debe usar el mutate para poder seguir trabajando despues con la base:
 
        class(base_antropologia$satisfaccion_rendimiento_academico) #para ver que tipo de variable es entendida por R
        #SI DICE CHARACTER HAY Q PASARLO A FACTOR OCUPANDO MUTATE Y AS.FACTOR 
        base_antropologia <- base_antropologia %>% 
          mutate(satisfaccion_rendimiento_academico = as.factor(satisfaccion_rendimiento_academico))
        
        

  #por ejemplo: base_antropologia <- base_antropologia %>% 
 #mutate(satisfaccion_rendimiento_academico = case_when(satisfaccion_rendimiento_academico== "Insatisfecho" ~ "Insatisfecho",
 #                                                      satisfaccion_rendimiento_academico== "Muy insatisfecho" ~ "Insatisfecho",
  #                                                     satisfaccion_rendimiento_academico== "Muy Satisfecho" ~ "Satisfecho",
   #                                                    satisfaccion_rendimiento_academico== "Satisfecho" ~ "Satisfecho"))
        
        
______________________________________________________________________________________________________________________________
__________________________________________________________________________________________________________________________________
# IF ELSE -----------------------------------------------------------------
________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________
        
        
        #OPERACION CONDICIONA, "SI PASA ESTO,HAZ ESTO"
      #se deben crear vectores: 
        
#a) ver las resouestas table(base_antropologia$nivel_educativo_madre) 
        
#b) tenemos q guardar la variable con la q trabajamos como objeto, asi que creamos un objeto y concatenamos las respuestas de la variable
    
    nivel_educativo_madre <- c("Ed. Media completa", "Ed. Tecnica superior (comp. o incomp.)", "Ed. Universitaria (comp. o incomp.)", "Posgrado (Magister, Doctorado)")      
#c) Aplicamos ifelse de la siguiente manera:
# -si el nivel educativo de la madre llega a Ed. Media completa, será Bajo,
# -si el nivel educativo de la madre llega a Ed. Tecnica superior (comp. o incomp.)" y "Ed. Universitaria (comp. o incomp.), será Medio,
# -si el nivel educativo de la madre llega a Ed. Posgrado (Magister, Doctorado), será Alto        
        
    #Estructura: (es para que se guien)
    Nueva variable creada <- ifelse(Variable utilizada == "Categoría 1", "Clasificación designada A",
                                    ifelse(Variable utilizada %in% c("Categoría 2", "Categoría 3"), "Clasificación designada B",
                                           ifelse(Variable utilizada == "Categoría 4", "Clasificación designada C", NA))) #Se agregó NA como valor si ninguna de las condiciones se cumple.
    
    #Ahora lo aplicamos...
    nivel_educativo_madre_ordinal <- ifelse(nivel_educativo_madre == "Ed. Media completa", "Bajo",
                                            ifelse(nivel_educativo_madre %in% c("Ed. Tecnica superior (comp. o incomp.)", "Ed. Universitaria (comp. o incomp.)"), "Medio",
                                                   ifelse(nivel_educativo_madre == "Posgrado (Magister, Doctorado)", "Alto", NA)))      
        
        
    
    

    
    
______________________________________________________________________________________________________________________________
____________________________________________________________________________________________________________________________
# Frecuencia --------------------------------------------------------------
_______________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________
  #paquete: sumarytools
freq(datos$religion) #si se ve desordenada ocupa
freq(datos$religion, prop = TRUE, order = "freq", report.nas =  FALSE) #eliminar valores NA y ordenar por frecuencia    
    #a veces si hay error es porque no esta en el tipo de clase de respuesta correcta, osea, probablkemente figura un valorcuando no corresponde
class(datos$religion) 

     #SOLO si no se eliminaron los NA, aplicar
datos %>% 
  mutate(religion = as.character(religion)) %>% #transformo a character
  mutate(religion = if_else(religion %in% c("Sin respuesta", "No aplica"), as.character(NA), religion)) %>% 
  freq(religion, prop = TRUE, order = "freq", report.nas =  FALSE)





__________________________________________________________________________________________________________________________
________________________________________________________________________________________________________________________________
# TABLA DE FRECUENCIA------------------------------------------------------------
______________________________________________________________________________________________________________________________
____________________________________________________________________________________________________________________________-

################## tabla de   --- paquete kable y knitr (y probablemente kableExtra)

#primero vemos como es la frecuencia con R
base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb()

#A partir de eso, generamos una tabla, que desarrollara una imagen con la funcion: 

base %>% 
  freq(identidad_genero_r, prop = TRUE, order = "freq", report.nas =  FALSE)%>% 
  tb() %>%
  kable(col.names = c("Género", "Frecuencia", "%", "% Acumulado"), #esto corresponde a lo que se dirá en la fila de las tablas
        caption = "Distribución de frecuencias de Género", #el titulo de la tabla
        digits = c(0, 0, 2, 2))%>%  #los decimales que va a tener cada cifra de cada columna establecida, en las primeras dos son 0 porque son nombres
  kable_classic(full_width = F, html_font = "Cambria")
    
# SOLO SI ES QUE te piden hacer un grafico de la frecuencia, se tiene que hacer un objeto de lo que se acaba de haces
# es decir, antes de "base %>% ...." debes agregar (siguiendo el ejmplo) f_identidad_genero:r <- 
#al tener el objeto de la tabla, ya puedes hacer un grafico de frecuencia: 
#empezandolo nombrandolo como g_identidad_genero_r para diferenciarlo del objeto creado, la g indica que es GRAFICO que se trabaja con la TABLA

se ocupa esto:
g_identidad_genero_r <- ggplot(edad_t, aes(x = Frecuencia, y = fct_reorder(Nombre, Frecuencia), fill= Nombre)) +
  geom_col() +
  labs(title = "Identidades de Genero",
       subtitle = "según datos de Encuestas Estudiantes Antropología 2024",
       x = "%",  # Esto establece el título del eje x, pero no afecta las etiquetas dentro del gráfico
       y = "Identidad de genero") +
  geom_text(aes(label = round(Frecuencia, 1)),  # Ahora esto añade etiquetas a todas las barras
            hjust = 1, size = 3, nudge_x = -0.9, fontface= "bold", color = "white") +
  scale_fill_viridis_d(option = "C", guide = "none") +
  theme_ipsum()


# Y LO GUARDO (SOLO ESTE GRAFICO SE GUARDA USANDO): 
   ggsave("outputs/edades_g.png", plot = edad_g, width = 10, height = 7, dpi = 300)



#PARA HACER CUALQUIE OTRO TIPO DE GRAFICO OCUPA: 


_______________________________________________________________________________________________________________________________
__________________________________________________________________________________________________________________________________

# HACER GRAFICOS  ---------------------------------------------------------
_______________________________________________________________________________________________________________________________
_____________________________________________________________________________________________________________________________

############# graficos Univariados   ---- paquete ggplot y ggplot2

 #Hay dos ociones, OPCION 1 las respuestas se mostrarán de arriba hacia abajo, por lo tanto el grafico sera horizontal

ggplot(actividad_ocio, aes(x = pct, y = fct_reorder(respuestas, pct), fill=respuestas)) +   #se llama a al variable que se utilizará, luego se establecen las categorias que se utilizaran
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Actividades de Ocio estudiantes Antropología",   #titulo para la tabla
       subtitle = "Encuesta Estudiantes 2024", #(opcional)   el subtitulo
       caption = "fuente: Encuesta Estudiantes Antropología 2024")+ # comoo una nota al pie
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

#OPCION 2, grafico sera vertical 

ggplot(actividad_ocio, aes(x =fct_reorder(respuestas, pct), y = pct, fill=respuestas)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Actividades de Ocio estudiantes Antropología", 
       caption = "fuente: Encuesta Estudiantes Antropología 2024")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")



_________________________________________________________________________________________________________________________
______________________________________________________________________________________________________________________________
# TABLA DE CONTINGENCIA ---------------------------------------------------
_____________________________________________________________________________________________________________________________
_______________________________________________________________________________________________________________________________


summarytools::ctable( x = base_antropologia$ea_01_horas_estudio_semana, y = base_antropologia$nivel_educacion_padre)

base_antropologia$nivel_educacion_padre <- base_antropologia$nivel_educacion_padre %>% 
  fct_relevel(c("Educación Básica", "Educación Media Educación Profesional", "Educación Técnica" )) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica

table(base_antropologia$nivel_educacion_padre)

base_antropologia$ea_01_horas_estudio_semana <- base_antropologia$ea_01_horas_estudio_semana %>% 
  fct_relevel(c("1 o 2 horas al día", "3 o 4 horas al día",  "5 o 6 horas por días Más de 7 horas por día" )) %>% 
  fct_drop("No aplica") #ordeno los resultados según raza y elimino la categoría no aplica

table(base_antropologia$ea_01_horas_estudio_semana)

ctable( x = base_antropologia$ea_01_horas_estudio_semana, y = base_antropologia$nivel_educacion_padre, prop = "t", justify = "l")

#guardarlo

contingencia_h_estudio_ed_padre <- ctable(
  x = base_antropologia$ea_01_horas_estudio_semana,
  y = base_antropologia$nivel_educacion_padre,
  prop = "t",  # Proporciones sobre el total
  justify = "l"  # Justificación de las celdas a la izquierda
)

contingencia_h_estudio_ed_padre  <- 
  base_antropologia %>%
  filter(nivel_educacion_padre != "Sin respuesta") %>%
  select(nivel_educacion_padre, ea_01_horas_estudio_semana) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% 
  prop.table(.,2) %>% 
  round(4)*100





