
# Prueba --------------------------------------------------------
##00. Lectura de paquetes a utilizar 
# cargue los siguientes paquetes con pacman:
## tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer
## knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra

#01.Inicio del trabajo
#01.01 Apertura de base
# Cargue la base: EncuestaAntropologia_2023.xlsx y asignela al enviroment como base
# Cargue el libro de códigos: Encuesta antropología 2023.csv y asignelo como libro

#01.02 Realice una primera mirada de la base con glimpse

#Conteste: ¿Cuántos casos hay en la base? ¿Cuántas variables?

#02. Renombrar variables
#02.01: observe las variables de la base de datos y del libro de códigos con names
#02.02: renombre 4 variables pertenecientes al grupo del cual usted participa

#03. Limpieza de valores/categorías
#03.01 Observe la base de datos con DataExplorer::create_report(base) 
#señale 2 variables que habría realizar una transformación de las categorías de las variables de su grupo 

#03.02 Limpieza inicial
#Luego de observar las siguiente frecuencia:
freq(base$le10, prop = TRUE, order = "freq", report.nas =  FALSE)
unique(base$le10)

#agrupe las siguientes categorías mediante ifelse: 
# a) genere la categoría "Indefinido" y coloque allí: "donde sea", "Todos los anteriores ", "Todos", "Todas las anteriores ".
# b) genere la categoría "Otros lugares de la casa" coloque: "Donde pueda encontrar espacio en mi casa".
# c) observe la transformación realizada mediante la aplicación de una frecuencia (freq) y unique ()

#04. Limpieza de respuestas múltiples
#Limpie le12: por qué dispositivos lee a través de medios digitales
#04.1.observe la variable con unique
#04.2.realice un vector respuesta con strsplit y realice la separación considerando las distintas respuestas se separan con punto y coma (;)
#04.3.realice unlist de ese vector
#04.4.realice una tabla de frecuencias de las respuestas

##05. Recodificaciones
#05.01: recodifique avr_notas: 
# estudiantes con notas mayor o igual a 6
# estudiantes menor o igual a 5,9
#05.02: compruebe lo realizado con freq

## 06. Análisis univariado de variables cualitativas 
# 06.0: utilice la siguiente línea de código para que en sus tablas los decimales aparezcan con coma: options(OutDec= ",") 
# 06.1.Realice 3 distribuciones de frecuencias que utilizará con su grupo, eliminando las categorías que no son necesarias
# 06.2 Formatee 1 tabla con kable
# 06.3 Guarde la tabla en formato png 
#*recuerde que para guardar con save_kable es posible que necesite pasar lo siguiente: webshot::install_phantomjs()  

#07. Gráficos categóricos univariados 
##07.01:Realice un gráfico de columnas (geom_col) de la variable le04, para ello siga el siguiente proceso: 
##07.01.a: realice un tabla con le04
##07.01.b: guardela como dfle04, realice un gráfico de barras horizontales. 
#07.02: A partir de la misma tabla (dfle04), realice un gráfico de tortas utilizando geom_bar (stat= "indentity") y coord_polar(theta = "y")
#**** Asegurese de que las etiquetas estén bien señaladas. 

# 08. Análisis bivariados variables cualitativas 
#08.01: elija # conjunto de colores desde https://www.color-hex.com/
#guardelos en el siguiente vector
colors <- c("#", "#", "#", "#", "#") 

#acorte los nombres de le06 como sigue para que aparezcan bien en la tabla, pasando la siguiente porción de código
base <- base %>% 
  mutate(le06 = fct_recode(le06,
                           "Ficción" = "Ficción (novelas de romance, acción, suspenso, fanfics, ciencia ficción, policiales, poesía, etc)",
                           "No ficción" = "No ficción (documentos científicos, académicos, revistas, biografías, etc)"))

#08.02. Realicé la siguiente tabla de contingencia con tidyverse, reemplazando donde corresponda: 
base %>%
  filter(genero != "Prefiero no responder") %>% #quiero eleminar "Prefiero no responder" de la variable género
  select(______, _______) %>% # quiero seleccionar tipo de literatura de preferencia (le06) y genero
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100 

#08.03: ¿Qué puede interpretar de la tabla realizada?



#08.03: grafique la relación entre género y tipo de literatura de preferencia le06, llenando los espacios en blanco
ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = ________)) + #ponga la variable dependiente
  geom_bar(position = "fill") +
  ylab("Proporciones") +
  xlab("Género")+
  theme_ipsum() +
  scale_fill_manual(values = colors)+
  labs (title = "______________",  # ponga título
        caption = "____________", #ponga de donde vienen los datos
        fill= "_____________") #ponga a que refieren colores




#09. Para finalizar: 
#a) Guarde el script como prueba1_nombresuyo_apellido (ejemplo: prueba1_sebastian_muñoz) en la misma carpeta donde se sitúa su proyecto
#b) Envíe los archivos en la tarea: script, proyecto, bases y tabla
#c) o Envíe por mail a: semunoz@uahurtado.cl







