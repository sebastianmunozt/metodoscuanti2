#Paquetes

install.packages("pacman")

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer,
               knitr, gt, summarytools, ggthemes, hrbrthemes, stringr, tidyselect, kableExtra)

install.packages("read.csv")

install.packages("tidyverse")

library("tidyverse")

#Bases

base <- read.csv(file = "Encuesta antropología 2023.csv")

#Libro de códigos

libro <- read.xlsx("EncuestaAntropologia_2023.xlsx") # esto no existe en la carpeta del rproject.

#Limpieza y transformación de las variables de su grupo u otras que pueda utilizar para la realización de sus análisis (por ejemplo, la recodificación de tipos de estudiantes si son prepandémicos, pandémicos o postpandémicos)

names(base)
names(libro)

#Variables del grupo

base <- base %>% dplyr::rename(fruta = HS01....Considerando.las.últimas.cuatro.semanas..En.promedio..cuántas.veces.al.día.comiste.fruta..,  
                               verduras = HS02..Considerando.las.últimas.cuatro.semanas..En.promedio..cuántas.veces.al.día.comiste.verduras.,
                               proteina_animal = HS03....Considerando.las.últimas.cuatro.semanas..En.promedio...cuántas.veces.en.la.semana.consumes.proteína.animal...vacuno..pescados..pollo..cerdo..huevos..etc.., 
                               proteina_no_animal = HS04....Considerando.las.últimas.cuatro.semanas..A.nivel.general..cuántas.veces.en.la.semana.consumes.proteína.no.animal...legumbres..seitán..alimentos.de.soja..etc., 
                               lacteos = HS05...Considerando.las.últimas.cuatro.semanas..A.nivel.general...Cuántas.veces.dentro.de.la.semana.consumes.lácteos...ej...leche..yogur..queso..mantequilla..etc..,
                               agua = HS06..Considerando.las.últimas.cuatro.semanas..A.nivel.general...Cuánta.agua.bebes.aproximadamente.dentro.del.día.,
                               deporte = HS07....Considerando.las.últimas.cuatro.semanas...Cuál.de.las.siguientes.actividades.deportivas.realizaste.,
                               frecuencia_deporte = HS08.....Considerando.las.últimas.cuatro.semanas..En.base.a.TODAS.las.actividades.físicas.o.deportivas.que.realizaste...cuánto.tiempo.dedicas.a.la.actividad.física.semanalmente..,
                               atencion_en_clases = HS09..Considerando.las.últimas.cuatro.semanas..Del.0.al.5...Te.ha.costado.mantener.la.atención.sobre.el.profesor.en.clases..Donde.0.refiere.a..no.me.costado.nada..y.5..me.ha.costado.mucho...,
                               distraccion_en_clases = HS10..Considerando.las.últimas.cuatro.semanas..Con.cuánta.frecuencia.te.encuentras.distraída.o.durante.el.desarrollo.de.la.clase...Considera.distracciones.producto.de.utilización.de.celular..ruidos..sueño.o.cansancio..Donde.0.refiere.a..nunca..y.5..siempre..,
                               concrecion_de_tareas = HS11..Considerando.las.últimas.cuatro.semanas..A.nivel.general..Te.ha.costado.terminar.tareas.trabajos.de.la.universidad.a.tiempo..,
                               atraso_en_entregas = HS12..En.base.a.tu.experiencia.académica.del.semestre.en.curso...con.qué.frecuencia.has.entregado.trabajos.atrasados.,
                               satisfaccion_organizacion = HS13..A.nivel.general..Qué.tan.satisfecha.o.te.sientes.con.tu.habilidad.para.organizar.tus.tiempos.de.estudio..Donde.1.refiere.a..muy.insatisfecha.o..y.5..a.muy.satisfecha.o.,
                               preparacion_clases = HS14..A.nivel.general..Te.sientes.preparada.o.para.realizar.trabajos.evaluaciones.de.la.universidad.en.base.de.lo.aprendido.en.tus.clases..,
                               satisfaccion_rendimiento = HS15..A.nivel.general..Qué.tan.satisfecha.o.te.sientes.con.tu.rendimiento.académico...Donde.1.refiere.a..Para.nada.satisfecha.o..y.5..a.muy.satisfecha.o.)



#Variables sociodemográficas

base <- base %>% dplyr::rename(genero = SD02...Con.qué.género.te.identificas.,  
                               anio_ingreso = SD05...En.qué.año.ingresaste.a.la.carrera.,
                               clase_social = SD08..En.la.sociedad..comúnmente..existen.distintos.grupos.o.clases.sociales..Las.personas.de.clase.social.alta.son.las.que.tienen.los.ingresos.más.altos..mayor.nivel.de.educación.y.trabajos.más.valorados..Las.personas.de.clase.social.baja.son.las.que.tienen.ingresos.más.bajos..menor.nivel.de.educación.y.trabajos.menos.valorados..Entre.estas.clases.existen.otras.intermedias..Según.su.opinión..a.cuál.de.los.siguientes.grupos.o.clases.sociales.pertenece.usted.. )

#Agrupacion de categorias de anio de ingreso

base <- base %>%
  mutate(anio_ingreso = str_extract(anio_ingreso, "\\d+")) %>% # solo números de la variable edad
  mutate(anio_ingreso = as.numeric(anio_ingreso)) %>%  #la transformo en variable numérica numérico
  mutate(anio_ingreso_r= case_when (anio_ingreso %in% c(2017:2019) ~ "prepandémicos", 
                           anio_ingreso %in% c(2020:2021) ~ "pandémicos", 
                           anio_ingreso %in% c(2022:2023) ~ "postpandémicos"))

freq(base$anio_ingreso_r, prop=TRUE,  report.nas = FALSE) %>% 
  tb() 

#Cambiar los nombres no basta, Sólo recodifican las categorías de una variable. 

                           
#Realización de tablas de frecuencias para todas las variables sociodemográficas.

webshot::install_phantomjs()
install.packages("kableExtra")
library("kableExtra")
install.packages("webshot")
library("webshot")


base %>% 
  freq(genero, prop = TRUE, report.nas = FALSE) %>%
  tb() %>% 
  kable(col.names = c ("Género", "Frecuencia", "%", "% Acumulado"),
        caption = "Identidad de genero estudiantes Antropología UAH",
        digits = c(0,0,2,2)) %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  save_kable(file = "tablas/identidadegenero1.png", zoom=3)

if(!dir.exists("tablas")) 
  
#Sólo presentan 1 tabla para variables sociodemográficas

#Realización de tablas de frecuencias para todas las variables de su grupo.
  
# No presentan tablas de frecuencias para las variables de su grupo.  

#Realización de 3 gráficos de barras para las tres variables más destacadas de su grupo

satisfaccion_organizacion  <- base %>% 
  freq(satisfaccion_organizacion, prop = TRUE, report.nas =  FALSE)%>% 
  tb() 


ggplot(satisfaccion_organizacion, aes(x = pct, y = satisfaccion_organizacion, pct, fill=satisfaccion_organizacion)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  labs(title = "Satisfacción en la organización de estudios", 
       subtitle = "Encuesta Estudiantes 2023",
       caption = "fuente: Encuesta Estudiantes Antropología 2023")+
  geom_text(aes(label = paste0(round(pct, 1), "%")), #concatena porcentaje con un decimal y "%".
            hjust = -0.1, size = 3, nudge_x = -.9, fontface= "bold")+
  theme_ipsum()+
  scale_fill_viridis_d(option = "C", guide = "none")

#Buen gráfico, sólo faltó explicitar que significan los números del 1 al 5.
# Sólo presentan 1 gráfico


#Realización de 2 gráficos de tortas de variables más destacadas de su grupo

dfproteina_animal<- base %>% 
  mutate(proteina_animal = if_else(proteina_animal =="Prefiero no responder", as.character(NA), proteina_animal)) %>%
  freq(proteina_animal, prop = TRUE, report.nas =  FALSE)%>% 
  tb()

ggplot(dfproteina_animal, aes(x = "", y = pct, fill = proteina_animal)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Consumo de proteina animal en Estudiantes Antropologia UAH", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "proteina_animal") +
  theme_ipsum() +
  scale_fill_viridis_d( guide = "legend", limits = dfproteina_animal$proteina_animal)

#grafico 2
dfsatisfaccionrendimiento<- base %>% 
  mutate(proteina_animal = if_else(proteina_animal =="Prefiero no responder", as.character(NA), proteina_animal)) %>%
  freq(proteina_animal, prop = TRUE, report.nas =  FALSE)%>% 
  tb()

ggplot(dfproteina_animal, aes(x = "", y = pct, fill = proteina_animal)) +
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + # esto permite hacer gráfico de torta
  labs(title = "Consumo de proteina animal en Estudiantes Antropologia UAH", 
       caption = "fuente: Encuesta de Estudiantes UAH 2023",
       fill = "proteina_animal") +
  theme_ipsum() +
  scale_fill_viridis_d( guide = "legend", limits = dfproteina_animal$proteina_animal)

#Presentan el mismo gráfico repetido 2 veces.

#Realización 3 tablas de contingencia donde pueda utilizar variables independientes y dependientes considerando los problemas centrales de su grupo
base %>%
  filter(genero != "Prefiero no responder") %>%
  select(genero, satisfaccion_organizacion) %>%
  droplevels() %>%
  table(.) %>% 
  addmargins(.,2) %>% #primero addmargins
  prop.table(.,2) %>% #segundo prop.table
  round(4)*100

#Sólo presentan 1 tabla cruzada.

#Realización de 3 gráficos que incluyan la información de dos variables (pueden ser los mismos de la tabla de contingencia)

ggplot(data = subset(base, genero != "Prefiero no responder"), aes(x = genero, fill = concrecion_de_tareas)) +
  geom_bar(position = "fill") +
  ylab("concrecion_de_tareas") +
  xlab("genero")+
  theme_ipsum() +
  scale_fill_manual(values = colors)+
  labs (title = "Capacidad de concretar las tareas según el género", 
        caption = "Encuesta Estudiantes Antropología UAH", 
        fill= "concrecion_de_tareas")

#Este gráfico no funciona porque el objeto con los colores (colors) no ha sido creado.
#Sólo presentan 1 gráfico.

#Realización de un índice aditivo

base <- mutate(base, frutas=car::recode(base$frutas,
                                                                   "'Más de 3 veces'=4;'2 veces'=2;'1 vez'=1;'Usualmente no consumo frutas'=0"))
base <- mutate(base, verduras=car::recode(base$verduras,
                                                             "'Más de 3 veces'=4;'3 veces'=3;'2 veces'=2;'1 vez'=1; 'usualmente no como verduras'=0"))
base <- mutate(base, proteina_animal=car::recode(base$proteina_animal,
                                                             "'4 veces o más'=4;'3 veces'=3;'2 veces'=2; '1 vez'= 1;'Usualmente no consumo estas proteínas'= 0"))
base <- mutate(base, proteina_no_animal=car::recode(base$proteina_no_animal,
                                                              "'4 veces o más'=4;'3 veces'=3;'2 veces'=2;'1 vez'=1;'Usualmente no consumo estas proteínas'= 0"))
base <- mutate(base, lacteos=car::recode(base$lacteos,
                                                           "'4 veces o más'= 4; '3 veces'=3;'2 veces'=2;'1 vez'=1;'Usualmente no consumo lácteos'= 0; 'No consumo nada de lácteos'= 0"))
base <- mutate(base, agua=car::recode(base$actfisica,
                                                           "'Sí'=2; 'No'=0"))
base <- mutate(base, deporte=car::recode(base$tiempo_actfisica,
                                                                  "'1 - 2 horas'=2;'3 - 4 horas'=3;'5 horas o más'=4"))
base <- mutate(base, tipo_de_deporte=car::recode(base$intensidad_actfisica,
                                                                      "'Baja'=1;'Moderada'=3;'Alta'=4"))
base <- mutate(base, atencion_en_clases=car::recode(base$concentracion,
                                                               "1:3=2;4:7=1;8:10=0"))
base <- mutate(base, distraccion=car::recode(base$distraccion,
                                                             "0:1=3;2:4=2;5:7=1; 8:10=0"))
base <- mutate(base, dificultad=car::recode(base$dificultad,
                                                            "'Sí'=0; 'No'=1"))
base <- mutate(base, satisfaccion_organizacion=car::recode(base$satisfaccion_organizacion,
                                                                           "1:3=1;4:6=2;7:9=3; 10=4"))
base <- mutate(base, preparacion=car::recode(base$preparacion,
                                                             "'Sí, siempre me siento preparadx'=3;'A veces me siento preparadx'=2;'Nunca me siento preparadx'=0"))
base <- mutate(base, satisfaccion_calificacion=car::recode(base$satisfaccion_calificacion,
                                                                           "1:3=1;4:6=2;7:9=3; 10=4"))


#Cruce del índice aditivo con otras dos variables

#Comentario: Si bien saben realizar los gráficos y las tablas no lograron finiquitar de buena manera, da la sensación que este trabajo fue hecho de manera despreocupada o a última hora.
# Tenían variables interesantes para la población de estudio, estos datos les pueden servir en trabajos futuros en que investiguen sobre la misma temática.