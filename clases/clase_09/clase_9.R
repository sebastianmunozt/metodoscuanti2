# Clase 9: Análisis de Datos Cuantitativos 

pacman::p_load(tidyverse, openxlsx, readxl,readr,janitor, forcats, writexl, DataExplorer, 
               datos,  knitr, gt, summarytools, ggthemes, hrbrthemes, foreign, DescTools, ineq)
#foreign: para leer spss. Varios paquetes para leer bases. 
#DescTools: para hacer análisis como la Moda
#ineq: inequidades


#Cargo base
# https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2020/Casen_en_Pandemia_2020_revisada202209.sav.zip
datos <- read.spss("datos/Casen_en_Pandemia_2020_revisada202209.sav", to.data.frame = TRUE)


#Exploración inicial
glimpse(datos)

# Análisis Univariado -----------------------------------------------------
#I. Medidas de tendencia central#### 

# Quiero ver los ingresos de los/as chilenos/as
mean(datos$y1)

#por qué no se puede hacer una media?
class(datos$y1)

datos$y1 <- as.numeric(datos$y1)
class(datos$y1)

#01. Media (de Ingresos)
#R base
mean(datos$y1, na.rm = TRUE) #remover casos NA!


#realicelo para edad: 
# mean(datos$______, na.rm = ______) 

#tidyverse
datos %>%
  summarise(media_y1 = mean(y1, na.rm = TRUE))

#realicelo para edad:
# datos %>%
#   summarise(media_____ = mean(____, na.rm = ______))


#02.Mediana (de Ingresos): divide la distribución en la mitad
#R base
median(datos$y1, na.rm = TRUE)

#realicelo para edad:
# ______(datos$____, na.rm = _____)

#tidyverse
datos %>%
  summarise(mediana_y1 = median(y1, na.rm = TRUE))

options(scipen = 999) # para que no me aparezca en notación científica


#realicelo para edad:
# datos %>%
#   summarise(mediana_____ = ______(______, na.rm = ______))

#03.Moda (de Ingresos)
Mode(datos$y1, na.rm = TRUE) # ojo es con mayúscula la M

#realicelo para edad:
# ______(______$______, na.rm = ________) # ojo es con mayúscula la M


#II. Medidas de dispersión #### 
#01. Varianza!
# Paso 1: Calcular la media
media <- mean(datos$y1, na.rm = TRUE)

# Paso 2: Calcular la suma de los cuadrados de las diferencias
sum_cuadrados_dif <- sum((datos$y1 - media)^2, na.rm = TRUE)

# Paso 3: Dividir la suma de los cuadrados de las diferencias entre n-1
n <- sum(!is.na(datos$y1))
varianza <- sum_cuadrados_dif / (n - 1)

# Imprimir la varianza
varianza

#Var(datos, na.rm= TRUE)
var <- var(datos$y1, na.rm = TRUE)

# Imprimir la varianza
var

#realicelo con edad
# var_edad <- _____(______$_____, na.rm = TRUE)

#02. Desviación estandar: 
#raiz cuadrada de varianza: sqrt()
sqrt(var(datos$y1, na.rm = TRUE))

#directamente
sd(datos$y1, na.rm = TRUE)

#realicelo con edad
# ____ (____$____, ______ = _____)


#III. Usos de summarise y group by para realizar comparaciones específicas####

#para el total del país
datos %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE),
            minimo = min(y1, na.rm = TRUE), #agrego mínimo 
            maximo = max(y1, na.rm = TRUE)) #agrego máximo

#quiero hacer una tabla para las comunas de la región metropolitana
names(datos)
unique(datos$region)

datos %>%
  filter(region == "Región Metropolitana de Santiago") %>%
  group_by(comuna) %>% # distingo por las comunas de la RM
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE)) 



ingresos_por_comuna_media <- datos %>%
  filter(region == "Región Metropolitana de Santiago") %>%
  group_by(comuna) %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE)) %>%
  arrange(desc(media)) # ordenar de mayor a menor por la media


ingresos_por_comuna_mediana <- datos %>%
  filter(region == "Región Metropolitana de Santiago") %>%
  group_by(comuna) %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE)) %>%
  arrange(desc(mediana)) # ordenar de mayor a menor por la mediana

ingresos_por_desvio <- datos %>%
  filter(region == "Región Metropolitana de Santiago") %>%
  group_by(comuna) %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE)) %>%
  arrange(desc(desv.est))


#mínimo y máximo
ingresos_por_media_minmax <-  
datos %>%
  filter(region == "Región Metropolitana de Santiago") %>%
  group_by(comuna) %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE),
            minimo = min(y1, na.rm = TRUE), #agrego mínimo 
            maximo = max(y1, na.rm = TRUE)) %>% #agrego máximo
  arrange(desc(media))


#práctica: realice el ejercicio anterior pero para Región de Valparaiso #### 

unique(datos$region)

#disclaimer: 
#esto es una encuesta, cuya muestra no permite realizar este tipo de inferencias. 

#group_by por sexo
unique(datos$sexo)

datos %>%
  filter(region == "Región Metropolitana de Santiago") %>%
  group_by(sexo) %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE),
            minimo = min(y1, na.rm = TRUE), #agrego mínimo 
            maximo = max(y1, na.rm = TRUE)) %>% #agrego máximo
  arrange(desc(media))


#práctica: realice el ejercicio anterior pero para Región de Valparaiso #### 
# unique(datos$______)




#cuartiles, deciles y percentiles####
#quantile (variable, probs = %; na.rm= TRUE)

datos %>%
  summarise(media = mean(y1, na.rm = TRUE),
            mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE),
            minimo = min(y1, na.rm = TRUE),
            maximo = max(y1, na.rm = TRUE),
            cuartil_1 = quantile(y1, probs = 0.25, na.rm = TRUE),
            cuartil_2 = quantile(y1, probs = 0.50, na.rm = TRUE),
            cuartil_3 = quantile(y1, probs = 0.75, na.rm = TRUE),
            decil_rico = quantile(y1, probs = 0.99, na.rm = TRUE)) %>%
  arrange(desc(media))


#IV. Medidas de desigualdad: Coeficiente Gini####
Gini(datos$y1, na.rm = TRUE)


#Ordeno las regiones según desigualdad
datos %>%
  group_by(region) %>%
  summarise(coeficiente_gini = Gini(y1, na.rm = TRUE), 
            media = mean(y1, na.rm = TRUE)) %>% 
  arrange(desc(coeficiente_gini)) # de mayor a menor


#Práctica casa ####
# realice media, mediana, desviación de variable edad. 
# realice lo mismo para las comunas de la región metropolitana.



# seleccione la región metropolitana y vea la desigualdad por comuna
in_comunas_stgo <- datos %>%
  filter(region == "Región Metropolitana de Santiago") %>% 
  group_by(comuna) %>%
  summarise(coeficiente_gini = Gini(y1, na.rm = TRUE), 
            media = mean(y1, na.rm = TRUE)) %>% 
  arrange(desc(coeficiente_gini)) 

# Graficos variables cuanti ----------------------------------------------------

#geom_boxplot####
ggplot(datos, aes(y = y1, x ="")) +
  geom_boxplot()


datos %>%
  summarise(mediana = median(y1, na.rm = TRUE),
            desv.est = sd(y1, na.rm = TRUE),
            minimo = min(y1, na.rm = TRUE),
            maximo = max(y1, na.rm = TRUE),
            cuartil_1 = quantile(y1, probs = 0.25, na.rm = TRUE),
            cuartil_2 = quantile(y1, probs = 0.50, na.rm = TRUE),
            cuartil_3 = quantile(y1, probs = 0.75, na.rm = TRUE))


# limito el gráfico: no considero desde el quinto 0.95

ggplot(datos, aes(y = y1, x= "")) +
  geom_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6) + 
  scale_y_continuous(limits = c(0, quantile(datos$y1, 0.95, na.rm = TRUE))) + #elimino los outlyer
  theme_classic()

#cambio el color de dentro
ggplot(datos, aes(y = y1, x="")) +
  geom_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6,  fill= "tomato") +
  scale_y_continuous(limits = c(0, quantile(datos$y1, 0.95, na.rm = TRUE))) +
  theme_minimal()+
  ylab("Ingresos")

#para hombre y mujer
ggplot(datos, aes(x= sexo, y = y1, fill=sexo)) + #en x pongo la variable de corte
  geom_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6) +
  scale_y_continuous(limits = c(0, quantile(datos$y1, 0.95, na.rm = TRUE))) + 
  guides(fill = "none")+
  theme_minimal()


# Box plot customization
ggplot(datos, aes(x= sexo, y = y1, fill=sexo)) + 
  stat_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6,
               geom = "errorbar",
               width = 0.15,
               color = 1) +  # Error bar color
  geom_boxplot(fill = 2,           # Box color
               alpha = 0.5,        # Transparency
               color = 1,          # Border color
               outlier.colour = 2) # Outlier color


#práctica ####
#Para la edad de toda la población
#calcula la mediana, el mínimo, el máximo, el cuartil 1, el 2 y el 3

datos %>%
  summarise(mediana = _____(____, na.rm = TRUE),
            desv.est = ____(____, na.rm = TRUE),
            minimo = ____(____, na.rm = TRUE),
            maximo = ____(____, na.rm = TRUE),
            cuartil_1 = quantile(____, probs = _____, na.rm = TRUE),
            cuartil_2 = quantile(____, probs = _____, na.rm = TRUE),
            cuartil_3 = quantile(____, probs = _____, na.rm = TRUE))


#Realiza un gráfico de caja y bigote , sin sacar los extremos
ggplot(datos, aes(y = _____)) +
  geom_boxplot()

#Realiza otro para la edad por sexo
ggplot(datos, aes(y = _____, x=_____)) +
  geom_boxplot()


#siguiendo modelo "emprolijalo"
ggplot(datos, aes(x= _____, y = _____, fill=______)) + 
  stat_boxplot(outlier.shape = NA, coef = 1.5, width = 0.6,
               geom = "errorbar",
               width = 0.15,
               color = 1) +  # Error bar color
  geom_boxplot(fill = 2,           # Box color
               alpha = 0.5,        # Transparency
               color = 1,          # Border color
               outlier.colour = 2) # Outlier color



#geom_histogram ()####

# Crear un histograma a partir de los mismos datos
ggplot(datos, aes(x = y1)) +
  geom_histogram(binwidth = 100000, color = "black", fill = "gray") + #binwidth: hacer rangos de 100.000
  labs(x = "Ingresos", y = "Frecuencia") +
  theme_classic()

ggplot(datos, aes(x = y1)) +
  geom_histogram(binwidth = 200000, color = "black", fill = "gray") + #binwidth:hago rangos de 200.000
  labs(x = "Ingresos", y = "Frecuencia") +
  theme_classic()

#los ingresos del país responden a una distribución normal??

# Crear un histograma sin los valores superiores extremos
ggplot(datos, aes(x = y1)) +
  geom_histogram(binwidth = 100000, color = "black", fill = "gray") +
  scale_x_continuous(limits = c(min(datos$y1), quantile(datos$y1, 0.99, na.rm = TRUE))) + # saco el 1%
  labs(x = "Ingresos", y = "Frecuencia") +
  theme_classic()


#práctica####
#realice histograma con la variable edad, cambie los binwidth








