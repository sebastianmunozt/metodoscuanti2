# Ayudantía 09
# Test de hipótesis

# Cargar paquetes --------------------------------------------------------------
pacman::p_load(tidyverse,
               openxlsx,
               summarytools,
               kableExtra,#Tablas elegantes
               webshot2,#exportar tablas
               chromote,
               hrbrthemes)#temas de gráficos


# Importar base de datos -------------------------------------------------------
base <- read.xlsx("base_antropologia_limpia.xlsx") %>% 
  dplyr::select(4:ncol(.)) # seleccionar desde la columna 4 en adelante

# Explorar base ----------------------------------------------------------------
str(base)
summary(base)
names(base)

# Pre procesamiento ------------------------------------------------------------
# Vamos a preparar rápidamnte los datos para el análisis

# Variables de interés: 
# ma_01:Nivel de información de basura en las calles
# ma_02:Interés en gestión de residuos
# identidad_genero_r: Identidad de género recodificadas

# Renombrar variables:
base <- base %>% dplyr::rename(interes_participar_ma = ma_02,  
                               nivel_info_basura = ma_01)

names(base)

#🔶Práctica: Complete el siguiente código para recodificar las variables cm_02 y cm_03, cambiando sus nombres a:
# cm_02: freq_escucha_mus
# cm_03: genero_music

# base <- _____ %>% dplyr::______(___________ = _____,  
# ___________ = _____)

# GRÁFICOS
# Análisis bivariados  (entre una variable numérica y una categórica) ----------
#Variable cuantitativa: nivel_info_basura
table(base$nivel_info_basura)# va del 1 al 10
str(base$nivel_info_basura)#es numérica

#¿El nivel de información de basura en las calles dependerá de la identidad de género?
ggplot(base, aes(x=identidad_genero_r, y=nivel_info_basura, fill=identidad_genero_r)) + #eje x y eje y
  geom_boxplot(alpha=0.3) + #boxplot: diagrama de caja y bigote 
  labs(title = "Interés en gestión de residuos según género", #establece etiquetas
       caption = "Fuente: Encuesta de Estudiantes UAH 2024", 
       y= "Interés en gestión de residuos",
       x= "Identidad de género",
       fill = "Identidad de género")+ #rellena con datos de identidad de género
  theme(legend.position="none") 

# Este gráfico representa la distribución del nivel de interés en la gestión de residuos,
# medido en una escala de 1 a 10, desagregado según la identidad de género de les estudiantes encuestades 
# en la UAH en 2024.

# Elementos del gráfico:
# 1. Mediana (línea gruesa dentro de la caja):
  # Persona de género diverso: mediana más alta (~6,5)
  # Mujer cisgénero: mediana intermedia (~5)
  # Hombre cisgénero: mediana más baja (~5), pero con gran dispersión

# 2. Rango intercuartílico (caja):
  # Representa el 50% central de los datos.
  # La caja de hombres cisgénero es más ancha: mayor variabilidad interna.

# 3. Bigotes y puntos atípicos (outliers):
  # Todos los grupos tienen valores extremos, pero las personas de género diverso presentan menos dispersión.

# Entonces, El gráfico muestra que el nivel de interés en gestión de residuos varía según la identidad de género. 
# En promedio, las personas de género diverso reportan un mayor interés (mediana más alta),seguidas por mujeres cisgénero. 
# En contraste, los hombres cisgénero presentan una distribución más dispersa y una mediana más baja. 
# Esto sugiere una posible relación entre identidad de género y conciencia ambiental en la muestra de estudiantes UAH 2024.

# 🔶Práctica: Interpretemos el siguiente gráfico
#¿El interés en gestión de residuos dependerá del sector donde vivo?
ggplot(base, aes(x=comuna_r1, y=interes_participar_ma, fill=comuna_r1)) + 
  geom_boxplot(alpha=0.3) +
  labs(title = "Interés en gestión de residuos según zona de la capital", 
       caption = "Fuente: Encuesta de Estudiantes UAH 2024",
       y= "Interés en gestión de residuos",
       fill = "Zona")+
  theme(legend.position="none")


# Prueba T de student (sirve sólo con un variable dicotómica) ----------------------------------------------------------

#Variable utilizada: género dicotómico (sólo para efectos de la explicación)

# Recodificamos la variable identidad_genero_r en genero_dicotomico
base<-base %>% mutate(genero_dicotomico= case_when(identidad_genero_r=="Hombre cisgenero" ~ "Hombre" ,
                                                   identidad_genero_r=="Mujer cisgenero" ~ "Mujer"))
unique(base$genero_dicotomico)

# Hipótesis (en torno a la media):
#H0: media del interés en gestión de residuos no depende del género (Media de hombres es igual a media de mujeres)
#H1: Media del interés en gestión de residuos si depende del género (la media es distinta)

# Aplicamos test
t_test_result <-t.test(interes_participar_ma ~ genero_dicotomico, data=base)

t_test_result
#  Interpretación
# Diferencia de medias:
  # Las mujeres tienen, en promedio, un interés ligeramente mayor que los hombres (5.99 vs. 5.62), 
  # pero esta diferencia no es estadísticamente significativa.

# Intervalo de confianza [-1.33, 0.59]:
  # Este rango incluye el 0, lo que refuerza que no se puede rechazar la hipótesis nula de igualdad de medias. 
  # Además, la amplitud indica cierta incertidumbre en la estimación.

t_test_result$p.value #p-value es mayor a 0.05 por lo que no hay evidencia que permita rechazar la hipótesis nula.
#Si bien los datos reflejan una diferencia de medias del interés en participar en gesttión de residuos entre ambos géneros,
# No existe evidencia significativa que permita rechazar la hipótesis nula 

# Valor-p = 0.447 muy por encima del umbral típico de significancia (0.05), lo que indica que no hay evidencia suficiente 
# para afirmar que las medias son distintas entre hombres y mujeres.
# Entonces, se aprueba la hipótesis nula (H0).

# Y si quiero hacer la misma prueba de hipótesis con otra variable que tenga más de dos grupos?

# Prueba ANOVA ---------------------------------------------------------------------------------------------------------
# Permite comparar las medias de tres o más grupos y determinar si al menos uno de ellos difiere significativamente 
# de los demás.
# ANOVA evalúa si la variabilidad entre grupos (por ejemplo, entre géneros, carreras, zonas, etc.) es mayor que 
# la variabilidad dentro de los grupos (entre individuos del mismo grupo).
# Si la variabilidad entre grupos es suficientemente grande, se concluye que al menos un grupo tiene una media distinta.

# Se compara el interés en participar en manejo ambiental (interes_participar_ma) entre tres grupos de identidad de género (identidad_genero_r).
 # Variable de grupo:
class(base$identidad_genero_r) #variable es caracter

 #La variable de grupo debe ser factor, así que la transformamos 
base$identidad_genero_r <- as.factor(base$identidad_genero_r)

# Análisis Anova
anova_result <- aov(interes_participar_ma ~ identidad_genero_r, data = base)

anova_result
#Interpretación
# La variabilidad explicada por la identidad de género es muy baja comparada con la variabilidad total (solo 4.9 frente a 907.8).
# Esto sugiere que la identidad de género no explica bien las diferencias en el interés por participar en manejo ambiental.
# El error estándar residual de 2.51 nos dice que, dentro de cada grupo de género, las respuestas individuales sobre interés 
# en manejo ambiental varían en promedio 2.5 puntos respecto al promedio del grupo. 
# Eso significa que hay bastante variabilidad interna,
# lo que hace más difícil encontrar diferencias significativas entre los grupos.

# Ver resultado
summary(anova_result)
# El análisis ANOVA indica que la identidad de género no tiene un efecto estadísticamente significativo sobre el interés 
# en participar en manejo ambiental, al menos en esta muestra.
# El valor-p = 0.679 es mucho mayor que el umbral típico de significancia (0.05), por lo tanto, 
# no hay evidencia estadística suficiente para afirmar que existen diferencias significativas en el nivel de interés 
# entre los distintos grupos de identidad de género.

# Hacemos una lista de pruebas diversas: 
# Aquí corremos varias pruebas estadísticas distintas para comparar grupos y guardamos sus p-valores en un mismo objeto 
# llamado tests. Las pruebas que usamos son: ANOVA: si hay normalidad y varianzas iguales; Welch: si hay normalidad, pero varianzas desiguales;
# Kruskal-Wallis (KW): si no hay normalidad; y BF (Bootstrapped F-test): una alternativa robusta.

tests <- list(
  ANOVA   = aov.test(ma_02 ~ identidad_genero_r, data = base)$p.value,
  Welch   = welch.test(ma_02 ~ identidad_genero_r, data = base)$p.value,
  BF      = bf.test(ma_02 ~ identidad_genero_r, data = base)$p.value,
  KW      = kw.test(ma_02 ~ identidad_genero_r, data = base)$p.value)
tests

## 1. Comprobar supuestos ---------------------------------------------------------

# a) Evaluamos normalidad por grupo
# Probamos si los datos se distribuyen normalmente dentro de cada grupo usando el test de Shapiro-Wilk. 
# Si el p-valor es mayor a 0.05, los datos sí parecen normales.
normalidad <- datos %>%
  group_by(identidad_genero_r) %>%      # ← agrupa primero
  shapiro_test(ma_02)                   # ← luego el test
# hay normalidad

# b) Evaluamos homogeneidad de varianzas
# Aplicamos el test de levene, que evalúa si los grupos tienen varianzas similares. 
# Es un requisito para usar el ANOVA clásico. Si el p-valor > 0.05, las varianzas pueden considerarse iguales.
varianzas <- datos %>%
  levene_test(ma_02 ~ identidad_genero_r)
# varianzas iguales

## 2. Decidir la prueba -----------------------------------------------------------
# Vamos a elegir qué prueba usar según los resultados anteriores: 
 # Si todo es normal y las varianzas son iguales → usamos ANOVA clásico.
 # Si las varianzas no son iguales → usamos Welch.
 # Si no hay normalidad, usamos Kruskal-Wallis (no requiere normalidad).
usa_anova <- all(normalidad$p > .05) && varianzas$p > .05
usa_welch <- varianzas$p <= .05                # varianzas desiguales
usa_kw    <- any(normalidad$p <= .05) && !usa_welch   # normalidad mala

## 3. Ejecutar la prueba adecuada 
# Aquí elegimos la prueba que corresponde según los supuestos anteriores. Solo una se ejecuta.
resultado <- if (usa_anova) {datos %>% anova_test(ma_02 ~ identidad_genero_r)
  } else if (usa_welch) {
  datos %>% welch_anova_test(ma_02 ~ identidad_genero_r)
    } else {
  datos %>% kruskal_test(ma_02 ~ identidad_genero_r)
      }

## 4. Revisar todo de un vistazo 
# Nos entrega un resumen ordenado con los resultados de la normalidad por grupo, el resultado del test de Levene (varianzas)
# y el resultado de la prueba estadística final
list(
  normalidad = normalidad %>% select(group = identidad_genero_r, p),
  varianzas  = varianzas %>% select(p),
  prueba     = resultado)

# Así nos aseguramos de usar una prueba que se adapte bien a los datos, y evitar errores en la interpretación. 
# Al final, solo usamos una prueba, pero guardamos y miramos todas para comparar y aprender.

