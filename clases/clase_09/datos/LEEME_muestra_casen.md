# Base didáctica de Casen 2020 para la clase 9

Fuente: Ministerio de Desarrollo Social y Familia, Encuesta Casen en Pandemia 2020, archivo SPSS revisado en septiembre de 2022.
https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020

20.000 personas seleccionadas sin reemplazo de las 185.437 de la base original. Muestreo aleatorio dentro de grupos de región, sexo y tramo de y1 (deciles o ausencia), con asignación proporcional redondeada y semilla 20261116. Se conservan las filas originales: no hay datos simulados ni ingresos imputados adicionales. No es una muestra aleatoria simple ni una reproducción del diseño muestral oficial.

CSV y SAV contienen los mismos casos. El CSV lleva etiquetas en las variables categóricas; el SAV conserva códigos y etiquetas SPSS. Las cinco variables son:

| Variable | Significado |
|---|---|
| region | Región, con su etiqueta original |
| comuna | Comuna, con su etiqueta original |
| sexo | Sexo registrado en la encuesta: Hombre/Mujer |
| edad | Edad en años |
| y1 | Monto mensual del sueldo o salario líquido del trabajo principal |

`y1` no es ingreso total de cada persona ni del hogar. Hay 4.359 valores válidos y 15.641 ausentes. Un valor ausente no significa ingreso cero. Usar `na.rm = TRUE` al resumirlo y reportar el número de valores válidos de cada grupo.

## Verificación sin ponderar

| Indicador | Base original | Muestra didáctica |
|---|---:|---:|
| Personas | 185.437 | 20.000 |
| Media de edad | 38,40 | 38,43 |
| Mediana de edad | 37 | 37 |
| Sueldo y1 medio, pesos | 653.892 | 651.583 |
| Sueldo y1 mediano, pesos | 400.000 | 400.000 |
| Percentil 99 de y1, pesos | 3.500.000 | 3.500.000 |
| Máximo de y1, pesos | 25.000.000 | 12.000.000 |
| Comunas presentes | 324 | 324 |

La mayor diferencia absoluta de porcentajes marginales de región/sexo es 0,0175 puntos porcentuales. Otras distribuciones y cruces pueden cambiar. Una submuestra no garantiza conservar extremos, medias o Gini: el ingreso máximo original no resultó seleccionado. Las comparaciones verificadas están en `../auditoria_muestra/` y el procedimiento en `../crear_muestra_casen.R`.

## Cargar en Google Colab con R

Subir `casen2020_clase9_20000.csv` desde el panel Archivos. Reemplazar la lectura del SAV grande por:

```r
datos <- read.csv("/content/casen2020_clase9_20000.csv",
                  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
str(datos)
sum(!is.na(datos$y1))
mean(datos$y1, na.rm = TRUE)
```

`y1` y `edad` ya son numéricos. No hay que convertir factores a números. Las etiquetas de región y sexo permiten conservar los filtros existentes del script.

Como alternativa compatible con el lector original de SPSS, subir el SAV pequeño y usar:

```r
datos <- foreign::read.spss("/content/casen2020_clase9_20000.sav",
                           to.data.frame = TRUE, use.value.labels = TRUE)
```

## Uso en clase

Permite practicar medias, medianas, varianza, dispersión, cuantiles, histogramas y cajas, así como comparaciones descriptivas por sexo o región. Revisar siempre el número de ingresos válidos antes de comparar grupos. Que una comuna aparezca no garantiza que tenga suficientes casos con ingreso: las comparaciones por comuna y los Gini de grupos pequeños son especialmente inestables.

Las proporciones se aproximan a las de las filas de la base original, no directamente a las de la población de Chile. La selección no conserva automáticamente la validez de los factores de expansión y del diseño complejo de Casen. Para resultados oficiales se debe usar la base completa, los ponderadores y el diseño correspondiente; esta base es para ejercicios didácticos.
