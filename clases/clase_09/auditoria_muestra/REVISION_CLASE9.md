# Revisión del cuaderno clase9

## Recomendación

Para este cuaderno conviene usar `casen2020_clase9_todos_5variables.sav`: contiene las 185.437 personas originales, pero solo región, comuna, sexo, edad y y1. Ocupa 3,30 MB frente a 275,56 MB del SAV original (98,8 % menos, unas 84 veces menor). Reabrí el archivo y comprobé igualdad exacta de valores, ausentes y etiquetas de las cinco variables. Por tanto, conserva los resultados de todos los análisis del cuaderno, incluidos los extremos y las comparaciones por comuna.

La muestra de 20.000 personas ocupa menos, pero las estimaciones por comuna pueden variar mucho. Se conserva como alternativa para ejercicios generales.

## Qué se verificó

Se leyó el cuaderno proporcionado en Descargas sin modificarlo. Sus paquetes de análisis necesarios son tidyverse, haven y DescTools; ggthemes, hrbrthemes e ineq no se usan en los procedimientos ejecutados.

Se ejecutaron las 40 celdas originales de análisis completas con ambas bases. Las seis celdas con huecos activos se identificaron como ejercicios, no como fallos de datos. En la versión adaptada se ejecutaron 46 celdas activas con la base completa de las variables de clase y con la muestra. Además, se completaron y ejecutaron por separado nueve ejercicios de edad, Valparaíso, Gini por comuna y gráficos en cada base: 18 comprobaciones. Los ejercicios del cuaderno entregado siguen sin resolver.

La carga del SAV pequeño se verificó utilizando la propia celda de carga del cuaderno. Los códigos de región 13 y 5 y la conversión de etiquetas con as_factor se conservaron. Se construyeron todos los gráficos activos y se revisaron visualmente 24 gráficos, incluidos los ejercicios de edad, en ambas bases. No se instaló ni actualizó software para probar el cuaderno: los paquetes de análisis ya estaban disponibles. Las pruebas fueron en R local, no en una sesión real de Colab con múltiples estudiantes.

## Qué mantiene la muestra de 20.000

Todos los cálculos son descriptivos y sin ponderadores.

| Procedimiento | Original | Muestra de 20.000 |
|---|---:|---:|
| Media de y1 | $653.892 | $651.583 |
| Mediana de y1 | $400.000 | $400.000 |
| Desviación estándar de y1 | $709.222 | $706.509 |
| Primer cuartil de y1 | $320.000 | $320.000 |
| Tercer cuartil de y1 | $750.000 | $720.000 |
| Percentil 95 de y1 | $1.800.000 | $1.800.000 |
| Percentil 99 de y1 | $3.500.000 | $3.500.000 |
| Gini de y1 | 0,42556 | 0,42465 |
| Media de edad | 38,402 | 38,427 |
| Mediana de edad | 37 | 37 |
| Desviación estándar de edad | 22,843 | 22,914 |
| Máximo de edad | 110 | 102 |
| Mínimo de y1 | $1.200 | $10.000 |
| Máximo de y1 | $25.000.000 | $12.000.000 |

Los histogramas de y1 con intervalos comunes de $100.000 y $200.000 tienen diferencias máximas de 0,32 y 0,35 puntos porcentuales por intervalo. En edad, con intervalos comunes de cinco años, la diferencia máxima es 0,33 puntos porcentuales. Las formas generales y los diagramas de caja son parecidos; los conteos y extremos no son iguales. Las escalas automáticas pueden cambiar al disminuir el máximo.

Las medias de ingreso por sexo cambian aproximadamente +0,15 % en hombres y −1,03 % en mujeres; sus medianas se mantienen. Por región, el mayor cambio relativo de la media es −5,34 % en Tarapacá. Algunos Gini regionales y sus posiciones en un ordenamiento también cambian.

## Límite principal de la muestra: comunas

Aunque aparecen las 324 comunas de la base original, 220 tienen menos de diez valores válidos de y1 y ocho no tienen ninguno. Los Gini y los rankings de medias por comuna no deben esperarse iguales. En la Región Metropolitana, la mediana de casos con ingreso válido por comuna es de 14, con un rango de cero a 143. Los cálculos se pueden practicar, pero un ranking no es una comparación sólida de la situación de cada comuna. Por eso, para seguir el cuaderno completo recomendamos conservar todas las filas y reducir solo columnas.

## Cambios de funcionamiento en las copias

- Carga del SAV pequeño, conservando los códigos y etiquetas. El CSV con nombres de regiones no es intercambiable directamente con `region == 13`.
- Instalación condicional de los tres paquetes utilizados, en vez de instalaciones y actualizaciones repetidas.
- Ejercicios con huecos comentados: Ejecutar todo no se detiene en expresiones incompletas.
- Conteo `n_validos` en resúmenes de y1 por grupo; mínimo y máximo devuelven NA cuando un grupo no tiene ingreso registrado.
- `na.rm = TRUE` al calcular el mínimo usado como límite del histograma.
- `coord_cartesian()` para acercar la vista sin quitar datos antes de calcular las cajas o el histograma.
- Corrección de explicaciones que confundían media con mediana de la caja, dispersión con error promedio, y resultados sin ponderar con cifras nacionales. Se retiraron comparaciones de salarios entre países sin fuente y cortes universales del Gini.

Los archivos recomendados son `../clase9_base_liviana.ipynb` y `../datos/casen2020_clase9_todos_5variables.sav`. La alternativa con menos filas es `../clase9_muestra_20000.ipynb` con `../datos/casen2020_clase9_20000.sav`. Los CSV de esta carpeta conservan los registros detallados de ejecución y las comparaciones.

Fuente de la base original: Ministerio de Desarrollo Social y Familia, Casen en Pandemia 2020, archivo SPSS revisado en septiembre de 2022: https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-en-pandemia-2020
