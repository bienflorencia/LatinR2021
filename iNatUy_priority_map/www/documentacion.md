---
title: "Documentación"
date: "11/02/2021"
output: 
  html_document: 
    keep_md: yes
    theme: sandstone
    # Esta opción no se está viendo reflejadas en la app:
    highlight: zenburn
editor_options: 
  chunk_output_type: console
---

# Objetivos

La biodiversidad a nivel global está disminuyendo a un ritmo sin precedentes. Para poder evaluar estos cambios hay cada vez más datos disponibles abiertamente en todo el mundo, sin embargo, en diversas regiones de Latinoamérica y el Caribe los datos de biodiversidad disponibles de manera abierta son limitados. Tal es el caso de Uruguay, en donde el [95% de su territorio permanece con muestreos insuficientes](https://doi.org/10.1038/s41598-020-79074-8). Para poder tomar mejores decisiones basadas en evidencia es sumamente crítico revertir la falta de datos primarios sobre la distribución geográfica de las especies. Para esto, la ciencia ciudadana se presenta como una herramienta comunitaria transformadora.   

Usando como base los datos ingresados en la plataforma [iNaturalist](https://www.inaturalist.org/observations?place_id=7259) para Uruguay (descargados el 21 de octubre de 2021), nos propusimos generar un mapa interactivo que ordene las áreas con déficit de datos de biodiversidad y nos permita resaltar aquellas en las que registros adicionales de biodiversidad podrían ser particularmente valiosos para llenar los vacíos de conocimiento.  

Haciendo uso de esta herramienta, usuarios y usuarias de la plataforma iNaturalist en Uruguay podrán decidir el destino de sus paseos en función de dónde registrar observaciones de la biodiversidad es más urgente y así contribuir a mejorar el conocimiento sobre la distribución de especies en el país.   

<br>

# Métricas

## Intensidad espacial

La cantidad de registros por unidad de área.

$$ IE = n_r / A_c $$

En donde $n_r$ y $A_c$ son la cantidad de registros y área para una celda determinada, respectivamente.

Por ejemplo, si en una celda se registran 5 organismos y el área de la celda es de 10 $km^2$, la IE es de 0.5.


## Intensidad temporal

La cantidad de registros con **pares año-mes** diferentes en una celda.

$$ IT = n_{año.mes} $$

En donde $n_{año.mes}$ es la cantidad de registros con **pares año-mes** diferentes para una celda determinada.

Por ejemplo, las fechas 2021-08-09 y 2021-08-31, están en el mismo par año-mes, pero las fechas 2021-08-09 y 2021-09-01 no.

<br>

## Un ejemplo ilustrativo

Supongamos que tenemos estas 4 celdas, todas de área = 10 km2, y con un total de 6 registros, cuyas fechas son las indicadas:

<img src="intensity.svg" width="100%" style="display: block; margin: auto auto auto 0;" />

<br>

- En la celda 1, tenemos 3 registros, por lo que la IE será 3/área = 0.3 (área = 10 km2). En cambio la IT es 2, ya que hay 3 registros pero **sólo hay dos combinaciones año-mes**: 2018-11 y 2021-06

- En la celda 2 tenemos 0 registros, así que tanto IE como IT serán 0.

- En la celda 3 hay un registro, así que: IE = 0.1 e IT = 1

- En la celda 4 hay dos registros del mismo día (y por lo tanto, del mismo año-mes: 2020-05), por lo que IE = 0.2 e IT = 1.  

En una tabla, así se verían los resultados hasta ahora:


```r
library(tidyverse)
tabla_ejemplo <- tibble(Celda = 1:4,
                        n = c(3, 0, 1, 2), 
                        IE = c(.3, 0, .1, .2),
                        IT = c(2, 0, 1, 1))
# Código para imprimir la tabla:
kableExtra::kbl(tabla_ejemplo) %>%
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = 'striped')
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Celda </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> IE </th>
   <th style="text-align:right;"> IT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

(n = número de registros en cada celda)

<br>

## Índice de Prioridad

La idea es combinar ambas medidas de intensidad para obtener un **índice de prioridad global**, que nos permita diferenciar celdas según sus relativas falta o presencia de datos.
En nuestra app, el IP se calcula con la función `calc_ip`, definida por el código:


```r
calc_ip <- function(it, ie, n.reg) {
  # 1. rescalamientos:
  it = scales::rescale(it, to = 0:1)
  ie = scales::rescale(ie, to = 0:1)
  # 2. suma:
  suma <- it + ie
  # 3. ranking
  r <- rank(suma, ties.method = 'min', na.last = TRUE)
  # 4. sin registros:
  # Para dar mayor prioridad a los que tienen 0 registros (innecesario?)
  r[n.reg == 0] <- 0
  # 5. rescalamiento final (ranking a percentiles):
  out <- scales::rescale(r, to = 1:0)
  return(out)
}
```

Para hacer esto hay que tener cuidado de **no inflar artificialmente ninguna de las dos intensidades**. Esa inflación puede ocurrir por el simple hecho de que estas se miden en unidades muy diferentes. En el ejemplo dado, vemos que la intensidad temporal toma valores naturalmente mucho más altos que la intensidad espacial, por el simple hecho de que la segunda refiere una la densidad (cantidad de registros sobre área, 10 km2 en este caso).

En concreto, la transformación se puede resumir con la ecuación:

$$ X' = {{ X - X_{min} } \over { X_{max} - X_{min} }} $$
En donde $X'$ es un valor rescalado, $X_{min}$ es el valor mínimo registrado para el conjunto de todas las celdas (en el ejemplo: 0.1 para IE, y 1 para IT; recordemos que los valores de la celda 2 quedan afuera de estos cálculos) y $X_{max}$ es el máximo (en el ejemplo: 0.3 para IE y 2 para IT).

Para el ejemplo anterior, los calculos de rescalamiento se pueden expresar como:

$$ IE_r = {{ IE - 0.1 } \over { 0.3 - 0.1 }} $$
$$ IT_r = {{ IT - 1 } \over { 2 - 1 }} $$

Una vez hechos los rescalamientos, la tabla del ejemplo queda así:


```r
tabla_ejemplo_resc <- 
  tabla_ejemplo %>% 
  mutate(IEr = scales::rescale(IE, to = 0:1),
         ITr = scales::rescale(IT, to = 0:1))

# Código para imprimir la tabla:
kableExtra::kbl(tabla_ejemplo_resc) %>%
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = 'striped')
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Celda </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> IE </th>
   <th style="text-align:right;"> IT </th>
   <th style="text-align:right;"> IEr </th>
   <th style="text-align:right;"> ITr </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.3333333 </td>
   <td style="text-align:right;"> 0.5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.6666667 </td>
   <td style="text-align:right;"> 0.5 </td>
  </tr>
</tbody>
</table>

<br>
Siguiendo con el ejemplo, el cálculo de IP se realizará así:


```r
tabla_ejemplo_resc_ip <- 
  tabla_ejemplo_resc %>% 
  mutate(IP = scales::rescale(IE + IT, to = 1:0))

# Código para imprimir la tabla
kableExtra::kbl(tabla_ejemplo_resc_ip) %>%
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = 'striped')
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Celda </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> IE </th>
   <th style="text-align:right;"> IT </th>
   <th style="text-align:right;"> IEr </th>
   <th style="text-align:right;"> ITr </th>
   <th style="text-align:right;"> IP </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 1.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.3333333 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.5217391 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.6666667 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4782609 </td>
  </tr>
</tbody>
</table>

Esta tabla ya está casi lista para ser usada en un mapa, a excepción de un detalle: la clasificación en categorías específicas de prioridad: **Alta**, **Media**, **Baja** o **Sin registros**. Para esto lo que necesitamos una regla que sirva para dividir los casos. Las reglas que utilizamos en la aplicación tienen cierta complejidad. De todas formas, para ilustrar este ejemplo, podemos arbitrariamente decidir que las celdas se van a clasificar según la regla:

- "**Baja**": cuando el índice de prioridad es menor o igual a 0.2
- "**Media**": cuando el índice de prioridad es mayor o igual a 0.2 y menor a 0.8
- "**Alta**": cuando el índice de prioridad es menor a 0.8
- "**Sin registros**": cuando no hay registros para la celda

<br>

A esto le agregamos dos cambios más:

- Las intensidades espacial y temporal de la celda 2, que hasta ahora figuraban como `NA`, pasan a ser 0. Esto sirve para mantener una coerencia con la cantidad de registros y facilitar operaciones matemáticas con estos valores, si es que fuera de interés. Debe notarse, sin embargo, que para los casos específicos de IE e IT, **no da lo mismo tener 0 o `NA` previo a los rescalamientos**.


```r
tabla_ejemplo_resc_ip %>%
  mutate(IP = replace_na(IP, 1)) %>%
  mutate(Etiqueta = case_when(
    n == 0 ~ "Sin registros",
    IP <= .2 ~ "Baja",
    .2 < IP & IP <= .8 ~ "Media",
    .8 < IP ~ "Alta"
  )) %>% 
  mutate_at(vars(matches("^I[ET]")), ~ replace_na(0)) %>% 
  # Código para imprimir la tabla:
  kableExtra::kbl() %>%
  kableExtra::kable_styling(full_width = FALSE, bootstrap_options = 'striped')
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Celda </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> IE </th>
   <th style="text-align:right;"> IT </th>
   <th style="text-align:right;"> IEr </th>
   <th style="text-align:right;"> ITr </th>
   <th style="text-align:right;"> IP </th>
   <th style="text-align:left;"> Etiqueta </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:left;"> Baja </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.0000000 </td>
   <td style="text-align:left;"> Sin registros </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.5217391 </td>
   <td style="text-align:left;"> Media </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.4782609 </td>
   <td style="text-align:left;"> Media </td>
  </tr>
</tbody>
</table>

