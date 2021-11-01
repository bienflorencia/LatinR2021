Este archivo es temporal, luego de ubicar estos textos en sus lugares adecuados, se puede borrar

## Función [`mketiquetas`](https://github.com/bienflorencia/LatinR2021/blob/610d5db05be34941ef1540c93eb0cfbfa93e5ce4/iNatUy_priority_map/R/funciones.R#L189)

> Escrito el 22/10/2021

Esta función intenta factorizar un vector de índices de prioridad (IP), en donde cada valor corresponde a una de las celdas en las que se divide el territorio. En el caso ideal, los niveles del factor resultante estarán representados todos en igual cantidad, porque el criterio apunta a dividir según percentiles (aunque los percentiles no son nombrados directamente, sino que se definen por implicancia: si la cantidad de niveles resultante es 5, entonces los percentiles necesariamente serán 0%-20%, 20%-40%, etc).

Descripción de lo que hace la función:

1. ordena las celdas según `indice_prioridad` (IP) y les asigna un rank (1 a la de menor IP, 2 a la segunda, etc...); ojo: esto sólo con las que IP no es NA. El resultado es el vector r (de rank)

Nota: al usar la función rank con el argumento ties.method = 'max' ocurre algo inesperado: los valores en r no necesariamente van del 1 al n (número de elementos a rankear) (por ejemplo, puede ir del 98 al 148, porque los 98 valores más bajos son iguales, un "empate", en cuyo caso el ties.method define que debe asignarse el valor máximo, o sea 98)

2. corta (con la función cut)  al r. Los puntos de corte se definen según la cantidad (argumento n de mketiquetas) y el rango de valores presentes en r (si rg es el rango de valores de r, entonces los puntos de corte son diff(rg) * probs + rg[1]).

O SEA: ponele que r tiene el ranking de las celdas según IP y ese ranking va del 98 al 148. Yo quiero partir en 5 partes ~iguales el conjunto. Bueno, entonces todos los que están entre 98 < r <=  108 van al primer grupo ("Muy baja"), luego los que 108 < r <= 118 van al grupo 2 ("Baja"), luego 118 < r <= 128, luego 128 < r <= 138 y luego 138 < r.

3. Todavía queda el tema de los que IP = NA. Esos son  puestos en una categoría aparte (grupo 6, también conocido como "Sin registros").
