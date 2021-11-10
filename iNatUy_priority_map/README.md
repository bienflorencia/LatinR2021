# Sobre la app

[En construcción]

## Pendientes:

> Una lista parcial con posibles incorporaciones a la app y/o datos

- Celdas con nombres? Ir asignando, a dedo, nombres a celdas... no importa cubrir todas. Por ejemplo: quebrada de los cuervos. Eventualmente, la app podría tener una función para seleccionar esas celdas o algo así.

- En la misma que la anterior, se podría pensar en departamentos, seccionales policiales, etc (cuencas?), como para elegir las celdas correspondientes

- Enlaces: [presentación en YouTube](https://youtu.be/8qltR0p_Czg), diapositivas?

- Incluir opción para elegir a qué nivel taxonómico se evalúa la riqueza (ahora es riqueza de especies, pero podría ser género, órdenes, familias...). Esto se me ocurrió pensando en que en algunos grupos, como insectos, arácnidos, briofitas, etc, la identificación a nivel de especie suele ser difícil. Este agregado podría implicar un cambio en la creación de la tabla de datos (`datos`) de base, ya que al momento filtramos para que queden solamente aquellas observaciones identificadas a nivel de especie. De esta forma, podría ser que estemos sesgando los datos a organismos de identificación más fácil (generalmente, aquellos de mayor tamaño). Concretamente, me inclino por crear 4 tablas: `datos_spp`, `datos_genero`, `datos_familia`, `datos_orden`, cada una con sus especificaciones en el procesamiento (i.e.: en vez de usar `taxon_id` tanto para filtrar los `NA`s, como para medir riqueza, se usarían otras columnas: `taxon_order_name`, `taxon_family_name`, `taxon_genus_name`, `taxon_species_name`; incluso haría una función que fabrique la tabla según el nivel taxonómico deseado, para evitar código redundante y disminuir chances de error).

- Agregar la métrica pendiente en el último tramo de la curva de acumulación, así como los gráficos correspondientes. Esta idea está hace rato, no se ha implementado por un tema de tiempos más que nada, ya que no solo implica bastante código, sino que también documentación para que pueda ser entendida por el público general.

- Descargas? Por ejemplo, descargar datos de la celda que se está visualizando, etc...

- Marcadores con observaciones individuales? Seguro que se puede, habría que ver qué tanto aporta.
