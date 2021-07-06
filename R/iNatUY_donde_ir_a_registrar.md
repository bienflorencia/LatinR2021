# ¿Dónde ir a registrar si sos un naturalista?

La idea de este análisis es resaltar áreas donde mayores observaciones
serían particularmente valiosas para rellenar vacíos de información.
Usaremos como base los datos ingresados en la plataforma
[iNaturalist](inaturalist.org) para Uruguay.

Un primer acercamiento a esta idea sería usar la ‘completitud’ de
muestreo por unidad espacial, el grado de aislamiento espacial y
temporal. Empezaremos por las últimas dos variables, y sumaremos luego
la completitud, generando curvas de acumulación de especies por grilla.

## Paquetes

    library(geouy)
    library(vegan)
    library(spaa)
    library(scales)
    library(sf)
    library(tidyverse)

## Datos

Usamos la capa de Departamentos de Uruguay y generamos una grilla de
celdas hexagonales de 25x25km de tamaño

    # Capa de Uruguay
    Uruguay <- load_geouy(c = "Dptos")

    ## Reading layer `c004' from data source `https://www.ambiente.gub.uy/geoserver/u19600217/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=u19600217:c004' using driver `GML'
    ## Simple feature collection with 20 features and 6 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -58.43935 ymin: -34.97398 xmax: -53.18105 ymax: -30.0855
    ## Geodetic CRS:  WGS 84

    # Uruguay - grillas de 10x10 km
    grid_Uruguay <- sf::st_make_grid(x = Uruguay, cellsize=25000, square = F)  %>% 
      st_intersection(.,st_union(Uruguay)) %>% st_as_sf() %>% 
      mutate(grid_ID = 1:nrow(.))

Descargamos observaciones en iNaturalist usando como criterio,
observaciones con ‘Uruguay’ como localización geográfica. El archivo fue
generado el 6 de julio de 2021. Limpiamos el conjunto de datos filtrando
registros de especies cultivadas/captivas, registros con coordenadas
oscuras, y registros que no han sido indentificados a nivel de especie.

    iNatUY <- read_csv('../data/observations-175157.csv', guess_max = 25000)

    iNatUY_GIS <- iNatUY %>% 
      mutate(year=lubridate::year(observed_on)) %>% 
      filter(captive_cultivated==FALSE & coordinates_obscured==FALSE) %>% 
      filter(!is.na(taxon_species_name)) %>% 
      select(observed_on, year,
             scientifiName=scientific_name, class=taxon_class_name, order=taxon_order_name, family=taxon_family_name, genus=taxon_genus_name, species=taxon_species_name,
             decimalLatitude=latitude, decimalLongitude=longitude, coordinatePrecision=positional_accuracy, public_positional_accuracy, iconic_taxon_name) %>% 
      as.data.frame %>% 
      sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>% 
      st_set_crs(4326) %>% st_transform(32721)

## Análisis

Calculamos las siguientes métricas por grilla:  
- La densidad de registros (como el número de observaciones) - La
riqueza de especies (el número de especies) - El último año en el que se
observó un organismo - La densidad temporal de los registros (como la
cantidad de años con registros)

Todos estos valores se re-escalaron a valores entre 0 y 1.

    grid_iNatUY_GIS <- st_join(x=grid_Uruguay,
                               y= iNatUY_GIS %>%
                                 select(species, class, family, year),
                               left=TRUE, join = st_contains) %>%
      group_by(grid_ID) %>%
      summarise(spatialIntensity=ifelse(n_distinct(species, na.rm = TRUE)==0, 0, n()),
                speciesRichness=n_distinct(species, na.rm = TRUE), 
                lastYearRecorded=ifelse(spatialIntensity==0, NA, max(year, na.rm = TRUE)),
                temporalIntensity=n_distinct(year, na.rm = TRUE), .groups = 'drop') %>% 
      mutate(spatialIntensity=ifelse(spatialIntensity==0, NA, rescale(spatialIntensity, to=c(0,1))),
             lastYearRecorded=rescale(lastYearRecorded, to=c(0,1)),
             temporalIntensity=ifelse(temporalIntensity==0, NA, rescale(temporalIntensity, to=c(0,1))))

## Algunos mapas mostrando las métricas calculadas

![Índice temporal (densidad de registros en el
tiempo)](iNatUY_donde_ir_a_registrar_files/figure-markdown_strict/unnamed-chunk-6-1.png)

![Índice espacial (densidad de
registros)](iNatUY_donde_ir_a_registrar_files/figure-markdown_strict/unnamed-chunk-7-1.png)

## Cálculo del índice

El índice combina valores de intensidad temporal (cantidad de años con
registros por celda) y espacial (cantidad de registros por celda). Las
grillas que se acercan al rojo combinan, por lo tanto, zonas donde han
habido pocos registros que a su vez se han registrado en menos años.

    grid_iNatUY_GIS %>% mutate(index=temporalIntensity+spatialIntensity) %>% 
      ggplot() +
      geom_sf(aes(fill=rescale(index, to=c(0,1)))) +  #show.legend = F
      scale_fill_fermenter(palette = 'Spectral', direction = 1, na.value="#79002d", n.breaks=8) +
      geom_sf(data = Uruguay, color='black', fill=NA) +
      theme_bw() +
      labs(fill='Prioridad de Observación')

![Zonas prioritarias donde ir a registrar
observaciones](iNatUY_donde_ir_a_registrar_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Las grillas con valores de **rojo** más intenso, son sitios donde hasta
el momento NO existen observaciones cargadas en la plataforma
iNaturalist para Uruguay.
