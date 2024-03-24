# Este código se corre una vez, con el fin de crear archivos con los leaflets y
# datos que serán utilizados por la aplicación shiny (i.e: los objetos finales
# necesarios)

require(tidyverse)
require(sf)
require(leaflet)

source('R/funciones.R', local = TRUE, encoding = 'UTF-8')

# Uruguay <- geouy::load_geouy(c = "Dptos")
# saveRDS(Uruguay, 'data/Uruguay_map.rds')

Uruguay <- readRDS('data/Uruguay_map.rds')

# iNatUY  <- readr::read_csv('data/observations-193326.csv', guess_max = 29000)
# saveRDS(iNatUY, 'data/iNatUY.rds')

iNatUY <- readRDS('data/iNatUY.rds')

# grid_Uruguay <-
#   sf::st_make_grid(x = Uruguay, cellsize = 24028.11413, square = F)  %>%
#   sf::st_intersection(., sf::st_union(Uruguay)) %>%
#   sf::st_as_sf() %>%
#   dplyr::mutate(grid_id = 1:nrow(.),
#                 area = sf::st_area(x)) %>%
#   filter(area != units::set_units(0,"m^2"))

# saveRDS(grid_Uruguay, 'grid_Uruguay.rds')
grid_Uruguay <- readRDS('data/grid_Uruguay.rds')

last_date <- max(iNatUY$observed_on, na.rm = TRUE)

iNatUY_GIS <- iNatUY %>%
  dplyr::mutate(year_month = paste0(lubridate::year(observed_on), '-',
                                    lubridate::month(observed_on)),
                last_year = observed_on + 365 >= last_date) %>%
  dplyr::filter(captive_cultivated == FALSE,
                coordinates_obscured == FALSE,
                quality_grade=='research',
                !is.na(taxon_species_name) & !is.na(iconic_taxon_name)) %>%
  dplyr::select(observed_on, year_month, last_year,
                taxon_id,
                scientific_name,
                class = taxon_class_name,
                order = taxon_order_name,
                family = taxon_family_name,
                genus = taxon_genus_name,
                species= taxon_species_name,
                latitude,
                longitude,
                iconic_taxon_name) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(32721)


# JOIN espacial -----
grid_join <- sf::st_join(
  x = grid_Uruguay,
  y = iNatUY_GIS %>%
    dplyr::select(taxon_id, species,
                  observed_on, year_month, last_year,
                  iconic_taxon_name),
  left = TRUE, join = sf::st_contains) %>%
  dplyr::filter(!is.na(iconic_taxon_name),
                # Animalia no sé si tiene sentido:
                iconic_taxon_name != 'Animalia')

# saveRDS(grid_join, 'data/grid_join.rds')
# grid_join <- readRDS('data/grid_join.rds')

# cantidad de particiones: 377
# grid_join$grid_id %>% unique %>% length

# mis_etiquetas <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta", "Sin registros")
# writeLines(mis_etiquetas, 'data/mis_etiquetas.txt')
mis_etiquetas <- readLines('iNatUy_priority_map/data/mis_etiquetas.txt')

# grid_join %>%
#   dplyr::filter(!is.na(iconic_taxon_name)) %>%
#   dplyr::count(iconic_taxon_name)

grid_iNatUy <-
  dplyr::mutate(grid_join, iconic_taxon_name = 'Todos') %>%
  dplyr::bind_rows(grid_join)

rownames(grid_iNatUy) <- NULL

# Esto es para tener el set completo de celdas en una tabla, con el número de
# registros para todos los casos, incluyedo ceros
grid_iNatUy_reg <-
  grid_iNatUy %>%
  sf::st_drop_geometry() %>%
  dplyr::count(grid_id, iconic_taxon_name) %>%
  tidyr::pivot_wider(id_cols = grid_id,
                     names_from = iconic_taxon_name,
                     values_from = n,
                     values_fill = list(n = 0)) %>%
  dplyr::left_join(sf::st_drop_geometry(grid_Uruguay), ., by = 'grid_id') %>%
  tidyr::pivot_longer(-1:-2,
                      names_to = 'iconic_taxon_name',
                      values_to = 'n_registros') %>%
  dplyr::mutate(n_registros = tidyr::replace_na(n_registros, 0))


# Por grupo
grid_iNatUY_ip <-
  grid_iNatUy_reg %>%
  dplyr::left_join(grid_iNatUy) %>%
  dplyr::group_by(grid_id, iconic_taxon_name) %>%
  # Indicadores:
  dplyr::summarise(
    n_registros = dplyr::first(n_registros),
    area = as.numeric(dplyr::first(area)) / 1e6,
    spatial_intensity = n_registros / area,
    temporal_intensity = dplyr::n_distinct(year_month, na.rm = TRUE),
    species_richness = dplyr::n_distinct(taxon_id, na.rm = TRUE),
    n_new_species_last_year =
      nuevas_spp(taxon_id[last_year], taxon_id[!last_year]),
    prop_new_species_last_year =
      dplyr::if_else(species_richness > 0,
                     n_new_species_last_year / species_richness,
                     0)) %>%
  dplyr::group_by(iconic_taxon_name) %>%
  # Reescalamientos:
  dplyr::mutate(
    indice_prioridad =
      calc_ip(temporal_intensity, spatial_intensity),
      etiqueta = mketiquetas(indice_prioridad, n_registros,
                           labels = mis_etiquetas)
    ) %>%
  dplyr::ungroup()

# Prueba ----
#
# Todos deben dar n = nrow(grid_Uruguay) (= 402)
grid_iNatUY_ip %>%
  group_by(iconic_taxon_name) %>%
  summarise(n = n_distinct(grid_id))

datos <- grid_Uruguay %>%
  dplyr::select(-area) %>%
  dplyr::left_join(grid_iNatUY_ip, by = 'grid_id') %>%
  sf::st_transform(crs = 4326)

saveRDS(datos, "iNatUy_priority_map/data/datos.rds")

# datos_split <- split(datos, datos$iconic_taxon_name)
# saveRDS(datos_split, "data/datos_split.rds")

# Datos x grupo:
d <- datos %>%
  dplyr::filter(iconic_taxon_name == 'Aves')

# Paleta de colores (colorblind safe):
pal <- colorFactor('RdYlBu', d$etiqueta, reverse = TRUE)

# Mapa base ------
mapa <-
  leaflet(d) %>%
  clearBounds() %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles(providers$OpenTopoMap,
                   options = providerTileOptions(noWrap = TRUE),
                   group = 'Open Topo Map') %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(noWrap = TRUE),
                   group = 'Imagen') %>%
  addPolygons(
    weight = .5,
    color = 'white',
    fillColor = ~pal(etiqueta),
    fillOpacity = .5,
    popup = ~mkpopup(grid_id, etiqueta, "Aves"),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 5,
                                        fillOpacity = .8,
                                        bringToFront = TRUE),
    group = "Grilla"
  ) %>%
  # Control de capas:
  addLayersControl(
    baseGroups = c("Open Street Map", "Open Topo Map", "Imagen"),
    overlayGroups = c("Grilla"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Leyenda de índice de prioridad
  addLegend(pal = pal,
            # values = ~ranking,
            values = ~etiqueta,
            opacity = .5,
            position = 'bottomright',
            title = 'Prioridad',
            group = 'Grilla')

mapa

# Pruebas -----
input <- list(grupo = 'Todos')
d <- data_filter(datos, input$grupo) %>%
  mutate(etiqueta = mketiquetas(indice_prioridad, labels = mis_etiquetas))

pal <- colorFactor('RdYlBu', d$etiqueta, reverse = TRUE)

m1 <- leaflet() %>%
  fitBounds(-58.8, -35.2, -52.8, -29.9) %>%
  addProviderTiles(providers$OpenTopoMap,
                   options = providerTileOptions(noWrap = TRUE),
                   group = 'Open Topo Map') %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(noWrap = TRUE),
                   group = 'Imagen') %>%
  addTiles(group = "Open Street Map") %>%
  # Control de capas:
  addLayersControl(
    baseGroups = c("Open Street Map", "Open Topo Map", "Imagen"),
    overlayGroups = c("Grilla"),
    options = layersControlOptions(collapsed = FALSE)
  )

m1

m1 %>%
  addPolygons(
    data = d,
    weight = .5,
    color = 'white',
    fillColor = ~pal(etiqueta),
    fillOpacity = .5,
    popup = ~mkpopup(grid_id, etiqueta, input$grupo),
    highlightOptions = highlightOptions(color = "white",
                                        weight = 5,
                                        fillOpacity = .8,
                                        bringToFront = TRUE),
    group = "Grilla"
  ) %>%
  # Leyenda de índice de prioridad
  addLegend(
    data = d,
    pal = pal,
    values = ~etiqueta,
    opacity = .5,
    position = 'bottomright',
    title = 'Prioridad',
    group = 'Grilla'
    )
