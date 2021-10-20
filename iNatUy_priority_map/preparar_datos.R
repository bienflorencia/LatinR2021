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
# iNatUY  <- readr::read_csv('../data/observations-175157.csv', guess_max = 25000)
# saveRDS(iNatUY, 'data/iNatUY.rds')
iNatUY <- readRDS('data/iNatUY.rds')

# grid_Uruguay <- 
#   sf::st_make_grid(x = Uruguay, cellsize = 25000, square = FALSE)  %>% 
#   st_intersection(., sf::st_union(Uruguay)) %>% 
#   sf::st_as_sf() %>% 
#   dplyr::mutate(grid_id = 1:nrow(.), 
#                 area = sf::st_area(x))
# saveRDS(grid_Uruguay, 'data/grid_Uruguay.rds')
grid_Uruguay <- readRDS('data/grid_Uruguay.rds')

last_date <- max(iNatUY$observed_on, na.rm = TRUE)

iNatUY_GIS <- iNatUY %>% 
  dplyr::mutate(year_month = paste0(lubridate::year(observed_on), '-', 
                                    lubridate::month(observed_on)),
                last_year = observed_on + 365 >= last_date) %>% 
  dplyr::filter(captive_cultivated == FALSE,
                coordinates_obscured == FALSE) %>% 
  dplyr::filter(!is.na(taxon_species_name) & !is.na(iconic_taxon_name)) %>% 
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
  left = TRUE, join = sf::st_contains)

# saveRDS(grid_join, 'data/grid_join.rds')
# grid_join <- readRDS('data/grid_join.rds')

# cantidad de particiones: 377
# grid_join$grid_id %>% unique %>% length

# mis_etiquetas <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta", "Sin registros")
# writeLines(mis_etiquetas, 'data/mis_etiquetas.txt')
mis_etiquetas <- readLines('data/mis_etiquetas.txt')

# grid_join %>%
#   dplyr::filter(!is.na(iconic_taxon_name)) %>% 
#   dplyr::count(iconic_taxon_name)

grid_iNatUY_ranking <- 
  grid_join %>%
  dplyr::filter(!is.na(iconic_taxon_name)) %>% 
  dplyr::group_by(grid_id) %>% 
  # Indicadores:
  dplyr::summarise(
    area = as.numeric(dplyr::first(area)),
    spatial_intensity = dplyr::n() / area,
    temporal_intensity = dplyr::n_distinct(year_month, na.rm = TRUE),
    species_richness = dplyr::n_distinct(taxon_id, na.rm = TRUE),
    n_new_species_last_year = 
      nuevas_spp(taxon_id[last_year], taxon_id[!last_year]),  
    prop_new_species_last_year = n_new_species_last_year / species_richness, 
    .groups = 'drop') %>% 
  # Reescalamientos:
  dplyr::mutate(
    spatial_intensity = scales::rescale(spatial_intensity, to = 0:1),
    temporal_intensity = scales::rescale(temporal_intensity, to = 0:1),
    indice_prioridad = 
      scales::rescale(temporal_intensity + spatial_intensity, to = 1:0),
    ranking = rank(indice_prioridad, ties.method = 'max', na.last = TRUE) %>%
      scales::rescale(to = 1:0)
    # etiqueta = domain2labels(indice_prioridad, 5, labels = mis_etiquetas)
    )

# Por grupo
grid_iNatUY_ranking_groups <-
  grid_join %>%
  dplyr::filter(!is.na(iconic_taxon_name)) %>% 
  dplyr::group_by(grid_id, iconic_taxon_name) %>% 
  # Indicadores:
  dplyr::summarise(
    area = as.numeric(dplyr::first(area)),
    spatial_intensity = n() / area,
    temporal_intensity = dplyr::n_distinct(year_month, na.rm = TRUE),
    species_richness = dplyr::n_distinct(taxon_id, na.rm = TRUE),
    n_new_species_last_year = 
      nuevas_spp(taxon_id[last_year], taxon_id[!last_year]),  
    prop_new_species_last_year = n_new_species_last_year / species_richness) %>% 
  # Reescalamientos:
  dplyr::mutate(
    spatial_intensity = scales::rescale(spatial_intensity, to = 0:1),
    temporal_intensity = scales::rescale(temporal_intensity, to = 0:1),
    indice_prioridad = 
      scales::rescale(temporal_intensity + spatial_intensity, to = 1:0),
    ranking = rank(indice_prioridad, ties.method = 'max', na.last = TRUE) %>%
      scales::rescale(to = 1:0)
    # etiqueta = domain2labels(indice_prioridad, 5, labels = mis_etiquetas)
    ) %>% 
  dplyr::ungroup() %>% 
  sf::st_drop_geometry()  %>% 
  tidyr::pivot_wider(
    id_cols = grid_id, 
    names_from = iconic_taxon_name, 
    values_from = c(indice_prioridad,
                    spatial_intensity,
                    temporal_intensity,
                    # etiqueta,
                    ranking,
                    species_richness,
                    n_new_species_last_year,
                    prop_new_species_last_year),
    values_fill = list(species_richness = 0, 
                       n_new_species_last_year = 0,
                       prop_new_species_last_year = 0))

datos <- grid_Uruguay %>% 
  dplyr::left_join(sf::st_drop_geometry(grid_iNatUY_ranking[-2]), 
                   by = 'grid_id') %>% 
  dplyr::left_join(grid_iNatUY_ranking_groups, by = 'grid_id') %>% 
  sf::st_transform(crs = 4326)

  # dplyr::mutate_at(dplyr::vars(tidyselect::starts_with('etiqueta')), 
  #                  ~ tidyr::replace_na(., mis_etiquetas[length(mis_etiquetas)]))

# saveRDS(datos, "data/datos.rds")

# Datos x grupo:
d <- datos %>% 
  data_filter(grupo = 'Aves') %>% 
  dplyr::mutate(etiqueta = domain2labels(indice_prioridad, 
                                         labels = mis_etiquetas))

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
  mutate(etiqueta = domain2labels(indice_prioridad, labels = mis_etiquetas))
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
