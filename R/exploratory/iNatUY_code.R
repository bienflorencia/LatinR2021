library(geouy)
library(vegan)
library(spaa)
library(scales)
library(sf)
library(tidyverse)

#----------------------- DATOS

# Capa de Uruguay
Uruguay <- load_geouy(c = "Dptos")

# Uruguay - grillas de 10x10 km
grid_Uruguay <- sf::st_make_grid(x = Uruguay, cellsize = 25000, square = FALSE)  %>%
  st_intersection(.,st_union(Uruguay)) %>% st_as_sf() %>%
  mutate(grid_ID = 1:nrow(.))

# GBIF.org (06 July 2021) GBIF Occurrence Download  https://doi.org/10.15468/dl.yx22rw
iNatUY_GBIF <- read_tsv("data/0317842-200613084148143/occurrence.txt", guess_max = 11000)

iNatUY_GBIF_GIS <- iNatUY_GBIF %>%
  mutate(year = lubridate::year(eventDate)) %>%
  select(species, scientificName, kingdom, class, order, family,  genus, specificEpithet, infraspecificEpithet,
                       eventDate, year, recordedBy, identifiedBy, iucnRedListCategory, taxonRank,
                       countryCode, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, coordinatePrecision) %>%
  filter(taxonRank != "FAMILY" & taxonRank != "GENUS") %>%
  as.data.frame %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>% st_transform(32721)

# Observaciones descargadas de iNaturalist con "Uruguay" como localización geográfica (06 July 2021)
iNatUY <- read_csv("data/observations-175157.csv", guess_max = 25000)

iNatUY_GIS <- iNatUY %>%
  mutate(year = lubridate::year(observed_on)) %>%
  filter(!captive_cultivated & coordinates_obscured == FALSE) %>%
  filter(!is.na(taxon_species_name)) %>%
  select(observed_on, year,
         scientifiName = scientific_name, class = taxon_class_name,
         order = taxon_order_name, family = taxon_family_name,
         genus = taxon_genus_name, species = taxon_species_name,
         decimalLatitude = latitude, decimalLongitude = longitude,
         coordinatePrecision = positional_accuracy,
         public_positional_accuracy,
         iconic_taxon_name) %>%
  as.data.frame %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(32721)


grid_iNatUY_GIS <- st_join(x = grid_Uruguay,
                           y = select(iNatUY_GIS, species, class, family, year),
                           left = TRUE, join = st_contains) %>%
  group_by(grid_ID) %>%
  summarise(spatialIntensity = ifelse(n_distinct(species, na.rm = TRUE) == 0, 0, n()),
            speciesRichness = n_distinct(species, na.rm = TRUE),
            lastYearRecorded = ifelse(spatialIntensity == 0, NA, max(year, na.rm = TRUE)),
            temporalIntensity = n_distinct(year, na.rm = TRUE), .groups = "drop") %>%
  mutate(spatialIntensity = ifelse(spatialIntensity == 0, NA, rescale(spatialIntensity, to = c(0, 1))),
         lastYearRecorded = rescale(lastYearRecorded, to = c(0, 1)),
         temporalIntensity = ifelse(temporalIntensity == 0, NA, rescale(temporalIntensity, to = c(0, 1))))

grid_iNatUY_GIS %>% arrange((spatialIntensity))
grid_iNatUY_GIS %>%
  mutate(lastYearRecorded = rescale(lastYearRecorded, to = c(0, 1))) %>%
  st_drop_geometry() %>%
  arrange((lastYearRecorded)) %>%
  print.data.frame()

grid_iNatUY_GIS %>%
  ggplot() +
  aes(fill = temporalIntensity) +
  geom_sf() +
  scale_fill_fermenter(
    palette = "Reds",
    na.value = "#ede8e8",
    n.breaks = 6
    ) +
  geom_sf(data = Uruguay, color = "black", fill = NA) +
  theme_bw() +
  labs(fill = "Índice temporal\n(densidad de registros)")


grid_iNatUY_GIS %>%
  ggplot() +
  aes(fill = spatialIntensity) +
  geom_sf() +
  scale_fill_fermenter(palette = "Greens", na.value = "#ede8e8", n.breaks = 6) +
  geom_sf(data = Uruguay, color = "black", fill = NA) +
  theme_bw() +
  labs(fill = "Índice espacial\n(densidad de registros)")

ggplot() +
  geom_sf(data = grid_iNatUY_GIS, aes(fill = lastYearRecorded)) +
  scale_fill_fermenter(palette = "Blues", na.value = "#ede8e8", n.breaks = 6) +
  geom_sf(data = Uruguay, color = "black", fill = NA) +
  theme_bw() +
  labs(fill = "Índice temporal\n(año del último registro)")


grid_iNatUY_GIS %>%
  mutate(index = temporalIntensity + spatialIntensity) %>%
  ggplot() +
  geom_sf(aes(fill = rescale(index, to = c(0, 1)))) +
  scale_fill_fermenter(
    palette = "Spectral",
    direction = 1,
    na.value = "#ede8e8",
    n.breaks = 6
    ) +
  geom_sf(data = Uruguay, colo = "black", fill = NA) +
  theme_bw() +
  labs(fill = "Prioridad de Observación")

#----------------------- FUNCIONES

# Cálculo de "completeness"
get_gridsCompleteness <- function(data_abundance) {
  GridSlope <- data.frame(Grid = integer(),
                          Slope = numeric(),
                          stringsAsFactors = FALSE)
  data_abundance <- as.data.frame(data_abundance)
  data_abundance$abundance <- as.integer(1)
  cells <- unique(data_abundance$GridID)
  splistT <- list()
  spaccum <- list()
  slope <- list()
  for (i in cells) {
    splist <- data_abundance[data_abundance$GridID == i, c(2:4)]
    splistT[[i]] = data2mat(splist)
    spaccum[[i]] = specaccum(splistT[[i]], method = "exact")
    slope[[i]] = (spaccum[[i]][[4]][length(spaccum[[i]][[4]])]-
                    spaccum[[i]][[4]][ceiling(length(spaccum[[i]][[4]])*0.9)])/
      (length(spaccum[[i]][[4]])- ceiling(length(spaccum[[i]][[4]])*0.9))
    GridSlope_i <- data.frame(Grid = i, Slope = slope[[i]], stringsAsFactors = FALSE)
    GridSlope <- rbind(GridSlope, GridSlope_i)
  }
  return(GridSlope)
}

# Cálculo de aislamiento espacial
# Cálculo de aislamiento temporal

get_temporalIsolation <- function(dataset){
  for(i in length(year)) {
    isolated <- df %>%
      dplyr::filter(OBSERVATION_DATE >= mindate, OBSERVATION_DATE <= maxdate) %>%
      count(latlong) %>%
      distinct()
  }
  isolated$isolation <- rescale(isolated$n, to = c(1, 0))

  isolation <- left_join(isolated, df, by = "latlong")
  return(isolation)
}
