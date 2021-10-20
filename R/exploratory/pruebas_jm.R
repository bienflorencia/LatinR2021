library(tidyverse)
library(sf)
library(vegan)

iNatUY <- read_csv('data/observations-175157.csv', guess_max = 25000)
Uruguay <- read_sf('data/Uruguay.shp')
grid_join <- readRDS('data/grid_join.rds')
mapita <- readRDS('data/mapita.rds')
source('R/funciones_jm.R', encoding = 'UTF-8', local = TRUE)

# load('data/grid_iNatUY_GIS.RData')

# Grilla UY ----
grid_Uruguay <-
  sf::st_make_grid(x = Uruguay, cellsize = 25000, square = F)  %>%
  st_intersection(.,st_union(Uruguay)) %>%
  st_as_sf() %>%
  mutate(grid_ID = 1:nrow(.))

last_date <- max(iNatUY$observed_on, na.rm = TRUE)

# Registros como SIG ----
iNatUY_GIS <- iNatUY %>%
  mutate(year = lubridate::year(observed_on),
         last_year = observed_on + 365 >= last_date) %>%
  filter(captive_cultivated == FALSE,
         coordinates_obscured == FALSE) %>%
  filter(!is.na(taxon_species_name)) %>%
  select(observed_on, year, last_year,
         taxon_id,
         scientifiName = scientific_name,
         class = taxon_class_name,
         order = taxon_order_name,
         family = taxon_family_name,
         genus = taxon_genus_name,
         species = taxon_species_name,
         decimalLatitude = latitude,
         decimalLongitude = longitude,
         coordinatePrecision = positional_accuracy,
         public_positional_accuracy,
         iconic_taxon_name) %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_transform(32721)

# JOIN espacial -----
grid_join <- st_join(x = grid_Uruguay,
                     y = iNatUY_GIS %>%
                       select(taxon_id, species, class, family,
                              observed_on, year, last_year,
                              iconic_taxon_name),
                     left = TRUE, join = st_contains)
# saveRDS(grid_join, 'data/grid_join.rds')

# Curvas de acumulación JM -----

# . `tabla` ----
#
# Perdón por el nombre poco desciptivo, es que la usé mucho
#
# grid_join <- readRDS('../data/grid_join.rds')
tabla <- grid_join %>%
  sf::st_drop_geometry() %>%
  dplyr::arrange(observed_on) %>%
  dplyr::filter(!is.na(taxon_id), !is.na(iconic_taxon_name))

## . Todo ----
acum <- curva_acum_jm(tabla$taxon_id)
plot(acum, type = 's', ylab = '# Especies', xlab = '# Observaciones')

## . Icónicos ----
plot(curva_acum_jm(tabla$iconic_taxon_name), type = 's',
     ylab = '# Grupos icónicos', xlab = '# Observaciones')

## . Familias ----
acum_fam <- curva_acum_jm(tabla$family)
plot(acum_fam, type = 's', ylab = '# Familias', xlab = '# Observaciones')

## . Icónicos x Spp -----
p <- tabla %>%
  group_by(iconic_taxon_name) %>%
  mutate(c_acum = curva_acum_jm(taxon_id),
         n_obs = row_number()) %>%
  ggplot() +
  aes(n_obs, c_acum, color = iconic_taxon_name) +
  geom_line() +
  ylab('# Especies') +
  xlab('# Observaciones')
print(p)
plotly::ggplotly(p) # Fancy!

## . Icónicos x Familias -----
p <- tabla %>%
  group_by(iconic_taxon_name) %>%
  mutate(c_acum = curva_acum_jm(family),
         n_obs = row_number()) %>%
  ggplot() +
  aes(n_obs, c_acum, color = iconic_taxon_name) +
  geom_line() +
  ylab('# Familias') +
  xlab('# Observaciones')
print(p)
plotly::ggplotly(p) # Fancy!

## . Grillas x Familias -----
tabla %>%
  group_by(grid_ID) %>%
  mutate(c_acum = curva_acum_jm(family),
         n_obs = row_number()) %>%
  ggplot() +
  aes(n_obs, c_acum, color = grid_ID) +
  geom_line() +
  ylab('# Familias') +
  xlab('# Observaciones')

## . Grillas x Spp -----
tabla %>%
  group_by(grid_ID) %>%
  mutate(spp_acum = curva_acum_jm(taxon_id),
         n_obs = row_number()) %>%
  ggplot() +
  aes(n_obs, spp_acum, color = grid_ID) +
  geom_line() +
  ylab('# Especies') +
  xlab('# Observaciones')

# grid_iNatUY_GIS ----

grid_iNatUY_GIS <-
  grid_join %>%
  group_by(grid_ID) %>%
  # Indicadores:
  summarise(
    spatialIntensity = ifelse(n_distinct(taxon_id, na.rm = TRUE), n(), 0),
    temporalIntensity = n_distinct(year, na.rm = TRUE),
    spsList = paste(species, collapse = ';'),
    speciesRichness = n_distinct(taxon_id, na.rm = TRUE),
    lastYearRecorded = ifelse(spatialIntensity, max(year, na.rm = TRUE), NA),
    nObservationsLastYear = sum(last_year),
    propObservationsLastYear = nObservationsLastYear / n(),
    speciesRichnessLastYear = n_distinct(taxon_id[last_year], na.rm = TRUE),
    nNewSpeciesLastYear = nuevas_spp(taxon_id[last_year], taxon_id[!last_year]),
    propNewSpeciesLastYear = nNewSpeciesLastYear / speciesRichness) %>%
  ungroup() %>%
  # Reescalamientos:
  mutate(spatialIntensity = ifelse(spatialIntensity,
                                   scales::rescale(spatialIntensity, to = 1:0),
                                   NA),
         lastYearRecorded = scales::rescale(lastYearRecorded, to = 0:1),
         temporalIntensity = ifelse(temporalIntensity,
                                    scales::rescale(temporalIntensity, to = 1:0),
                                    NA),
         indice_prioridad =
           scales::rescale(temporalIntensity + spatialIntensity, to = 0:1),
         ranking = rank(indice_prioridad,
                        ties.method = 'max',
                        na.last = TRUE) %>%
           scales::rescale(to = 1:0) %>%
           ifelse(is.na(indice_prioridad), NA, .))
# ranking = replace_na(ranking, 1))

# saveRDS(grid_iNatUY_GIS, 'data/grid_iNatUY_GIS_jm.rds')

# Apuntes -----
#
# Diversidad efectiva:
#
# http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/EffectiveNumberOfSpecies.htm
#
# Diversidades de diferentes órdenes (orden = q):
#
# q = 0: D = Riqueza de especies
#
# q = 1: D = exp(H); H = índice de Shannon
#
# q = 2: D = transformaciones de otros índices...
#
# Usar diversidad efectiva para comparar comunidades. En este caso no hay datos
# de frecuencia muy útiles, así que la diversidad a usar supongo que será
#
# This mathematical artifact, not biology, is responsible for the high
# similarity values published in this paper and elsewhere. The similarity
# between canopy and understory butterfly communities, using the Gini-Simpson
# index, is 0.975, indicating high similarity, even though (as the authors of
# the paper note) the communities are really quite distinct from each other. The
# similarity between habitats is also high, 0.935, and similarity between months
# is high as well, 0.978. Not only in this paper but in all published papers
# using this methodology, the Gini-Simpon similarity values are always high for
# diverse communities, between 0.93 and 0.99, regardless of what is being
# measured.
#
# Diversidad z: https://en.wikipedia.org/wiki/Zeta_diversity

# Vegan: rarefacción ----
#
library(vegan)
S_grid <- as.data.frame(grid_join[, 1:2]) %>%
  group_by(grid_ID) %>%
  summarise(n = n_distinct(taxon_id, na.rm = TRUE))
S <- S_grid$n
names(S) <- S_grid$grid_ID

sumasGrid <- count(as.data.frame(grid_join[,1:2]), grid_ID, taxon_id) %>%
  pivot_wider(grid_ID, names_from = taxon_id, values_from = n, values_fill = list(n = 0)) %>%
  as.matrix()
raremax <- min(rowSums(sumasGrid))
Srare <- rarefy(sumasGrid, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(sumasGrid, step = 20, sample = raremax, col = "blue", cex = 0.6)

# Slopes -----

# Todas las pendientes para grid x iconic (c/Spp):
grid_iconic_slopes <- tabla %>%
  group_by(grid_ID, iconic_taxon_name) %>%
  summarise(slope = get_acc(taxon_id) %>% get_slope)

View(grid_iconic_slopes)

# Todas las curvas para grid x iconic (c/Spp):
grid_iconic_acc <-
  tabla %>%
  # filter(grid_ID %in% 1:2) %>% 
  group_by(grid_ID, iconic_taxon_name) %>%
  nest() %>%
  mutate(acc = purrr::map(data, function(x) get_acc(x$taxon_id))) %>% 
  select(-data) %>% 
  ungroup()

acc178 <- filter(grid_iconic_acc,
                 grid_ID == 178, 
                 iconic_taxon_name == "Plantae") %>% 
  pull(acc) %>% 
  .[[1]]

s <- get_slope(acc178)
N <- length(acc178$richness)
emax <- acc178$richness[N]
plot(acc178, main = 'SAC - Plantae - grid_ID = 178 (MVD)')
abline(emax - s * N, s, col = 'red', lwd = 2)

# Otra forma de obtener las pendientes por grilla:
lista_acc <- tabla %>%
  split(.$grid_ID) %>%
  map(~ get_acc(.$taxon_id))

plot(lista_acc[[178]])

# Función de gráficos del vegan:
vegan:::plot.specaccum

pendientes <- sapply(lista_acc, get_slope)

# No hice la prueba todavía, pero debería funcionar:
grid_iNatUY_GIS_slope <- tibble(grid_ID = as.integer(names(pendientes)),
                                slope = pendientes)

left_join(grid_iNatUY_GIS, grid_iNatUY_GIS_slope)
