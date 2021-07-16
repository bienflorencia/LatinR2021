
## MENSAJE PARA FLO! ----
##
## FLO! Abajo puse funciones con documentación roxygen que estoy acostumbrado.
##
## Hice una función casera para las curvas de acumulación más básicas del mundo
## creo. Lo bueno es que se puede acoplar bien a un group_by + mutate.
##
## También cambié la función tuya por un par de funciones mías, más chicas...
##
## get_acc hace el spaccum para un vector de observaciones (ver abajo la
## documentación)
##
## get_slope hace el cálculo de la pendiente en el tramo final, tomando como
## input el output de get_acc
##
## La idea es que estas dos trabajarían bien con group_by + summarise o con nest
## + mutate + purrr::map... en fin. Puse muchos ejemplos en pruebas_jm.R y mismo
## en la documentación de esas funciones (aunque más abstractos).
##
## Algo bonito que tiene RStudio es que en la parte de los ejemplos, donde hay
## código, si das Ctrl+Enter se ejecuta la línea en la que está el mouse (o lo
## que esté sombreado, igual que en cualquier script).
##
## Hay más ejemplos en pruebas_jm.R y un gráfico en
## iNatUY_donde_ir_a_registrar_viz.html
##
## (( Hace poco también descubrí que el Ctrl+Enter también sirve para código que
## está en las páginas de ayuda !! ))
##
## (( TAMBIÉN RECOMIENDO EL SHORTCUT: CTRL+SHIFT+O !! ))
##
## (( CAPAZ QUE EN MAC LOS CTRL HAY QUE CAMBIARLOS TODOS POR EL BOTÓN ESE QUE
## USAN USTEDES ))


#' Curva de acumulación JMB
#'
#' El alrogitmo es bieeeen básico: toma un vector con observaciones: a, a, b, a,
#' b, c, ... y devuelve la riqueza* acumulada a medida que se van agregando
#' observaciones, siguiendo el orden en que estas observaciones vienen. En el
#' ejemplo de arriba:
#'
#' taxon_obs: a, a, b, a, b, c, ...
#'
#' out:       1, 1, 2, 2, 2, 3, ...
#'
#' *: riqueza en el sentido más tradicional (diversidad de orden cero si nos
#' ponemos técniques...)
#'
#' @param taxon_obs observaciones de las categorías. Puede ser de varios tipos,
#'   hice pruebas con character y numeric hasta ahora, solamente.
#'
#' @return vector integer con riqueza de especies acumuladas, en donde la
#'   posición iésima en el vector corresponde a la observación iésima.
#' @export
#'
#' @examples
#' obs <- c("a", "b", "b", "b", "b", "a", "b", "c", "b", "b")
#' curva_acum_jm(obs)
#' ## a b b b b a b c b b
#' ## 1 2 2 2 2 2 2 3 3 3
curva_acum_jm <- function(taxon_obs) {
  taxones <- unique(taxon_obs)
  N <- length(taxon_obs)
  muestra <- !logical(length(taxones))
  out <- integer(N)
  s <- 0L
  for (i in 1:N) {
    w <- which(taxon_obs[i] == taxones & muestra)
    if(length(w)) {
      s <- s + 1L
      muestra[w] <- FALSE
    }
    out[i] <- s
  }
  names(out) <- taxon_obs
  return(out)
}

#' Curva de acumulación con vegan::specaccum
#'
#' Toma un vector de observaciones de taxones y devuelve la curva de acumulación
#' que genera la función \code{\link[vegan]{specaccum}}. Conceptualmente es
#' igual que en \code{curva_acum_jm}
#'
#' @param taxon_obs observaciones de las categorías. Puede ser de varios tipos,
#'   hice pruebas con character y numeric hasta ahora, solamente.
#' @param method ver \code{\link[vegan]{specaccum}}
#'
#' @return Objeto \code{\link[vegan]{specaccum}}: una Species Accumulation Curve
#'   (SAC).
#' @export
#'
#' @examples
#' obs <- sample(letters, size = 100, replace = TRUE)
#' sac <- get_acc(obs)
#' s <- get_slope(sac)
#' plot(sac, main = paste0('Acumulación de Riqueza de letras\n',
#'                         'pendiente final: ', round(s, 3)))
#' N <- length(sac$richness)
#' emax <- sac$richness[N]
#' abline(emax - s * N, s, col = 'red', lwd = 2)
get_acc <- function(taxon_obs, method = 'exact') {
  N <- length(taxon_obs)
  K <- ceiling(N * .9)
  spmat <- tibble::tibble(plots = 1:N, taxon_obs = taxon_obs, n = 1L) %>%
    tidyr::pivot_wider(plots, names_from = taxon_obs, values_from = n,
                       values_fill = list(n = 0))
  return(vegan::specaccum(spmat, method = method))
}

#' Pendiente final de SAC
#'
#' @param sac objeto del tipo \code{\link[vegan]{specaccum}}
#' @param last_perc proporción final de la curva usada para calcular la
#'   pendiente
#'
#' @return Un número correspondiente a la pendiente en el tramo final de la SAC
#' @export
#'
#' @examples
#' d <- tibble(
#'   grid_ID = sample(1:4, size = 1000, replace = TRUE),
#'   iconic_taxon_name = sample(LETTERS[1:10], size = 1000, replace = TRUE),
#'   species = sample(letters, size = 1000, replace = TRUE)
#'   )
#' d %>%
#'   group_by(grid_ID, iconic_taxon_name) %>%
#'   summarise(slope = get_acc(species) %>% get_slope)
#'   
#' dacc <- d %>%
#'   # filter(grid_ID %in% 1:2) %>% 
#'   group_by(grid_ID, iconic_taxon_name) %>%
#'   nest() %>%
#'   mutate(acc = purrr::map(data, function(x) get_acc(x$species))) %>% 
#'   select(-data) %>% 
#'   ungroup()
#' filter(dacc, grid_ID == 3, iconic_taxon_name == 'C')$acc[[1]] %>% 
#'   plot
get_slope <- function(sac, last_perc = .1) {
  s <- sac$richness
  N <- length(s)
  K <- ceiling(N * (1 - last_perc))
  out <- (s[N]- s[K]) / (N - K)
  return(out)
}

#' Cantidad de especies nuevas
#'
#' @param nuevas Muestra nueva de especies
#' @param viejas Muestra vieja de especies
#'
#' @return integer. Número de especies nuevas
#' @export
#'
#' @examples
#' nuevas_spp(c('a', 'c', 'f'), c('a', 'a', 'c', 'b', 'c'))
nuevas_spp <- function(nuevas, viejas) {
  un <- unique(nuevas)
  uv <- unique(viejas)
  out <- length(un) - sum(un %in% uv)
  return(out)
}
