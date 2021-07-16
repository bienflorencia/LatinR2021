
## MENSAJE PARA FLO! ----
##
## FLO! Abajo puse funciones con documentación roxygen que estoy acostumbrado.
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

#' Cálculo de 'completeness'
#'
#' The function ```get_gridsSlopes``` finds a species accumulation curve (SAC)
#' for each grid-cell using the method ‘exact’ of the function ```specaccum```
#' of the vegan package and then calculates the degree of curvilinearity as the
#' mean slope of the last 10% of the curve.
#' 
#' @param data_abundance data.frame with abundance data
#'
#' @return
#' @export
#'
#' @examples
get_gridsCompleteness <- function(data_abundance){
  GridSlope <- data.frame(Grid=integer(), Slope=numeric(), stringsAsFactors=FALSE)
  data_abundance <- as.data.frame(data_abundance) 
  data_abundance$abundance <- as.integer(1)
  cells <- unique(data_abundance$GridID)
  splistT <- list()
  spaccum <- list()
  slope <- list()
  for (i in cells) {
    splist <- data_abundance[data_abundance$GridID == i,c(2:4)]
    splistT[[i]] = data2mat(splist) 
    spaccum[[i]] = specaccum(splistT[[i]], method = "exact")
    slope[[i]] = (spaccum[[i]][[4]][length(spaccum[[i]][[4]])]-
                    spaccum[[i]][[4]][ceiling(length(spaccum[[i]][[4]])*0.9)])/
      (length(spaccum[[i]][[4]])- ceiling(length(spaccum[[i]][[4]])*0.9))
    GridSlope_i <- data.frame(Grid=i, Slope=slope[[i]], stringsAsFactors=FALSE)
    GridSlope <- rbind(GridSlope, GridSlope_i)
  }
  return(GridSlope)
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
