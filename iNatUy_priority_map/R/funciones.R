
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

#' Sustituta de leaflet::colorQuantile
#'
#' Conveniently maps data values (numeric or factor/character) to colors
#' according to a given palette, which can be provided in a variety of formats.
#'
#' colorNumeric is a simple linear mapping from continuous numeric data to an
#' interpolated palette.
#'
#' colorBin also maps continuous numeric data, but performs binning based on
#' value (see the cut function). colorBin defaults for the cut function are
#' include.lowest = TRUE and right = FALSE.
#'
#' colorQuantile similarly bins numeric data, but via the quantile function.
#'
#' colorFactor maps factors to colors. If the palette is discrete and has a
#' different number of colors than the number of factors, interpolation is used.
#'
#' The palette argument can be any of the following:
#'
#' A character vector of RGB or named colors. Examples: palette(), c("#000000",
#' "#0000FF", "#FFFFFF"), topo.colors(10)
#'
#' The name of an RColorBrewer palette, e.g. "BuPu" or "Greens".
#'
#' The full name of a viridis palette: "viridis", "magma", "inferno", or
#' "plasma".
#'
#' A function that receives a single value between 0 and 1 and returns a color.
#' Examples: colorRamp(c("#000000", "#FFFFFF"), interpolate = "spline").
#'
#' @param palette The colors or color function that values will be mapped to
#'
#' @param domain The possible values that can be mapped.
#'
#'   For \code{colorNumeric} and \code{colorBin}, this can be a simple numeric
#'   range (e.g. \code{c(0, 100)}); \code{colorQuantile} needs representative
#'   numeric data; and \code{colorFactor} needs categorical data.
#'
#'   If \code{NULL}, then whenever the resulting color function is called, the
#'   \code{x} value will represent the domain. This implies that if the function
#'   is invoked multiple times, the encoding between values and colors may not
#'   be consistent; if consistency is needed, you must provide a non-NULL
#'   domain.
#'
#' @param n Number of equal-size quantiles desired. For more precise control,
#'   use the probs argument instead.
#'
#' @param probs See quantile. If provided, the n argument is ignored.
#'
#' @param na.color The color to return for NA values. Note that na.color = NA is
#'   valid.
#'
#' @param alpha Whether alpha channels should be respected or ignored. If TRUE
#'   then colors without explicit alpha information will be treated as fully
#'   opaque.
#'
#' @param reverse Whether the colors (or color function) in palette should be
#'   used in reverse order. For example, if the default order of a palette goes
#'   from blue to green, then reverse = TRUE will result in the colors going
#'   from green to blue.
#'
#' @param right parameter supplied to cut. See Details
#'
#' @return A function that takes a single parameter x; when called with a vector
#'   of numbers (except for colorFactor, which expects factors/characters),
#'   #RRGGBB color strings are returned (unless alpha = TRUE in which case
#'   #RRGGBBAA may also be possible).
#' @export
#'
#' @examples
#' vals <- c( 2, 0, 2, 127, 2, 3, 1, 8, 7, 3, 4, 16, 3, 5, 4, 6, 1, 3, 5, 1, 1,
#' 4, 4, 1, 11, 1, 0, 1, 3, 2, 3, 7, 14, 2, 9, 16, 1, 6, 5, 4, 3, 5, 10, 8, 7,
#' 4, 0, 11, 5, 4, 7, 3, 2, 8, 1, 3, 4, 3, 3, 21, 11, 6, 2, 10, 5, 5, 2, 2, 5,
#' 2, 5, 10, 27, 0, 5, 9, 6, 6, 5, 4, 20, 7, 4, 6, 8, 3, 5, 20, 4, 2, 8, 3, 7,
#' 10, 1, 8, 7, 2, 1, 9, 2, 5, 137, 4, 3, 0, 3, 5, 6, 7, 2, 17, 4, 4, 4, 4, 3,
#' 2, 0, 2, 5, 1, 0, 3, 1, 2, 0, 10, 12, 3, 7, 5, 0, 11, 5, 4, 2, 2, 1, 0, 4, 6,
#' 2, 3, 2, 7, 6, 2, 1, 5, 1, 9, 2, 0, 4, 0, 7, 3, 4, 4, 0, 0, 1, 0, 5, 8, 1, 2,
#' 4, 10, 12, 15, 2, 5, 3, 2, 3, 1, 5, 0, 7, 67, 1, 2, 5, 1, 2, 0, 3, 6, 1, 7,
#' 2, 1, 4, 2, 2, 8, 0, 3, 3, 7, 3, 16, 8, 6, 18, 2, 20, 5, 3, 28, 2, 6, 5, 5,
#' 6, 1, 10, 2, 18, 7, 5, 7, 8, 6, 6, 7, 7, 10, 4, 17, 6, 12, 5, 5, 4, 4, 3, 6,
#' 8, 2, 2, 0, 7, 6, 4, 3, 4, 4, 6, 3, 39, 4, 2, 1, 3, 2, 3, 1, 0, 6, 6, 0, 4,
#' 1, 14, 13, 0, 25, 2, 4, 5, 5, 11, 2, 2, 0, 17, 6, 2, 3, 5, 7, 1, 4, 3, 2, 19,
#' 2, 2, 6, 1, 1, 2, 1, 2, 1, 1, 4, 3, 0, 4, 3 )
#'
#' pal0 <- leaflet::colorQuantile("RdYlGn", vals, n = 9, 
#'                                na.color = "#4d0012", reverse = FALSE)
#' 
#' pal0(vals)
#'
#' pal1 <- colorQuantile_hacked("RdYlGn", vals, n = 9, 
#'                              na.color = "#4d0012", reverse = FALSE)
#' 
#' pal1(vals)
colorQuantile_hacked <- function (palette, domain, n = 4,
                                  probs = seq(0, 1, length.out = n + 1), 
                                  na.color = "#808080", 
                                  alpha = FALSE, 
                                  reverse = FALSE, 
                                  right = FALSE) {
  if (!is.null(domain)) {
    bins <- quantile(domain, probs, na.rm = TRUE, names = FALSE)
    bins <- unique(bins)
    return(leaflet:::withColorAttr(
      "quantile", 
      list(probs = probs, na.color = na.color),
      leaflet::colorBin(palette, 
                        domain = NULL, 
                        bins = bins, 
                        na.color = na.color, 
                        alpha = alpha,
                        reverse = reverse)
    ))
  }
  colorFunc <- leaflet::colorFactor(palette, domain = 1:(length(probs) - 1), 
                                    na.color = na.color, alpha = alpha, 
                                    reverse = reverse)
  leaflet:::withColorAttr(
    "quantile",
    list(probs = probs, na.color = na.color),
    function(x) {
      binsToUse <- quantile(x, probs,
                            na.rm = TRUE,
                            names = FALSE)
      ints <- cut(
        x,
        binsToUse,
        labels = FALSE,
        include.lowest = TRUE,
        right = right
      )
      if (any(is.na(x) != is.na(ints)))
        warning("Some values were outside the color scale",
                " and will be treated as NA")
      colorFunc(ints)
    })
}

#' Title
#'
#' @param indice_prioridad 
#' @param n 
#' @param right 
#' @param labels 
#'
#' @return
#' @export
#'
#' @examples
#' eti <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta", "Sin registros")
#' idp <- c(0, .76, .89, NA, .98, 1, .99, .88, NA)
#' domain2labels(idp)
#' domain2labels(idp, 5, labels = eti)
#' table(domain2labels(idp, 5, labels = eti))
domain2labels <- function(x, n = 5, 
                       labels = NULL,
                       right = FALSE) {
  probs <- seq(0, 1, length.out = n + 1)
  r <- rank(x[!is.na(x)], ties.method = 'max') 
    # scales::rescale(to = 1:0)
  rg <- range(r)
  
  # bins <- quantile(x, probs, na.rm = TRUE, names = FALSE)
  # bins <- unique(bins)
  corte <- cut(
    r,
    diff(rg) * probs + rg[1],
    labels = FALSE,
    include.lowest = TRUE,
    right = right
  )
  ints <- as.integer(x)
  ints[!is.na(x)] <- corte
  ints[is.na(x)] <- n + 1
  if (!is.null(labels)) {
    if (length(labels) != (n + 1))
      stop('labels debe tener n + 1 elementos (', n + 1, ') pero tiene: ',
           length(labels))
    out <- labels[ints]
    out <- factor(out, levels = labels)
  } else {
    out <- factor(ints)
  }
  # print(tibble(x, out)) # debug
  return(out)
}

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

#' Popup maker
#'
#' El objeto popup es un vector character con el código HTML usado al hacer
#' click en un hexágono.
#'
#' @param datos Tabla con los datos. Espera la presencia de varias columnas
#' @param grupo Grupo taxonómico para el que corresponden los datos
#'
#' @return Vector character con código HTML
#' @export
#'
#' @examples
mkpopup <- function(grid_id, etiqueta, grupo = "Todos") {
  
  # grupos aceptados:
  grac <- c("Todos", "Aves", "Mammalia", "Amphibia", "Animalia", "Plantae",
            "Mollusca", "Insecta", "Arachnida", "Fungi", "Reptilia", 
            "Actinopterygii", "Chromista", "Protozoa")
  
  if (!(tolower(grupo) %in% tolower(grac))) 
    stop("grupo debe ser alguno de los aceptados:\n", 
         stringr::str_wrap(paste(grac, collapse = ', '), 
                           80, indent = 2, exdent = 2))

  grupo_html <- paste0('<em style="color:grey">Grupo: ', 
                       stringr::str_to_title(grupo),
                       ' - ')
  
  # gr <- NULL
  # gr <- if (tolower(grupo) != 'todos') paste0('_', stringr::str_to_title(grupo))

  # cols_base <- c('grid_id', 'ranking', 'indice_prioridad', 'species_richness',
  #                'n_new_species_last_year', 'prop_new_species_last_year')
  # d <- sf::st_drop_geometry(datos)[c('grid_id', paste0(cols_base[-1], gr))]
  # names(d) <- cols_base
  
  out <- paste0(
    grupo_html, "Grid ID:", grid_id, '</em>', 
    "<br><strong>Prioridad: </strong>",
    etiqueta
    # replace_na(round(100 * datos$ranking, 1), 0), "%",
    # "<br><strong>Índice de prioridad: </strong>",
    # replace_na(round(datos$indice_prioridad, 2), 1),
    # "<br><strong>Especies registradas: </strong>",
    # datos$species_richness,
    # "<br><strong>Especies nuevas, en el último año: </strong>",
    # datos$n_new_species_last_year,
    # " (", round(100 * datos$prop_new_species_last_year, 1),
    # " %)"
    )
  return(out)
}


data_filter <- function(datos, grupo = "Todos") {
  # grupos aceptados:
  grac <- c("Todos", "Aves", "Mammalia", "Amphibia", "Animalia", "Plantae", 
            "Mollusca", "Insecta", "Arachnida", "Fungi", "Reptilia",
            "Actinopterygii", "Chromista", "Protozoa")
  
  if (!(tolower(grupo) %in% tolower(grac))) 
    stop("grupo debe ser alguno de los aceptados:\n", 
         stringr::str_wrap(paste(grac, collapse = ', '), 
                           80, indent = 2, exdent = 2))
  
  gr <- NULL
  gr <- if (tolower(grupo) != 'todos') paste0('_', stringr::str_to_title(grupo))

  cols_base <- c('grid_id', 'ranking', 'indice_prioridad', 
                 'species_richness', 'n_new_species_last_year', 
                 'prop_new_species_last_year')
  # out <- sf::st_drop_geometry(datos)[c('grid_id', paste0(cols_base[-1], gr))]
  
  columnas <- c('grid_id', paste0(cols_base[-1], gr), 'x')
  
  # print(columnas) # debug
  out <- dplyr::select(datos, tidyselect::all_of(columnas))
  
  names(out) <- c(cols_base, 'x')
  return(out)
}
  