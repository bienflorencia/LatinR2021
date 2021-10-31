## MENSAJE PARA FLO!
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

#' Índice de Prioridad
#'
#' @param ti temporal intensity
#' @param si spatial intensity
#' @param n.reg numero de registros
#'
#' @return
#' @export
#'
#' @examples
#' registros <- c(23, 3, 2, 0, 1, 1, 1, 2, 0)
#' area <- 10
#' ti <- c(9, 1, 2, 0, 1, 1, 1, 2, 0)
#' calc_ip(ti, registros / area, registros)
calc_ip <- function(ti, si, n.reg) {
  # 1. rescalamientos:
  ti = scales::rescale(ti, to = 0:1)
  si = scales::rescale(si, to = 0:1)
  # 2. suma:
  suma <- ti + si
  # 3. ranking
  r <- rank(suma, ties.method = 'min', na.last = TRUE)
  # 4. sin registros:
  # Para dar mayor prioridad a los que tienen 0 registros (innecesario?)
  r[n.reg == 0] <- 0
  # 5. rescalamiento final (ranking a percentiles):
  out <- scales::rescale(r, to = 1:0)
  return(out)
}

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

#' Filtrar datos por grupo
#'
#' Obsoleta
#'
#' Filtra la tabla de datos para quedarse sólo con las columnas correspondientes a
#' un grupo en particular.
#'
#' @param datos
#' @param grupo
#'
#' @return
#' @export
#'
#' @examples
data_filter <- function(datos, grupo = "Todos") {
  # grupos aceptados:
  if (!exists(grac))
    grac <- c("Todos", "Aves", "Mammalia", "Amphibia", "Animalia", "Plantae",
              "Mollusca", "Insecta", "Arachnida", "Fungi", "Reptilia",
              "Actinopterygii", "Chromista", "Protozoa")

  if (!(tolower(grupo) %in% tolower(grac)))
    stop("grupo debe ser alguno de los aceptados:\n",
         stringr::str_wrap(paste(grac, collapse = ', '),
                           80, indent = 2, exdent = 2))

  gr <- NULL
  gr <- if (tolower(grupo) != 'todos') paste0('_', stringr::str_to_title(grupo))

  if (!exists('cols_base'))
    cols_base <- c('grid_id', 'area', 'percentil', 'indice_prioridad',
                   'spatial_intensity',
                   'temporal_intensity', 'species_richness',
                   'n_new_species_last_year', 'prop_new_species_last_year')

  cols_base_a <- cols_base[cols_base != 'area']
  # out <- sf::st_drop_geometry(datos)[c('grid_id', paste0(cols_base[-1], gr))]

  # print(cols_base) # debug
  columnas <- c('grid_id', 'area', paste0(cols_base_a[-1], gr), 'x')

  # print(columnas) # debug
  out <- dplyr::select(datos, tidyselect::all_of(columnas))

  names(out) <- gsub(paste0('_', grupo, '$'), '', names(out))
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

#' Hacer etiquetas de prioridad
#'
#' @param x percentiles correspondientes (ej: salida de percentil_corresp)
#' @param n cantidad de categorías deseadas. En caso de que labels no sea
#'   `NULL`, debe ser `length(labels) - 1`
#' @param labels etiquetas a asignar: debe tener n + 1 elementos
#'
#' @return
#' @export
#'
#' @examples
#' eti <- c("Muy baja", "Baja", "Media", "Alta", "Muy alta", "Sin registros")
#' idp <- c(0, .76, .89, 1, .98, 1, .99, .88, 1)
#' registros <- c(23, 3, 2, 0, 1, 1, 1, 2, 0)
#' p <- percentil_corresp(idp, registros)
#' mketiquetas(idp, registros)
#' mketiquetas(p, registros, 5, labels = eti)
#' table(mketiquetas(p, registros, 5, labels = eti))
mketiquetas <- function(x, n.reg, n = 5, labels = NULL) {
  if (!is.null(labels) && length(labels) != n + 1L) {
    warning('\n\tn esperado (length(labels) - 1):\t', length(labels) - 1L,
            '\n\tn encontrado:\t\t\t\t', n,
            '\n\t-->> Se modifica n de manera acorde (n = ',
            n <- length(labels) - 1L, ')')
  }
  out <- integer(length(x))
  corte <- cut(x[n.reg > 0], n, labels = FALSE, include.lowest = TRUE)
  out[n.reg > 0] <- corte
  out[n.reg == 0] <- n + 1L
  if (!is.null(labels)) {
    out <- labels[out]
    out <- factor(out, levels = labels)
  } else {
    out <- factor(out)
  }
  # print(tibble(x, out)) # debug
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

  # cols_base <- c('grid_id', 'percentil', 'indice_prioridad', 'species_richness',
  #                'n_new_species_last_year', 'prop_new_species_last_year')
  # d <- sf::st_drop_geometry(datos)[c('grid_id', paste0(cols_base[-1], gr))]
  # names(d) <- cols_base

  out <- paste0(
    grupo_html, "ID Celda: ", grid_id, '</em>',
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
