
#' Add together two numbers
#'
#' @import ggplot2
#' @export

geom_horizon <- function(mapping = NULL, data = NULL,
                         position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = TRUE,
                         inherit.aes = TRUE,
                         origin = 'midpoint',
                         horizonscale = 6,
                         rm.outliers = FALSE,
                         reverse = FALSE) {
  layer(
    data = data,
    mapping = mapping,
    stat = 'horizon',
    geom = GeomHorizon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      origin = origin,
      horizonscale = horizonscale,
      rm.outliers = rm.outliers,
      reverse = reverse,
      ...)
  )
}

#' Add together two numbers
#'
#' @import ggplot2
#' @export

GeomHorizon <- ggproto("GeomHorizon", GeomRibbon,
                       default_aes = aes(colour = NA, fill = NA,
                                         size = 0.5, linetype = 1,
                                         alpha = NA,
                                         origin = 'midpoint',
                                         horizonscale = 6,
                                         rm.outliers = FALSE)
)


