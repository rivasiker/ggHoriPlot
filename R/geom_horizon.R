
#' Horizon plot
#'
#' This is a horizon plot
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @section Aesthetics:
#' \code{geom_horizon()} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#' \item \strong{\code{x}}
#' \item \strong{\code{y}}
#' \item \code{xend}
#' \item \code{alpha}
#' \item \code{colour}
#' \item \code{fill}
#' }
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param origin Origin of the horizon plot. It can either be a string, namely \code{"midpoint"} (the default), \code{"median"},
#' \code{"mean"}, \code{"min"} or \code{"quantiles"}, or a user-specified number.
#' @param horizonscale Cutpoints of the horizon plot. It can either be an integer specifying the number of ranges (default is \code{6}),
#' or a user-supplied numeric vector with the cutpoints defining the different ranges.
#' @param rm.outliers If \code{TRUE}, all the values below \code{quantile(y, 0.25)-1.5*IQR(y)} and above
#' \code{quantile(y, 0.75)+1.5*IQR(y)} are excluded from the origin and cutpoint calculations (default is \code{FALSE}).
#' @param reverse If \code{TRUE}, the horizon peaks for the values below the origin are reversed (default is \code{FALSE}).
#' @param mirror If \code{TRUE}, the horizon peaks for all the values are reversed (default is \code{FALSE}).
#'
#' @examples
#' # Generate data
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#'
#' # Basic plot with default colors
#' h <- ggplot(huron) + geom_horizon(aes(year, level))
#'
#' # Add color scheme
#' h + theme_void() + scale_fill_hcl()
#'
#' # Add cupoints
#' ggplot(huron) +
#'   geom_horizon(aes(year, level, fill = ..Cutpoints..)) +
#'   theme_void() +
#'   scale_fill_hcl()
#'
#' # Reverse negative ranges
#' ggplot(huron) +
#'   geom_horizon(aes(year, level, fill = ..Cutpoints..), reverse = TRUE) +
#'   theme_void() +
#'   scale_fill_hcl()
#'
#'
#' @import ggplot2
#'
#' @export


geom_horizon <- function(mapping = NULL, data = NULL,
                         position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = TRUE,
                         inherit.aes = TRUE) {
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
      ...)
  )
}

#' @noRd

GeomHorizon <- ggproto("GeomHorizon", GeomRibbon,
                       default_aes = aes(colour = NA, fill = NA,
                                         size = 0.5, linetype = 1,
                                         alpha = NA)
)


