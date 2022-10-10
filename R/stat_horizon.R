
#' @rdname geom_horizon
#' @param geom Used geom, default to 'ribbon'
#'
#' @importFrom ggplot2 layer
#'
#'
#' @export



stat_horizon <- function(mapping = NULL,
                         data = NULL,
                         geom = "ribbon",
                         position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = TRUE,
                         inherit.aes = TRUE,
                         origin = 'midpoint',
                         horizonscale = 6,
                         rm.outliers = FALSE,
                         reverse = FALSE,
                         mirror = FALSE) {
  layer(
    stat = StatHorizon,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      origin = origin,
      horizonscale = horizonscale,
      rm.outliers = rm.outliers,
      reverse = reverse,
      mirror = mirror,
      ...
    )
  )
}


#' @importFrom ggplot2 ggproto Stat
#' @importFrom glue glue
#' @importFrom dplyr tibble
#' @noRd

StatHorizon <- ggproto(
  "StatHorizon",
  Stat,
  compute_group = function(data,
                           scales,
                           origin = 'midpoint',
                           horizonscale = 6,
                           rm.outliers = FALSE,
                           reverse = FALSE,
                           mirror = FALSE) {
    build_horizon_df_2(data, origin, horizonscale, rm.outliers, reverse, mirror)
  },
  compute_layer = function (self, data, params, layout)
  {
    params <- params[intersect(names(params), self$parameters())]
    args <- c(list(data = quote(data), scales = quote(scales)),
              params)
    ggplot2:::dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      tryCatch(
        do.call(self$compute_panel, args),
        error = function(e) {
          warning(
            glue::glue(
              "Computation failed in `{ggplot2:::snake_class(self)}()`:\n{e$message}"
            )
          )
          tibble()
        }
      )
    })
  },
  required_aes = c("x", "y"),
  optional_aes = c("xend"),
  dropped_aes = c("y", "xend"),
  default_aes = aes(fill = ..Categories.., group = ..group..)

)




