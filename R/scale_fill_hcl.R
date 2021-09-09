

#' General color function
#' @keywords internal
#'
#' @param reverse Logical, indicating whether the palette should be inverted.
#' @param palette A character string, indicating the palette to be used.
#'
#' @return A function, that, given an integer, returns a character vector
#' of colors of the specified palette.
#'
#' @importFrom grDevices hcl.colors

hcl_generic <- function(reverse, palette, ...) {
  function(n) {
    hcl.colors(n, palette=palette, rev=!reverse)
  }
}

#' Create your own discrete scale
#'
#' These functions allow you to specify your own set of mappings from levels in the
#' data to aesthetic values.
#'
#' The functions `scale_colour_manual()`, `scale_fill_manual()`, `scale_size_manual()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_manual()` and `scale_fill_manual()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call (see examples). The function
#' `scale_discrete_manual()` is a generic scale that can work with any aesthetic or set
#' of aesthetics provided via the `aesthetics` argument.
#'
#' @inheritParams ggplot2::scale_fill_discrete
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics
#' @param palette the name of the palette to generate colors from. A list of all
#' available palettes can be found by running \code{colorspace::hcl_palettes()}
#' (default is "RdYlBu").
#' @param reverse If \code{TRUE}, the order of the colors is reversed (default is \code{FALSE})'
#'
#' @return Scale layer for the fill aesthetic.
#'
#' @importFrom ggplot2 discrete_scale
#' @export

scale_fill_hcl <- function(..., palette = "RdYlBu", reverse = FALSE) {

  pal <- hcl_generic(reverse, palette)
  discrete_scale("fill", 'hcl', palette = pal, ...)

}
