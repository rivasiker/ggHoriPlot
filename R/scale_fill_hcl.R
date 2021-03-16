
#' Add together two numbers
#' @export

hcl_generic <- function(reverse, palette, ...) {
  function(n) {
    hcl.colors(n, palette=palette, rev=!reverse)
  }
}

scale_fill_hcl <- function(reverse = FALSE, palette = "RdYlBu", ...) {

  pal <- hcl_generic(reverse, palette)
  discrete_scale("fill", 'hcl', palette = pal, ...)

}
