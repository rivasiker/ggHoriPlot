
#' Internal function
#'
#' This function calculates the different positive and
#' negative boundaries for each data point
#' based on the origin and the cutpoint vector provided.
#'
#' @importFrom dplyr as_tibble
#'
#' @param y Numeric vector of values to be transformed.
#' @param origin Numeric origin of the horizon plot.
#' @param vec Numeric vector of cutpoints.
#'
#' @return A data frame, which has been transformed with the supplied
#' origin and vector of cuts.
#'
#' @keywords internal


posneg <- function(y, origin, vec) {

  tab <- list()
  pos_lst <- vec[vec >= origin]
  neg_lst <- vec[vec < origin]

  if (length(pos_lst) != 0) {
    test <- origin
    for (i in 1:(length(pos_lst))) {
      min_compare <- pos_lst[i]
      new_col <- unlist(
        lapply(
          y,
          function(x) ifelse(
            x>test,
            abs(min(x,min_compare)-test)/abs(min_compare-test),
            0)))
      tab[[paste0('ypos', i)]] <- new_col
      test <- pos_lst[i]
    }
  }


  if (length(neg_lst) != 0) {
    test <- origin
    for (i in 1:(length(neg_lst))) {
      max_compare <- neg_lst[i]
      new_col <- unlist(
        lapply(
          y,
          function(x) ifelse(
            x<test,
            abs(max(x,max_compare)-test)/abs(max_compare-test),
            0)))
      tab[[paste0('yneg', i)]] <- new_col
      test <- neg_lst[i]

    }
  }

  as_tibble(tab)
}
