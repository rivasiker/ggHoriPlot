

utils::globalVariables(c("ymin", "ymax", "fill", "y", "outlier", "x", "group",
                         "xend", "value"))

#' Compute group for stat_horizon
#'
#' @param data A data frame.
#' @inheritParams geom_horizon
#'
#' @return A data frame transformed for a horizon plot.
#'
#' @importFrom dplyr mutate between select transmute bind_cols
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_detect
#' @importFrom stats IQR median quantile setNames
#' @keywords internal

build_horizon_df_2 <- function(data, origin, horizonscale,
                               rm.outliers, reverse, mirror) {

  # Find outliers if necessary
  if (rm.outliers) {
    data <- data %>%
      mutate(
        outlier = between(
          y,
          quantile(y, 0.25, na.rm=TRUE)-1.5*IQR(y, na.rm=TRUE),
          quantile(y, 0.75, na.rm=TRUE)+1.5*IQR(y, na.rm=TRUE)))
  } else {
    data <- data %>%
      mutate(outlier = TRUE)
  }

  # Compute origin and horizonscale
  ori_sca_lst <- compute_ori_sca(
    data, origin,
    horizonscale)
  ori <- ori_sca_lst$ori
  vec_cut <- ori_sca_lst$vec_cut

  # Remove outliers
  data <- select(data, -outlier)

  # Add xend if necessary
  if ('xend' %in% names(data)) {
    data <- data %>%
      pivot_longer(c(x, xend)) %>%
      transmute(x = value, y)
  } else {
    data <- data %>% select(x, y)
  }

  # Modify the data frame by the cutpoints
  data <- data %>%
    bind_cols(posneg(data$y, ori, vec_cut))
  # Tidy up the data frame
  data <- data %>% select(-y) %>% pivot_longer(starts_with('y'))
  colnames(data) <- c("x","fill","value")
  data$ymin<-0
  data$ymax <- data$value
  data <- data %>%
    transmute(x, ymin, ymax, fill, group = fill)
  binnames <- unique(data$group)
  binnames <- c(rev(binnames[str_detect(binnames, 'yneg')]),
                binnames[str_detect(binnames, 'ypos')])
  cutpoint_vec <- rev(sort(c(vec_cut, ori)))
  # Calculate the categories and cutpoints
  data$Categories <- factor(data$group, rev(binnames))
  new_vec <- rep(NA, length(cutpoint_vec)-1)
  for (i in 1:(length(cutpoint_vec)-1)) {
    new_vec[i] <- paste0('[', signif(cutpoint_vec[i+1]), ', ',
                         signif(cutpoint_vec[i]), ')')
  }
  new_vec <- setNames(new_vec, rev(binnames))
  data$Cutpoints <- factor(new_vec[data$group], new_vec)

  # Reverse negative if necessary
  if (reverse) {
    data <- data %>%
      mutate(
        ymin = ifelse(str_detect(group, 'neg'), 1, 0),
        ymax = ifelse(str_detect(group, 'neg'), 1-ymax, ymax)
      )
  }

  # Mirror everything if necessary
  if (mirror) {
    ymin_vec <- data$ymin
    data <- data %>%
      mutate(
        ymin = -ymax+1,
        ymax = ymin_vec+1
      )
  }

  data
}






