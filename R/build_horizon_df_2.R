

utils::globalVariables(c("ymin", "ymax", "fill", "y", "outlier", "x", "group",
                         "xend", "value"))


#' Check if a variable is a whole number
#'
#' @param x A number.
#' @param tol Toleration threshold.
#'
#' @keywords internal

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#' Compute group for stat_horizon
#'
#' @param data A data frame.
#' @inheritParams geom_horizon
#'
#' @importFrom dplyr mutate between select transmute bind_cols
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_detect
#' @importFrom grDevices hcl.colors
#' @importFrom stats IQR median quantile setNames
#' @keywords internal



build_horizon_df_2 <- function(data, origin, horizonscale,
                               rm.outliers, reverse, mirror) {

  if (rm.outliers) {
    data <- data %>%
      mutate(
        outlier = between(
          y,
          quantile(y, 0.25, na.rm=TRUE)-1.5*IQR(y, na.rm=TRUE),
          quantile(y, 0.75, na.rm=TRUE)+1.5*IQR(y, na.rm=TRUE)))
  } else {
    data <- data %>%
      mutate(
        outlier = TRUE)
  }

  if (!is.numeric(horizonscale)) {
    stop('please, provide a valid horizonscale')
  }


  # If the origin is the median or mean
  if (origin %in% c('median', 'mean', 'midpoint')) {
    # Define origin cutpoint
    ori <- ifelse(origin == 'median',
                 median(data$y[data$outlier], na.rm = TRUE),
                 ifelse(origin == 'mean',
                        mean(data$y[data$outlier], na.rm = TRUE),
                        sum(range(data$y[data$outlier], na.rm = TRUE))/2))
    # If the horizon scale is an integer
    if (length(horizonscale)==1) {
      if (is.wholenumber(horizonscale)) {
        # Save the number of cuts
        ncut <- horizonscale
        # Calculate separation between cutpoints
        sca <- (range(data$y[data$outlier], na.rm = TRUE)[2]-
                  range(data$y[data$outlier], na.rm = TRUE)[1])/horizonscale
        # Calculate cutpoint vectos
        vec_cut <- c()
        # Positive cutpoints
        for (i in 1:(ncut%/%2)) {
          vec_cut <- c(vec_cut, ori+sca*i)
        }
        # Negative cutpoints
        for (i in 1:((ncut+1)%/%2)) {
          vec_cut <- c(vec_cut, ori-sca*i)
        }
        # If the horizon scale is a vector of numbers
      }
    } else {
      # Save the number of cuts
      ncut <- length(horizonscale)
      # Save them as cutpoints directly
      vec_cut <- horizonscale
    }

  } else if (origin == 'quantiles') {
    if ((length(horizonscale)==1) & (is.wholenumber(horizonscale))) {
      ncut <- horizonscale
      ori <- quantile(data$y[data$outlier],
                      (ncut%/%2)/ncut, na.rm = TRUE)
      vec_cut <- c()
      for (i in 0:ncut) {
        if (i != ncut%/%2) {
          vec_cut <- c(vec_cut,
                       quantile(data$y[data$outlier],
                                (i)/ncut, na.rm = TRUE))
        }
      }
    } else {
      stop("the horizonscale should be a positive integer when using origin = 'quantiles'")
    }
    # If the origin is numeric
  } else if (origin == 'min') {
    ncut <- horizonscale
    ori <- min(data$y[data$outlier], na.rm = TRUE)
    sca <- (range(data$y[data$outlier], na.rm = TRUE)[2]-
              range(data$y[data$outlier], na.rm = TRUE)[1])/horizonscale
    vec_cut <- c()
    for (i in 1:ncut) {
      vec_cut <- c(vec_cut, ori+sca*i)
    }
  } else if (is.numeric(origin)) {
    # Save origin cutpoint
    ori <- origin
    if (length(horizonscale)==1) {
      if (is.wholenumber(horizonscale)) {
        ncut <- horizonscale
        sca <- (range(data$y[data$outlier], na.rm = TRUE)[2]-
                  range(data$y[data$outlier], na.rm = TRUE)[1])/horizonscale
        vec_cut <- c()
        # Positive cutpoints
        for (i in 1:(ncut%/%2)) {
          vec_cut <- c(vec_cut, ori+sca*i)
        }
        # Negative cutpoints
        for (i in 1:((ncut+1)%/%2)) {
          vec_cut <- c(vec_cut, ori-sca*i)
        }
      }
    } else {
      ncut <- length(horizonscale)
      vec_cut <- horizonscale
    }
  } else {
    stop('please, provide a valid origin')
  }

  data <- select(data, -outlier)

  if ('xend' %in% names(data)) {
    data <- data %>%
      pivot_longer(c(x, xend)) %>%
      transmute(x = value, y)
  } else {
    data <- data %>% select(x, y)
  }

  vec_cut <- c(sort(vec_cut[vec_cut > ori]),
               rev(sort(vec_cut[vec_cut < ori])))

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


  data$Categories <- factor(data$group, rev(binnames))


  new_vec <- rep(NA, length(cutpoint_vec)-1)
  for (i in 1:(length(cutpoint_vec)-1)) {

    new_vec[i] <- paste0('[', signif(cutpoint_vec[i+1]), ', ',
                         signif(cutpoint_vec[i]), ')')

  }



  new_vec <- setNames(new_vec, rev(binnames))
  data$Cutpoints <- factor(new_vec[data$group], new_vec)
  if (reverse) {
    data <- data %>%
      mutate(
        ymin = ifelse(str_detect(group, 'neg'), 1, 0),
        ymax = ifelse(str_detect(group, 'neg'), 1-ymax, ymax)
      )
  }

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






