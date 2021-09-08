

#' Check if a variable is a whole number
#'
#' @param x A number.
#' @param tol Toleration threshold.
#'
#' @keywords internal

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


#' Compute the origin and scale of the horizon plot
#'
#' @param data A data frame.
#' @inheritParams geom_horizon
#'
#' @importFrom stats IQR median quantile
#'
#' @keywords internal

compute_ori_sca <- function(data, origin,
                            horizonscale) {

  # Check if horizonscale is valid
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

  # Sort cutpoints
  vec_cut <- c(sort(vec_cut[vec_cut > ori]),
               rev(sort(vec_cut[vec_cut < ori])))

  list(
    ori = ori,
    vec_cut = vec_cut)

}


