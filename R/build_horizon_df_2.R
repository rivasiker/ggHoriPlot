
#' Add together two numbers
#'
#' @import ggplot2
#' @export

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

build_horizon_df_2 <- function(data, origin, horizonscale, rm.outliers, reverse) {

  if (rm.outliers) {
    data <- data %>%
      mutate(
        outlier = between(
          y,
          quantile(y, 0.25, na.rm=T)-1.5*IQR(y, na.rm=T),
          quantile(y, 0.75, na.rm=T)+1.5*IQR(y, na.rm=T)))
  } else {
    data <- data %>%
      mutate(
        outlier = TRUE)
  }

  # If the origin is the median or mean
  if (origin %in% c('median', 'mean', 'midpoint')) {
    # Define origin cutpoint
    ori = ifelse(origin == 'median', median(data$y[data$outlier], na.rm = T), NA)
    ori = ifelse(origin == 'mean', mean(data$y[data$outlier], na.rm = T), ori)
    ori = ifelse(origin == 'midpoint', sum(range(data$y[data$outlier], na.rm = T))/2, ori)
    # If the horizon scale is an integer
    if (length(horizonscale)==1) {
      if (is.wholenumber(horizonscale)) {
        # Save the number of cuts
        ncut <- horizonscale
        # Calculate separation between cutpoints
        sca <- (range(data$y[data$outlier], na.rm = T)[2]-range(data$y[data$outlier], na.rm = T)[1])/horizonscale
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
    if (length(horizonscale)==1) {
      if (is.wholenumber(horizonscale)) {
        ncut <- horizonscale
        ori <- quantile(data$y[data$outlier], (ncut%/%2)/ncut, na.rm = T)
        vec_cut <- c()
        for (i in 0:ncut) {
          if (i != ncut%/%2) {
            vec_cut <- c(vec_cut, quantile(data$y[data$outlier], (i)/ncut, na.rm = T))
          }
        }
      }
    } else {
      stop("The horizonscale should be a positive integer when using origin = 'quantiles'")
    }
    # If the origin is numeric
  } else if (origin == 'min') {
    ncut <- horizonscale
    ori <- min(data$y[data$outlier], na.rm = T)
    sca <- (range(data$y[data$outlier], na.rm = T)[2]-range(data$y[data$outlier], na.rm = T)[1])/horizonscale
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
        sca <- (range(data$y[data$outlier], na.rm = T)[2]-range(data$y[data$outlier], na.rm = T)[1])/horizonscale
        vec_cut <- c()
        # Positive cutpoints
        for (i in 1:(ncut%/%2)) {
          vec_cut <- c(vec_cut, ori+sca*i)
        }
        # Negative cutpoints
        for (i in 1:((ncut+1)%/%2)) {
          vec_cut <- c(vec_cut, ori-sca*i)
        }
      } else {
        stop('The horizonscale should be a vector or a number')
      }

    } else {
      ncut <- length(horizonscale)
      vec_cut <- horizonscale
    }
  }


  data <- select(data, -outlier)

  if ('xend' %in% names(data)) {
    data <- data %>%
      pivot_longer(c(x, xend)) %>%
      transmute(x = value, y)
  } else {
    data <- data %>% select(x, y)
  }

  vec_cut <- c(sort(vec_cut[vec_cut > ori]), rev(sort(vec_cut[vec_cut < ori])))
  # print(ori)
  # print(vec_cut)
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

  data
}






