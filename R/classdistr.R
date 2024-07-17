#' Classified observed data into a distribution class.
#'
#' Based on a series of statistical tests, uses bootstrapping to classify
#' observed data into one of the following distributions: Binary, Uniform,
#' Skewed, Normal or Bimodal.
#' @importFrom moments skewness kurtosis
#' @importFrom nortest ad.test
#' @importFrom stats ks.test quantile shapiro.test density
#' @param x A numeric vector of observations
#' @keywords internal
#'

.classify_distribution <- function(x,
                                   potential_distrs) {

  # Step 0: Check for binary data
  if (length(unique(x)) == 2) {
    return("Binary")
  }else{
    return(.classify_sample(x, potential_distrs))
  }

}


#' Helper function to classify a single sample
#'
#' @param sample sample observations
#' @param potential_distrs The types of distributions to fit
#'
#' @keywords internal

.classify_sample <- function(sample, potential_distrs) {
  tmp <- model_select(sample, potential_distrs)
  tmp <- sub(".*::d", "", attributes(tmp)$density)
  return(tmp)
}

#' Helper function to check for outliers
#'
#' @param data Observed data
#'
#' @keywords internal
.check_for_outliers <- function(data) {
  q <- quantile(data, c(0.25, 0.75))
  iqr <- diff(q)
  lower_fence <- q[1] - 1.5 * iqr
  upper_fence <- q[2] + 1.5 * iqr
  outliers <- data < lower_fence | data > upper_fence
  return(outliers)
}

#' Helper function to check for recommendations
#' @param x The observations
#' @param distr_with_outliers The likely distribution with outliers
#' @param distr_without_outliers The likely distribution without outliers
#' @param outliers Does the data have IQR outliers
#' @param classInt_pref The preferred classInt style
#' @param nclasses The number of desired classes for classInt
#' @param potential_distrs The types of distributions to fit
#' @importFrom classInt classIntervals
#' @importFrom univariateML model_select
#'
#' @keywords internal
.recommend <- function(x,
                       distr,
                       outliers,
                       classint_pref,
                       nclasses) {
  if (distr == "Binary") {
    tmp <- distr
    attributes(tmp)$model <- distr
    tmp <- list(norm = "minmax", brks = range(x),
                mdl = tmp)
  } else {
    if(distr %in%
       c("unif", "norm")){
      if (sum(outliers) == 0) {
        tmp <- list(norm = "minmax", brks = range(x),
                    mdl = model_select(x, distr))
      }
      if (sum(outliers) > 0) {
        tmp <- list(norm = "goalpost", brks = range(x[!outliers]),
                    mdl = model_select(x[!outliers], distr))
      }
    } else{
      if (!is.null(nclasses)) {
        tmp <- list(norm = classint_pref,
                    brks = classIntervals(x, n = nclasses,
                                          style = classint_pref)$brks,
                    mdl = model_select(x, distr))
      } else {
        tmp <- list(norm = classint_pref,
                    brks = classIntervals(x, style = classint_pref)$brks,
                    mdl = model_select(x, distr))
      }
    }
  }

  return(tmp)
}

