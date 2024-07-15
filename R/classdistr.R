#' Classified observed data into a distribution class.
#'
#' Based on a series of statistical tests, uses bootstrapping to classify
#' observed data into one of the following distributions: Binary, Uniform, Skewed, Normal or Bimodal.
#' @importFrom moments skewness kurtosis
#' @importFrom nortest ad.test
#' @param x A numeric vector of observations
#' @param n_bootstrap Number of bootstrap iterations
#' @param pc_bootstrap Sampling proportion for bootstrapping
#' @keywords internal
#'

.classify_distribution <- function(data, n_bootstrap = 20, pc_bootstrap = 0.7) {

  # Step 0: Check for binary data
  if (length(unique(data)) == 2) {
    return("Binary")
  }

  # Bootstrap resampling
  bootstrap_results <- replicate(n_bootstrap, {
    sample_data <- sample(data,
                          size = min(c(ceiling(pc_bootstrap*(length(data))), 5000)),
                          replace = FALSE)
    .classify_sample(sample_data)
  })

  # Aggregate results
  result_table <- table(bootstrap_results)
  most_likely_distribution <- names(result_table)[which.max(result_table)]

  return(most_likely_distribution)
}


#' Helper function to classify a single sample
#'
#' @param sample Bootstrap sample
#'
#' @keywords internal

.classify_sample <- function(sample) {
  # Step 1: Uniform Distribution
  ks_test <- ks.test(jitter(sample), "punif",
                     min = min(sample), max = max(sample))
  if (ks_test$p.value > 0.05) {
    return("Uniform")
  }

  # Step 2: Skewed Distribution
  skew <- skewness(sample)
  if (skew > 1) {
    return("Right Skewed")
  } else if (skew < -1) {
    return("Left Skewed")
  }

  # Step 3: Normal Distribution
  shapiro_test <- shapiro.test(sample)
  ad_test <- ad.test(sample)
  if (shapiro_test$p.value > 0.05 && ad_test$p.value > 0.05) {
    return("Normal")
  }

  # Step 4: Bimodal Distribution
  density_est <- density(sample)
  peaks <- which(diff(sign(diff(density_est$y))) == -2) + 1
  if (length(peaks) > 1) {
    return("Bimodal")
  }

  return("Unclassified")
}

#' Helper function to check for outliers
#'
#' @param data Observed data
#'
#' @keywords internal
.check_for_outliers = function(data){
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
#' @importFrom classInt classIntervals
#'
#' @keywords internal
.recommend = function(x,
                      distr,
                      outliers,
                      classInt_pref,
                      nclasses){
  if(distr == "Binary"){
    tmp = list(norm = "minmax", brks = range(x))
  }
  if(distr %in%
     c("Uniform", "Normal", "Bimodal", "Unclassified") &&
     sum(outliers) == 0){
    tmp = list(norm = "minmax", brks = range(x))
  }
  if(sum(outliers) > 0 &&
     distr %in% c("Uniform", "Normal", "Bimodal", "Unclassified")){
    tmp = list(norm = "goalpost", brks = range(x[!outliers]))
  }
  if(grepl("Skewed", distr)){
    if(!is.null(nclasses)){
      tmp = list(norm = classInt_pref, brks = classIntervals(x, n = nclasses, style = classInt_pref)$brks)
    }else{
      tmp = list(norm = classInt_pref, brks = classIntervals(x, style = classInt_pref)$brks)
    }

  }
  return(tmp)
}



