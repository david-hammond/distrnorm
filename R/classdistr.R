#' Classified observed data into a distribution class.
#'
#' Using bootstrapping to estimate likeliness, this function classifies
#' observed data into one of the following: Binary, Uniform, Skewed, Normal or Bimodal.
#' @importFrom moments skewness kurtosis
#' @importFrom nortest ad.test
#' @param x A numeric vector of observations
#' @param n_bootstrap Number of bootstap iterations
#' @examples
#' set.seed(1234)
#'
#' # Uniform distribution test
#' x <- runif(100)
#' classify_distribution(x)
#'
#' # Normal distribution tests
#' x <- rnorm(100)
#' classify_distribution(x)
#'
#' # Skewed data
#' x <- c(1, 2, 2, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100)
#' classify_distribution(x)
#'
#' x <- c(1, 2, 2, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100, 1000000)
#' classify_distribution(x)
#'
#' x <- rlnorm(100)
#' classify_distribution(x)
#'
#' x <- exp(1:100)
#' classify_distribution(x)
#'
#' x <- rpois(100, lambda = 0.5)
#' classify_distribution(x)
#'
#' x <- rweibull(100, shape = 0.5)
#' classify_distribution(x)
#'
#' ### Bimodal data NOT RUN
#' # library(truncnorm)
#' # nn <- 1e4
#' # sims <- c(rtruncnorm(nn/2, a=1, b=5, mean=2, sd=.5),
#' #          rtruncnorm(nn/2, a=1, b=5, mean=4, sd=.5))
#' # classify_distribution(sims)
#' @export
#'

# Function to classify distribution using bootstrapping
classify_distribution <- function(data, n_bootstrap = 1000, pc_bootstrap = 0.9) {

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

counter = 0
for (i in 1:100){
  x = rnorm(100)
  if(classify_distribution(x) == "Normal"){
    counter = counter +1
  }
}

