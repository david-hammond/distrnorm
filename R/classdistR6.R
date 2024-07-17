#' Creates a recommended classInt based on the type of distribution.
#'
#' Creates a modldistr R6 class for recommending a classInt based on the shape
#' of the distribution of the observed data
#' @importFrom R6 R6Class
#' @importFrom COINr n_prank
#' @importFrom piecenorms piecenorm
#' @param x A numeric vector of observations
#' @param polarity Which direction should the normalisation occur, defaults to
#' 1 but can either be:
#' \itemize{
#' \item \strong{1:}: Lowest value is normalised to 0, highest value is
#' normalised to 1
#' \item \strong{-1:} Highest value is normalised to 0, lowest value is
#' normalised to 1
#' }
#' @param n_bootstrap Number of bootstrap iterations
#' @param pc_bootstrap Sampling proportion for bootstrapping
#' @param classint_pref Preference for classInt breaks
#' @param num_classes_pref Preference for number of classInt breaks
#' @examples
#' set.seed(1234)
#'
#' # Binary distribution test
#' x <- sample(c(0,1), 100, replace = TRUE)
#' modldistr$new(x)
#' # Uniform distribution test
#' x <- runif(100)
#' modldistr$new(x)
#'
#' # Normal distribution tests
#' x <- rnorm(100)
#' modldistr$new(x)
#'
#' # Skewed data
#' x <- c(1, 2, 2, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100)
#' modldistr$new(x)
#'
#' x <- c(1, 2, 2, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100, 1000000)
#' modldistr$new(x)
#'
#' x <- rlnorm(100)
#' modldistr$new(x)
#'
#' x <- exp(1:100)
#' modldistr$new(x)
#'
#' x <- rpois(100, lambda = 0.5)
#' modldistr$new(x)
#'
#' x <- rweibull(100, shape = 0.5)
#' modldistr$new(x)
#'
#' ### Bimodal data NOT RUN
#' # library(truncnorm)
#' # nn <- 1e4
#' # sims <- c(rtruncnorm(nn/2, a=1, b=5, mean=2, sd=.5),
#' #          rtruncnorm(nn/2, a=1, b=5, mean=4, sd=.5))
#' # modldistr$new(sims)
#' @export
#'

modldistr <- R6::R6Class("modldistr",
  #' @description
  #' Creates a new instance of this [R6][R6::R6Class]
  #' class.
  lock_objects = FALSE,
  public = list(
                #' @field data (`numeric()`)\cr
                #'   Original observations
                data = NULL,
                #' @field outliers (`logical()`)\cr
                #'   Logical vector indicating is observations are
                #'   outliers
                outliers = NULL,
                #' @field fitted_distribution (`character()`)\cr
                #'   Suggested distribution
                fitted_distribution = NULL,
                #' @field normalisation (`character()`)\cr
                #'   Recommended class interval style based on
                #'   distribution
                normalisation = NULL,
                #' @field breaks (`numeric()`)\cr
                #'   Recommended breaks for classes
                breaks = NULL,
                #' @field number_of_classes (`numeric()`)\cr
                #'   Number of classes identified
                number_of_classes = NULL,
                #' @field normalised_data (`numeric()`)\cr
                #'   Normalised values based on recommendations
                normalised_data = NULL,
                #' @field polarity (`numeric(1)`)\cr
                #'   Which direction should the normalisation occur
                polarity = NULL,
                #' @field percentiles (`numeric()`)\cr
                #'   Observation percentiles
                percentiles = NULL,
                #' @field fittedmodel (`character()`)\cr
                #'   Fitted univariate model
                fittedmodel = NULL,
                #' @field model (`univariateML()`)\cr
                #'   Fitted univariate model parameters
                model = NULL,
                #' @description
                #' Create a new modldistr object.
                #' @param x A numeric vector of observations
                #' @param n_bootstrap Number of bootstrap iterations
                #' @param pc_bootstrap Sampling proportion for bootstrapping
                #' @param classint_preference Prefernce for classInt breaks
                #' @param num_classes_pref Preference for number of classes for
                #' classInt intervals
                #' @param potential_distrs The types of distributions to fit
                #' @return A new `modldistr` object.
                initialize = function(x,
                                      polarity = 1,
                                      n_bootstrap = 20,
                                      pc_bootstrap = 0.8,
                                      classint_preference = "jenks",
                                      num_classes_pref = NULL,
                                      potential_distrs = c("unif",
                                                           "power",
                                                           "norm",
                                                           "lnorm",
                                                           "weibull",
                                                           "pareto",
                                                           "exp")) {
                  stopifnot("The data has no variance, execution halted." =
                              sd(x) != 0)
                  self$data <- x
                  self$outliers <- .check_for_outliers(x)
                  self$polarity <- polarity
                  tmp <-
                    .classify_distribution(x,
                                           n_bootstrap,
                                           pc_bootstrap,
                                           potential_distrs)
                  tmp <- .recommend(x, tmp,
                                    self$outliers,
                                    classint_preference,
                                    num_classes_pref)
                  self$normalisation <- tmp$norm
                  self$breaks <- tmp$brks
                  self$fitted_distribution <- attributes(tmp$mdl)$model
                  self$distrmodel = tmp$mdl
                  self$number_of_classes <- length(tmp$brks) - 1
                  self$normalised_data <-
                    piecenorm(x, self$breaks, self$polarity)
                  self$percentiles <- n_prank(x)
                },
                #' @description Prints the modldistr
                print = function() {
                  cat("Likely Distribution: \n")
                  print(self$fitted_distribution)
                  cat("Suggested Normalisation: \n")
                  print(self$normalisation)
                  cat("Suggested Breaks: \n")
                  print(self$breaks)
                },
                #' @description Plots the normalised values against the original
                plot = function() {
                  plot(self$data, self$normalised_data,
                       xlab = "Original",
                       ylab = "Normalised")
                },
                #' @description Histogram of normalised values against the
                #' original
                hist = function() {
                  # Set up the plotting area to have 2 rows and 1 column
                  par(mfrow = c(2, 1))

                  # Plot the original data histogram
                  hist(self$data,
                       main = "Original Distribution",
                       xlab = "Original")

                  # Plot the normalised data histogram
                  hist(self$normalised_data,
                       main = "Normalised Distribution",
                       xlab = "Normalised")

                  # Reset the plotting area to the default single plot layout
                  par(mfrow = c(1, 1))
                },
                #' @description Allows user to set manual breaks
                #' @param brks User Defined Breaks
                setManualBreaks = function(brks) {
                  self$breaks <- brks
                  self$normalisation <- "Manual Breaks"
                  self$number_of_classes <- length(brks) - 1
                  self$normalised_data <-
                    piecenorm(self$data, brks, self$polarity)
                })
)
