#' Creates a recommended classInt based on the type of distribution.
#'
#' Creates a classdistr R6 class for recommending a classInt based on the shape of the distribution of the observed data
#' @importFrom R6 R6Class
#' @param x A tidy long data frame
#' @param pivot_column The column name on which the pivot will occur
#' @param pivot_value The column name of the values to be pivotted
#' @examples
#'#' @examples
#' set.seed(1234)
#'
#' # Binary distribution test
#' x <- sample(c(0,1), 100, replace = TRUE)
#' classdistr$new(x)
#' # Uniform distribution test
#' x <- runif(100)
#' classdistr$new(x)
#'
#' # Normal distribution tests
#' x <- rnorm(100)
#' classdistr$new(x)
#'
#' # Skewed data
#' x <- c(1, 2, 2, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100)
#' classdistr$new(x)
#'
#' x <- c(1, 2, 2, 2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10, 20, 30, 50, 100, 1000000)
#' classdistr$new(x)
#'
#' x <- rlnorm(100)
#' classdistr$new(x)
#'
#' x <- exp(1:100)
#' classdistr$new(x)
#'
#' x <- rpois(100, lambda = 0.5)
#' classdistr$new(x)
#'
#' x <- rweibull(100, shape = 0.5)
#' classdistr$new(x)
#'
#' ### Bimodal data NOT RUN
#' # library(truncnorm)
#' # nn <- 1e4
#' # sims <- c(rtruncnorm(nn/2, a=1, b=5, mean=2, sd=.5),
#' #          rtruncnorm(nn/2, a=1, b=5, mean=4, sd=.5))
#' # classdistr$new(sims)
#' @export
#'

classdistr <- R6::R6Class("classdistr",
                            #' @description
                            #' Creates a new instance of this [R6][R6::R6Class] class.
                            #' @field data (`numeric()`)\cr
                            #'   Original observations
                            #' @field outliers (`logical()`)\cr
                            #'   Logical vector indicating is observations are outliers
                            #' @field likely_distribution_incl_outliers (`character()`)\cr
                            #'   Likely distribution type including outliers
                            #' @field likely_distribution_excl_outliers (`character()`)\cr
                            #'   Likely distribution type eccluding outliers
                            #' @field recommended_normalisation (`character()`)\cr
                            #'   Recommended class intervals based on distribution
                            #' @field recommended_breaks (`numeric()`)\cr
                            #'   Recommended breaks for classes
                            #' @field number_of_classes (`numeric()`)\cr
                            #'   Number of classes identified
                            lock_objects = FALSE,
                            public = list(
                              data = NULL,
                              outliers = NULL,
                              likely_distribution = NULL,
                              recommended_normalisation = NULL,
                              recommended_breaks = NULL,
                              number_of_classes = NULL,
                              #' @description
                              #' Create a new classdistr object.
                              #' @param x A numeric vector of observations
                              #' @param n_bootstrap Number of bootstrap iterations
                              #' @param pc_bootstrap Sampling proportion for bootstrapping
                              #' @param classInt_preference Prefernce for classInt breaks
                              #' @param nclasses Preference for number of classes for classInt intervals
                              #' @return A new `classdistr` object.
                              initialize = function(x,
                                                    n_bootstrap = 20,
                                                    pc_bootstrap = 0.7,
                                                    classInt_preference = 'jenks',
                                                    nclasses = NULL) {
                                self$data = x
                                self$outliers = .check_for_outliers(x)
                                self$likely_distribution = .classify_distribution(x[!self$outliers])
                                tmp = .recommend(x, self$likely_distribution,
                                                 self$outliers,
                                                 classInt_preference,
                                                 nclasses)
                                self$recommended_normalisation = tmp$norm
                                self$recommended_breaks = tmp$brks
                                self$number_of_classes = length(tmp$brks) - 1

                              },
                              #' @description
                              #' Prints the key and the head matrix
                             print = function() {
                             cat("Likely Distribution: \n")
                             print(self$likely_distribution)
                             cat("Recommended Normalisation: \n")
                             print(self$recommended_normalisation)
                             cat("Recommended Cut Breaks: \n")
                             print(self$recommended_breaks)
                             }

                            )
)

