#' Creates a recommended classInt based on the type of distribution.
#'
#' Creates a classdistr R6 class for recommending a classInt based on the shape of the distribution of the observed data
#' @importFrom R6 R6Class
#' @param x A tidy long data frame
#' @param pivot_column The column name on which the pivot will occur
#' @param pivot_value The column name of the values to be pivotted
#' @examples
#' data(wb)
#' mdl <- tidymodl$new(wb,
#'                     pivot_column = "indicator",
#'                     pivot_value = "value")
#' ### Use mdldata for modelling
#' fit = lm(data = mdl$mdldata, gni ~ gcu + ppt)
#'
#' ### Can be used to add a yhat value for processed data
#'
#' ### This is useful for imputation purposes as below
#'
#' ### NOT RUN
#' # Use for xgboost imputation
#' # library(mixgb)
#' # imp = mixgb(mdl$mdldata, save.models = T)
#' # tmp = mdl$reconstitute(imp$imputed.data[[1]])
#'
#' ### NOT RUN
#' # Use for mice imputation
#' # library(mice)
#' # imp = mice(as.data.frame(scale(mdl$matrix)), print = FALSE)
#' # tmp = mdl$reconstitute(complete(imp))
#'
#' ### In this example we will just use dummy new data
#' nc = ncol(mdl$mdldata)
#' nr = nrow(mdl$mdldata)
#' dm = nc*nr
#' dummy = matrix(runif(dm),
#'         ncol = nc) |>
#'         data.frame()
#' names(dummy) = names(mdl$mdldata)
#' tmp = mdl$reconstitute(dummy)
#'

#' @export
#'

classdistr <- R6::R6Class("classdistr",
                            #' @description
                            #' Creates a new instance of this [R6][R6::R6Class] class.
                            #' @field data (`data.frame()`)\cr
                            #'   The original tidy long data frame
                            #' @field mdldata (`data.frame()`)\cr
                            #'   The model matrix version of the data
                            lock_objects = FALSE,
                            public = list(
                              data = NULL,
                              outliers = NULL,
                              likely_distribution_incl_outliers = NULL,
                              likely_distribution_excl_outliers = NULL,
                              recommended_classInt = NULL,
                              recommended_classInt_brks = NULL,
                              #' @description
                              #' Create a new tidymodl object.
                              #' @param df A tidy long data frame
                              #' @param pivot_column The column name on which the pivot will occur
                              #' @param pivot_value The column name of the values to be pivotted
                              #' @return A new `tidymodl` object.
                              initialize = function(x,
                                                    pivot_column,
                                                    pivot_value) {
                                self$data = df
                                tmp = .get_dm(df,
                                              pivot_column,
                                              pivot_value)
                                self$mdldata = tmp$mat[,-1]
                                private$key_ind = tmp$mat$id
                                private$master = tmp$master
                                private$key = tmp$key
                              },
                              #' @description
                              #' Adds a results matrix
                              #' @param m A results matrix
                              reconstitute = function(m = NULL) {
                                if(!is.null(m)){
                                  df = private$master
                                  m = as.data.frame(m)
                                  m$id = private$key_ind
                                  m = m |>
                                    pivot_longer(cols = -id, names_to = "vid", values_to = "yhat")
                                  df = df |> left_join(m, by = c("vid", "id"))
                                  df = df[, c(names(self$data), "yhat")]
                                }else{
                                  df = self$data
                                }
                                return(df)
                              },
                              #' @description
                              #' Prints the key and the head matrix
                              print = function() {
                                cat("Variable Key: \n")
                                print(private$key)
                                cat("Head Data Matrix: \n")
                                print(head(self$mdldata, 5))
                              }
                            ),
                        private = list(
                          master = NULL,
                          key_ind = NULL,
                          key = NULL
                        )
)

#' Get pivot_column key data frame
#'
#' @param tmp A long data frame
#' @param pivot_column The column name on which the pivot will occur
#'
#'
#' @keywords internal

.get_key = function(tmp, pivot_column){
  key = data.frame(variable = unique(tmp[,pivot_column]), vid = apply(unique(tmp[,pivot_column]), 1, tolower))
  names(key)[1] = pivot_column
  key$vid = tm::removePunctuation(key$vid)
  key$vid = tm::removeWords(key$vid, words = stopwords())
  key$vid = abbreviate(key$vid, minlength = 3)
  key$vid = make.unique(key$vid)
  return(key)
}

#' Get pivot_column key data frame
#'
#' @param df A long data frame
#' @param pivot_column The column name on which the pivot will occur
#' @param pivot_value The column name of the values to be pivoted
#'
#' @keywords internal
.get_dm = function(df, pivot_column, pivot_value){
  obs_fields = paste0(".data$", setdiff(names(df), pivot_value))
  cmd <- paste0("df <- df |> complete(", paste(obs_fields, collapse = ","), ")")
  eval(parse(text = cmd))
  key = .get_key(df, pivot_column)
  df = df |> left_join(key, by = pivot_column) |>
    select(-all_of(pivot_column))
  obs_fields = setdiff(names(df), c("vid", pivot_value))
  y = decompose_table(df, "id", all_of(obs_fields))
  y$matrix = y$child_table |>
    pivot_wider(names_from = .data$vid, values_from = .data$value)
  df = df |> left_join(y$parent_table, by = c("iso3c", "year")) |>
    left_join(key, by = "vid")
  tmp = list(mat = y$matrix, master = df, key = key)
  return(tmp)
}

