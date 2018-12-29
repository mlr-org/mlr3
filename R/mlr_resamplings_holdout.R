#' @title Holdout Resampling
#'
#' @format [R6Class] object
#' @name mlr_resamplings_holdout
#' @format [R6::R6Class] inheriting from [Resampling].
#'
#' @description
#' Simple holdout: A single split into training and test.
#' Parameter `ratio` determines the ratio of observation in the train set.
#'
#' @include Resampling.R
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rho = mlr_resamplings$get("holdout")
#' rho$instantiate(task)
#'
#' # Individual sets:
#' rho$train_set(1)
#' rho$test_set(1)
#' intersect(rho$train_set(1), rho$test_set(1))
#'
#' # Internal storage:
#' rho$instance # simple list
ResamplingHoldout = R6Class("ResamplingHoldout", inherit = Resampling,
  public = list(
    initialize = function(id = "holdout") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))),
        param_vals = list(ratio = 2/3)
      )
      self$has_duplicates = FALSE
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$train
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$test
    },

    iters = 1L
  ),

  private = list(
    .sample = function(ids) {
      nr = round(length(ids) * self$param_vals$ratio)
      ii = shuffle(ids, nr)
      list(train = ii, test = setdiff(ids, ii))
    },
    .combine = function(instances) {
      list(train = do.call(c, map(instances, "train")), test = do.call(c, map(instances, "test")))
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("holdout", ResamplingHoldout)
