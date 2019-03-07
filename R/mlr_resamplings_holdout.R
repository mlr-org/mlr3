#' @title Holdout Resampling
#'
#' @name mlr_resamplings_holdout
#' @format [R6::R6Class] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @description
#' Splits data into a single training set and a test set.
#' Parameter `ratio` determines the ratio of observation in the train set (default: 2/3).
#'
#' @inheritSection Learner Fields
#' @inheritSection Learner Methods
#'
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
    initialize = function(id = "holdout", param_vals = list(ratio = 2/3)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))),
        param_vals = param_vals
      )
    },

    iters = 1L
  ),

  private = list(
    .sample = function(ids) {
      nr = round(length(ids) * self$param_set$values$ratio)
      ii = shuffle(ids, nr)
      list(train = ii, test = setdiff(ids, ii))
    },

    .get_train = function(i) {
      self$instance$train
    },

    .get_test = function(i) {
      self$instance$test
    },

    .combine = function(instances) {
      list(train = do.call(c, map(instances, "train")), test = do.call(c, map(instances, "test")))
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("holdout", ResamplingHoldout)
