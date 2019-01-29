#' @title Cross Validation Resampling
#'
#' @format [R6Class] object
#' @name mlr_resamplings_cv
#' @format [R6::R6Class] inheriting from [Resampling].
#'
#' @description
#' Cross validation with `folds` folds (default: 10).
#'
#' @include Resampling.R
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rcv = mlr_resamplings$get("cv")
#' rcv$param_vals = list(folds = 3)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance # table
ResamplingCV = R6Class("ResamplingCV", inherit = Resampling,
  public = list(
    initialize = function() {
      super$initialize(
        id = "cv",
        param_set = ParamSet$new(params = list(ParamInt$new("folds", lower = 1L, tags = "required"))),
        param_vals = list(folds = 10L)
      )
    }
  ),

  active = list(
    iters = function() {
      self$param_vals$folds
    }
  ),

  private = list(
    .sample = function(ids) {
      data.table(
        row_id = ids,
        fold = shuffle(seq_along0(ids) %% self$param_vals$folds + 1L),
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    },

    .combine = function(instances) {
      rbindlist(instances)
    },

    deep_clone = function(name, value) {
      if (name == "instance") copy(value) else value
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("cv", ResamplingCV)
