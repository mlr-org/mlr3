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
    initialize = function(id = "cv") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamInt$new("folds", lower = 1L, tags = "required"))),
        param_vals = list(folds = 10L)
      )
      self$has_duplicates = FALSE
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance[list(i), "row_id", on = "fold"][[1L]]
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

    .combine = function(instances) {
      rbindlist(instances)
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("cv", ResamplingCV)
