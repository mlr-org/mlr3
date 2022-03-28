#' @title Insample Resampling
#'
#' @name mlr_resamplings_insample
#' @include Resampling.R
#'
#' @description
#' Uses all observations as training and as test set.
#'
#' @templateVar id insample
#' @template resampling
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("penguins")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' insample = rsmp("insample")
#' insample$instantiate(task)
#'
#' # Train set equal to test set:
#' setequal(insample$train_set(1), insample$test_set(1))
#'
#' # Internal storage:
#' insample$instance # just row ids
ResamplingInsample = R6Class("ResamplingInsample", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(id = "insample",
        label = "Insample Resampling", man = "mlr3::mlr_resamplings_insample")
    },

    #' @template field_iters
    iters = 1L
  ),

  private = list(
    .sample = function(ids, ...) {
      ids
    },

    .get_train = function(i) {
      self$instance
    },

    .get_test = function(i) {
      self$instance
    },

    .combine = function(instances) {
      do.call(c, instances)
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("insample", function() ResamplingInsample$new())
