#' @title Leave-One-Out Cross Validation
#'
#' @name mlr_resamplings_loo
#' @include Resampling.R
#'
#' @description
#' Splits data using leave-one-observation-out.
#' This is identical to cross validation with the number of folds set
#' to the number of observations.
#'
#' @templateVar id loo
#' @template section_dictionary_resampling
#'
#' @references
#' `r format_bib("bischl_2012")`
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rcv = rsmp("loo")
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance # vector
ResamplingLOO = R6Class("ResamplingLOO", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(id = "loo", man = "mlr3::mlr_resamplings_loo")
    }
  ),

  active = list(
    #' @field iters (`integer(1)`)\cr
    #' Returns the number of resampling iterations which is the number of rows of the task
    #' provided to instantiate. Is `NA` if the resampling has not been instantiated.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$instance)) NA_integer_ else length(self$instance)
    }
  ),

  private = list(
    .sample = function(ids, ...) {
      shuffle(ids)
    },

    .get_train = function(i) {
      self$instance[-i]
    },

    .get_test = function(i) {
      self$instance[i]
    },

    .combine = function(instances) {
      do.call(c, instances)
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("loo", ResamplingLOO)
