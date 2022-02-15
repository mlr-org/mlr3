#' @title Leave-One-Out Cross-Validation
#'
#' @name mlr_resamplings_loo
#' @include Resampling.R
#'
#' @description
#' Splits data using leave-one-observation-out.
#' This is identical to cross-validation with the number of folds set
#' to the number of observations.
#'
#' If this resampling is combined with the grouping features of tasks,
#' it is possible to create custom splits based on an arbitrary factor variable,
#' see the examples.
#'
#' @templateVar id loo
#' @template resampling
#'
#' @references
#' `r format_bib("bischl_2012")`
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("penguins")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' loo = rsmp("loo")
#' loo$instantiate(task)
#'
#' # Individual sets:
#' loo$train_set(1)
#' loo$validation_set(1)
#'
#' # Disjunct sets:
#' intersect(loo$train_set(1), loo$validation_set(1))
#'
#' # Internal storage:
#' loo$instance # vector
#'
#' # Combine with group feature of tasks:
#' task = tsk("penguins")
#' task$set_col_roles("island", add_to = "group")
#' loo$instantiate(task)
#' loo$iters # one fold for each level of "island"
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
