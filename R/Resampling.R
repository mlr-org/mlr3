#' @title Abstract resampling class
#'
#' @description
#' Abstraction for resampling strategies.
#'
#' Predefined resamplings are stored in [mlr_resamplings].
#'
#' @section Usage:
#' ```
#' r = Resampling$new(id)
#' r$id
#' r$iters
#' r$par_set
#' r$par_vals
#' r$instantiate(task)
#' r$is_instantiated
#' r$train_set(i)
#' r$test_set(i)
#' r$checksum
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   identifier for this object.
#'
#' * `i` (`integer(1)`):
#'   Get the `i`-th training/test set.
#'
#' @section Details:
#' `$new()` creates a new object of class [Resampling].
#'
#' `$id` (`character(1)`) stores the identifier of the object.
#'
#' `iters` (`integer(1)`) calculates the resulting number of iterations, given the current `par_vals`.
#'
#' `par_set` (`[paradox::ParamSet()]`) describes available parameters.
#'
#' `par_vals` (`named list`) stores the currently set parameter values.
#' You can set parameters by assigning a named list of new parameters to this slot.
#'
#' `instantiate` materializes fixed training and test splits for a given task.
#'
#' `is_instantiated` returns `TRUE` if the resampling has been instantiated, and `FALSE` otherwise.
#'
#' `train_set()` returns the training set for the `i`-th iteration.
#'
#' `test_set()` returns the test set for the `i`-th iteration.
#'
#' `checksum` returns a unique string hash for the instantiation.
#'
#' @name Resampling
#' @keywords internal
#' @family Resampling
NULL

#' @include helper_R6.R
#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NULL,
    par_set = NULL,

    initialize = function(id, par_set = ParamSet$new(), par_vals = list()) {
      self$id = assert_id(id)
      self$par_set = assert_par_set(par_set)
      private$.par_vals = assert_par_vals(par_vals, par_set)
    },

    train_set = method_not_implemented,
    test_set = method_not_implemented,
    instantiate = method_not_implemented
  ),

  active = list(
    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      self$par_set$check(rhs)
      private$.par_vals = rhs
    },

    is_instantiated = function() {
      !is.null(private$instance)
    },

    checksum = function() {
      if (is.null(private$instance))
        return(NA_character_)
      if (is.na(private$hash))
        private$hash = digest::digest(private$instance, algo = "murmur32")
      private$hash
    }
  ),

  private = list(
    instance = NULL,
    hash = NA_character_,
    .par_vals = NULL
  )
)


assert_resampling = function(resampling) {
  assert_class(resampling, "Resampling")
}


assert_resampling_index = function(r, i) {
  if (!r$is_instantiated)
    stopf("Resampling %s has not been instantiated yet", r$id)
  assert_int(i, lower = 1L, upper = r$iters, coerce = TRUE)
}
