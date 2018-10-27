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
#' r$instance
#' r$train_set(i)
#' r$test_set(i)
#' r$hash
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
#' `$iters` (`integer(1)`) calculates the resulting number of iterations, given the current `par_vals`.
#'
#' `$par_set` (`[paradox::ParamSet()]`) describes available parameters.
#'
#' `$par_vals` (`named list`) stores the currently set parameter values.
#' You can set parameters by assigning a named list of new parameters to this slot.
#'
#' `$instantiate` materializes fixed training and test splits for a given task.
#'
#' `$is_instantiated` returns `TRUE` if the resampling has been instantiated, and `FALSE` otherwise.
#'
#' `$instance` stores the instantiated realization of the resampling. This is an arbitrary object, do
#'   not work directly with it. Instead, use `$train_set()` and `$test_set()`.
#'
#' `$train_set()` returns the training set for the `i`-th iteration.
#'
#' `$test_set()` returns the test set for the `i`-th iteration.
#'
#' `$hash` stores a checksum (`character(1)`) calculated on the `id`, `par_vals` and the instantiation.
#'   If the object is not instantiated yet, `NA` is returned.
#'
#' @name Resampling
#' @keywords internal
#' @family Resampling
#' @examples
#' r = mlr_resamplings$get("subsampling")
#'
#' # Default parametrization
#' r$par_vals
#'
#' # Do only 3 repeats on 10% of the data
#' r$par_vals = list(ratio = 0.1, repeats = 3)
#' r$par_vals
#'
#'
#' # Instantiate on iris task
#' task = mlr_tasks$get("iris")
#' r$instantiate(task)
#'
#' # Extract train/test sets
#' train_set = r$train_set(1)
#' print(train_set)
#' intersect(train_set, r$test_set(1))
#'
#' # Another example: 10-fold CV
#' r = mlr_resamplings$get("cv")$instantiate(task)
#' r$train_set(1)
NULL

#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NULL,
    par_set = NULL,
    instance = NULL,
    has_duplicates = NA,

    initialize = function(id, par_set = ParamSet$new(), par_vals = list()) {
      self$id = assert_id(id)
      self$par_set = assert_par_set(par_set)
      private$.par_vals = assert_par_vals(par_vals, par_set)
    },

    train_set = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    test_set = function(...) stopf("Method not implemented, should have been overloaded during construction"),
    instantiate = function(...) stopf("Method not implemented, should have been overloaded during construction"),

    print = function(...) {
      pv = self$par_vals
      catf("%s<%s> with %i iterations", if (self$is_instantiated) "Instantiated " else "", class(self)[1L], self$iters)
      catf("Parameters: %s", stri_key_val(pv))
      catf(stri_wrap(initial = "\nPublic: ", setdiff(ls(self), c("initialize", "print"))))
    }
  ),


  active = list(
    hash = function() {
      if (is.null(self$instance))
        return(NA_character_)
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, self$par_vals, self$instance), algo = "xxhash64")
      private$.hash
    },

    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      self$par_set$check(rhs)
      private$.par_vals = rhs
    },

    is_instantiated = function() {
      !is.null(self$instance)
    }
  ),

  private = list(
    .hash = NA_character_,
    .par_vals = NULL,
    .instantiate = function(instance) {
      self$instance = instance
      private$.hash = NA_character_
      self
    }
  )
)
