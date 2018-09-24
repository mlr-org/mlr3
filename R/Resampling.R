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
#' `$train_set()` returns the training set for the `i`-th iteration.
#'
#' `$test_set()` returns the test set for the `i`-th iteration.
#'
#' `$hash` returns a unique string hash for the instantiation.
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
    has_duplicates = NA,

    initialize = function(id, par_set = ParamSet$new(), par_vals = list()) {
      self$id = assert_id(id)
      self$par_set = assert_par_set(par_set)
      private$.par_vals = assert_par_vals(par_vals, par_set)
    },

    train_set = method_not_implemented,
    test_set = method_not_implemented,
    instantiate = method_not_implemented,

    print = function(...) {
      pv = self$par_vals
      catf("<%s> with %i iterations", class(self)[1L], self$iters)
      catf("Parameters: %s", paste0(paste(names(pv), pv, sep = "=")), collapse = ", ")
      catf(stri_list("\nPublic: ", setdiff(ls(self), c("initialize", "print"))))
    }
  ),


  active = list(
    hash = function() {
      if (is.null(private$.instance))
        return(NA_character_)
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, private$.par_vals, private$.instance), algo = "xxhash64")
      private$.hash
    },

    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      self$par_set$check(rhs)
      private$.par_vals = rhs
    },

    is_instantiated = function() {
      !is.null(private$.instance)
    }
  ),

  private = list(
    .hash = NA_character_,
    .instance = NULL,
    .par_vals = NULL,
    .instantiate = function(instance) {
      private$.instance = instance
      private$.hash = NA_character_
      self
    }
  )
)
