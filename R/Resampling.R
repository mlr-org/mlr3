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
#' r$is_instantiated
#' r$checksum
#' ```
#'
#' @section Arguments:
#' * `id` ([Task]):
#'   Task to train/predict on.
#' * `model` (any):
#'   Fitted model as returned by `train`.
#'
#' @section Details:
#' `$new()` creates a new object of class [Learner].
#'
#' `$id` (`character(1)`) stores the identifier of the object.
#'
#' FIXME: Not finished
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
  assert_r6(resampling, "Resampling")
}


assert_resampling_index = function(r, i) {
  if (!r$is_instantiated)
    stopf("Resampling %s has not been instantiated yet", r$id)
  asInt(i, lower = 1L, upper = r$iters)
}
