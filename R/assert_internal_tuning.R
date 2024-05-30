#' Assert Internal Tuning
#'
#' Asserts that internal tuning is correctly configured for a [`Learner`].
#' @param learner ([`Learner`])\cr
#'   The learner.
#' @param ids (`character()`)\cr
#'   The parameter ids to check.
#' @param ... (any)\cr
#'   Additional parameters.
#' @export
#' @keywords internal
assert_internal_tuning = function(learner, ids, ...) {
  assert_subset(ids, learner$param_set$ids(tags = "internal_tuning"))
  UseMethod("assert_internal_tuning")
}

#' @export
assert_internal_tuning.default = function(learner, ids, ...) {
  learner
}
