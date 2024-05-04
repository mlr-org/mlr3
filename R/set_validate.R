#' @title Enable Validation for Learner
#'
#' @description
#' Helper function to configure validation for a learner.
#'
#' @details
#' This is especially useful for learners such as [`mlr3tuning::AutoTuner`] or [`mlr3pipelines::GraphLearner`],
#' where configuring, where the `$validate` fields need to be configured on multiple levels.
#'
#' @param learner (any)\cr
#'   The learner.
#' @param validate (`numeric(1)`, `"inner_valid"`, `"test"`, or `NULL`)\cr
#'   Which validation set to use.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @export
#' @return Modified [`Learner`]
#' @examples
#' learner = lrn("classif.debug")
#' set_validate(learner, 0.2)
#' learner$validate
set_validate = function(learner, validate, ...) {
  if (!"validation" %in% learner$properties) {
    stopf("Learner '%s' does not support validation.", learner$id)
  }
  UseMethod("set_validate")
}

#' @export
set_validate.Learner = function(learner, validate, ...) {
  learner$validate = validate
  invisible(learner)
}
