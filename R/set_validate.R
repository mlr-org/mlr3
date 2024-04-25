#' @title Enable Validation for Learner
#' @description
#' Helper function to configure validation for a learner.
#'
#' @details
#' This is especially useful for learners such as [`mlr3tuning::AutoTuner`] or [`mlr3pipelines::GraphLearner`],
#' where configuring, where the validation set needs to be configured on different levels.
#'
#' @param learner (any)\cr
#'   The learner.
#' @param validate (`numeric(1)`, `"inner_valid"` or `NULL`)\cr
#'   Which validation set to use.
#' @param ... (any)\cr
#'   Additional arguments passed to the methods.
#'
#' @export
#' @return The modified learner.
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
