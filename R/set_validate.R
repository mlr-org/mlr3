#' @title Enable Validation for Learner
#'
#' @description
#' Helper function to configure the `$validate` field(s) of a [`Learner`].
#'
#' This is especially useful for learners such as `AutoTuner` of \CRANpkg{mlr3tuning} or `GraphLearner` of \CRANpkg{mlr3pipelines} which have multiple levels of `$validate` fields.,
#' where the `$validate` fields need to be configured on multiple levels.
#'
#' @param learner (any)\cr
#'   The learner.
#' @param validate (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)\cr
#'   Which validation set to use.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @export
#' @return Modified [`Learner`]
#' @rdname mlr_sugar
#' @examples
#' learner = lrn("classif.debug")
#' set_validate(learner, 0.2)
#' learner$validate
set_validate = function(learner, validate, ...) {
  UseMethod("set_validate")
}

#' @export
set_validate.Learner = function(learner, validate, ...) {
  if (!"validation" %chin% learner$properties) {
    error_input("Learner '%s' does not support validation.", learner$id)
  }
  learner$validate = validate
  invisible(learner)
}
