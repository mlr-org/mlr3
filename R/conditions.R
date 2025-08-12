#' @title Learner Conditions
#' @name mlr_learner_conditions
#'
#' @description
#' These functions are used to create conditions for errors related to [`Learner`]s.
#' This extends those from [mlr3misc::mlr_conditions].
#'
#' @section Errors:
#' * `Mlr3ErrorLearner`: Base class for mlr3 errors.
#' * `Mlr3ErrorLearnerTrain`: Errors during training.
#' * `Mlr3ErrorLearnerPredict`: Errors during prediction.
#'
#' @param msg (`character(1)`)\cr
#'   Message to be printed. (formatted with `sprintf()`)
#' @param ... (`any`)\cr
#'   Additional arguments to be passed to `sprintf()`.
#' @param class (`character()`)\cr
#'   Additional classes to be added to the error.
#' @param signal (`logical(1)`)\cr
#'   If `TRUE`, the error object is returned without stopping the interpreter.
#' @export
error_learner = function(msg, ..., class = NULL, signal = TRUE) {
  error_mlr3(msg, ..., class = c(class, "Mlr3ErrorLearner"), signal = signal)
}

#' @rdname mlr_learner_conditions
#' @export
error_learner_train = function(msg, ..., class = NULL, signal = TRUE) {
  error_learner(msg, ..., class = c(class, "Mlr3ErrorLearnerTrain"), signal = signal)
}

#' @rdname mlr_learner_conditions
#' @export
error_learner_predict = function(msg, ..., class = NULL, signal = TRUE) {
  error_learner(msg, ..., class = c(class, "Mlr3ErrorLearnerPredict"), signal = signal)
}
