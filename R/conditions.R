#' @title Learner Classes
#' @name mlr_learner_conditions
#' @param msg (`character(1)`)\cr
#'   Message to be printed.
#' @param ... (`any`)\cr
#'   Additional arguments to be passed to [mlr3misc::stopf()].
#' @param class (`character()`)\cr
#'   Additional classes to be added to the error.
#' @param silent (`logical(1)`)\cr
#'   If `TRUE`, the error is returned as a condition object instead of being stopped.
#' @export
error_learner = function(msg, ..., class = NULL, silent = FALSE) {
  condition = mlr3_error(msg, ..., class = c(class, "mlr3ErrorLearner"))
  if (silent) {
    return(condition)
  }
  stop(condition)
}
#' @rdname mlr_learner_conditions
#' @export
error_learner_train = function(msg, ..., class = NULL, silent = FALSE) {
  condition = error_learner(msg, ..., class = c(class, "mlr3ErrorLearnerTrain"), silent = TRUE)
  if (silent) {
    return(condition)
  }
  stop(condition)
}

#' @rdname mlr_learner_conditions
#' @export
error_learner_predict = function(msg, ..., class = NULL, silent = FALSE) {
  condition = error_learner(msg, ..., class = c(class, "mlr3ErrorLearnerPredict"), silent = TRUE)
  if (silent) {
    return(condition)
  }
  stop(condition)
}
