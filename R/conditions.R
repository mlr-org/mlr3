#' @title Learner Classes
#' @name mlr_learner_conditions
#' @param msg (`character(1)`)\cr
#'   Message to be printed.
#' @param ... (`any`)\cr
#'   Additional arguments to be passed to [mlr3misc::stopf()].
#' @param class (`character()`)\cr
#'   Additional classes to be added to the error.
#' @export
error_learner = function(msg, ..., class = NULL) {
  stop(condition_learner(msg, ..., class = class))
}

#' @rdname mlr_learner_conditions
#' @export
condition_learner = function(msg, ..., class = NULL) {
  mlr3misc::condition_mlr3(msg, ..., class = c(class, "mlr3ErrorLearner"))
}

#' @rdname mlr_learner_conditions
#' @export
error_learner_train = function(msg, ..., class = NULL) {
  stop(condition_learner_train(msg, ..., class = class))
}

#' @rdname mlr_learner_conditions
#' @export
condition_learner_train = function(msg, ..., class = NULL) {
  condition_learner(msg, ..., class = c(class, "mlr3ConditionLearnerTrain"))
}

#' @rdname mlr_learner_conditions
#' @export
error_learner_predict = function(msg, ..., class = NULL) {
  stop(condition_learner_predict(msg, ..., class = class))
}

#' @rdname mlr_learner_conditions
#' @export
condition_learner_predict = function(msg, ..., class = NULL) {
  condition_learner(msg, ..., class = c(class, "mlr3ConditionLearnerPredict"))
}
