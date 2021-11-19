#' @title Predict Method for Learners
#'
#' @description
#' Extends the generic [stats::predict()] with a method for [Learner].
#' Note that this function is intended as glue code to be used in third party packages.
#' We recommend to work with the [Learner] directly, i.e. calling `learner$predict()`
#' or `learner$predict_newdata()` directly.
#'
#' Performs the following steps:
#' * Sets additional hyperparameters passed to this function.
#' * Creates a [Prediction] object by calling `learner$predict_newdata()`.
#' * Returns (subset of) [Prediction].
#'
#' @param object ([Learner])\cr
#'   Any [Learner].
#' @param newdata ([data.frame()])\cr
#'   New data to predict on.
#' @param predict_type (`character(1)`)\cr
#'   The predict type to return.
#'   Set to `<Prediction>` to retrieve the complete [Prediction] object.
#'   If set to `NULL` (default), the first predict type for the respective class of the [Learner]
#'   as stored in [mlr_reflections] is used.
#' @param ... (any)\cr
#'   Hyperparameters to pass down to the [Learner].
#'
#' @export
#' @examples
#' task = tsk("spam")
#'
#' learner = lrn("classif.rpart", predict_type = "prob")
#' learner$train(task)
#' predict(learner, task$data(1:3), predict_type = "response")
#' predict(learner, task$data(1:3), predict_type = "prob")
#' predict(learner, task$data(1:3), predict_type = "<Prediction>")
predict.Learner = function(object, newdata, predict_type = NULL, ...) {
  assert_data_frame(newdata)
  assert_string(predict_type, null.ok = TRUE)

  if (...length()) {
    pars = assert_list(list(...), names = "unique")
    tags = object$param_set$tags
    predict_pars = names(tags)[map_lgl(tags, is.element, el = "predict")]
    i = which(names(pars) %nin% predict_pars)
    if (length(i)) {
      stopf("Unknown parameters: %s", str_collapse(names(pars)[i]))
    }

    object = object$clone()
    object$param_set$values = insert_named(object$param_set$values, list(...))
  }

  prediction = object$predict_newdata(newdata)

  if (identical(predict_type, "<Prediction>")) {
    return(prediction)
  }

  predict_type = predict_type %??% head(names(mlr_reflections$learner_predict_types[[object$task_type]]), 1L)
  if (predict_type %nin% prediction$predict_types) {
    stopf("Predict type '%s' not available", predict_type)
  }

  prediction[[predict_type]]
}
