#' @title Prediction Object for Regression
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerRegr], i.e.
#' the predicted response and standard error.
#'
#' @section Construction:
#' ```
#' p = PredictionRegr$new(row_ids, truth, response = NULL, se = NULL)
#' ```
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Row ids of the observations in the test set.
#'
#' * `truth` :: `numeric()`\cr
#'   True (observed) response.
#'
#' * `response` :: `numeric()`\cr
#'   Vector of numeric response values.
#'   One element for each observation in the test set.
#'
#' * `se` :: `numeric()`\cr
#'   Numeric vector of predicted standard error.
#'   One element for each observation in the test set.
#'
#' @section Fields:
#' All fields from [Prediction], and additionally:
#'
#' * `response` :: `numeric()`\cr
#'   Access to the stored predicted response.
#'
#' * `se` :: `numeric()`\cr
#'   Access to the stored standard error.
#'
#' The field `task_type` is set to `"regr"`.
#'
#' @family Prediction
#' @export
#' @examples
#' task = mlr_tasks$get("boston_housing")
#' learner = mlr_learners$get("regr.featureless")
#' learner$predict_type = "se"
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' p$predict_types
#' head(as.data.table(p))
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    response = NULL,
    se = NULL,
    initialize = function(row_ids, truth, response = NULL, se = NULL) {
      self$row_ids = assert_atomic_vector(row_ids)
      self$truth = assert_numeric(truth)
      self$response = assert_numeric(response, null.ok = TRUE)
      self$se = assert_numeric(se, null.ok = TRUE)
      self$task_type = "regr"
      self$predict_types = c("response", "se")[c(!is.null(response), !is.null(se))]
    })
)

#' @export
convert_prediction.TaskRegr = function(task, predicted) {
  n = task$nrow
  assert_numeric(predicted$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(predicted$se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)

  predicted
}

#' @export
as_prediction.TaskRegr = function(task, row_ids, predicted) {
  PredictionRegr$new(row_ids = row_ids, truth = task$truth(row_ids), response = predicted$response, se = predicted$se)
}

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  if (is.null(x$row_ids)) {
    return(data.table())
  }
  data.table(row_id = x$row_ids, truth = x$truth, response = x$response, se = x$se)
}


#' @export
rbind.PredictionRegr = function(...) {
  dots = list(...)
  assert_list(dots, "PredictionRegr")

  x = map_dtr(dots, function(p) {
    list(row_ids = p$row_ids, truth = p$truth, response = p$response, se = p$se)
  }, .fill = FALSE)

  p = PredictionRegr$new(row_ids = x$row_ids, truth = x$truth, response = x$response, se = x$se)
  return(p)
}
