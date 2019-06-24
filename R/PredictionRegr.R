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
    initialize = function(row_ids, truth = NULL, response = NULL, se = NULL) {
      self$data$row_ids = assert_atomic_vector(row_ids)
      self$data$truth = assert_numeric(truth)
      self$data$response = assert_numeric(response, null.ok = TRUE)
      self$data$se = assert_numeric(se, null.ok = TRUE)
      self$task_type = "regr"
    }
  ),

  active = list(
    response = function() self$data$response %??% rep(NA_real_, length(self$data$truth)),
    se = function() self$data$se %??% rep(NA_real_, length(self$data$truth))
  )
)

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  data = x$data
  if (is.null(data$row_ids)) {
    return(data.table())
  }
  data.table(row_id = data$row_ids, truth = data$truth, response = data$response, se = data$se)
}


#' @export
c.PredictionRegr = function(...) {
  dots = list(...)
  assert_list(dots, "PredictionRegr")

  x = map_dtr(dots, function(p) {
    list(row_ids = p$data$row_ids, truth = p$data$truth, response = p$data$response)
  }, .fill = FALSE)

  se = discard(map(dots, function(p) p$data$se), is.null)
  if (length(se) > 0L && length(se) < length(dots)) {
    stopf("Cannot rbind predictions: Standard error for some experiments, not all")
  }
  se = do.call(c, se)

  PredictionRegr$new(row_ids = x$row_ids, truth = x$truth, response = x$response, se = se)
}
