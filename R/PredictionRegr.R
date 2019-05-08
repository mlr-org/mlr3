#' @title Prediction Object for Regression
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerRegr].
#'
#' @section Construction:
#' ```
#' p = PredictionRegr$new(task = NULL, response = NULL, se = NULL,
#'   row_ids = task$row_ids, truth = task$truth())
#' ```
#'
#' * `task` :: [TaskRegr]\cr
#'   Task for which the predictions are made. Used to extract the row ids and the true
#'   response. Must be subsetted to test set.
#'
#' * `response` :: `numeric()`\cr
#'   Vector of numeric response values.
#'   One element for each observation in the test set.
#'
#' * `se` :: `numeric()`\cr
#'   Numeric vector of predicted standard error.
#'   One element for each observation in the test set.
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Row ids of the task. Per default, these are extracted from the `task`.
#'
#' * `truth` :: `numeric()`\cr
#'   True (observed) response. Per default, this is extracted from the `task`.
#'
#' @section Fields:
#' See [Prediction].
#'
#' The field `task_type` is set to `"regr"`.
#'
#' @family Prediction
#' @export
#' @examples
#' task = mlr_tasks$get("bh")
#' learner = mlr_learners$get("regr.featureless")
#' learner$predict_type = "se"
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' p$predict_types
#' head(as.data.table(p))
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    se = NULL,
    initialize = function(task = NULL, response = NULL, se = NULL, row_ids = task$row_ids, truth = task$truth()) {
      predictionregr_initialize(self, task, row_ids, truth, response, se)
    }
  )
)

predictionregr_initialize = function(self, task, row_ids, truth, response, se) {
  self$task_type = "regr"
  self$row_ids = assert_atomic_vector(row_ids)
  n = length(row_ids)

  self$truth = assert_numeric(truth, len = n, any.missing = FALSE)
  self$response = assert_numeric(response, len = n, any.missing = FALSE, null.ok = TRUE)
  self$se = assert_numeric(se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)
  self$predict_types = c("response", "se")[c(!is.null(response), !is.null(se))]
}

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  if (is.null(x$row_ids))
    return(data.table())
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
