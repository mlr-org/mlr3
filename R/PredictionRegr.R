#' @title Prediction Object for Regression
#'
#' @name PredictionRegr
#' @format [R6Class] object inheriting from [Prediction].
#' @description
#' This object stores the predictions returned by a learner of class [LearnerRegr].
#'
#' @section Usage:
#'
#' Inherits from [Prediction]
#'
#' ```
#' # Construction
#' p = PredictionRegr$new(task = NULL, response = NULL, se = NULL)
#'
#' # Members
#' p$task_type
#' p$predict_types
#' p$response
#' p$row_ids
#' p$se
#' p$truth
#'
#' # S3 methods
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]):
#'   Task used for prediction. Used to extract `row_ids` and `truth`.
#'   Set to `NULL` to skip all argument checks during initialization.
#'   Slots `p$row_ids` and `p$truth` need to be set manually in this case
#' * `truth` (`numeric()`):
#'   Numeric vector of true response.
#' * `response` (`numeric()`):
#'   Numeric vector of predictions. One element for each observation in the test set.
#' * `se` (`numeric()`):
#'   Numeric vector of predicted standard error. One element for each observation in the test set.
#'
#' @section Details:
#' * `$task_type` (`character(1)`) stores the task type this prediction object is intended for: `"regr"`.
#' * `$predict_types` ([character]) stores the predict types available: a subset of `c("response", "se")`.
#' * `$response` stores the predicted values.
#' * `row_ids` stores the row IDs.
#' * `$se` stores the predicted standard errors (if available), or is `NULL`.
#' * `$truth` stores the true values.
#' * `$new()` initializes a new object of class [Prediction].
#' * The prediction object can be transformed to a simple [data.table()]
#'   with [data.table::as.data.table].
#' @family Prediction
#' @examples
#' task = mlr_tasks$get("bh")
#' learner = mlr_learners$get("regr.featureless")
#' learner$predict_type = "se"
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' p$predict_types
#' head(as.data.table(p))
NULL

#' @export
#' @include Prediction.R
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    se = NULL,
    initialize = function(task = NULL, response = NULL, se = NULL) {
      predictionregr_initialize(self, task, response, se)
    }
  )
)

predictionregr_initialize = function(self, task, response, se) {
  self$task_type = "regr"
  if (!is.null(task)) {
    self$row_ids = row_ids = task$row_ids
    self$truth = task$truth()
    n = length(row_ids)
  } else {
    n = NULL
  }

  self$predict_types = c("response", "se")[c(!is.null(response), !is.null(se))]
  self$response = assert_numeric(response, len = n, any.missing = FALSE, null.ok = TRUE)
  self$se = assert_numeric(se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)
}

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  tab = as.data.table.Prediction(x)
  if (!is.null(x$se)) {
    tab = insert_named(tab, list("se" = x$se))
  }
  tab
}
