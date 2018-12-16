#' @title Prediction Object for Regression
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerRegr].
#'
#' @section Usage:
#' ```
#' # Construction
#' p = PredictionRegr$new(task = NULL, response = NULL, se = NULL)
#' #
#' p$predict_types
#' p$truth
#' p$response
#' p$se
#' #
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]):\cr
#'   Task used for prediction. Used to extract `row_ids` and `truth`.
#'   Set to `NULL` to skip all argument checks during initialization.
#'   Slots `p$row_ids` and `p$truth` need to be set manually in this case
#' * `truth` (`numeric()`):\cr
#'   Numeric vector of true response.
#' * `response` (`numeric()`):\cr
#'   Numeric vector of predictions. One element for each observation in the test set.
#' * `se` (`numeric()`):\cr
#'   Numeric vector of predicted standard error. One element for each observation in the test set.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Prediction].
#'
#' * `$predict_types` ([character()]) stores the predict types available: a subset of `c("response", "se")`.
#'
#' * `$truth` stores the true values.
#'
#' * `$response` stores the predicted values.
#'
#' * `$se` stores the predicted standard errors (if available), or is `NULL`.
#'
#' * The prediction object can be transformed to a simple [data.table::data.table()]
#'   with [data.table::as.data.table()].
#' @name PredictionRegr
#' @export
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
  if (!is.null(task)) {
    self$row_ids = row_ids = task$row_ids[[1L]]
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
  if (!is.null(x$se))
    tab[, "se" := x$se]
  tab
}
