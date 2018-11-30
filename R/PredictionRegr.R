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
#' * `truth` (`numeric`):\cr
#'   Numeric vector of true response.
#' * `response` (`numeric`):\cr
#'   Numeric vector of predictions. One element for each observation in the test set.
#' * `se` (`numeric`):\cr
#'   Numeric vector of predicted standard error. One element for each observation in the test set.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Prediction].
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
NULL

#' @include Prediction.R
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    se = NULL,
    initialize = function(task = NULL, response = NULL, se = NULL) {
      if (!is.null(task)) {
        self$row_ids = row_ids = task$row_ids[[1L]]
        self$truth = task$truth()
        n = length(row_ids)

        if (!is.null(response)) {
          assert_numeric(response, len = n, any.missing = FALSE)
        }

        if (!is.null(se)) {
          assert_numeric(se, len = n, lower = 0, any.missing = FALSE)
        }

        self$response = response
        self$se = se
      }
    }
  )
)

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  tab = as.data.table.Prediction(x)
  if (!is.null(x$se))
    tab[, "se" := x$se]
  tab
}
