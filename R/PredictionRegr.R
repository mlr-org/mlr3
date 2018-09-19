#' @title Prediction Object for Regression
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerRegr].
#'
#' @section Usage:
#' ```
#' p = PredictionRegr$new(task, response, se = NULL)
#'
#' p$response
#' p$se
#'
#' as.data.table(p)
#'
#' @section Arguments:
#' * `task` ([TaskRegr]): Used to extract essential information to assert the correctness of `response`.
#' * `response` (`numeric`): Predicted response of the same length as number of observations in the test set.
#' * `se` (`numeric`): Predicted standard error of the same length as number of observations in the test set.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Prediction].
#'
#' `$response` stores the predicted values.
#'
#' `$se` stores the predicted standard errors (if available), or is `NULL`.
#'
#' Object can be transformed to a simple [data.table::data.table()] with `data.table::as.data.table()`.
#' @name PredictionRegr
#' @export
#' @family Prediction
#' @keywords internal
NULL

#' @include Prediction.R
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    se = NULL,
    initialize = function(task, response, se = NULL) {
      self$response = assert_numeric(response, len = task$nrow, any.missing = FALSE)
      self$se = assert_numeric(se, len = task$nrow, any.missing = FALSE, lower = 0, null.ok = TRUE)
    }
  )
)

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  tab = data.table(response = x$response)
  if (!is.null(x$se))
    tab[, "se" := x$se]
  tab
}
