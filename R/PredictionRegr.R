#' @title Prediction Object for Regression
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerRegr].
#'
#' @section Usage:
#' ```
#' # Construction
#' p = PredictionRegr$new(truth, response, se = NULL)
#' #
#' p$truth
#' p$response
#' p$se
#' #
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `truth` \[[numeric]\]:\cr
#'   Numeric vector of true response.
#' * `response` \[[numeric]\]:\cr
#'   Numeric vector of predictions. Must have length `length(truth)`.
#' * `se` \[[numeric]\]:\cr
#'   Numeric vector of predicted standard error. Must have length `length(truth)`.
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
#' * The prediction object can be transformed to a simple [`data.table()`][data.table::data.table()]
#'   with [`as.data.table()`][data.table::as.data.table()].
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
    initialize = function(truth, response, se = NULL) {
      self$truth = assert_numeric(truth, any.missing = FALSE)
      self$response = assert_numeric(response, len = length(truth), any.missing = FALSE)
      if (!is.null(se))
        self$se = assert_numeric(se, len = length(truth), any.missing = FALSE, lower = 0)
    }
  )
)

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  tab = data.table(response = x$response, truth = x$truth)
  if (!is.null(x$se))
    tab[, "se" := x$se]
  tab
}
