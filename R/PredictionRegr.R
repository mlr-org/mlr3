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
#' p = learner$train(task)$predict(task)
#' p$predict_types
#' head(as.data.table(p))
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    initialize = function(row_ids, truth = NULL, response = NULL, se = NULL) {
      self$data$row_ids = assert_row_ids(row_ids)
      n = length(row_ids)
      self$data$truth = assert_numeric(truth, len = n, null.ok = TRUE)
      self$data$response = assert_numeric(response, len = n, any.missing = FALSE, null.ok = TRUE)
      self$data$se = assert_numeric(se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)
      self$task_type = "regr"
    }
  ),

  active = list(
    response = function() self$data$response %??% rep(NA_real_, length(self$data$row_ids)),
    se = function() self$data$se %??% rep(NA_real_, length(self$data$row_ids)),
    missing = function() {
      miss = logical(length(self$data$row_ids))
      if (!is.null(self$data$response))
        miss = miss | is.na(self$data$response)
      if (!is.null(self$data$se))
        miss = miss | is.na(self$data$se)

      self$data$row_ids[miss]
    }
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
c.PredictionRegr = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionRegr")
  assert_flag(keep_duplicates)

  x = map_dtr(dots, function(p) {
    list(row_ids = p$data$row_ids, truth = p$data$truth, response = p$data$response)
  }, .fill = FALSE)

  se = discard(map(dots, function(p) p$data$se), is.null)
  if (length(se) > 0L && length(se) < length(dots)) {
    stopf("Cannot rbind predictions: Standard error for some predictions, not all")
  }
  se = do.call(c, se)

  if (!keep_duplicates) {
    keep = !duplicated(x$row_ids, fromLast = TRUE)
    x = x[keep]
    se = se[keep]
  }

  PredictionRegr$new(row_ids = x$row_ids, truth = x$truth, response = x$response, se = se)
}
