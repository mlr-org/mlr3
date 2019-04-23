#' @title Prediction Object for Regression
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerRegr].
#' The field `task_type` is set to `"classif"`.
#'
#' @section Construction:
#' ```
#' p = PredictionRegr$new(task = NULL, response = NULL, se = NULL)
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
#' @note
#' It is possible to initialize this object without any arguments.
#' This allows to manually construct [Prediction] objects in a piecemeal fashion.
#' Required are "row_ids", "truth", and "predict_type".
#' Depending on the value of "predict_types", "response" and "se" must also be set.
#'
#' @section Fields:
#' @inheritSection Prediction Fields
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
