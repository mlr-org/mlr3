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
#' p = PredictionRegr$new(task = NULL, row_ids = task$row_ids, truth = task$truth(), response = NULL, se = NULL)
#' ```
#'
#' * `task` :: [TaskRegr]\cr
#'   Task, used to extract defaults for `row_ids` and `truth`.
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
#'   Numeric vector of predicted standard errors.
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
#' task = tsk("boston_housing")
#' learner = lrn("regr.featureless", predict_type = "se")
#' p = learner$train(task)$predict(task)
#' p$predict_types
#' head(as.data.table(p))
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), response = NULL, se = NULL) {
      assert_row_ids(row_ids)
      n = length(row_ids)

      self$task_type = "regr"
      self$predict_types = c("response", "se")[c(!is.null(response), !is.null(se))]
      self$data$tab = data.table(
        row_id = row_ids,
        truth = assert_numeric(truth, len = n, null.ok = TRUE)
      )

      if (!is.null(response)) {
        self$data$tab$response = assert_numeric(response, len = n, any.missing = FALSE)
      }

      if (!is.null(se)) {
        self$data$tab$se = assert_numeric(se, len = n, lower = 0, any.missing = FALSE)
      }
    },

    help = function() {
      open_help("mlr3::PredictionRegr")
    }
  ),

  active = list(
    response = function() {
      self$data$tab$response %??% rep(NA_real_, length(self$data$row_ids))
    },

    se = function() {
      self$data$tab$se %??% rep(NA_real_, length(self$data$row_ids))
    },

    missing = function() {
      miss = logical(nrow(self$data$tab))
      if ("response" %in% self$predict_types) {
        miss = is.na(self$response)
      }
      if ("se" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$se)
      }

      self$data$tab$row_id[miss]
    }
  )
)

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  copy(x$data$tab)
}

#' @export
c.PredictionRegr = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionRegr")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Probabilities for some predictions, not all")
  }

  tab = map_dtr(dots, function(p) p$data$tab, .fill = FALSE)

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_id", fromLast = TRUE)
  }

  PredictionRegr$new(row_ids = tab$row_id, truth = tab$truth, response = tab$response, se = tab$se)
}
