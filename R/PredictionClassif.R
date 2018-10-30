#' @title Prediction Object for Classification
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif].
#'
#' @section Usage:
#' ```
#' p = PredictionClassif$new(task, response, prob = NULL)
#'
#' p$response
#' p$truth
#' p$prob
#'
#' as.data.table(p)
#'
#' @section Arguments:
#' * `task` ([TaskRegr]): Used to extract essential information to assert the correctness of `response`.
#' * `response` (`numeric`): Predicted class labels as `factor` of the same length as number of observations in the test set.
#' * `prob` (`matrix`): Numeric matrix of class probabilities with `task$class_n` columns and rows equal to the number of
#'   observations in the test set. Columns must be named with class levels.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Prediction].
#'
#' `$response` stores the predicted class labels.
#'
#' `$truth` stores the true class labels.
#'
#' `$prob` stores the label probabilities (if available), or is `NULL`.
#'
#' Object can be transformed to a simple [data.table::data.table()] with `data.table::as.data.table()`.
#' @name PredictionClassif
#' @export
#' @family Prediction
#' @keywords internal
NULL

#' @include Prediction.R
PredictionClassif = R6Class("PredictionClassif", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    prob = NULL,
    initialize = function(task, response, prob = NULL) {
      classes = task$levels(task$target_names)
      if (is.character(response))
        response = factor(response, levels = classes)
      self$response = assert_factor(response, len = task$nrow, levels = classes, any.missing = FALSE)
      self$truth = task$truth()[[1L]]

      if (!is.null(prob)) {
        assert_matrix(prob, nrow = task$nrow, ncol = length(classes))
        assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
        assert_names(colnames(prob), permutation.of = classes)
        if (!is.null(rownames(prob)))
          rownames(prob) = NULL
        self$prob = prob[, match(colnames(prob), classes), drop = FALSE]
      }
    }
  )
)

#' @export
as.data.table.PredictionClassif = function(x, ...) {
  tab = data.table(response = x$response, truth = x$truth)
  if (!is.null(x$prob))
    tab[, paste0("prob.", colnames(x$prob)) := as.data.table(x$prob)]
  tab
}
