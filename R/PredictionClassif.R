#' @title Prediction Object for Classification
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif].
#'
#' @section Usage:
#' ```
#' p = PredictionClassif$new(truth, response, prob = NULL)
#'
#' p$truth
#' p$response
#' p$prob
#'
#' as.data.table(p)
#'
#' @section Arguments:
#' * `truth` ([factor]): Factor of true class labels (as returned by `task$truth()`). Note that empty levels may not be dropped.
#' * `response` ([factor] | [character]): Vector of predicted class labels. Must have length `length(truth)`.
#' * `prob` ([matrix]): Numeric matrix of class probabilities with `levels(truth)` columns and `length(truth)` rows.
#'   Columns must be named with class levels.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Prediction].
#'
#' `$truth` stores the predicted class labels as factor.
#'
#' `$response` stores the predicted class labels as factor.
#'
#' `$prob` stores the label probabilities (if available) as matrix, or is `NULL`.
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
    initialize = function(truth, response, prob = NULL) {
      self$truth = assert_factor(truth, any.missing = FALSE)
      classes = levels(truth)

      if (is.character(response))
        response = factor(response, levels = classes)
      self$response = assert_factor(response, len = length(truth), levels = classes, any.missing = FALSE)

      if (!is.null(prob)) {
        assert_matrix(prob, nrow = length(truth), ncol = length(classes))
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
