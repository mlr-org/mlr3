#' @title Prediction Object for Classification
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif].
#'
#' @section Usage:
#' ```
#' # Construction
#' p = PredictionClassif$new(task, response, prob = NULL)
#' #
#' p$predict_types
#' p$truth
#' p$response
#' p$prob
#' #
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]):\cr
#'   Task used for prediction. Used to extract `row_ids` and `truth`.
#'   Set to `NULL` to skip all argument checks during initialization.
#'   Slots `p$row_ids` and `p$truth` need to be set manually in this case
#' * `response` (`factor()` | `character()`):\cr
#'   Vector of predicted class labels.
#' * `prob` (`matrix`):\cr
#'   Numeric matrix of class probabilities with one column for each class in `task$all_classes`
#'   and one row for each observation in the test set.
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Prediction].
#'
#' * `$predict_types` ([character()]) stores the predict types available: a subset of `c("response", "prob")`.
#'
#' * `$truth` stores the predicted class labels as factor.
#'
#' * `$response` stores the predicted class labels as factor.
#'
#' * `$prob` stores the label probabilities (if available) as matrix, or is `NULL`.
#'
#' * The prediction object can be transformed to a simple [data.table::data.table()]
#'   with [data.table::as.data.table()].
#' @name PredictionClassif
#' @export
#' @family Prediction
NULL

#' @include Prediction.R
PredictionClassif = R6Class("PredictionClassif", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    prob = NULL,
    initialize = function(task = NULL, response = NULL, prob = NULL) {
      if (!is.null(task)) {
        self$row_ids = row_ids = task$row_ids[[1L]]
        self$truth = task$truth()
        n = length(row_ids)
        classes = task$all_classes

        if (!is.null(response)) {
          if (is.character(response))
            response = factor(response, levels = classes)
          assert_factor(response, len = n, levels = classes, any.missing = FALSE)
        }

        if (!is.null(prob)) {
          assert_matrix(prob, nrow = n, ncol = length(classes))
          assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
          assert_names(colnames(prob), permutation.of = classes)
          if (!is.null(rownames(prob)))
            rownames(prob) = row_ids
          self$prob = prob[, match(colnames(prob), classes), drop = FALSE]
        }

        if (is.null(response) && !is.null(prob)) {
          # calculate response from prob
          response = factor(colnames(prob)[unname(apply(prob, 1L, which_max))], levels = classes)
        }
      }

      self$predict_types = c("response", "prob")[c(!is.null(response), !is.null(prob))]
      self$response = response
      self$prob = prob
    }
  )
)

#' @export
as.data.table.PredictionClassif = function(x, ...) {
  tab = as.data.table.Prediction(x)
  if (!is.null(x$prob))
    tab[, paste0("prob.", colnames(x$prob)) := as.data.table(x$prob)]
  tab
}
