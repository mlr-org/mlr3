#' @title Prediction Object for Classification
#'
#' @name PredictionClassif
#' @format [R6Class] object inheriting from [Prediction].
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif].
#' If probabilities are provided via construction and response is missing,
#' the response is calculated from the probabilities: the class label with the highest
#' probability is chosen. In case of ties, a random class label of the tied labels picked.
#'
#' @section Usage:
#' Inherits from [Prediction]
#' ```
#' # Construction
#' p = PredictionClassif$new(task, response, prob = NULL)
#'
#' # Members
#' p$predict_types
#' p$response
#' p$row_ids
#' p$truth
#'
#' # S3 methods
#' as.data.table(p)
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]):
#'   Task used for prediction. Used to extract `row_ids` and `truth`.
#'   Set to `NULL` to skip all argument checks during initialization.
#'   Slots `p$row_ids` and `p$truth` need to be set manually in this case
#' * `response` (`factor()` | `character()`): Vector of predicted class labels.
#' * `prob` (`matrix`):
#'   Numeric matrix of class probabilities with one column for each class in `task$all_classes`
#'   and one row for each observation in the test set.
#'
#' @section Details:
#' * `$predict_types` ([character]) stores the predict types available: a subset of `c("response", "se")`.
#' * `$response` stores the predicted values.
#' * `row_ids` stores the row IDs.
#' * `$truth` stores the true values.
#' * `$new()` initializes a new object of class [Prediction].
#' * The prediction object can be transformed to a simple [data.table()]
#'   with [data.table::as.data.table].
#' @export
#' @family Prediction
#' @examples
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' learner$predict_type = "prob"
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' p$predict_types
#' head(as.data.table(p))
NULL

#' @include Prediction.R
PredictionClassif = R6Class("PredictionClassif", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    prob = NULL,
    initialize = function(task = NULL, response = NULL, prob = NULL) {
      predictionclassif_initialize(self, task, response, prob)
    }
  )
)

predictionclassif_initialize = function(self, task, response, prob) {
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
      assert_matrix(prob, nrows = n, ncols = length(classes))
      assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
      assert_names(colnames(prob), permutation.of = classes)
      if (is.null(rownames(prob)))
        rownames(prob) = row_ids
      self$prob = prob[, match(colnames(prob), classes), drop = FALSE]
    }

    if (is.null(response) && !is.null(prob)) {
      # calculate response from prob
      response = factor(colnames(prob)[unname(apply(prob, 1L, which_max))], levels = classes)
    }
  } else {
    if (!is.null(response) && is.character(response))
      response = factor(response)
    assert_factor(response, any.missing = FALSE, null.ok = TRUE)
    assert_matrix(prob, null.ok = TRUE)
    assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1, null.ok = TRUE)
  }

  self$predict_types = c("response", "prob")[c(!is.null(response), !is.null(prob))]
  self$response = response
  self$prob = prob
}

#' @export
as.data.table.PredictionClassif = function(x, ...) {
  tab = as.data.table.Prediction(x)
  if (!is.null(x$prob))
    tab[, paste0("prob.", colnames(x$prob)) := as.data.table(x$prob)]
  tab
}
