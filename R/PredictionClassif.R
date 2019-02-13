#' @title Prediction Object for Classification
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif].
#' If probabilities are provided via construction and response is missing,
#' the response is calculated from the probabilities: the class label with the highest
#' probability is chosen. In case of ties, a random class label of the tied labels picked.
#'
#' The `task_type` is set to `"classif"`.
#'
#' @section Construction:
#' ```
#' p = PredictionClassif$new(task = NULL, response = NULL, prob = NULL)
#' ```
#'
#' * `task` :: [TaskClassif]\cr
#'   Task for which the predictions are made. Used to extract the row ids and the true
#'   labels. Must be subsetted to test set.
#'
#' * `response` :: (`factor()` | `character()`)\cr
#'   Vector of predicted class labels.
#'   One element for each observation in the test set.
#'
#' * `prob` :: `matrix()`\cr
#'   Numeric matrix of class probabilities with one column for each class
#'   and one row for each observation in the test set.
#'
#' Note that it is allowed to initialize this object without any arguments in order
#' to allow to manually construct [Prediction] objects in a piecemeal fashion.
#' Required are "row_ids", "truth", and "predict_type". Depending on the value of
#' "predict_types", also "response" and "prob" must be set.
#'
#' @inheritSection Prediction Fields
#'
#' @family Prediction
#' @export
#' @examples
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' learner$predict_type = "prob"
#' e = Experiment$new(task, learner)$train()$predict()
#' p = e$prediction
#' p$predict_types
#' head(as.data.table(p))
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
  self$task_type = "classif"
  if (!is.null(task)) {
    self$row_ids = row_ids = task$row_ids
    self$truth = task$truth()
    n = length(row_ids)
    classes = task$all_classes

    if (!is.null(response)) {
      response = factor(response, levels = classes)
      assert_factor(response, len = n, any.missing = FALSE)
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
      i = max.col(prob, ties.method = "random")
      response = factor(colnames(prob)[i], levels = classes)
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
  if (!is.null(x$prob)) {
    prob = as.data.table(x$prob)
    setnames(prob, names(prob), paste0("prob.", names(prob)))
    tab = ref_cbind(tab, prob)
  }

  tab
}
