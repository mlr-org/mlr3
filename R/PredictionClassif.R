#' @title Prediction Object for Classification
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif].
#' The field `task_type` is set to `"classif"`.
#'
#' If probabilities are provided via construction and response is missing,
#' the response is calculated from the probabilities: the class label with the highest
#' probability is chosen. In case of ties, a label is selected randomly.
#'
#' It is possible to set the probability threshold if probabilities are stored:
#'
#' * For binary problems only a single threshold value can be set.
#'   If the probability exceeds the threshold, the positive class is predicted.
#'   If the probability equals the threshold, the label is selected randomly.
#' * For binary and multi-class problems, a named numeric vector of thresholds can be set.
#'   The length and names must correspond to the number of classes and class names, respectively.
#'   To determine the class label, the probabilities are divided by the threshold.
#'   This results in a ratio > 1 if the probability exceeds the threshold, and a ratio < 1 otherwise.
#'   Note that it is possible that either none or multiple ratios are greater than 1 at the same time.
#'   Anyway, the class label with maximum ratio is determined.
#'   In case of ties in the ratio, one of the tied class labels is selected randomly.
#'
#' @note
#' It is possible to initialize this object without any arguments.
#' This allows to manually construct [Prediction] objects in a piecemeal fashion.
#' Required are "row_ids", "truth", "predict_type" and "class_names".
#' Depending on the value of "predict_types", "response" and "prob" must also be set.
#'
#' The factor objects "truth" and "response" must have identical levels (this includes their order).
#' For binary classification, the true label should be the first level.
#'
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
#' * `response` :: `factor()`\cr
#'   Vector of predicted class labels.
#'   One element for each observation in the test set.
#'
#' * `prob` :: `matrix()`\cr
#'   Numeric matrix of class probabilities with one column for each class
#'   and one row for each observation in the test set.
#'
#' @section Fields:
#' * `threshold` :: `numeric(1)`\cr
#'   Probability threshold between 0 and 1.
#'   Assigning a value to this field modifies the stored responses.
#'
#' * `confusion` :: `matrix()`\cr
#'   Confusion matrix resulting from the comparison of truth and response.
#'   Truth is in columns, predicted response in rows.
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
#'
#' # confusion matrix
#' p$confusion
#'
#' # change threshold
#' p$threshold = mlr3misc::set_names(c(0.05, 0.9, 0.05), task$class_names)
#' p$confusion
PredictionClassif = R6Class("PredictionClassif", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    prob = NULL,
    initialize = function(task = NULL, response = NULL, prob = NULL) {
      predictionclassif_initialize(self, task, response, prob)
    }
  ),

  active = list(
    threshold = function(rhs) {
      if (missing(rhs))
        return(private$.threshold)
      if (!is.matrix(self$prob))
        stopf("Cannot set threshold, no probabilities available")
      lvls = colnames(self$prob)

      if (length(rhs) == 1L) {
        if (length(lvls) != 2L)
          stopf("Setting a single threshold only supported for binary classification problems")
        assert_number(rhs, lower = 0, upper = 1)
        ind = max.col(cbind(self$prob[, 1L], rhs), ties.method = "random")
      } else {
        assert_numeric(rhs, any.missing = FALSE, lower = 0, upper = 1, len = length(lvls))
        assert_names(names(rhs), permutation.of = lvls)
        rhs = rhs[lvls] # reorder rhs so it is in the same order as levels

        # multiply all rows by threshold, then get index of max element per row
        w = ifelse(rhs > 0, 1 / rhs, Inf)
        ind = max.col(self$prob %*% diag(w), ties.method = "random")
      }
      private$.threshold = rhs
      self$response = factor(lvls[ind], levels = lvls)
    },

    confusion = function() {
      table(response = self$response, truth = self$truth, useNA = "ifany")
    }
  ),

  private = list(
    .threshold = NULL
  )
)

predictionclassif_initialize = function(self, task, response, prob) {
  self$task_type = "classif"

  if (!is.null(task)) {
    class_names = task$class_names
    self$row_ids = row_ids = task$row_ids
    self$truth = task$truth()
    n = length(row_ids)

    if (!is.null(response)) {
      response = factor(response, levels = class_names)
      assert_factor(response, len = n, any.missing = FALSE)
    }

    if (!is.null(prob)) {
      assert_matrix(prob, nrows = n, ncols = length(class_names))
      assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
      assert_names(colnames(prob), permutation.of = class_names)
      if (!is.null(rownames(prob)))
        rownames(prob) = NULL
    }

    if (is.null(response) && !is.null(prob)) {
      # calculate response from prob
      i = max.col(prob, ties.method = "random")
      response = factor(colnames(prob)[i], levels = class_names)
    }
  } else {
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
