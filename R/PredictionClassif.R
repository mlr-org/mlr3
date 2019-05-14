#' @title Prediction Object for Classification
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object stores the predictions returned by a learner of class [LearnerClassif], i.e.
#' the predicted response and class probabilities.
#'
#' If response is not provided during construction, but class probabilities are,
#' the response is calculated from the probabilities: the class label with the highest
#' probability is chosen. In case of ties, a label is selected randomly.
#'
#' @note
#' It is possible to initialize this object without a task, by manually providing `row_ids` and `truth`.
#' In this case, the class names are taken from `truth`, and must be identical to `task$class_names`
#' (this includes the order of the levels).
#' This is especially important for binary classification tasks, where the positive class must be the first level.
#'
#' @section Construction:
#' ```
#' p = PredictionClassif$new(task = NULL, response = NULL, prob = NULL,
#'   row_ids = task$row_ids, truth = task$truth())
#' ```
#'
#' * `task` :: [TaskClassif]\cr
#'   Task for which the predictions are made. Used to extract the row ids and the true
#'   labels. Must be subsetted to test set.
#'
#' * `response` :: `factor()`\cr
#'   Vector of predicted class labels.
#'   One element for each observation in the test set.
#'   Character vectors are automatically converted to factors.
#'
#' * `prob` :: `matrix()`\cr
#'   Numeric matrix of class probabilities with one column for each class
#'   and one row for each observation in the test set.
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Row ids of the task. Per default, these are extracted from the `task`.
#'
#' * `truth` :: `factor()`\cr
#'   True (observed) labels. Per default, these are extracted from the `task`.
#'
#' @section Fields:
#' All fields from [Prediction], and additionally:
#'
#' * `threshold` :: `numeric(1)`\cr
#'   Probability threshold between 0 and 1.
#'   Assigning a value to this field modifies the stored responses.
#'
#' * `confusion` :: `matrix()`\cr
#'   Confusion matrix resulting from the comparison of truth and response.
#'   Truth is in columns, predicted response in rows.
#'
#' The field `task_type` is set to `"classif"`.
#'
#' @section Thresholding:
#' If probabilities are stored, it is possible to manually set the threshold which determines the predicted class label.
#' Usually, the label of the class with the highest predicted probability is selected.
#' For binary classification problems, such an threshold defaults to 0.5.
#' For cost-sensitive or imbalanced classification problems, manually adjusting the threshold can increase
#' the predictive performance.
#'
#' * For binary problems only a single threshold value can be set.
#'   If the probability exceeds the threshold, the positive class is predicted.
#'   If the probability equals the threshold, the label is selected randomly.
#' * For binary and multi-class problems, a named numeric vector of thresholds can be set.
#'   The length and names must correspond to the number of classes and class names, respectively.
#'   To determine the class label, the probabilities are divided by the threshold.
#'   This results in a ratio > 1 if the probability exceeds the threshold, and a ratio < 1 otherwise.
#'   Note that it is possible that either none or multiple ratios are greater than 1 at the same time.
#'   Anyway, the class label with maximum ratio is selected.
#'   In case of ties in the ratio, one of the tied class labels is selected randomly.
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
    initialize = function(task = NULL, response = NULL, prob = NULL, row_ids = task$row_ids, truth = task$truth()) {
      predictionclassif_initialize(self, task, row_ids, truth, response, prob)
    }),

  active = list(
    threshold = function(rhs) {
      if (missing(rhs)) {
        return(private$.threshold)
      }
      if (!is.matrix(self$prob)) {
        stopf("Cannot set threshold, no probabilities available")
      }
      lvls = colnames(self$prob)

      if (length(rhs) == 1L) {
        if (length(lvls) != 2L) {
          stopf("Setting a single threshold only supported for binary classification problems")
        }
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
    }),

  private = list(
    .threshold = NULL
  )
)

predictionclassif_initialize = function(self, task, row_ids, truth, response, prob, class_names) {

  self$task_type = "classif"
  self$row_ids = assert_atomic_vector(row_ids)
  n = length(row_ids)

  if (is.null(task)) {
    self$truth = assert_factor(truth, len = n)
    lvls = levels(truth)
  } else {
    lvls = task$class_names
    self$truth = as_factor(truth, levels = lvls, len = n)
  }

  if (!is.null(response)) {
    self$response = as_factor(response, levels = lvls, len = n, any.missing = FALSE)
  }

  if (!is.null(prob)) {
    assert_matrix(prob, nrows = n, ncols = length(lvls))
    assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
    assert_names(colnames(prob), permutation.of = lvls)
    if (!is.null(rownames(prob))) {
      rownames(prob) = NULL
    }
    self$prob = prob

    if (is.null(self$response)) {
      # calculate response from prob
      i = max.col(prob, ties.method = "random")
      self$response = factor(colnames(prob)[i], levels = lvls)
    }
  }

  self$predict_types = c("response", "prob")[c(!is.null(self$response), !is.null(self$prob))]
}

#' @export
as.data.table.PredictionClassif = function(x, ...) {
  if (is.null(x$row_ids)) {
    return(data.table())
  }
  tab = data.table(row_id = x$row_ids, truth = x$truth, response = x$response)
  if (!is.null(x$prob)) {
    prob = as.data.table(x$prob)
    setnames(prob, names(prob), paste0("prob.", names(prob)))
    tab = ref_cbind(tab, prob)
  }

  tab
}

#' @export
rbind.PredictionClassif = function(...) {

  dots = list(...)
  assert_list(dots, "PredictionClassif")

  x = map_dtr(dots, function(p) {
    list(row_ids = p$row_ids, truth = p$truth, response = p$response)
  }, .fill = FALSE)

  prob = discard(map(dots, "prob"), is.null)
  if (length(prob) > 0L && length(prob) < length(dots)) {
    stopf("Cannot rbind predictions: Probabilities for some experiments, not all")
  }
  prob = Reduce(rbind_named, prob)

  PredictionClassif$new(row_ids = x$row_ids, truth = x$truth, response = x$response, prob = prob)
}
