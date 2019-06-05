#' @title Prediction Object for Classification
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Prediction].
#' @include Prediction.R
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerClassif], i.e.
#' the predicted response and class probabilities.
#'
#' If the response is not provided during construction, but class probabilities are,
#' the response is calculated from the probabilities: the class label with the highest
#' probability is chosen. In case of ties, a label is selected randomly.
#'
#' @note
#' If this object is constructed manually, make sure that the factors for truth and response
#' have the same levels, in the same order.
#' In case of binary classification tasks, the positive class label must be the first level.
#'
#' @section Construction:
#' ```
#' p = PredictionClassif$new(row_ids, truth, response = NULL, prob = NULL)
#' ```
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Row ids of the observations in the test set.
#'
#' * `truth` :: `factor()`\cr
#'   True (observed) labels. See the note on manual construction.
#'
#' * `response` :: (`character()` | `factor()`)\cr
#'   Vector of predicted class labels.
#'   One element for each observation in the test set.
#'   Character vectors are automatically converted to factors.
#'   See the note on manual construction.
#'
#' * `prob` :: `matrix()`\cr
#'   Numeric matrix of class probabilities with one column for each class
#'   and one row for each observation in the test set.
#'   Columns must be named with class labels, rownames are automatically removed.
#'
#' @section Fields:
#' All fields from [Prediction], and additionally:
#'
#' * `response` :: `factor()`\cr
#'   Access to the stored predicted class labels.
#'
#' * `prob` :: `matrix()`\cr
#'   Access to the stored probabilities.
#'
#' * `confusion` :: `matrix()`\cr
#'   Confusion matrix resulting from the comparison of truth and response.
#'   Truth is in columns, predicted response is in rows.
#'
#' The field `task_type` is set to `"classif"`.
#'
#' @section Methods:
#'
#' * `set_threshold(th)`\cr
#'   `numeric()` -> named `list()`\cr
#'   Sets the prediction threshold, and returns a named list with updated `response` and `prob`.
#'   This list can be stored in the experiment (see examples).
#'   See the section on thresholding for more information.
#'
#' @section Thresholding:
#' If probabilities are stored, it is possible to change the threshold which determines the predicted class label.
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
#' th = c(0.05, 0.9, 0.05)
#' names(th) = task$class_names
#'
#' # new predictions
#' p$set_threshold(th)$response
#'
#' # update the threshold in the experiment
#' e$score()$performance # score before thresholding
#' e$prediction = e$prediction$set_threshold(th)
#' e$score()$performance # score after thresholding
PredictionClassif = R6Class("PredictionClassif", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    response = NULL,
    prob = NULL,
    initialize = function(row_ids, truth, response = NULL, prob = NULL) {
      self$row_ids = assert_atomic_vector(row_ids)
      self$truth = assert_factor(truth)
      self$response = assert_factor(response, null.ok = TRUE)
      self$prob = assert_matrix(prob, null.ok = TRUE)
      self$task_type = "classif"
      self$predict_types = c("response", "prob")[c(!is.null(response), !is.null(prob))]
    },

    set_threshold = function(threshold) {
      if (!is.matrix(self$prob)) {
        stopf("Cannot set threshold, no probabilities available")
      }
      lvls = colnames(self$prob)

      if (length(threshold) == 1L) {
        assert_number(threshold, lower = 0, upper = 1)
        if (length(lvls) != 2L) {
          stopf("Setting a single threshold only supported for binary classification problems")
        }
        prob = cbind(self$prob[, 1L], threshold)
      } else {
        assert_numeric(threshold, any.missing = FALSE, lower = 0, upper = 1, len = length(lvls))
        assert_names(names(threshold), permutation.of = lvls)
        threshold = threshold[lvls] # reorder thresh so it is in the same order as levels

        # multiply all rows by threshold, then get index of max element per row
        w = ifelse(threshold > 0, 1 / threshold, Inf)
        prob = self$prob %*% diag(w)
      }

      ind = max.col(prob, ties.method = "random")
      return(list(row_ids = self$row_ids, response = factor(lvls[ind], levels = lvls), prob = self$prob))
    }
  ),

  active = list(
    confusion = function() {
      table(response = self$response, truth = self$truth, useNA = "ifany")
    }
  )
)

#' @export
convert_prediction.TaskClassif = function(task, predicted) {
  n = task$nrow
  lvls = task$class_names

  if (!is.null(predicted$response)) {
    predicted$response = as_factor(predicted$response, levels = lvls)
    assert_factor(predicted$response, len = n, any.missing = FALSE)
  }

  prob = predicted$prob
  if (!is.null(prob)) {
    assert_matrix(prob, nrows = n, ncols = length(lvls))
    assert_numeric(prob, any.missing = FALSE, lower = 0, upper = 1)
    assert_names(colnames(prob), permutation.of = lvls)
    if (!is.null(rownames(prob))) {
      rownames(prob) = NULL
      predicted$prob = prob
    }

    if (is.null(predicted$response)) {
      # calculate response from prob
      i = max.col(prob, ties.method = "random")
      predicted$response = factor(colnames(prob)[i], levels = lvls)
    }
  }

  predicted$row_ids = task$row_ids
  set_class(predicted, c("PredictionDataClassif", "PredictionData"))
}


#' @export
as_prediction.TaskClassif = function(task, predicted) {
  PredictionClassif$new(row_ids = predicted$row_ids, truth = task$truth(predicted$row_ids), response = predicted$response, prob = predicted$prob)
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

  prob = Reduce(x = prob, f = function(x, y) {
    assert_set_equal(colnames(x), colnames(y))
    rbind(x, y[, match(colnames(x), colnames(y)), drop = FALSE])
  })

  PredictionClassif$new(row_ids = x$row_ids, truth = x$truth, response = x$response, prob = prob)
}
