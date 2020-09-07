#' @title Prediction Object for Classification
#'
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
#' If this object is constructed manually, make sure that the factor levels for `truth`
#' have the same levels as the task, in the same order.
#' In case of binary classification tasks, the positive class label must be the first level.
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
#'   Note that there are the following edge cases for threshold equal to `0` which are handled specially:
#'   1. With threshold 0 the resulting ratio gets `Inf` and thus gets always selected.
#'      If there are multiple ratios with value `Inf`, one is selected according to `ties_method` (randomly per default).
#'   2. If additionally the predicted probability is also 0, the ratio `0/0` results in `NaN` values.
#'      These are simply replaced by `0` and thus will never get selected.
#'
#' @family Prediction
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' learner$train(task)
#' p = learner$predict(task)
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
#' p$score(measures = msr("classif.ce"))
PredictionClassif = R6Class("PredictionClassif", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskClassif])\cr
    #'   Task, used to extract defaults for `row_id` and `truth`.
    #'
    #' @param row_id (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param truth (`factor()`)\cr
    #'   True (observed) labels. See the note on manual construction.
    #'
    #' @param response (`character()` | `factor()`)\cr
    #'   Vector of predicted class labels.
    #'   One element for each observation in the test set.
    #'   Character vectors are automatically converted to factors.
    #'   See the note on manual construction.
    #'
    #' @param prob (`matrix()`)\cr
    #'   Numeric matrix of posterior class probabilities with one column for each class
    #'   and one row for each observation in the test set.
    #'   Columns must be named with class labels, row names are automatically removed.
    #'   If `prob` is provided, but `response` is not, the class labels are calculated from
    #'   the probabilities using [max.col()] with `ties.method` set to `"random"`.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    initialize = function(task = NULL, row_id = task$row_ids, truth = task$truth(), response = NULL, prob = NULL, check = TRUE) {
      # TODO: switch to new interface with pdata as single argument after all learners have been
      #       migrated
      pdata = new_prediction_data(
        list(row_id = row_id, truth = truth, response = response, prob = prob),
        task_type = "classif"
      )

      if (check) {
        pdata = check_prediction_data(pdata)
      }
      self$data = pdata
      self$predict_types = intersect(c("response", "prob"), names(pdata))
    },


    #' @template field_task_type
    task_type = "classif",


    #' @template field_man
    man = "mlr3::PredictionClassif",


    #' @description
    #' Sets the prediction response based on the provided threshold.
    #' See the section on thresholding for more information.
    #'
    #' @param threshold (`numeric()`).
    #' @param ties_method (`character(1)`)\cr
    #'   One of `"random"`, `"first"` or `"last"` (c.f. [max.col()]) to determine how to deal with
    #'   tied probabilities.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    set_threshold = function(threshold, ties_method = "random") {
      if (!is.matrix(self$data$prob)) {
        stopf("Cannot set threshold, no probabilities available")
      }
      lvls = levels(self$truth)

      if (length(threshold) == 1L) {
        assert_number(threshold, lower = 0, upper = 1)
        if (length(lvls) != 2L) {
          stopf("Setting a single threshold only supported for binary classification problems")
        }
        prob = cbind(self$data$prob[, 1L], threshold)
      } else {
        assert_numeric(threshold, any.missing = FALSE, lower = 0, upper = 1, len = length(lvls))
        assert_names(names(threshold), permutation.of = lvls)
        threshold = threshold[lvls] # reorder thresh so it is in the same order as levels

        # multiply all rows by threshold, then get index of max element per row
        prob = self$data$prob %*% diag(1 / threshold) # can generate Inf for threshold 0
        prob[is.na(prob)] = 0 # NaN results from 0 * Inf, replace with 0, c.f. #452
      }

      ind = max.col(prob, ties.method = ties_method)
      self$data$response = factor(lvls[ind], levels = lvls)
      invisible(self)
    }
  ),


  active = list(
    #' @field response (`factor()`)\cr
    #' Access to the stored predicted class labels.
    response = function(rhs) {
      assert_ro_binding(rhs)
      self$data$response %??% factor(rep(NA, length(self$data$row_id)), levels(self$data$truth))
    },

    #' @field prob (`matrix()`)\cr
    #' Access to the stored probabilities.
    prob = function(rhs) {
      assert_ro_binding(rhs)
      self$data$prob
    },

    #' @field confusion (`matrix()`)\cr
    #' Confusion matrix, as resulting from the comparison of truth and response.
    #' Truth is in columns, predicted response is in rows.
    confusion = function(rhs) {
      assert_ro_binding(rhs)
      table(response = self$data$response, truth = self$data$truth, useNA = "ifany")
    },

    #' @field missing (`integer()`)\cr
    #'   Returns `row_id` for which the predictions are missing or incomplete.
    missing = function(rhs) {
      assert_ro_binding(rhs)
      is_missing_prediction_data(self$data)
    }
  )
)

#' @export
as.data.table.PredictionClassif = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_id", "truth", "response")])

  if ("prob" %in% x$predict_types) {
    prob = as.data.table(x$data$prob)
    setnames(prob, names(prob), paste0("prob.", names(prob)))
    tab = rcbind(tab, prob)
  }

  tab
}
