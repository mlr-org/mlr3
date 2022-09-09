#' @title Prediction Object for Regression
#'
#' @include Prediction.R
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerRegr], i.e.
#' the predicted response and standard error.
#'
#' @template seealso_prediction
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
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskRegr])\cr
    #'   Task, used to extract defaults for `row_ids` and `truth`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param truth (`numeric()`)\cr
    #'   True (observed) response.
    #'
    #' @param response (`numeric()`)\cr
    #'   Vector of numeric response values.
    #'   One element for each observation in the test set.
    #'
    #' @param se (`numeric()`)\cr
    #'   Numeric vector of predicted standard errors.
    #'   One element for each observation in the test set.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), response = NULL, se = NULL, check = TRUE) {
      pdata = new_prediction_data(
        list(row_ids = row_ids, truth = truth, response = response, se = se),
        task_type = "regr"
      )

      if (check) {
        pdata = check_prediction_data(pdata)
      }
      self$task_type = "regr"
      self$man = "mlr3::PredictionRegr"
      self$data = pdata
      self$predict_types = intersect(c("response", "se"), names(pdata))
    }
  ),

  active = list(
    #' @field response (`numeric()`)\cr
    #' Access the stored predicted response.
    response = function(rhs) {
      assert_ro_binding(rhs)
      self$data$response %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field se (`numeric()`)\cr
    #' Access the stored standard error.
    se = function(rhs) {
      assert_ro_binding(rhs)
      self$data$se %??% rep(NA_real_, length(self$data$row_ids))
    }
  )
)


#' @export
as.data.table.PredictionRegr = function(x, ...) { # nolint
  as.data.table(x$data[c("row_ids", "truth", "response", "se")])
}
