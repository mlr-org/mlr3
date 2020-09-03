#' @title Prediction Object for Regression
#'
#' @include Prediction.R
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerRegr], i.e.
#' the predicted response and standard error.
#' Additionally, probability distributions implemented in \CRANpkg{distr6} are supported.
#'
#' @family Prediction
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
    #'   Task, used to extract defaults for `row_id` and `truth`.
    #'
    #' @param row_id (`integer()`)\cr
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
    #' @param distr ([distr6::VectorDistribution])\cr
    #'   [VectorDistribution][distr6::VectorDistribution] from \CRANpkg{distr6}.
    #'   Each individual distribution in the vector represents the random variable 'survival time'
    #'   for an individual observation.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    initialize = function(task = NULL, row_id = task$row_ids, truth = task$truth(), response = NULL, se = NULL, distr = NULL, check = TRUE) {
      pdata = new_prediction_data(
        list(row_id = row_id, truth = truth, response = response, se = se, distr = distr),
        task_type = "regr"
      )

      if (check) {
        pdata = check_prediction_data(pdata)
      }
      self$data = pdata
      self$predict_types = intersect(c("response", "se", "distr"), names(pdata))
    },

    #' @template field_task_type
    task_type = "regr",


    #' @template field_man
    man = "mlr3::PredictionRegr"
  ),

  active = list(
    #' @field response (`numeric()`)\cr
    #' Access the stored predicted response.
    response = function(rhs) {
      assert_ro_binding(rhs)
      self$data$response %??% rep(NA_real_, length(self$data$row_id))
    },

    #' @field se (`numeric()`)\cr
    #' Access the stored standard error.
    se = function(rhs) {
      assert_ro_binding(rhs)
      self$data$se %??% rep(NA_real_, length(self$data$row_id))
    },

    #' @field distr ([distr6::VectorDistribution])\cr
    #' Access the stored vector distribution.
    #' Requires package \CRANpkg{distr6}.
    distr = function() {
      if ("distr" %in% self$predict_types) {
        require_namespaces("distr6")
      }
      return(self$data$distr)
    },

    #' @field missing (`integer()`)\cr
    #'   Returns `row_id` for which the predictions are missing or incomplete.
    missing = function(rhs) {
      assert_ro_binding(rhs)
      miss = logical(length(self$data$row_id))

      if ("response" %in% self$predict_types) {
        miss = is.na(self$response)
      }

      if ("se" %in% self$predict_types) {
        miss = miss | is.na(self$se)
      }

      self$data$row_id[miss]
    }
  )
)


#' @export
as.data.table.PredictionRegr = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_id", "truth", "response", "se")])

  if ("distr" %in% x$predict_types) {
    require_namespaces("distr6")
    tab$distr = list(x$distr)
  }
  tab
}

#' @export
c.PredictionRegr = function(..., keep_duplicates = TRUE) { # nolint
  dots = list(...)
  assert_list(dots, "PredictionRegr")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  pdata = invoke(c.PredictionDataRegr, .args = map(dots, "data"), keep_duplicates = keep_duplicates)
  as_prediction(pdata)
}
