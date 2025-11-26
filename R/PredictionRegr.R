#' @title Prediction Object for Regression
#'
#' @include Prediction.R
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerRegr], i.e.
#' the predicted response and standard error.
#' Additionally, probability distributions implemented in package `distr6` are supported.
#'
#' @template seealso_prediction
#' @export
#' @examples
#' task = tsk("california_housing")
#' learner = lrn("regr.featureless", predict_type = "se")
#' p = learner$train(task)$predict(task)
#' p$predict_types
#' head(as.data.table(p))
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
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
    #' @param quantiles (`matrix()`)\cr
    #'   Numeric matrix of predicted quantiles. One row per observation, one column per quantile.
    #'
    #' @param distr (`VectorDistribution`)\cr
    #'   `VectorDistribution` from package distr6 (in repository \url{https://raphaels1.r-universe.dev}).
    #'   Each individual distribution in the vector represents the random variable 'survival time'
    #'   for an individual observation.
    #'
    #' @param weights (`numeric()`)\cr
    #'   Vector of measure weights for each observation. Should be constructed from
    #'   the `Task`'s `weights_measure` column.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    #'
    #' @param extra (`list()`)\cr
    #'   List of extra data to be stored in the prediction object.
    initialize = function(
      task = NULL,
      row_ids = task$row_ids,
      truth = task$truth(),
      response = NULL,
      se = NULL,
      quantiles = NULL,
      distr = NULL,
      weights = NULL,
      check = TRUE,
      extra = NULL
    ) {
      pdata = new_prediction_data(
        list(row_ids = row_ids, truth = truth, response = response, se = se, quantiles = quantiles, distr = distr, weights = weights, extra = extra),
        task_type = "regr"
      )

      if (check) {
        pdata = check_prediction_data(pdata)
      }
      self$task_type = "regr"
      self$man = "mlr3::PredictionRegr"
      self$data = pdata
      predict_types = intersect(names(mlr_reflections$learner_predict_types[["regr"]]), names(pdata))
      # response is in saved in quantiles matrix
      if ("quantiles" %chin% predict_types) predict_types = union(predict_types, "response")
      self$predict_types = predict_types
      if (is.null(pdata$response)) private$.quantile_response = attr(quantiles, "response")
    }
  ),

  active = list(
    #' @field response (`numeric()`)\cr
    #' Access the stored predicted response.
    response = function(rhs) {
      assert_ro_binding(rhs)
      if (!is.null(private$.quantile_response)) return(self$data$quantiles[, private$.quantile_response])
      self$data$response %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field se (`numeric()`)\cr
    #' Access the stored standard error.
    se = function(rhs) {
      assert_ro_binding(rhs)
      self$data$se %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field quantiles (`matrix()`)\cr
    #' Matrix of predicted quantiles. Observations are in rows, quantile (in ascending order) in columns.
    quantiles = function(rhs) {
      assert_ro_binding(rhs)
      self$data$quantiles
    },

    #' @field distr (`VectorDistribution`)\cr
    #' Access the stored vector distribution.
    #' Requires package `distr6`(in repository \url{https://raphaels1.r-universe.dev}) .
    distr = function() {
      if ("distr" %chin% self$predict_types) {
        require_namespaces("distr6", msg = "To predict probability distributions, please install %s")
      }
      self$data$distr
    }
  ),

  private = list(
    .quantile_response = NULL
  )
)


#' @export
as.data.table.PredictionRegr = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_ids", "truth", "response", "se")])

  if ("quantiles" %chin% x$predict_types) {
    tab = rcbind(tab, as.data.table(x$data$quantiles))
    set(tab, j = "response", value = x$response)
  }

  if ("distr" %chin% x$predict_types) {
    require_namespaces("distr6", msg = "To predict probability distributions, please install %s")
    tab$distr = list(x$distr)
  }

  if (!is.null(x$data$weights)) {
    tab$weights = x$data$weights
  }

  if (!is.null(x$data$extra)) {
    tab = cbind(tab, as.data.table(x$data$extra))
  }

  tab
}
