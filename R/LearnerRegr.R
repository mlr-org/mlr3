#' @title Regression Learner
#'
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for regression problems:
#'
#' * `task_type` is set to `"regr"`.
#' * Creates [Prediction]s of class [PredictionRegr].
#' * Possible values for `predict_types` are:
#'   - `"response"`: Predicts a numeric response for each observation in the test set.
#'   - `"se"`: Predicts the standard error for each value of response for each observation in the test set.
#'   - `"distr"`: Probability distribution as `VectorDistribution` object (requires package `distr6`, available via
#'     repository \url{https://raphaels1.r-universe.dev}).
#'  - `"quantiles"`: Predicts quantile estimates for each observation in the test set.
#'
#' Predefined learners can be found in the [dictionary][mlr3misc::Dictionary] [mlr_learners].
#' Essential regression learners can be found in this dictionary after loading \CRANpkg{mlr3learners}.
#' Additional learners are implement in the Github package \url{https://github.com/mlr-org/mlr3extralearners}.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_data_formats
#' @template param_packages
#' @template param_label
#' @template param_man
#' @template param_task_type
#'
#' @template seealso_learner
#' @export
#' @examples
#' # get all regression learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^regr"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("regr.rpart")
#' lrn("classif.featureless")
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, task_type = "regr", param_set = ps(), predict_types = "response", feature_types = character(), properties = character(), data_formats, packages = character(), label = NA_character_, man = NA_character_) {
      super$initialize(id = id, task_type = task_type, param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats, packages = packages,
        label = label, man = man)
    },

    #' @description
    #' Predicts outcomes for new data in `newdata` using the model fitted during `$train()`.
    #' This method is faster than `$predict_newdata()` as it skips assertions, type conversions, encapsulation, and logging.
    #'
    #' Unlike `$predict_newdata()`, this method does not return a [Prediction] object.
    #' Instead, it returns a list with either a `"response"` or `"prob"` element, depending on the prediction type.
    #'
    #' Note that `state$predict_time` and `state$log` will remain empty after using this method.
    #' Some learners may not support this method and may fail when it is called.
    #' Prefer `$predict_newdata()` unless performance is critical.
    #'
    #' If the model was trained via [resample()] or [benchmark()], you must pass the associated task object stored in the corresponding [ResampleResult] or [BenchmarkResult].
    #'
    #' @param newdata [`data.table::data.table()`]\cr
    #'   New data to predict on.
    #' @param task ([Task]).
    #'
    #' @return `list()` with elements `"response"`, `"se"` or `"quantiles"` depending on the predict type.
    predict_newdata_fast = function(newdata, task = NULL) {
      if (is.null(task) && is.null(self$state$train_task)) stopf("No task stored, and no task provided")
      feature_names = self$state$train_task$feature_names %??% task$feature_names

      # add data and most common used meta data
      fake_task = list(
        # some learners require that newdata contains the features names in the same order as the training data
        data = function(...) newdata[, feature_names, with = FALSE],
        feature_names = feature_names,
        nrow = nrow(newdata)
      )

      # train failed, use fallback
      if (is.null(self$model) && !is.null(self$state$fallback_state$model)) {
        return(self$fallback$predict_newdata_fast(newdata))
      }
      pred = get_private(self)$.predict(fake_task)

      # predict missing predictions with fallback
      miss = logical(fake_task$nrow)
      if (!is.null(pred$response)) {
        miss = is.na(pred$response)
      }

      if (!is.null(pred$se)) {
        miss = miss | is.na(pred$se)
      }

      if (!is.null(pred$quantiles)) {
        miss = miss | apply(pred$quantiles, 1L, anyMissing)
      }

      miss_ids = which(miss)
      if (length(miss_ids) && !is.null(self$state$fallback_state$model)) {
        pred_miss = self$fallback$predict_newdata_fast(newdata[miss_ids, ])

        if (!is.null(pred$response)) {
          pred$response[miss_ids] = pred_miss$response
        }

        if (!is.null(pred$se)) {
          pred$se[miss_ids] = pred_miss$se
        }

        if (!is.null(pred$quantiles)) {
          pred$quantiles[miss_ids, ] = pred_miss$quantiles
        }
      }

      return(pred)
    }
  ),

  active = list(

    #' @field quantiles (`numeric()`)\cr
    #' Numeric vector of probabilities to be used while predicting quantiles.
    #' Elements must be between 0 and 1, not missing and provided in ascending order.
    #' If only one quantile is provided, it is used as response.
    #' Otherwise, set `$quantile_response` to specify the response quantile.
    quantiles = function(rhs) {
      if (missing(rhs)) {
        return(private$.quantiles)
      }

      if ("quantiles" %nin% self$predict_types) {
        stopf("Learner does not support predicting quantiles")
      }
      private$.quantiles = assert_numeric(rhs, lower = 0, upper = 1, any.missing = FALSE, min.len = 1L, sorted = TRUE, .var.name = "quantiles")

      if (length(private$.quantiles) == 1) {
        private$.quantile_response = private$.quantiles
      }
    },

    #' @field quantile_response (`numeric(1)`)\cr
    #' The quantile to be used as response.
    quantile_response = function(rhs) {
      if (missing(rhs)) {
        return(private$.quantile_response)
      }

      if ("quantiles" %nin% self$predict_types) {
        stopf("Learner does not support predicting quantiles")
      }

      private$.quantile_response = assert_number(rhs, lower = 0, upper = 1, .var.name = "response")
      private$.quantiles = sort(union(private$.quantiles, private$.quantile_response))
    }
  ),


  private = list(
    .quantiles = NULL,
    .quantile_response = NULL
  )
)
