#' @title Regression Learner for Debugging
#'
#' @name mlr_learners_regr.debug
#' @include LearnerRegr.R
#'
#' @description
#' A simple [LearnerRegr] used primarily in the unit tests and for debugging purposes.
#' If no hyperparameter is set, it simply constantly predicts the mean value of the training data.
#' The following hyperparameters trigger the following actions:
#' \describe{
#'    \item{predict_missing:}{Ratio of predictions which will be NA.}
#'    \item{predict_missing_type:}{To to encode missingness. \dQuote{na} will insert NA values, \dQuote{omit} will just return fewer predictions than requested.}
#'    \item{save_tasks:}{Saves input task in `model` slot during training and prediction.}
#'    \item{threads:}{Number of threads to use. Has no effect.}
#'    \item{x:}{Numeric tuning parameter. Has no effect.}
#'    \item{validate:}{Whether to evaluate the response on the validation set. This parameter can be either `NULL`,
#'    a ratio, `"test"`, or `"inner_valid_task"`.}
#'    \item{response:}{Whether to evaluate }
#' }
#'
#' @templateVar id regr.debug
#' @template learner
#'
#' @template seealso_learner
#' @export
#' @examples
#' task = tsk("")
#' learner = lrn("regr.debug", save_tasks = TRUE)
#' learner$train(task, row_ids = 1:20)
#' prediction = learner$predict(task, row_ids = 21:32)
#'
#' learner$model$task_train
#' learner$model$task_predict
LearnerRegrDebug = R6Class("LearnerRegrDebug", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      check_response = crate(function(x) {
        if (is.null(x)) return(TRUE)
        if (isTRUE(all.equal(x, "tune"))) return(TRUE)
        if (test_numeric(x, len = 1L, any.missing = FALSE)) return(TRUE)

        "Must either be 'tune', a numeric value, or NULL."
      })
      super$initialize(
        id = "regr.debug",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = ps(
          predict_missing      = p_dbl(0, 1, default = 0, tags = "predict"),
          predict_missing_type = p_fct(c("na", "omit"), default = "na", tags = "predict"),
          save_tasks           = p_lgl(default = FALSE, tags = c("train", "predict")),
          threads              = p_int(1L, tags = c("train", "threads")),
          x                    = p_dbl(0, 1, tags = "train"),
          validate             = p_uty(default = NULL, tags = "train", custom_check = check_validate),
          response             = p_uty(default = NULL, tags = c("train", "inner_tuning"), custom_check = check_response)
        ),
        properties = c("missings", "validation", "inner_tuning"),
        man = "mlr3::mlr_learners_regr.debug",
        data_formats = c("data.table", "Matrix"),
        label = "Debug Learner for Regression"
      )
    },
    #' @description
    #' Retrieves the inner validation scores.
    #' @return named `list()`
    inner_valid_scores = function() {
      if (is.null(self$model)) {
        stopf("No model trained yet.")
      }
      if (is.null(self$model$inner_valid_scores)) {
        stopf("No inner validation.")
      }
      self$model$inner_valid_scores
    },
    #' @description
    #' Retrieves the inner tuned values.
    #' In this case
    #' @return named `list()`
    inner_tuning_values = function() {
      if (is.null(self$model)) {
        stopf("No model trained yet.")
      }
      self$model$inner_tuning_values
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      truth = task$truth()
      model = list(
        se = sd(truth),
        pid = Sys.getpid()
      )

      valid_truth = if (!is.null(pv$validate)) task$inner_valid_task$truth() else NULL
      if (isTRUE(all.equal(pv$response, "tune"))) {
        if (is.null(task$inner_valid_task)) {
          stopf("Can only tune if a validation set is present.")
        }
        model$response = mean(c(truth, valid_truth))
      } else {
        model$response = mean(truth)
      }

      if (isTRUE(pv$save_tasks)) {
        model$task_train = task$clone(deep = TRUE)
      }

      if (!is.null(pv$validate)) {
        pred = private$.make_prediction(task$inner_valid_task, model, self$param_set$get_values(tags = "predict"))
        model$inner_valid_scores = list(
          mse = mean((pred$response - valid_truth)^2),
          mae = mean(abs(pred$response - valid_truth))
        )
      }

      set_class(model, "regr.debug_model")
    },

    .make_prediction = function(task, model, pv) {
      prediction = named_list(mlr_reflections$learner_predict_types[["regr"]][[self$predict_type]])
      missing_type = pv$predict_missing_type %??% "na"
      n = task$nrow

      for (pt in names(prediction)) {
        value = rep.int(model[[pt]], n)
        if (!is.null(pv$predict_missing)) {
          ii = sample.int(n, n * pv$predict_missing)
          value = switch(missing_type,
            "na" = replace(value, ii, NA),
            "omit" = value[ii]
          )
        }

        prediction[[pt]] = value
      }

      return(prediction)
    },

    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")

      if (isTRUE(pv$save_tasks)) {
        self$state$model$task_predict = task$clone(deep = TRUE)
      }

      private$.make_prediction(task, self$model, pv)

    }
  )
)

#' @export
set_inner_tuning.LearnerRegrDebug = function(learner, disable = FALSE, validate = NULL, response = NULL, ...) {
  pv = insert_named(learner$param_set$values, list(response = response, validate = validate))

  if (disable) {
    if (isTRUE(all.equal(pv$response, "tune"))) {
      stopf("Parameter 'response' of learner %s must not be 'tune' to disable inner tuning.", learner$id)
    }
  } else {
    if (is.null(pv$validate)) {
      stopf("Parameter 'validate' must be provided to enable inner tuning.")
    }
    if (isTRUE(all.equal(pv$response, "tune"))) {
      stopf("Parameter 'response' of learner %s must not be 'tune' to disable inner tuning.", learner$id)
    }
  }
  learner$param_set$values = pv
  invisible(learner)
}


#' @include mlr_learners.R
mlr_learners$add("regr.debug", function() LearnerRegrDebug$new())
