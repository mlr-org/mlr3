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
#'    \item{count_marshaling:}{If `TRUE`, `marshal_model` will increase the `marshal_count` by 1. This value is initialized to `FALSE`.}
#' }
#'
#' @templateVar id regr.debug
#' @template learner
#'
#' @template seealso_learner
#' @export
#' @examples
#' task = tsk("mtcars")
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
      param_set = ps(
        predict_missing      = p_dbl(0, 1, default = 0, tags = "predict"),
        predict_missing_type = p_fct(c("na", "omit"), default = "na", tags = "predict"),
        save_tasks           = p_lgl(default = FALSE, tags = c("train", "predict")),
        threads              = p_int(1L, tags = c("train", "threads")),
        x                    = p_dbl(0, 1, tags = "train"),
        count_marshaling     = p_lgl(tags = c("train", "required"))
      )
      param_set$set_values(count_marshaling = FALSE)
      super$initialize(
        id = "regr.debug",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = param_set,
        properties = "missings",
        man = "mlr3::mlr_learners_regr.debug",
        data_formats = c("data.table", "Matrix"),
        label = "Debug Learner for Regression"
      )
    },
    #' @description
    #' Marshals the learner.
    marshal = function() {
      learner_marshal(self)
    },
    #' @description
    #' Unmarshal the learner.
    unmarshal = function() {
      learner_unmarshal(self)
    }
  ),
  active = list(
    #' @field marshaled (logical(1))\cr
    #' Whether the learner has been marshaled.
    marshaled = function() {
      learner_marshaled(self)
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      truth = task$truth()
      model = list(
        response = mean(truth),
        se = sd(truth),
        pid = Sys.getpid()
      )

      if (isTRUE(pv$save_tasks)) {
        model$task_train = task$clone(deep = TRUE)
      }
      if (isTRUE(pv$count_marshaling)) {
        model$marshal_count = 0L
      }
      set_class(model, "regr.debug_model")
    },

    .predict = function(task) {
      n = task$nrow
      pv = self$param_set$get_values(tags = "predict")

      if (isTRUE(pv$save_tasks)) {
        self$state$model$task_predict = task$clone(deep = TRUE)
      }

      prediction = named_list(mlr_reflections$learner_predict_types[["regr"]][[self$predict_type]])
      missing_type = pv$predict_missing_type %??% "na"

      for (pt in names(prediction)) {
        value = rep.int(self$model[[pt]], n)
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
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.debug", function() LearnerRegrDebug$new())


#' @export
#' @method marshal_model regr.debug_model
marshal_model.regr.debug_model = function(model, ...) {
  if (!is.null(model$marshal_count)) {
    model$marshal_count = model$marshal_count + 1
  }
  newclass = c("regr.debug_model_marshaled", "marshaled")
  structure(list(marshaled = model, packages = "mlr3"), class = newclass)
}

#' @export
#' @method unmarshal_model regr.debug_model_marshaled
unmarshal_model.regr.debug_model_marshaled = function(model, ...) {
  model$marshaled
}
