#' @title Classification Learner for Debugging
#'
#' @aliases mlr_learners_classif.debug
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#' @include LearnerClassif.R
#'
#' @description
#' A simple [LearnerClassif] used primarily in the unit tests and for debugging purposes.
#' If no hyperparameter is set, it simply constantly predicts a randomly selected label.
#' The following hyperparameters trigger the following actions:
#' \describe{
#'    \item{message_train:}{Outputs a message during train.}
#'    \item{message_predict:}{Outputs a message during predict.}
#'    \item{warning_train:}{Signals a warning during train.}
#'    \item{warning_predict:}{Signals a warning during predict.}
#'    \item{error_train:}{Raises an exception during train.}
#'    \item{error_predict:}{Raises an exception during predict.}
#'    \item{segfault_train:}{Provokes a segfault during train.}
#'    \item{segfault_predict:}{Provokes a segfault during predict.}
#'    \item{predict_missing}{Ratio of predictions which will be NA.}
#'    \item{save_tasks:}{Saves input task in `model` slot during training and prediction.}
#'    \item{x:}{Numeric parameter. Ignored.}
#' }
#' Note that segfaults may not work on your operating system.
#' Also note that if they work, they will tear down your R session immediately!
#' @export
#' @examples
#' learner = LearnerClassifDebug$new()
#' learner$param_set$values = list(message_train = TRUE, save_tasks = TRUE)
#'
#' # this should signal a message
#' task = mlr_tasks$get("iris")
#' learner$train(task)
#' learner$predict(task)
#'
#' # task_train and task_predict are the input tasks for train() and predict()
#' names(learner$model)
LearnerClassifDebug = R6Class("LearnerClassifDebug", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.debug") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamLgl$new("message_train", tags = "train"),
            ParamLgl$new("message_predict", tags = "predict"),
            ParamLgl$new("warning_train", tags = "train"),
            ParamLgl$new("warning_predict", tags = "predict"),
            ParamLgl$new("error_train", tags = "train"),
            ParamLgl$new("error_predict", tags = "predict"),
            ParamLgl$new("segfault_train", tags = "train"),
            ParamLgl$new("segfault_predict", tags = "predict"),
            ParamDbl$new("predict_missing", lower = 0, upper = 1, default = 0, tags = "predict"),
            ParamLgl$new("save_tasks", tags = c("train", "predict")),
            ParamDbl$new("x", lower = 0, upper = 1, tags = "train")
          )
        ),
        properties = c("twoclass", "multiclass", "missings")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if (isTRUE(pv$message_train)) {
        message("Message from classif.debug->train()")
      }
      if (isTRUE(pv$warning_train)) {
        warning("Warning from classif.debug->train()")
      }
      if (isTRUE(pv$error_train)) {
        stop("Error from classif.debug->train()")
      }
      if (isTRUE(pv$segfault_train)) {
        get("attach")(structure(list(), class = "UserDefinedDatabase"))
      }

      model = list(response = as.character(sample(task$truth(), 1L)))
      if (isTRUE(pv$save_tasks)) {
        model$task_train = task$clone(deep = TRUE)
      }
      set_class(model, "classif.debug_model")
    },

    predict_internal = function(task) {
      n = task$nrow
      pv = self$param_set$get_values(tags = "predict")
      if (isTRUE(pv$message_predict)) {
        message("Message from classif.debug->predict()")
      }
      if (isTRUE(pv$warning_predict)) {
        warning("Warning from classif.debug->predict()")
      }
      if (isTRUE(pv$error_predict)) {
        stop("Error from classif.debug->predict()")
      }
      if (isTRUE(pv$segfault_predict)) {
        get("attach")(structure(list(), class = "UserDefinedDatabase"))
      }

      if (isTRUE(pv$save_tasks)) {
        self$data$model$task_predict = task$clone(deep = TRUE)
      }

      response = prob = NULL

      if ("response" %in% self$predict_type) {
        response = rep.int(unclass(self$model$response), n)
        if (!is.null(pv$predict_missing)) {
          ii = sample.int(n, n * pv$predict_missing)
          response = replace(response, ii, NA)
        }
      }

      if ("prob" %in% self$predict_type) {
        cl = task$class_names
        prob = matrix(runif(n * length(cl)), nrow = n)
        prob = prob / rowSums(prob)
        colnames(prob) = cl

        if (!is.null(pv$predict_missing)) {
          ii = sample.int(n, n * pv$predict_missing)
          prob[ii, 1L] = NA_real_
        }
      }

      PredictionClassif$new(task = task, response = response, prob = prob)
    }
  )
)
