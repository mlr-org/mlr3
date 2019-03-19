#' @title Toy Classification Learner
#'
#' @name mlr_learners_classif.debug
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
#'    \item{save_tasks:}{Saves input task in `model` slot during training and prediction.}
#'    \item{x:}{Numeric parameter. Ignored.}
#' }
#' Note that segfaults may not work on your operating system.
#' Also note that if they work, they will tear down your R session immediately!
#' @export
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
            ParamLgl$new("save_tasks", tags = c("train", "predict")),
            ParamDbl$new("x", lower = 0, upper = 1, tags = "train")
          )
        ),
        properties = "missings"
      )
    },

    train = function(task) {
      pv = self$params("train")
      if (isTRUE(pv$message_train))
        message("Message from classif.debug->train()")
      if (isTRUE(pv$warning_train))
        warning("Warning from classif.debug->train()")
      if (isTRUE(pv$error_train))
        stop("Error from classif.debug->train()")
      if (isTRUE(pv$segfault_train))
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )

      if (isTRUE(pv$save_tasks)) {
        self$model = list(task$clone(deep = TRUE))
      } else {
        label = sample(task$truth(), 1L)
        self$model = set_class(as.character(label), "unittest")
      }
      self
    },

    predict = function(task) {
      pv = self$params("predict")
      if (isTRUE(pv$message_predict))
        message("Message from classif.debug->predict()")
      if (isTRUE(pv$warning_predict))
        warning("Warning from classif.debug->predict()")
      if (isTRUE(pv$error_predict))
        stop("Error from classif.debug->predict()")
      if (isTRUE(pv$segfault_predict))
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )
      if (isTRUE(pv$save_tasks)) {
        self$model[[2]] = task$clone(deep = TRUE)
        label = sample(task$truth(), 1L)
        PredictionClassif$new(response = rep.int(as.character(label), task$nrow))
      } else {
        PredictionClassif$new(response = rep.int(unclass(self$model), task$nrow))
      }
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.debug", LearnerClassifDebug)
