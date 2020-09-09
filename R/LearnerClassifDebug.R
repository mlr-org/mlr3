#' @title Classification Learner for Debugging
#'
#' @name mlr_learners_classif.debug
#' @include LearnerClassif.R
#'
#' @description
#' A simple [LearnerClassif] used primarily in the unit tests and for debugging purposes.
#' If no hyperparameter is set, it simply constantly predicts a randomly selected label.
#' The following hyperparameters trigger the following actions:
#' \describe{
#'    \item{message_train:}{Probability to output a message during train.}
#'    \item{message_predict:}{Probability to output a message during predict.}
#'    \item{warning_train:}{Probability to signal a warning during train.}
#'    \item{warning_predict:}{Probability to signal a warning during predict.}
#'    \item{error_train:}{Probability to raises an exception during train.}
#'    \item{error_predict:}{Probability to raise an exception during predict.}
#'    \item{segfault_train:}{Probability to provokes a segfault during train.}
#'    \item{segfault_predict:}{Probability to provokes a segfault during predict.}
#'    \item{predict_missing}{Ratio of predictions which will be NA.}
#'    \item{save_tasks:}{Saves input task in `model` slot during training and prediction.}
#'    \item{x:}{Numeric tuning parameter. Has no effect.}
#' }
#' Note that segfaults may not be triggered on your operating system.
#' Also note that if they work, they will tear down your R session immediately!
#'
#' @templateVar id classif.featureless
#' @template section_dictionary_learner
#'
#' @section Meta Information:
#' `r rd_info(lrn("classif.debug"))`
#'
#' @section Parameters:
#' `r rd_info(lrn("classif.debug")$param_set)`
#'
#' @template seealso_learner
#' @export
#' @examples
#' learner = lrn("classif.debug")
#' learner$param_set$values = list(message_train = 1, save_tasks = TRUE)
#'
#' # this should signal a message
#' task = tsk("iris")
#' learner$train(task)
#' learner$predict(task)
#'
#' # task_train and task_predict are the input tasks for train() and predict()
#' names(learner$model)
LearnerClassifDebug = R6Class("LearnerClassifDebug", inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "classif.debug",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamDbl$new("message_train", lower = 0, upper = 1, default = 0, tags = "train"),
            ParamDbl$new("message_predict", lower = 0, upper = 1, default = 0, tags = "predict"),
            ParamDbl$new("warning_train", lower = 0, upper = 1, default = 0, tags = "train"),
            ParamDbl$new("warning_predict", lower = 0, upper = 1, default = 0, tags = "predict"),
            ParamDbl$new("error_train", lower = 0, upper = 1, default = 0, tags = "train"),
            ParamDbl$new("error_predict", lower = 0, upper = 1, default = 0, tags = "predict"),
            ParamDbl$new("segfault_train", lower = 0, upper = 1, default = 0, tags = "train"),
            ParamDbl$new("segfault_predict", lower = 0, upper = 1, default = 0, tags = "predict"),
            ParamDbl$new("predict_missing", lower = 0, upper = 1, default = 0, tags = "predict"),
            ParamLgl$new("save_tasks", default = FALSE, tags = c("train", "predict")),
            ParamDbl$new("x", lower = 0, upper = 1, tags = "train")
          )
        ),
        properties = c("twoclass", "multiclass", "missings"),
        man = "mlr3::mlr_learners_classif.debug"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      roll = function(name) {
        name %in% names(pv) && pv[[name]] > runif(1L)
      }

      if (roll("message_train")) {
        message("Message from classif.debug->train()")
      }
      if (roll("warning_train")) {
        warning("Warning from classif.debug->train()")
      }
      if (roll("error_train")) {
        stop("Error from classif.debug->train()")
      }
      if (roll("segfault_train")) {
        get("attach")(structure(list(), class = "UserDefinedDatabase"))
      }

      model = list(response = as.character(sample(task$truth(), 1L)), pid = Sys.getpid())
      if (isTRUE(pv$save_tasks)) {
        model$task_train = task$clone(deep = TRUE)
      }
      set_class(model, "classif.debug_model")
    },

    .predict = function(task) {
      n = task$nrow
      pv = self$param_set$get_values(tags = "predict")
      lookup = function(name) {
        name %in% names(pv) && pv[[name]] > runif(1L)
      }

      if (lookup("message_predict")) {
        message("Message from classif.debug->predict()")
      }
      if (lookup("warning_predict")) {
        warning("Warning from classif.debug->predict()")
      }
      if (lookup("error_predict")) {
        stop("Error from classif.debug->predict()")
      }
      if (lookup("segfault_predict")) {
        get("attach")(structure(list(), class = "UserDefinedDatabase"))
      }

      if (isTRUE(pv$save_tasks)) {
        self$state$model$task_predict = task$clone(deep = TRUE)
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

      list(response = response, prob = prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.debug", LearnerClassifDebug)
