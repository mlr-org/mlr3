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
#'    \item{error_predict:}{Probability to raise an exception during predict.}
#'    \item{error_train:}{Probability to raises an exception during train.}
#'    \item{message_predict:}{Probability to output a message during predict.}
#'    \item{message_train:}{Probability to output a message during train.}
#'    \item{predict_missing:}{Ratio of predictions which will be NA.}
#'    \item{predict_missing_type:}{To to encode missingness. \dQuote{na} will insert NA values, \dQuote{omit} will just return fewer predictions than requested.}
#'    \item{save_tasks:}{Saves input task in `model` slot during training and prediction.}
#'    \item{segfault_predict:}{Probability to provokes a segfault during predict.}
#'    \item{segfault_train:}{Probability to provokes a segfault during train.}
#'    \item{sleep_train:}{Function returning a single number determining how many seconds to sleep during `$train()`.}
#'    \item{sleep_predict:}{Function returning a single number determining how many seconds to sleep during `$predict()`.}
#'    \item{threads:}{Number of threads to use. Has no effect.}
#'    \item{warning_predict:}{Probability to signal a warning during predict.}
#'    \item{warning_train:}{Probability to signal a warning during train.}
#'    \item{x:}{Numeric tuning parameter. Has no effect.}
#' }
#' Note that segfaults may not be triggered reliably on your operating system.
#' Also note that if they work as intended, they will tear down your R session immediately!
#'
#' @templateVar id classif.debug
#' @template learner
#'
#' @template seealso_learner
#' @export
#' @examples
#' learner = lrn("classif.debug")
#' learner$param_set$values = list(message_train = 1, save_tasks = TRUE)
#'
#' # this should signal a message
#' task = tsk("penguins")
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
        param_set = ps(
          error_predict        = p_dbl(0, 1, default = 0, tags = "predict"),
          error_train          = p_dbl(0, 1, default = 0, tags = "train"),
          message_predict      = p_dbl(0, 1, default = 0, tags = "predict"),
          message_train        = p_dbl(0, 1, default = 0, tags = "train"),
          predict_missing      = p_dbl(0, 1, default = 0, tags = "predict"),
          predict_missing_type = p_fct(c("na", "omit"), default = "na", tags = "predict"),
          save_tasks           = p_lgl(default = FALSE, tags = c("train", "predict")),
          segfault_predict     = p_dbl(0, 1, default = 0, tags = "predict"),
          segfault_train       = p_dbl(0, 1, default = 0, tags = "train"),
          sleep_train          = p_uty(tags = "train"),
          sleep_predict        = p_uty(tags = "train"),
          threads              = p_int(1L, tags = c("train", "threads")),
          warning_predict      = p_dbl(0, 1, default = 0, tags = "predict"),
          warning_train        = p_dbl(0, 1, default = 0, tags = "train"),
          x                    = p_dbl(0, 1, tags = "train"),
          iter                 = p_int(1, default = 1, tags = c("train", "hotstart"))
        ),
        properties = c("twoclass", "multiclass", "missings", "hotstart_forward"),
        man = "mlr3::mlr_learners_classif.debug",
        data_formats = c("data.table", "Matrix"),
        label = "Debug Learner for Classification"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      roll = function(name) {
        name %in% names(pv) && pv[[name]] > runif(1L)
      }

      if (!is.null(pv$sleep_train)) {
        secs = assert_number(pv$sleep_train())
        Sys.sleep(max(0, secs))
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

      model = list(response = as.character(sample(task$truth(), 1L)), pid = Sys.getpid(), iter = pv$iter,
        id = UUIDgenerate())
      if (isTRUE(pv$save_tasks)) {
        model$task_train = task$clone(deep = TRUE)
      }

      set_class(model, "classif.debug_model")
    },

    .predict = function(task) {
      n = task$nrow
      pv = self$param_set$get_values(tags = "predict")
      roll = function(name) {
        name %in% names(pv) && pv[[name]] > runif(1L)
      }

      if (!is.null(pv$sleep_predict)) {
        secs = assert_number(pv$sleep_predict())
        Sys.sleep(max(0, secs))
      }

      if (roll("message_predict")) {
        message("Message from classif.debug->predict()")
      }
      if (roll("warning_predict")) {
        warning("Warning from classif.debug->predict()")
      }
      if (roll("error_predict")) {
        stop("Error from classif.debug->predict()")
      }
      if (roll("segfault_predict")) {
        get("attach")(structure(list(), class = "UserDefinedDatabase"))
      }

      if (isTRUE(pv$save_tasks)) {
        self$state$model$task_predict = task$clone(deep = TRUE)
      }

      response = prob = NULL
      missing_type = pv$predict_missing_type %??% "na"

      if ("response" %in% self$predict_type) {
        response = rep.int(unclass(self$model$response), n)
        if (!is.null(pv$predict_missing)) {
          ii = sample.int(n, n * pv$predict_missing)
          response = switch(missing_type,
            "na" = replace(response, ii, NA),
            "omit" = response[ii]
          )
        }
      }

      if ("prob" %in% self$predict_type) {
        cl = task$class_names
        prob = matrix(runif(n * length(cl)), nrow = n)
        prob = prob / rowSums(prob)
        colnames(prob) = cl

        if (!is.null(pv$predict_missing)) {
          ii = sample.int(n, n * pv$predict_missing)
          prob = switch(missing_type,
            "na" = {
              prob[ii, ] = NA_real_
              prob
            },
            "omit" = {
              prob[ii, , drop = FALSE]
            }
          )
        }
      }

      list(response = response, prob = prob)
    },

    .hotstart = function(task) {
      model = self$model
      pars = self$param_set$get_values(tags = "train")
      id = self$model$id

      model = list(response = as.character(sample(task$truth(), 1L)), pid = Sys.getpid(), iter = pars$iter,
        id = id)
      set_class(model, "classif.debug_model")
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.debug", LearnerClassifDebug)
