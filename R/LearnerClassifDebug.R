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
#'    \item{iter:}{Integer parameter for testing hotstarting.}
#'    \item{validate:}{How to construct the internal validation data. This parameter can be either `NULL`,
#'    a ratio, `"test"`, or `"inner_valid"`.}
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
          sleep_predict        = p_uty(tags = "predict"),
          threads              = p_int(1L, tags = c("train", "threads")),
          warning_predict      = p_dbl(0, 1, default = 0, tags = "predict"),
          warning_train        = p_dbl(0, 1, default = 0, tags = "train"),
          x                    = p_dbl(0, 1, tags = "train"),
          validate             = p_uty(default = NULL, tags = "train", custom_check = check_validate),
          iter                 = p_int(1, default = 1, tags = c("train", "hotstart", "tune")),
          early_stopping       = p_lgl(default = FALSE, tags = "train")
        ),
        properties = c("twoclass", "multiclass", "missings", "hotstart_forward", "validation", "inner_tuning"),
        man = "mlr3::mlr_learners_classif.debug",
        data_formats = c("data.table", "Matrix"),
        label = "Debug Learner for Classification"
      )
    }
  ),
  active = list(
    #' @description
    #' Retrieves the inner validation scores.
    #' If `early_stopping` was `FALSE`, this returns an empty list.
    #' @return named `list()`
    inner_valid_scores = function() {
      if (is.null(self$model$inner_valid_scores)) {
        stopf("No inner validation scores available.")
      }
      self$state$inner_valid_scores
    },
    #' @description
    #' Retrieves the inner tuned values, in this case the value of `iter`.
    #' If parameter `validate` was `NULL`, this returns an empty list.
    #' @return named `list()`
    inner_tuned_values = function() {
      if (is.null(self$state$inner_tuned_values)) {
        stopf("No inner tuned values available.")
      }
      self$state$inner_tuned_values
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

      valid_truth = if (!is.null(pv$validate)) {
        if (is.null(task$inner_valid_task)) {
          stopf("No inner validation task present, but parameter 'validate' is not NULL.")
        }
        task$inner_valid_task$truth()
      }

      if (isTRUE(pv$early_stopping) && is.null(valid_truth)) {
        stopf("Early stopping is only possible when a validation task is present.")
      }

      model = list(response = as.character(sample(task$truth(), 1L)), pid = Sys.getpid(), id = UUIDgenerate(),
        iter = if (isTRUE(pv$early_stopping)) sample(pv$iter %??% 1L, 1L) else pv$iter %??% 1L
      )

      if (!is.null(valid_truth)) {
        valid_pred = private$.make_prediction(task$inner_valid_task, model, self$param_set$get_values(tags = "predict"))

        valid_pred = as_prediction(as_prediction_data(valid_pred, task = task$inner_valid_task, check = TRUE, train_task = task))

        model$inner_valid_scores = list(acc = mlr3measures::acc(valid_truth, valid_pred$response))
        if (self$predict_type == "prob") {
          model$inner_valid_scores$mbrier = mlr3measures::mbrier(valid_truth, valid_pred$prob)
        }
      }

      if (isTRUE(pv$save_tasks)) {
        model$task_train = task$clone(deep = TRUE)
      }

      set_class(model, "classif.debug_model")
    },

    .extract_inner_tuned_values = function() {
      if (!isTRUE(self$state$param_vals$early_stopping)) {
        named_list()
      } else {
        self$model["iter"]
      }
    },
    .extract_inner_valid_scores = function() {
      if (is.null(self$model$inner_valid_scores)) {
        named_list()
      } else {
        self$model$inner_valid_scores
      }
    },
    .predict = function(task) {
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

      private$.make_prediction(task, self$model, pv)
    },

    .make_prediction = function(task, model, pv) {
      n = task$nrow
      response = prob = NULL
      missing_type = pv$predict_missing_type %??% "na"

      if ("response" %in% self$predict_type) {
        response = rep.int(unclass(model$response), n)
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


#' @export
set_inner_tuning.LearnerClassifDebug = function(learner, disable = FALSE, ...) {
  prev_pvs = learner$param_set$values
  on.exit({learner$param_set$values = prev_pvs}, add = TRUE)
  learner$param_set$set_values(...)
  pv = learner$param_set$values
  if (disable) {
    learner$param_set$set_values(early_stopping = FALSE)
  } else {
    learner$param_set$set_values(early_stopping = TRUE)
    if (is.null(pv$validate)) {
      stopf("Parameter 'validate' must be set to enable inner tuning.")
    }
    if (is.null(pv$iter)) {
      stopf("Parameter 'iter' must be set to enable inner tuning.")
    }

  }
  on.exit({}, add = FALSE)
  invisible(learner)
}


#' @include mlr_learners.R
mlr_learners$add("classif.debug", function() LearnerClassifDebug$new())
