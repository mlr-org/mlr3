#' @title Model Extractor Callback
#'
#' @include CallbackResample.R
#' @name mlr3.model_extractor
#'
#' @description
#' This [CallbackResample] extracts information from the model after training with a user-defined function.
#' This way information can be extracted from the model without saving the model (`store_models = FALSE`).
#' The `fun` must be a function that takes a learner as input and returns the extracted information as named list (see example).
#' The callback is very helpful to call `$selected_features()`, `$importance()`, `$oob_error()` on the learner.
#'
#' @param fun (`function(learner)`)\cr
#'   Function to extract information from the learner.
#'   The function must have the argument `learner`.
#'   The function must return a named list.
#'
#' @examples
#' task = tsk("pima")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#'
#' # define function to extract selected features
#' selected_features = function(learner) list(selected_features = learner$selected_features())
#'
#' # create callback
#' callback = clbk("mlr3.model_extractor", fun = selected_features)
#'
#' rr = resample(task, learner, resampling = resampling, store_models = FALSE, callbacks = callback)
#'
#' rr$data_extra
NULL

load_callback_model_extractor = function() {
  callback_resample("mlr3.model_extractor",
    label = "Model Extractor Callback",
    man = "mlr3::mlr3.model_extractor",

    on_resample_end = function(callback, context) {
      assert_function(callback$state$fun, args = "learner")
      context$data_extra = invoke(callback$state$fun, learner = context$learner)
    }
  )
}

#' @title Callback Holdout Task
#'
#' @include CallbackResample.R
#' @name mlr3.holdout_task
#'
#' @description
#' This [CallbackResample] predicts on an additional holdout task after training.
#'
#' @param task ([Task])\cr
#'  The holdout task.
#'
#' @examples
#' task = tsk("pima")
#' task_holdout = task$clone()
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#' splits = partition(task, 0.7)
#'
#' task$filter(splits$train)
#' task_holdout$filter(splits$test)
#'
#' callback = clbk("mlr3.holdout_task", task = task_holdout)
#'
#' rr = resample(task, learner, resampling = resampling, callbacks = callback)
#'
#' rr$data_extra
NULL

load_callback_holdout_task = function() {
  callback_resample("mlr3.holdout_task",
    label = "Callback Holdout Task",
    man = "mlr3::mlr3.holdout_task",

    on_resample_before_predict = function(callback, context) {
      pred = context$learner$predict(callback$state$task)
      context$data_extra = list(prediction_holdout = pred)
    }
  )
}
