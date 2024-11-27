#' @title Score Measures Callback
#'
#' @include CallbackEvaluation.R
#' @name mlr3.score_measures
#'
#' @description
#' This `CallbackEvaluation` scores measures directly on the worker.
#' This way measures that require a model can be scores without saving the model.
#'
#' @examples
#' clbk("mlr3.score_measures", measures = msr("classif.ce"))
#'
#' task = tsk("pima")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#'
#' callback = clbk("mlr3.score_measures", measures = msr("selected_features"))
#'
#' rr = resample(task, learner, resampling = resampling, callbacks = callback)
#'
#' rr$learners[[1]]$state$selected_features
NULL

load_callback_score_measures = function() {
  callback_evaluation("mlr3.score_measures",
    label = "Score Measures Callback",
    man = "mlr3::mlr3.score_measures",

    on_evaluation_end = function(callback, context) {
      measures = as_measures(callback$state$measures)

      # Score measures on the test set
      pred = as_prediction(context$pdatas$test)
      res = pred$score(measures, context$task, context$learner)
      context$learner$state$selected_features = res
    }
  )
}
