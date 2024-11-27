#' @title Evaluation Context
#'
#' @description
#' A [CallbackEvaluation] accesses and modifies data during [resample()] and [benchmark()] via the `ContextEvaluation`.
#' See the section on fields for a list of modifiable objects.
#' See [callback_evaluation()] for a list of stages that access `ContextEvaluation`.
#'
#' @export
ContextEvaluation = R6Class("ContextEvaluation",
  inherit = Context,
  public = list(

    #' @field task ([Task])\cr
    #' The task to be evaluated.
    #' The task is unchanged during the evaluation.
    task = NULL,

    #' @field learner ([Learner])\cr
    #' The learner to be evaluated.
    #' The learner contains the models after stage `on_evaluation_before_train`.
    learner = NULL,

    #' @field resampling [Resampling]\cr
    #' The resampling strategy to be used.
    #' The resampling is unchanged during the evaluation.
    resampling = NULL,

    #' @field param_values `list()`\cr
    #' The parameter values to be used.
    #' Is usually only set while tuning.
    param_values = NULL,

    #' @field sets (`list()`)\cr
    #' The train and test set.
    #' The sets are available on stage `on_evaluation_before_train``.
    sets = NULL,

    #' @field test_set (`integer()`)\cr
    #' Validation test set.
    #' The set is only available when using internal validation.
    test_set = NULL,

    #' @field predict_sets (`list()`)\cr
    #' The prediction sets stored in `learner$predict_sets`.
    #' The sets are available on stage `on_evaluation_before_predict`.
    predict_sets = NULL,

    #' @field pdatas (List of [PredictionData])\cr
    #' The prediction data.
    #' The data is available on stage `on_evaluation_end`.
    pdatas = NULL,

    #' @field data_extra (list())\cr
    #' Data saved in the [ResampleResult] or [BenchmarkResult].
    #' Use this field to save results.
    data_extra = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([Task])\cr
    #' The task to be evaluated.
    #' @param learner ([Learner])\cr
    #' The learner to be evaluated.
    #' @param resampling ([Resampling])\cr
    #' The resampling strategy to be used.
    #' @param param_values (`list()`)\cr
    #' The parameter values to be used.
    initialize = function(task, learner, resampling, param_values) {
      # no assertions to avoid overhead
      self$task = task
      self$learner = learner
      self$resampling = resampling

      super$initialize(id = "evaluate", label = "Evaluation")
    }
  )
)
