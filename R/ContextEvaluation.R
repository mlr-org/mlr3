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

    #' @field iteration (`integer()`)\cr
    #' The current iteration.
    iteration = NULL,

    #' @field pdatas (List of [PredictionData])\cr
    #' The prediction data.
    #' The data is available on stage `on_evaluation_end`.
    pdatas = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([Task])\cr
    #' The task to be evaluated.
    #' @param learner ([Learner])\cr
    #' The learner to be evaluated.
    #' @param resampling ([Resampling])\cr
    #' The resampling strategy to be used.
    #' @param iteration (`integer()`)\cr
    #' The current iteration.
    initialize = function(task, learner, resampling, iteration) {
      # no assertions to avoid overhead
      self$task = task
      self$learner = learner
      self$resampling = resampling
      self$iteration = iteration

      super$initialize(id = "evaluate", label = "Evaluation")
    }
  ),

  active = list(

    #' @field data_extra (list())\cr
    #' Data saved in the [ResampleResult] or [BenchmarkResult].
    #' Use this field to save results.
    #' Must be a `list()`.
    data_extra = function(rhs) {
      if (missing(rhs)) {
        return(private$.data_extra)
      }
      private$.data_extra = assert_list(rhs)
    }
  ),

  private = list(
    .data_extra = NULL
  )
)
