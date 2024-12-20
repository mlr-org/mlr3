#' @title Resample Context
#'
#' @description
#' A [CallbackResample] accesses and modifies data during [resample()] and [benchmark()] via the `ContextResample`.
#' See the section on fields for a list of modifiable objects.
#' See [callback_resample()] for a list of stages that access `ContextResample`.
#'
#' @export
ContextResample = R6Class("ContextResample",
  inherit = Context,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([Task])\cr
    #'   The task to be evaluated.
    #' @param learner ([Learner])\cr
    #'   The learner to be evaluated.
    #' @param resampling ([Resampling])\cr
    #'   The resampling strategy to be used.
    #' @param iteration (`integer()`)\cr
    #'   The current iteration.
    initialize = function(task, learner, resampling, iteration) {
      # no assertions to avoid overhead
      private$.task = task
      private$.learner = learner
      private$.resampling = resampling
      private$.iteration = iteration

      super$initialize(id = "evaluate", label = "Evaluation")
    }
  ),

  active = list(

    #' @field task ([Task])\cr
    #' The task to be evaluated.
    #' The task is unchanged during the evaluation.
    #' The task is read-only.
    task = function(rhs) {
      assert_ro_binding(rhs)
      private$.task
    },

    #' @field learner ([Learner])\cr
    #' The learner to be evaluated.
    #' The learner contains the models after stage `on_resample_before_train`.
    learner = function(rhs) {
      if (missing(rhs)) {
        return(private$.learner)
      }
      private$.learner = assert_learner(rhs)
    },

    #' @field resampling [Resampling]\cr
    #' The resampling strategy to be used.
    #' The resampling is unchanged during the evaluation.
    #' The resampling is read-only.
    resampling = function(rhs) {
      assert_ro_binding(rhs)
      private$.resampling
    },

    #' @field iteration (`integer()`)\cr
    #' The current iteration.
    #' The iteration is read-only.
    iteration = function(rhs) {
      assert_ro_binding(rhs)
      private$.iteration
    },

    #' @field pdatas (List of [PredictionData])\cr
    #' The prediction data.
    #' The data is available on stage `on_resample_end`.
    pdatas = function(rhs) {
      if (missing(rhs)) {
        return(private$.pdatas)
      }
      private$.pdatas = assert_list(rhs, "PredictionData")
    },

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
    .task = NULL,
    .learner = NULL,
    .resampling = NULL,
    .iteration = NULL,
    .pdatas = NULL,
    .data_extra = NULL
  )
)
