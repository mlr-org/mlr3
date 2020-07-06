#' @title Spiral Classification Task Generator
#'
#' @name mlr_task_generators_spirals
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the spirals task in [mlbench::mlbench.spirals()].
#'
#' @templateVar id spirals
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("spirals")$generate(10)$data()
TaskGeneratorSpirals = R6Class("TaskGeneratorSpirals",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("cycles", lower = 1L, default = 1L),
        ParamDbl$new("sd", lower = 0, default = 0)
      ))

      super$initialize(id = "spirals", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_spirals")
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.spirals, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("spirals", TaskGeneratorSpirals)
