#' @title XOR Classification Task Generator
#'
#' @name mlr_task_generators_xor
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the xor task in [mlbench::mlbench.xor()].
#'
#' @templateVar id xor
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("xor")$generate(10)$data()
TaskGeneratorXor = R6Class("TaskGeneratorXor",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("d", lower = 1L)
      ))

      super$initialize(id = "xor", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_xor")
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.xor, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("xor", TaskGeneratorXor)
