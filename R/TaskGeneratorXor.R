#' @title XOR Classification Task Generator
#'
#' @usage NULL
#' @name mlr_task_generators_xor
#' @format [R6::R6Class] inheriting from [TaskGenerator].
#' @include TaskGenerator.R
#'
#' @section Construction:
#' ```
#' TaskGeneratorXor$new()
#' mlr_task_generators$get("xor")
#' tgen("xor")
#' ```
#'
#' @description
#' A [TaskGenerator] for the xor task in [mlbench::mlbench.xor()].
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("xor")$generate(10)$data()
TaskGeneratorXor = R6Class("TaskGeneratorXor",
  inherit = TaskGenerator,
  public = list(
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
