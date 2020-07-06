#' @title Simplex Classification Task Generator
#'
#' @name mlr_task_generators_xor
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the simplex task in [mlbench::mlbench.xor()].
#'
#' Note that the generator implemented in \CRANpkg{mlbench} returns
#' fewer samples than requested.
#'
#' @templateVar id simplex
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("simplex")$generate(10)$data()
TaskGeneratorSimplex = R6Class("TaskGeneratorSimplex",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("d", lower = 1L, default = 3L),
        ParamInt$new("sides", lower = 1L, default = 1L),
        ParamDbl$new("sd", lower = 0, default = 0.1),
        ParamLgl$new("center", default = TRUE)
      ))

      super$initialize(id = "simplex", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_xor")
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
mlr_task_generators$add("simplex", TaskGeneratorSimplex)
