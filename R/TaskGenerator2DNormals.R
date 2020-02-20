#' @title 2D Normals Classification Task Generator
#'
#' @name mlr_task_generators_2dnormals
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the 2d normals task in [mlbench::mlbench.2dnormals()].
#'
#' @templateVar id 2dnormals
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("2dnormals")$generate(10)$data()
TaskGenerator2DNormals = R6Class("TaskGenerator2DNormals",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("cl", lower = 2L),
        ParamDbl$new("r", lower = 1L),
        ParamDbl$new("sd", lower = 0L)
      ))

      super$initialize(id = "2dnormals", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_2dnormals")
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.2dnormals, n = n, .args = self$param_set$values)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("2dnormals", TaskGenerator2DNormals)
