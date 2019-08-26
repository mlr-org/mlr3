#' @title 2d Normals Classification Task Generator
#'
#' @usage NULL
#' @aliases mlr_task_generators_2dnormals
#' @format [R6::R6Class] inheriting from [TaskGenerator].
#' @include TaskGenerator.R
#'
#' @section Construction:
#' ```
#' TaskGenerator2DNormals$new()
#' mlr_task_generators$get("2dnormals")
#' tgen("2dnormals")
#' ```
#'
#' @description
#' A [TaskGenerator] for the 2d normals task in [mlbench::mlbench.2dnormals()].
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("2dnormals")$generate(10)$data()
TaskGenerator2DNormals = R6Class("TaskGenerator2DNormals",
  inherit = TaskGenerator,
  public = list(
    initialize = function() {
      param_set = ParamSet$new(list(
        ParamInt$new("cl", lower = 2L),
        ParamDbl$new("r", lower = 1L),
        ParamDbl$new("sd", lower = 0L)
      ))
      super$initialize(id = "2dnormals", "classif", "mlbench", param_set)
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
