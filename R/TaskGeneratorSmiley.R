#' @title Smiley Classification Task Generator
#'
#' @aliases mlr_task_generators_smiley
#' @format [R6::R6Class] inheriting from [TaskGenerator].
#' @include TaskGenerator.R
#'
#' @section Construction:
#' ```
#' TaskGeneratorSmiley$new()
#' mlr_task_generators$get("smiley")
#' tgen("smiley")
#' ```
#'
#' @description
#' A [TaskGenerator] for the smiley task in [mlbench::mlbench.smiley()].
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("smiley")$generate(10)$data()
TaskGeneratorSmiley = R6Class("TaskGeneratorSmiley",
  inherit = TaskGenerator,
  public = list(
    initialize = function() {
      param_set = ParamSet$new(list(
        ParamDbl$new("sd1", lower = 0L),
        ParamDbl$new("sd2", lower = 0L)
      ))
      super$initialize(id = "smiley", "classif", "mlbench", param_set)
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.smiley, n = n, .args = self$param_set$values)
      colnames(data$x) = sprintf("x.%i", seq_col(data$x))
      data = insert_named(as.data.table(data$x), list(classes = data$classes))
      TaskClassif$new(sprintf("%s_%i", self$id, n), data, target = "classes")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("smiley", TaskGeneratorSmiley)
