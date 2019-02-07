#' @title Smiley Classification Task Generator
#'
#' @name mlr_task_generators_smiley
#' @format [R6::R6Class] inheriting from [TaskGenerator].
#' @description
#' A [TaskGenerator] for the smiley task in [mlbench::mlbench.smiley()].
#' @include TaskGenerator.R
#' @export
TaskGeneratorSmiley = R6Class("TaskGeneratorSmiley",
  inherit = TaskGenerator,
  public = list(
    initialize = function(...) {
      param_set = ParamSet$new(list(
        ParamDbl$new("sd1", lower = 0L),
        ParamDbl$new("sd2", lower = 0L)
      ))
      super$initialize(id = "smiley", "classif", "mlbench", param_set, list(...))
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.smiley, n = n, .args = self$param_set$param_vals)
      data = insert_named(as.data.table(data$x), list(class = data$classes))
      TaskClassif$new(sprintf("%s_%i", self$id, n), as_data_backend(data), target = "class")
    }
  )
)

mlr_task_generators$add("smiley", TaskGeneratorSmiley)
