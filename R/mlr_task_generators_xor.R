#' @title XOR Classification Task Generator
#'
#' @name mlr_task_generators_xor
#' @format [R6::R6Class] inheriting from [TaskGenerator].
#' @description
#' A [TaskGenerator] for the xor task in [mlbench::mlbench.xor()].
#' @include TaskGenerator.R
#' @export
TaskGeneratorXor = R6Class("TaskGeneratorXor",
  inherit = TaskGenerator,
  public = list(
    initialize = function(...) {
      param_set = ParamSet$new(list(
        ParamInt$new("d", lower = 1L)
      ))
      super$initialize(id = "xor", "classif", "mlbench", param_set, list(...))
    },

    plot = function(n) {
      require_namespaces(self$packages)
      plot(private$.generate_data(assert_count(n)))
    }
  ),

  private = list(
    .generate_data = function(n) {
      data = invoke(mlbench::mlbench.xor, n = n, .args = self$param_vals)
      insert_named(as.data.table(data$x), list(class = data$classes))
    },

    .generate_task = function(n) {
      data = private$.generate_data(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as_data_backend(data), target = "class")
    }
  )
)

mlr_task_generators$add("xor", TaskGeneratorXor)
