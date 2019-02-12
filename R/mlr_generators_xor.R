#' @title XOR Classification Task Generator
#'
#' @name mlr_generators_xor
#' @format [R6::R6Class] inheriting from [Generator].
#' @description
#' A [Generator] for the xor task in [mlbench::mlbench.xor()].
#' @include Generator.R
#' @export
GeneratorXor = R6Class("GeneratorXor",
  inherit = Generator,
  public = list(
    initialize = function(...) {
      param_set = ParamSet$new(list(
        ParamInt$new("d", lower = 1L)
      ))
      super$initialize(id = "xor", "classif", "mlbench", param_set, list(...))
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.xor, n = n, .args = self$param_set$values)
      data = insert_named(as.data.table(data$x), list(class = data$classes))
      TaskClassif$new(sprintf("%s_%i", self$id, n), as_data_backend(data), target = "class")
    }
  )
)

mlr_generators$add("xor", GeneratorXor)
