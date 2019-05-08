#' @title XOR Classification Task Generator
#'
#' @aliases mlr_generators_xor
#' @format [R6::R6Class] inheriting from [Generator].
#' @include Generator.R
#'
#' @description
#' A [Generator] for the xor task in [mlbench::mlbench.xor()].
#' @export
#' @examples
#' mlr_generators$get("xor")$generate(10)$data()
GeneratorXor = R6Class("GeneratorXor",
  inherit = Generator,
  public = list(
    initialize = function(id = "xor") {
      param_set = ParamSet$new(list(
        ParamInt$new("d", lower = 1L)
      ))
      super$initialize(id = id, "classif", "mlbench", param_set)
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.xor, n = n, .args = self$param_set$values)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)
