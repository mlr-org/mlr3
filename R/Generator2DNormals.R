#' @title 2d Normals Classification Task Generator
#'
#' @aliases mlr_generators_2dnormals
#' @format [R6::R6Class] inheriting from [Generator].
#' @include Generator.R
#'
#' @description
#' A [Generator] for the 2d normals task in [mlbench::mlbench.2dnormals()].
#' @export
#' @examples
#' mlr_generators$get("2dnormals")$generate(10)$data()
Generator2DNormals = R6Class("Generator2DNormals",
  inherit = Generator,
  public = list(
    initialize = function(id = "2dnormals") {
      param_set = ParamSet$new(list(
        ParamInt$new("cl", lower = 2L),
        ParamDbl$new("r", lower = 1L),
        ParamDbl$new("sd", lower = 0L)
      ))
      super$initialize(id = id, "classif", "mlbench", param_set)
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.2dnormals, n = n, .args = self$param_set$values)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)
