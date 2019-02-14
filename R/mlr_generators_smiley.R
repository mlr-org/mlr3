#' @title Smiley Classification Task Generator
#'
#' @name mlr_generators_smiley
#' @format [R6::R6Class] inheriting from [Generator].
#' @include Generator.R
#'
#' @description
#' A [Generator] for the smiley task in [mlbench::mlbench.smiley()].
#' @export
GeneratorSmiley = R6Class("GeneratorSmiley",
  inherit = Generator,
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
      data = invoke(mlbench::mlbench.smiley, n = n, .args = self$param_set$values)
      data = insert_named(as.data.table(data$x), list(class = data$classes))
      TaskClassif$new(sprintf("%s_%i", self$id, n), as_data_backend(data), target = "class")
    }
  )
)

mlr_generators$add("smiley", GeneratorSmiley)
