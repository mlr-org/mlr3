#' @title Smiley Classification Task Generator
#'
#' @aliases mlr_generators_smiley
#' @format [R6::R6Class] inheriting from [Generator].
#' @include Generator.R
#'
#' @description
#' A [Generator] for the smiley task in [mlbench::mlbench.smiley()].
#' @export
#' @examples
#' mlr_generators$get("smiley")$generate(10)$data()
GeneratorSmiley = R6Class("GeneratorSmiley",
  inherit = Generator,
  public = list(
    initialize = function(id = "smiley") {
      param_set = ParamSet$new(list(
        ParamDbl$new("sd1", lower = 0L),
        ParamDbl$new("sd2", lower = 0L)
      ))
      super$initialize(id = id, "classif", "mlbench", param_set)
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

#' @include mlr_generators.R
mlr_generators$add("smiley", GeneratorSmiley)
