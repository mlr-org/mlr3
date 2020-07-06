#' @title Circles Classification Task Generator
#'
#' @name mlr_task_generators_circles
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the circle binary classification task in [mlbench::mlbench.circle()].
#' Creates a large circle containing a smaller circle.
#'
#' @templateVar id circles
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("circles")$generate(10)$data()
TaskGeneratorCircles = R6Class("TaskGeneratorCircles",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("d", lower = 2L, default = 2L)
      ))

      super$initialize(id = "circles", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_circles")
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.xor, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("circles", TaskGeneratorCircles)

