#' @title Simplex Classification Task Generator
#'
#' @name mlr_task_generators_simplex
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the simplex task in [mlbench::mlbench.simplex()].
#'
#' Note that the generator implemented in \CRANpkg{mlbench} returns
#' fewer samples than requested.
#'
#' @templateVar id simplex
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("simplex")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorSimplex = R6Class("TaskGeneratorSimplex",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("d", lower = 1L, default = 3L),
        ParamInt$new("sides", lower = 1L, default = 1L),
        ParamDbl$new("sd", lower = 0, default = 0.1),
        ParamLgl$new("center", default = TRUE)
      ))

      super$initialize(id = "simplex", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_simplex")
    },

    #' @description
    #' Creates a simple plot of generated data.
    #' @param n (`integer(1)`)\cr
    #'   Number of samples to draw for the plot. Default is 200.
    #' @param pch (`integer(1)`)\cr
    #'   Point char. Passed to [plot()].
    #' @param ... (any)\cr
    #'   Additional arguments passed to [plot()].
    plot = function(n = 200L, pch = 19L, ...) {
      plot(private$.generate_obj(n), pch = pch, ...)
    }
  ),

  private = list(
    .generate_obj = function(n) {
      invoke(mlbench::mlbench.simplex, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("simplex", TaskGeneratorSimplex)
