#' @title Spiral Classification Task Generator
#'
#' @name mlr_task_generators_spirals
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the spirals task in [mlbench::mlbench.spirals()].
#'
#' @templateVar id spirals
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("spirals")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorSpirals = R6Class("TaskGeneratorSpirals",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        cycles = p_int(1L, default = 1L),
        sd     = p_dbl(0, default = 0)
      )

      super$initialize(id = "spirals", "classif", "mlbench", ps,
        label = "Spiral Classification", man = "mlr3::mlr_task_generators_spirals")
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
      invoke(mlbench::mlbench.spirals, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("spirals", TaskGeneratorSpirals)
