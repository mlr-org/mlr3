#' @title XOR Classification Task Generator
#'
#' @name mlr_task_generators_xor
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the xor task in [mlbench::mlbench.xor()].
#'
#' @templateVar id xor
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("xor")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorXor = R6Class("TaskGeneratorXor",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        d = p_int(1L, default = 1L)
      )

      super$initialize(id = "xor", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_xor")
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
      invoke(mlbench::mlbench.xor, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("xor", TaskGeneratorXor)
