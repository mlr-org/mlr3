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
#' @template task_generator
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
      ps = ps(
        center = p_lgl(default = TRUE),
        d      = p_int(1L, default = 3L),
        sd     = p_dbl(0, default = 0.1),
        sides  = p_int(1L, default = 1L)
      )

      super$initialize(id = "simplex", "classif", "mlbench", ps,
        label = "Simplex Classification", man = "mlr3::mlr_task_generators_simplex")
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
mlr_task_generators$add("simplex", function() TaskGeneratorSimplex$new())
