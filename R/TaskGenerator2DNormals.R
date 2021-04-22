#' @title 2D Normals Classification Task Generator
#'
#' @name mlr_task_generators_2dnormals
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the 2d normals task in [mlbench::mlbench.2dnormals()].
#'
#' @templateVar id 2dnormals
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("2dnormals")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGenerator2DNormals = R6Class("TaskGenerator2DNormals",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        cl = p_int(2L),
        r  = p_dbl(1L),
        sd = p_dbl(0L)
      )

      super$initialize(id = "2dnormals", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_2dnormals")
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
      invoke(mlbench::mlbench.2dnormals, n = n, .args = self$param_set$values)
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("2dnormals", TaskGenerator2DNormals)
