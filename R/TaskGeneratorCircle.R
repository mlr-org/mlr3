#' @title Circle Classification Task Generator
#'
#' @name mlr_task_generators_circle
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the circle binary classification task in [mlbench::mlbench.circle()].
#' Creates a large circle containing a smaller circle.
#'
#' @templateVar id circle
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("circle")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorCircle = R6Class("TaskGeneratorCircle",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        d = p_int(lower = 2L, default = 2L)
      )

      super$initialize(id = "circle", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_circle")
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
      invoke(mlbench::mlbench.circle, n = n, .args = self$param_set$values, .opts = allow_partial_matching)
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("circle", TaskGeneratorCircle)
