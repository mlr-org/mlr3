#' @title Smiley Classification Task Generator
#'
#' @name mlr_task_generators_smiley
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the smiley task in [mlbench::mlbench.smiley()].
#'
#' @templateVar id smiley
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("smiley")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorSmiley = R6Class("TaskGeneratorSmiley",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        sd1 = p_dbl(lower = 0L),
        sd2 = p_dbl(lower = 0L)
      )

      super$initialize(id = "smiley", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_smiley")
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
      obj = invoke(mlbench::mlbench.smiley, n = n, .args = self$param_set$values)
      colnames(obj$x) = sprintf("x.%i", seq_col(obj$x))
      obj
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("smiley", TaskGeneratorSmiley)
