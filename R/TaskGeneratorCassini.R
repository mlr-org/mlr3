#' @title Cassini Classification Task Generator
#'
#' @name mlr_task_generators_cassini
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the cassini task in [mlbench::mlbench.cassini()].
#'
#' @templateVar id cassini
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("cassini")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorCassini = R6Class("TaskGeneratorCassini",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        relsize1 = p_int(1L, default = 2L),
        relsize2 = p_int(1L, default = 2L),
        relsize3 = p_int(1L, default = 1L)
      )

      super$initialize(id = "cassini", "classif", "mlbench", ps,
        label = "Cassini Classification", man = "mlr3::mlr_task_generators_cassini")
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
      pv = self$param_set$values
      relsize = c(pv$relsize1 %??% 2L, pv$relsize2 %??% 2L, pv$relsize3 %??% 1L)
      invoke(mlbench::mlbench.cassini, n = n, .args = list(relsize = relsize), .opts = allow_partial_matching)
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), convert_mlbench(obj), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("cassini", function() TaskGeneratorCassini$new())
