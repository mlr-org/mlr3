#' @title Random Walk Task Generator
#'
#' @name mlr_task_generators_random_walk
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for a random walk:
#' \eqn{
#' X_n = x_0 + \sum_{i=1}^{n} Z_j
#' }
#'
#'
#' @templateVar id random_walk
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("random_walk")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorRandomWalk = R6Class("TaskGeneratorRandomWalk",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        d = p_int(1L, default = 1L)
      )

      super$initialize(id = "random_walk", task_type = "regr", param_set = ps,
        label = "Random Walk", man = "mlr3::mlr_task_generators_random_walk")
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
      data.table(
        y = cumsum(sample(c(-1L, 1L), n, replace = TRUE)),
        x = seq.POSIXt(Sys.time(), length = n, by = "weeks")
    },

    .generate = function(n) {
      obj = private$.generate_obj(n)
      TaskRegr$new(sprintf("%s_%i", self$id, n), obj, target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("random_walk", function() TaskGeneratorRandomWalk$new())

