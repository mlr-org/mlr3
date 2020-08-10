#' @title Moons Classification Task Generator
#'
#' @name mlr_task_generators_moons
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] creating two interleaving half circles ("moons") as
#' binary classification problem.
#'
#' @templateVar id moons
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("moons")
#' plot(generator, n = 200)
#'
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorMoons = R6Class("TaskGeneratorMoons",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("sigma", lower = 0, default = 1, tags = "required")
      ))
      ps$values = list(sigma = 1)

      super$initialize(id = "moons", task_type = "classif", param_set = ps,
        man = "mlr3::mlr_task_generators_moons")
    },

    #' @description
    #' Creates a simple plot of generated data.
    #' @param n (`integer(1)`)\cr
    #'   Number of samples to draw for the plot. Default is 200.
    #' @param pch (`integer(1)`)\cr
    #'   Point char. Passed to [plot()].
    #' @param ... (any)\cr
    #'   Additional arguments passed to [plot()].
    plot = function(n = 200, pch = 19L, ...) {
      tab = private$.generate_obj(n)
      plot(tab$x1, tab$x2, data = tab, pch = pch, col = tab$y, ...)
    }
  ),

  private = list(
    .generate_obj = function(n) {
      sigma = self$param_set$values$sigma

      n1 = n %/% 2L
      n2 = n - n1
      mu = c(rep(-2.5, n1), rep(2.5, n2))
      x = c(runif(n1, 0, pi), runif(n2, pi, 2 * pi))

      data.table(
        y = factor(rep(c("A", "B"), c(n1, n2)), levels = c("A", "B")),
        x1 = 5  * cos(x) + rnorm(n, mean = mu, sd = sigma),
        x2 = 10 * sin(x) + rnorm(n, mean = mu, sd = sigma)
      )
    },

    .generate = function(n) {
      tab = private$.generate_obj(n)
      TaskClassif$new(sprintf("%s_%i", self$id, n), tab, target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("moons", TaskGeneratorMoons)
