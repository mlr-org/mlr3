#' @title Moons Classification Task Generator
#'
#' @name mlr_task_generators_moons
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] creating two two interleaving half circles ("moons").
#'
#' @templateVar id moons
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' task = tgen("moons")$generate(200)
#' tab = task$data()
#' plot(tab$x1, tab$x2, col = tab$y)
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
    }
  ),

  private = list(
    .generate = function(n) {
      sigma = self$param_set$values$sigma

      n1 = n %/% 2L
      n2 = n - n1
      mu = c(rep(-2.5, n1), rep(2.5, n2))
      x = c(runif(n1, 0, pi), runif(n2, pi, 2 * pi))

      tab = data.table(
        y = factor(c(rep("+", n1), rep("-", n2))),
        x1 = 5  * cos(x) + rnorm(n, mean = mu, sd = sigma),
        x2 = 10 * sin(x) + rnorm(n, mean = mu, sd = sigma)
      )

      TaskClassif$new(sprintf("%s_%i", self$id, n), tab, target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("moons", TaskGeneratorMoons)
