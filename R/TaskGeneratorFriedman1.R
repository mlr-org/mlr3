#' @title Friedman1 Regression Task Generator
#'
#' @name mlr_task_generators_friedman1
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the friedman1 task in [mlbench::mlbench.friedman1()].
#'
#' @templateVar id friedman1
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("friedman1")
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorFriedman1 = R6Class("TaskGeneratorFriedman1",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        sd = p_dbl(0L, default = 1)
      )

      super$initialize(id = "friedman1", "regr", "mlbench", ps, man = "mlr3::mlr_task_generators_friedman1")
    }
  ),

  private = list(
    .generate = function(n) {
      obj = invoke(mlbench::mlbench.friedman1, n = n, .args = self$param_set$values)
      colnames(obj$x) = c(sprintf("important%i", 1:5), sprintf("unimportant%i", 1:5))
      data = insert_named(as.data.table(obj$x), list(y = obj$y))
      TaskRegr$new(sprintf("%s_%i", self$id, n), data, target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("friedman1", TaskGeneratorFriedman1)
