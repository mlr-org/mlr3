#' @title Friedman1 Regression Task Generator
#'
#' @usage NULL
#' @aliases mlr_task_generators_friedman1
#' @format [R6::R6Class] inheriting from [TaskGenerator].
#' @include TaskGenerator.R
#'
#' @section Construction:
#' ```
#' TaskGeneratorFriedman1$new()
#' mlr_task_generators$get("friedman1")
#' tgen("friedman1")
#' ```
#'
#' @description
#' A [TaskGenerator] for the friedman1 task in [mlbench::mlbench.friedman1()].
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("friedman1")$generate(10)$data()
TaskGeneratorFriedman1 = R6Class("TaskGeneratorFriedman1",
  inherit = TaskGenerator,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("sd", lower = 0L, default = 1)
      ))

      super$initialize(id = "friedman1", "regr", "mlbench", ps)
    }
  ),

  private = list(
    .generate = function(n) {
      data = invoke(mlbench::mlbench.friedman1, n = n, .args = self$param_set$values)
      colnames(data$x) = c(sprintf("important%i", 1:5), sprintf("unimportant%i", 1:5))
      data = insert_named(as.data.table(data$x), list(y = data$y))
      TaskRegr$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("friedman1", TaskGeneratorFriedman1)
