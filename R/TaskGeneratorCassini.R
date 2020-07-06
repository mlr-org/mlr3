#' @title Cassini Classification Task Generator
#'
#' @name mlr_task_generators_cassini
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the cassini task in [mlbench::mlbench.cassini()].
#'
#' @templateVar id cassini
#' @template section_dictionary_task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' tgen("cassini")$generate(10)$data()
TaskGeneratorCassini = R6Class("TaskGeneratorCassini",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("relsize1", lower = 1L, default = 2L),
        ParamInt$new("relsize2", lower = 1L, default = 2L),
        ParamInt$new("relsize3", lower = 1L, default = 1L)
      ))

      super$initialize(id = "cassini", "classif", "mlbench", ps, man = "mlr3::mlr_task_generators_cassini")
    }
  ),

  private = list(
    .generate = function(n) {
      pv = self$param_set$values
      relsize = c(pv$relsize1 %??% 2L, pv$relsize2 %??% 2L, pv$relsize3 %??% 1L)
      data = invoke(mlbench::mlbench.cassini, n = n, .args = list(relsize = relsize), .opts = allow_partial_matching)
      TaskClassif$new(sprintf("%s_%i", self$id, n), as.data.frame(data), target = "classes")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("cassini", TaskGeneratorCassini)
