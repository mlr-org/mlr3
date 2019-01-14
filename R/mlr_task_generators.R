#' @title Dictionary of Task Generators
#'
#' @name mlr_task_generators
#' @description
#' A simple [Dictionary] storing generator functions returning a [Task].
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Task
#' @family Generator
NULL

#' @include Dictionary.R
DictionaryTaskGenerators = R6Class("DictionaryTaskGenerators",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_task_generators = DictionaryTaskGenerators$new()

TaskGenerator = R6Class("TaskGenerator",
  public = list(
    id = NULL,
    param_set = NULL,
    param_vals = NULL,
    packages = NULL,
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), packages = character(0L)) {
      self$id = assert_id(id)
      self$param_set = assert_param_set(param_set)
      self$param_vals = assert_param_vals(param_vals, param_set)
      self$packages = assert_set(packages)
    }
  )
)

TaskGeneratorXor = R6Class("TaskGeneratorXor",
  inherit = TaskGenerator,
  public = list(
    initialize = function(...) {
      param_set = ParamSet$new(list(
        ParamInt$new("d", lower = 1L)
      ))
      super$initialize(id = "xor", param_set, list(...))
    },

    generate = function(n) {
      n = assert_count(n, coerce = TRUE)
      data = invoke(mlbench::mlbench.xor, n = n, .args = self$param_vals)
      data = insert_named(as.data.table(data$x), list(class = data$classes))
      TaskClassif$new(sprintf("%s_%i", self$id, n), as_data_backend(data), target = "class")
    }
  )
)

mlr_task_generators$add("xor", TaskGeneratorXor)

TaskGenerator2DNormals = R6Class("TaskGenerator2DNormals",
  inherit = TaskGenerator,
  public = list(
    initialize = function(...) {
      param_set = ParamSet$new(list(
        ParamInt$new("cl", lower = 2L),
        ParamDbl$new("r", lower = 1L),
        ParamDbl$new("sd", lower = 0L)
      ))
      super$initialize(id = "2dnormals", param_set, list(...))
    },

    generate = function(n) {
      n = assert_count(n, coerce = TRUE)
      data = invoke(mlbench::mlbench.2dnormals, n = n, .args = self$param_vals)
      data = insert_named(as.data.table(data$x), list(class = data$classes))
      TaskClassif$new(sprintf("%s_%i", self$id, n), as_data_backend(data), target = "class")
    }
  )
)

mlr_task_generators$add("2dnormals", TaskGenerator2DNormals)

if (FALSE) {
  g = TaskGeneratorXor$new(d = 3)
  g$param_vals
  g$generate(100)

  g = TaskGenerator2DNormals$new()
  task = g$generate(1000)
  lrn = mlr_learners$get("classif.rpart")
  Experiment$new(task, lrn)$train()$predict()$score()$performance
}
