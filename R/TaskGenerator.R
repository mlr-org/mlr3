#' @title Task Generators
#'
#' @name TaskGenerator
#' @format [R6::R6Class] object.
#' @description
#' Creates a [Task] of arbitrary size.
#' Predefined task generators are stored in [mlr_task_generators].
#'
#' @section Usage:
#' ```
#' # Construction
#' g = TaskGenerator$new(id, task_type, packages = character(0L), param_set = ParamSet$new(), param_vals = list())
#'
#' g$id
#' g$task_type
#' g$param_set
#' g$generate(n)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`): Identifier for this object.
#' * `task_type` (`character(1)`): Type of the task generated. E.g., `"classif"` or `"regr"`.
#' * `packages` (`character()`]: Set of required packages.
#' * `param_set` ([paradox::ParamSet]): Set of hyperparameters.
#' * `param_vals` (named `list()`): List of hyperparameter settings.
#' * `n` (`integer(1)`): Number of observations to generate.
#'
#' @section Details:
#' * `$id` (`character(1)`) stores the identifier of the object.
#' * `$task_type` (`character()`) stores the type of generated task.
#' * `$packages` (`character()`) stores the names of required packages.
#' * `$param_set` ([paradox::ParamSet]) describes the available hyperparameter and possible settings.
#' * `$generate(n)` creates a task of type `task_type` with `n` observations.
#'
#' @family TaskGenerators
NULL

#' @export
TaskGenerator = R6Class("TaskGenerator",
  public = list(
    id = NULL,
    task_type = NULL,
    param_set = NULL,
    packages = NULL,
    initialize = function(id, task_type, packages = character(0L), param_set = ParamSet$new(), param_vals = list()) {
      self$id = assert_id(id)
      self$param_set = assert_param_set(param_set)
      self$param_set$values = param_vals
      self$packages = assert_set(packages)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
    },

    generate = function(n) {
      n = assert_count(n, coerce = TRUE)
      require_namespaces(self$packages)
      private$.generate(n)
    }
  )
)
