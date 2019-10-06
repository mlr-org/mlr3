#' @title TaskGenerator Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' Creates a [Task] of arbitrary size.
#' Predefined task generators are stored in the [mlr3misc::Dictionary] [mlr_task_generators],
#' e.g. [`xor`][mlr_task_generators_xor].
#'
#' @section Construction:
#' ```
#' g = TaskGenerator$new(id, task_type, packages = character(), param_set = ParamSet$new(), man = NA_character_)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the learner.
#'
#' * `task_type` :: `character(1)`\cr
#'   Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `man` :: `character(1)`\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'
#' @section Fields:
#' All variables passed to the constructor, and additionally:
#'
#' * `task_type` :: `character(1)`\cr
#'   Stores the type of class this learner can operate on, e.g. `"classif"` or `"regr"`.
#'   A complete list of task types is stored in [`mlr_reflections$task_types$type`][mlr_reflections].
#'
#' @section Methods:
#' * `generate(n)`\cr
#'   `integer(1)` -> [Task]\cr
#'   Creates a task of type `task_type` with `n` observations, possibly using additional settings stored in `param_set`.
#'
#' * `help()`\cr
#'   () -> `NULL`\cr
#'   Opens the corresponding help page referenced by `$man`.
#'
#' @family TaskGenerator
#' @export
TaskGenerator = R6Class("TaskGenerator",
  public = list(
    id = NULL,
    task_type = NULL,
    param_set = NULL,
    packages = NULL,
    man = NULL,

    initialize = function(id, task_type, packages = character(), param_set = ParamSet$new(), man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      self$param_set = assert_param_set(param_set)
      self$packages = assert_set(packages)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
      self$man = assert_string(man, na.ok = TRUE)
    },

    generate = function(n) {
      n = assert_count(n, coerce = TRUE)
      require_namespaces(self$packages)
      private$.generate(n)
    }
  )
)
