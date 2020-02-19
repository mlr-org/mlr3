#' @title TaskGenerator Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' Creates a [Task] of arbitrary size.
#' Predefined task generators are stored in the [mlr3misc::Dictionary] [mlr_task_generators],
#' e.g. [`xor`][mlr_task_generators_xor].
#'
#' @family TaskGenerator
#' @export
TaskGenerator = R6Class("TaskGenerator",
  public = list(
    #' @template field_id
    id = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @template field_param_set
    param_set = NULL,

    #' @template field_packages
    packages = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Create a new instance.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the learner.
    #'
    #' @param task_type (`character(1)`)\cr
    #'   Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
    #'
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   A warning is signaled by the constructor if at least one of the packages is not installed.
    #'   The packages will be loaded (not attached) via [requireNamespace()] for `$train()`/`$predict()`.
    #'
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Set of hyperparameters.
    #'
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    initialize = function(id, task_type, packages = character(), param_set = ParamSet$new(), man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      self$param_set = assert_param_set(param_set)
      self$packages = assert_set(packages)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
      self$man = assert_string(man, na.ok = TRUE)

      check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for TaskGenerator '%s'", id))
    },

    #' @description
    #' Creates a task of type `task_type` with `n` observations, possibly using additional settings stored in `param_set`.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows to generate.
    #' @return [Task].
    generate = function(n) {
      n = assert_count(n, coerce = TRUE)
      require_namespaces(self$packages)
      private$.generate(n)
    }
  )
)
