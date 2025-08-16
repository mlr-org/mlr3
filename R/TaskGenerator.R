#' @title TaskGenerator Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' Creates a [Task] of arbitrary size.
#' Predefined task generators are stored in the [dictionary][mlr3misc::Dictionary] [mlr_task_generators],
#' e.g. [`xor`][mlr_task_generators_xor].
#'
#' @template param_id
#' @template param_param_set
#' @template param_task_type
#' @template param_packages
#' @template param_man
#' @template param_label
#'
#' @template seealso_task_generator
#' @export
TaskGenerator = R6Class("TaskGenerator",
  inherit = Mlr3Component,
  public = list(
    #' @template field_task_type
    task_type = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, task_type, packages = character(0), param_set = ps(), additional_configuration = character(0), label, man) {
      if (!missing(label) || !missing(man)) {
        mlr3component_deprecation_msg("label and man are deprecated for TaskGenerator construction and will be removed in the future.")
      }

      super$initialize(dict_entry = id, dict_shortaccess = "tgen",
        param_set = param_set, packages = packages, additional_configuration = additional_configuration
      )

      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      cat_cli({
        msg_h = if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label)
        cli_h1("{.cls {class(self)[1L]}} ({self$id}){msg_h}")
        cli_li("Task type: {self$task_type}")
        cli_li("Packages: {.pkg {self$packages}}")
        cli_li("Parameters: {as_short_string(self$param_set$values, 1000L)}")
        cli_li("Manual: {.help {self$man}}")
      })
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

convert_mlbench = function(obj) {
  y = factor(LETTERS[as.integer(obj$classes)], levels = LETTERS[seq_len(uniqueN(obj$classes))])
  X = set_col_names(obj$x, sprintf("x%i", seq_col(obj$x)))
  insert_named(as.data.table(X), list(y = y))
}
